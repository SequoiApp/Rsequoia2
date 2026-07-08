#' Read Etalab cadastral data
#'
#' Downloads and combines Etalab cadastral GeoJSON data for one or more
#' communes.
#'
#' @param insee `character`. INSEE commune code(s).
#' @param layer `character`. Etalab layer to read. Either `"parcelles"` or
#'   `"lieux_dits"`.
#'
#' @return An `sf` object containing the requested Etalab layer.
#'
#' @keywords internal
read_etalab <- function(insee, layer = c("parcelles", "lieux_dits")) {
  layer <- match.arg(layer)
  insee <- unique(insee)

  url <- sprintf(
    "https://cadastre.data.gouv.fr/bundler/cadastre-etalab/communes/%s/geojson/%s",
    insee,
    layer
  )

  do.call(rbind, lapply(url, sf::read_sf))
}

#' Retrieve a cadastral parcel geometry from Etalab
#'
#' @param idu `character` Cadastral parcel identifier.
#'
#' @return An `sf` object containing the parcel geometry.
#' @export
get_parca_etalab <- function(idu){

  if (is.null(idu) || length(idu) == 0) {
    cli::cli_abort("{.arg idu} must be a non-empty vector.")
  }

  if (!inherits(idu, "character")) {
    cli::cli_abort("{.arg idu} must be {.cls character}, not {.cls {class(idu)}}.")
  }

  idu_parts <- idu_split(idu)
  etalab <- read_etalab(unique(idu_parts$insee), "parcelles")

  invalid_idu <- !idu %in% etalab$id
  has_invalid_idu <- sum(invalid_idu) > 0
  if (has_invalid_idu){
    cli::cli_abort("Invalid idu detected: {.vals {idu[invalid_idu]}}")
  }

  etalab <- etalab[etalab$id %in% idu,]

  etalab$idu <- etalab$id
  etalab$prefixe <- pad_left(etalab$prefixe, 3)
  etalab$section <- pad_left(etalab$section, 2)
  etalab$numero <- pad_left(etalab$numero, 4)
  etalab$insee <- pad_left(etalab$commune, 5)
  etalab$com_code  <- substr(etalab$commune, 3, 5)
  etalab$contenance <- etalab$contenance / 10000

  # Add COG info
  cog <- get_cog(verbose = FALSE)
  etalab <- etalab |>
    merge(cog$com[, c("COM", "NCC_COM", "DEP")], by.x = "insee", by.y = "COM") |>
    merge(cog$dep[, c("DEP", "NCC_DEP", "REG")], all.x = TRUE) |>
    merge(cog$reg[, c("REG", "NCC_REG")], all.x = TRUE)

  source <- seq_field("source")$name
  etalab[[source]] <- "etalab"

  etalab <- seq_normalize(etalab, "raw_parca")

  return(etalab)
}

#' Retrieve a "Lieud-dit" from Etalab
#'
#' @param idu `character` Cadastral parcel identifier.
#'
#' @return An `sf` object containing the parcel geometry.
#' @export
get_lieux_dits <- function(idu) {
  idu <- check_idu(idu)

  idu_parts <- idu_split(idu)
  etalab <- read_etalab(unique(idu_parts$insee), "lieux_dits")

  names(lieux_dits)[names(lieux_dits) == "nom"] <- seq_field("locality")$name

  lieux_dits
}

#' Download and format cadastral parcel(s)
#'
#' Downloads parcel geometries from the Etalab cadastre API.
#' Lieux-dits and administrative attributes (commune, departement, region)
#' are joined automatically.
#'
#' @param idu `character` Vector of IDU identifiers.
#' @param lieu_dit `logical` If `TRUE`, attach lieux-dits to each parcel.
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @importFrom stats setNames
#'
#'
#' @return An `sf` object of parcels with harmonized attributes.
#' @export
get_parca <- function(idu, lieu_dit = FALSE, verbose = TRUE){
  idu <- check_idu(idu)
  etalab <- get_parca_etalab(idu)

  idu_field <- seq_field("idu")$name

  # Ajout des lieux dits
  if (lieu_dit){
    if (verbose) cli::cli_alert_info("Downloading and joining Lieux dits...")
    locality <- seq_field("locality")$name
    lieux_dits <- get_lieux_dits(idu)
    etalab <- sf::st_join(etalab, lieux_dits[locality], largest = TRUE, suffix = c("_drop", "")) |>
      suppressWarnings()
    etalab[[paste0(locality, "_drop")]] <- NULL
    if (verbose) cli::cli_alert_success("Lieux dits joined.")
  }

  raw_parca <- seq_normalize(etalab, "parca") |>
    sf::st_transform(2154)

  if (verbose) cli::cli_alert_success("Parca successfuly downloaded.")

  return(invisible(raw_parca))
}

#' Remove fully overlapping cadastral parcels
#'
#' Corrects PARCA geometries when one parcel fully contains another parcel.
#'
#' When full overlaps are detected, the function downloads the complete Etalab
#' cadastre for the affected communes and subtracts the official inner parcels
#' from each containing parcel.
#'
#' @param parca An `sf` polygon object containing cadastral parcels.
#' @param verbose `logical`. If `TRUE`, display correction messages.
#'
#' @return An `sf` object with corrected geometries.
#'
#' @details
#' Full overlaps are detected with `sf::st_contains_properly()`. Only parcels
#' containing other parcels are modified; all other geometries are kept
#' unchanged.
#'
#' @export
parca_remove_full_overlap <- function(parca, verbose = TRUE){

  covers <- sf::st_contains_properly(parca, sparse = FALSE)
  nested_pairs <- as.data.frame(which(covers, arr.ind = TRUE))
  names(nested_pairs) <- c("outer", "inner")

  if (nrow(nested_pairs) == 0) {
    if (verbose){
      cli::cli_alert_success("No fully overlapping parcels detected")
    }
    return(parca)
  }

  inner_idu <- parca[nested_pairs$inner, ][[seq_field("idu")$name]]
  outer_idu <- parca[nested_pairs$outer, ][[seq_field("idu")$name]]

  if (verbose) {
    cli::cli_alert_info(
      "Fully overlapping parcels detected; applying automatic correction."
    )

    for (i in seq_along(inner_idu)) {
      cli::cli_bullets(c(
        ">" = "{.val {inner_idu[i]}} is fully contained in {.val {outer_idu[i]}}"
      ))
    }
  }

  insee_to_download <- unique(idu_split(outer_idu)$insee)
  cadastre <- read_etalab(insee_to_download, "parcelles")
  cadastre <- sf::st_transform(cadastre, sf::st_crs(parca))

  all_prf_on_outer <- sf::st_contains_properly(parca, cadastre, sparse = FALSE)
  full_nested_pairs <- as.data.frame(which(all_prf_on_outer, arr.ind = TRUE))
  names(full_nested_pairs) <- c("outer", "inner")

  geom <- sf::st_geometry(parca)
  full_geom <- sf::st_geometry(cadastre)
  inner_by_outer <- split(full_nested_pairs$inner, full_nested_pairs$outer)
  outer_rows <- as.integer(names(inner_by_outer))

  geom[outer_rows] <- lapply(outer_rows, function(i) {
    inner_rows <- inner_by_outer[[as.character(i)]]
    sf::st_difference(geom[i], sf::st_union(full_geom[inner_rows]))[[1]]
  })

  parca <- sf::st_set_geometry(parca, geom)
  return(parca)
}

#' Download, enrich and write cadastral geometries
#'
#' This function reads the `matrice.xlsx` file from a project directory,
#' downloads the geometry of each parcel, enriches the dataset with information
#' from the matrice, and writes the resulting layer to disk.
#'
#' The resulting object is returned invisibly as an `sf` polygons layer.
#' The output file is automatically written into the working directory defined
#' by `dirname`.
#'
#' @inheritParams create_matrice
#' @inheritParams get_parca
#'
#' @details
#' **Automatic "lieu-dit" completion**
#' If the function detects rows in the matrice where the field `"LIEU_DIT"`
#' is missing, the corresponding "lieu-dit" values will be downloaded
#' automatically and added to the output.
#' Existing `"LIEU_DIT"` values are **never overwritten**.
#'
#' @return An `sf` object
#'
#' @export
seq_parca <- function(
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE){

  layer_info <- seq_layer("parca")
  filename <- layer_info$filename
  key <- layer_info$key

  path <- list.files(dirname, filename, recursive = T, full.names = T)

  if (length(path) > 0 && !overwrite) {
    cli::cli_warn(
      "{.file {basename(path)}} already exists. Use {.arg overwrite = TRUE} to replace it."
      )
    return(invisible(setNames(path, key)))
  }

  # read matrice
  m <- read_matrice(dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(m[[identifier]])

  idu <- seq_field("idu")$name
  insee <- seq_field("insee")$name
  prefix <- seq_field("prefix")$name
  section <- seq_field("section")$name
  numero <- seq_field("number")$name
  lieu_dit <- seq_field("locality")$name

  # create idu
  m[[idu]] <- paste0(
    pad_left(m[[insee]], 5),
    pad_left(m[[prefix]], 3),
    pad_left(m[[section]], 2),
    pad_left(m[[numero]], 4)
  )

  # check empty lieudit in matrice
  have_empty_lieu_dit <- any(is.na(m[[lieu_dit]]))

  if (verbose) cli::cli_h2("DOWNLOADING PARCA")
  parca <- get_parca(
    m[[idu]],
    lieu_dit = have_empty_lieu_dit,
    verbose = verbose
  )

  names(parca)[names(parca) == lieu_dit] <- "RAW_LIEU_DIT"

  # merge raw_parca with matrice
  parca <- merge(m, parca, by = idu, all.x = TRUE, suffixes = c("", ".raw_parca")) |>
    sf::st_as_sf() |>
    sf::st_transform(2154)

  parca[[lieu_dit]] <- ifelse(
    is.na(parca[[lieu_dit]]),
    parca$RAW_LIEU_DIT,
    parca[[lieu_dit]]
  )

  # format parca
  parca <- parca |>
    seq_normalize("parca")

  parca[[identifier]] <- id

  if (verbose) cli::cli_h2("CLEANING PARCA")
  parca_clean <- parca_remove_full_overlap(parca, verbose = verbose)

  if (verbose) cli::cli_h2("CHECKING PARCA")
  parca_clean <- parca_check_area(parca_clean, verbose = verbose)

  if (verbose) cli::cli_h2("SAVING PARCA & MATRICE")

  parca_path <- seq_write(
    parca,
    "v.cad.etalab.poly",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  parca_clean_path <- seq_write(
    parca_clean,
    "v.seq.parca.poly",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  # rewrite matrice properly
  date_str <- format(Sys.time(), "%Y%m%dT%H%M%S")
  m_path <- list.files(
    dirname,
    pattern = "_matrice\\.xlsx$",
    ignore.case = TRUE,
    full.names = TRUE,
    recursive = TRUE
  )
  secure_m_path <- sprintf(
    "%s_%s.%s",
    tools::file_path_sans_ext(m_path),
    date_str,
    tools::file_ext(m_path)
  )
  file.rename(m_path, secure_m_path)
  seq_write(
    sf::st_drop_geometry(parca_clean),
    "matrice",
    dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  if (verbose) {
    cli::cli_alert_success(
      "{.file _matrice.xlsx} also saved as {.file {basename(secure_m_path)}} for safety."
    )
  }

  return(invisible(c(parca = parca_path, parca_clean = parca_clean_path)))
}

#' Check inconsistencies between cadastral and cartographic areas
#'
#' This function compares cadastral areas (in m2) with
#' cartographic areas computed from geometry ([sf::st_area()]).
#'
#' @param parca `sf` Object from [Rsequoia2::seq_parca()] representing cadastral
#' parcels.
#' @param atol `numeric` Absolute difference tolerance in m2. Default to `500m`.
#' @param rtol `numeric` Relative difference. Default to `0.05`
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @return The input `parca` with four additional fields:
#'   `AREA_SIG` (cartographic area in ha),
#'   `ATOL_AREA` (absolute difference in m2),
#'   `RTOL_AREA` (relative difference),
#'   `CHECK_AREA` (logical flag).
#'
#' @importFrom sf st_area
#'
#' @export
parca_check_area <- function(
    parca,
    atol = 500,
    rtol = 0.05,
    verbose = TRUE){

  cad_area <- seq_field("cad_area")$name
  gis_area <- seq_field("gis_area")$name
  idu <- seq_field("idu")$name

  no_cad_area <- is.na(parca[[cad_area]])
  if (any(no_cad_area)){
    no_cad_idu <- parca[no_cad_area, ][[idu]]
    cli::cli_bullets(c(
      "x" =  "Missing cadastral area for {length(no_cad_idu)} parcel{?s}: {.val {no_cad_idu}}",
      ">" = "{.field {cad_area}} need to be manually filled in PARCA layer."
    ))
  }

  parca[[gis_area]] <- as.numeric(sf::st_area(parca)) / 10000
  parca$ATOL_AREA <- abs(parca[[cad_area]] - parca[[gis_area]])
  parca$RTOL_AREA <- parca$ATOL_AREA / parca[[cad_area]]
  parca$CHECK_AREA <- (parca$ATOL_AREA >= atol/10000 & parca$RTOL_AREA >= rtol)

  bad_idu <- parca[[idu]][parca$CHECK_AREA] |> stats::na.exclude()
  n_bad_idu <- length(bad_idu)

  if (n_bad_idu > 0) {
    cli::cli_warn(
      "Detected {n_bad_idu} IDU{?s} with area inconsistencies (cadastre vs GIS): {.val {bad_idu}}"
    )
  }

  if (verbose){
    if (n_bad_idu == 0){
      cli::cli_alert_success("No area inconsistencies (cadastre vs GIS) detected.")
    }
  }

  parca$ATOL_AREA <- NULL
  parca$RTOL_AREA <- NULL

  return(invisible(parca))
}

