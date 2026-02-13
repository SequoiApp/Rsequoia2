#' Retrieve a BDP parcel geometry from BDP
#'
#' @param idu `character` Cadastral parcel identifier.
#'
#' @return An `sf` object containing the parcel geometry.
#' @export
get_parca_bdp <- function(idu){

  if (is.null(idu) || length(idu) == 0) {
    cli::cli_abort("{.arg idu} must be a non-empty character vector.")
  }

  if (!inherits(idu, "character")) {
    cli::cli_abort("{.arg idu} must be {.cls character}, not {.cls {class(idu)}}.")
  }

  idu_parts <- idu_split(idu)

  bdp <- happign::get_apicarto_cadastre(
    idu_parts$insee,
    code_abs = idu_parts$prefix,
    section = idu_parts$section,
    numero = idu_parts$numero,
    type = "parcelle",
    source = "bdp") |>
    suppressWarnings()

  bdp$idu <- idu_build(bdp$code_dep, bdp$code_com, bdp$com_abs, bdp$section, bdp$numero)

  source <- seq_field("source")$name
  bdp[[source]] <- "bdp"

  return(bdp)
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

  url <- "https://cadastre.data.gouv.fr/bundler/cadastre-etalab/communes/%s/geojson/parcelles"
  idu <- unique(idu)
  idu_parts <- idu_split(idu)

  urls <- sprintf(url, unique(idu_parts$insee))

  etalab <- lapply(urls, read_sf)
  etalab <- do.call(rbind, etalab)

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
get_lieux_dits <- function(idu){
  idu_parts <- idu_split(idu)
  insee <- unique(idu_parts$insee)
  urls <- paste0("https://cadastre.data.gouv.fr/bundler/cadastre-etalab/communes/",
                 insee,"/geojson/lieux_dits")

  lieux_dits <- lapply(urls, sf::read_sf)
  lieux_dits <- do.call(rbind, lieux_dits)

  names(lieux_dits)[names(lieux_dits) == "nom"] <- seq_field("locality")$name

  return(lieux_dits)
}

#' Download and format cadastral parcel(s)
#'
#' Downloads parcel geometries from the Etalab cadastre API and optionally
#' replaces them with higher-quality BDP geometries when available.
#' Lieux-dits and administrative attributes (commune, departement, region)
#' are joined automatically.
#'
#' @param idu `character` Vector of IDU identifiers.
#' @param bdp_geom `logical` If `TRUE`, replace Etalab geometries with BDP when possible.
#' @param lieu_dit `logical` If `TRUE`, attach lieux-dits to each parcel.
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @importFrom stats setNames
#'
#' @details
#' The BDP ("Base de Donnees Parcellaire") is an older IGN product that is no
#' longer updated. It was originally derived from Etalab cadastral parcels,
#' but the geometries were manually corrected by IGN to better match reality
#'
#' Cadastral limits and the real terrain rarely match perfectly. Using BDP may
#' therefore improve spatial accuracy, but cannot guarantee exact
#' correspondence with legal cadastral boundaries.
#'
#' @return An `sf` object of parcels with harmonized attributes.
#' @export
get_parca <- function(idu, bdp_geom = FALSE, lieu_dit = FALSE, verbose = TRUE){
  idu <- unique(idu)
  etalab <- get_parca_etalab(idu)

  idu_field <- seq_field("idu")$name

  if (bdp_geom){
    if (verbose) cli::cli_alert_info("Downloading BDP from IGN...")
    tryCatch({
      bdp <- get_parca_bdp(idu)
      idx <- match(etalab[[idu_field]], bdp$idu)
      etalab$geometry[!is.na(idx)] <- bdp$geometry[idx[!is.na(idx)]]

      source <- seq_field("source")$name
      etalab[[source]][!is.na(idx)] <- bdp[[source]][idx[!is.na(idx)]]

      if (verbose) {
        valid_bdp_idu <- intersect(etalab[[idu_field]], bdp$idu)
        if (length(valid_bdp_idu) > 0) {
          cli::cli_alert_success(
            "{length(valid_bdp_idu)} of {length(etalab[[idu_field]])} ETALAB geom successfully replaced with BDP geom."
          )
        }
      }
    }, error = \(e) cli::cli_warn("BDP not available, ETALAB geom only is used.")
    )
  }

  # Ajout des lieux dits
  if (lieu_dit){
    if (verbose) cli::cli_alert_info("Downloading and joining Lieux dits...")
    locality <- seq_field("locality")$name
    lieux_dits <- get_lieux_dits(idu)
    etalab <- sf::st_join(etalab, lieux_dits[locality], largest = TRUE, suffix = c("_XX", "")) |>
      suppressWarnings()
    etalab[[paste0(locality, "_XX")]] <- NULL
    if (verbose) cli::cli_alert_success("Lieux dits joined.")
  }

  raw_parca <- seq_normalize(etalab, "parca") |>
    sf::st_transform(2154)

  return(invisible(raw_parca))
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
#' **`bdp_geom`**
#' The use and behaviour of `bdp_geom` are described in [Rsequoia2::get_parca()].
#'
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
    bdp_geom = TRUE,
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

  # retrieve parca
  raw_parca <- get_parca(
    m[[idu]],
    bdp_geom = bdp_geom,
    lieu_dit = have_empty_lieu_dit,
    verbose = verbose
  )

  names(raw_parca)[names(raw_parca) == lieu_dit] <- "RAW_LIEU_DIT"

  # merge raw_parca with matrice
  seq_parca <- merge(m, raw_parca, by = idu, all.x = TRUE, suffixes = c("", ".raw_parca")) |>
    sf::st_as_sf() |>
    sf::st_transform(2154)

  seq_parca[[lieu_dit]] <- ifelse(
    is.na(seq_parca[[lieu_dit]]),
    seq_parca$RAW_LIEU_DIT,
    seq_parca[[lieu_dit]]
  )

  # format parca
  seq_parca <- seq_parca |>
    parca_check_area(verbose = verbose) |>
    seq_normalize("parca")

  seq_parca[[identifier]] <- id

  # write parca
  parca_path <- seq_write(
    seq_parca,
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
    sf::st_drop_geometry(seq_parca),
    "matrice",
    dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  if (verbose) {
    cli::cli_alert_success(
      "UA also saved as {.file {basename(secure_m_path)}} for safety."
    )
  }

  return(invisible(parca_path))
}

#' Check inconsistencies between cadastral and cartographic areas
#'
#' This function compares cadastral areas (in m²) with
#' cartographic areas computed from geometry ([sf::st_area()]).
#'
#' @param parca `sf` Object from [Rsequoia2::seq_parca()] representing cadastral
#' parcels.
#' @param atol `numeric` Absolute difference tolerance in m². Default to `500m`.
#' @param rtol `numeric` Relative difference. Default to `0.05`
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @return The input `parca` with four additional fields:
#'   `AREA_SIG` (cartographic area in ha),
#'   `ATOL_AREA` (absolute difference in m²),
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

  parca[[gis_area]] <- as.numeric(sf::st_area(parca)) / 10000
  parca$ATOL_AREA <- abs(parca[[cad_area]] - parca[[gis_area]])
  parca$RTOL_AREA <- parca$ATOL_AREA / parca[[cad_area]]
  parca$CHECK_AREA <- (parca$ATOL_AREA >= atol/10000 & parca$RTOL_AREA >= rtol)

  bad_idu <- parca$IDU[parca$CHECK_AREA]
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

