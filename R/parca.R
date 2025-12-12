#' Retrieve a BDP parcel geometry from BDP
#'
#' @param idu `character` Cadastral parcel identifier.
#'
#' @return An `sf` object containing the parcel geometry.
#' @export
get_parca_bdp <- function(idu){
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

  return(bdp)
}

#' Retrieve a cadastral parcel geometry from Etalab
#'
#' @param idu `character` Cadastral parcel identifier.
#'
#' @return An `sf` object containing the parcel geometry.
#' @export
get_parca_etalab <- function(idu){
  url <- "https://cadastre.data.gouv.fr/bundler/cadastre-etalab/communes/%s/geojson/parcelles"
  idu_parts <- idu_split(idu)

  urls <- sprintf(url, unique(idu_parts$insee))

  etalab_geom <- lapply(urls, read_sf)
  etalab_geom <- do.call(rbind, etalab_geom)
  etalab_geom$prefixe <- pad_left(etalab_geom$prefixe, 3)
  etalab_geom$section <- pad_left(etalab_geom$section, 2)
  etalab_geom$numero <- pad_left(etalab_geom$numero, 4)

  etalab_geom <- etalab_geom[etalab_geom$id %in% idu,]

  names(etalab_geom)[names(etalab_geom) == "id"] <- "idu"
  return(etalab_geom)
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

  names(lieux_dits)[names(lieux_dits) == "nom"] <- "lieu_dit"

  return(lieux_dits)
}

#' Download and format cadastral parcel(s)
#'
#' Downloads parcel geometries from the Etalab cadastre API and optionally
#' replaces them with higher-quality BDP geometries when available.
#' Lieux-dits and administrative attributes (commune, département, région)
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
#' The BDP ("Base de Données Parcellaire") is an older IGN product that is no
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

  if (bdp_geom){
    if (verbose) cli::cli_alert_info("Downloading BDP from IGN...")
    tryCatch({
      bdp <- get_parca_bdp(idu)
      idx <- match(etalab$idu, bdp$idu)
      etalab$geometry[!is.na(idx)] <- bdp$geometry[idx[!is.na(idx)]]
      if (verbose) {
        valid_bdp_idu <- intersect(etalab$idu, bdp$idu)
        if (length(valid_bdp_idu) > 0) {
          cli::cli_alert_success(
            "{length(valid_bdp_idu)} of {length(etalab$idu)} ETALAB geom successfully replaced with BDP geom."
          )
        }
      }
    }, error = \(e) cli::cli_alert_warning("BDP not available, ETALAB geom only is used.")
    )
  }

  missing_idu <- setdiff(idu, etalab$idu)
  if (length(missing_idu) > 0) {
      cli::cli_warn("Geometry not found for {length(missing_idu)} IDU(s): {.val {missing_idu}}")
  }


  # Ajout des lieux dits
  if (lieu_dit){
    if (verbose) cli::cli_alert_info("Downloading and joining Lieux dits...")
    lieux_dits <- get_lieux_dits(idu)
    etalab <- sf::st_join(etalab, lieux_dits[,"lieu_dit"], largest = TRUE) |>
      suppressWarnings()
    if (verbose) cli::cli_alert_success("Lieux dits joined.")
  }else{
    etalab$lieu_dit <- NA
  }

  # Add COG info
  raw_parca <- etalab |>
    merge(happign::com_2025[, c("COM", "NCC_COM", "DEP")], by.x = "commune", by.y = "COM") |>
    merge(happign::dep_2025[, c("DEP", "NCC_DEP", "REG")], all.x = TRUE) |>
    merge(happign::reg_2025[, c("REG", "NCC_REG")], all.x = TRUE)

  raw_parca <- seq_normalize(raw_parca, "raw_parca")

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

  # read matrice
  m <- read_matrice(dirname)

  idu <- seq_field("idu")$name
  insee <- seq_field("insee")$name
  prefix <- seq_field("prefix")$name
  section <- seq_field("section")$name
  numero <- seq_field("numero")$name
  lieu_dit <- seq_field("lieu_dit")$name
  tx_boisee <- seq_field("tx_boisee")$name

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
  seq_parca <- merge(m, raw_parca, by = "IDU", all.x = TRUE) |>
    sf::st_as_sf() |>
    sf::st_transform(2154)

  seq_parca[[lieu_dit]] <- ifelse(
    is.na(seq_parca[[lieu_dit]]),
    seq_parca$RAW_LIEU_DIT,
    seq_parca[[lieu_dit]]
  )

  seq_parca[[tx_boisee]] <- ifelse(seq_parca[[tx_boisee]] >= 0.5, "BOISEE", "NON BOISEE")

  # format parca
  seq_parca <- seq_normalize(seq_parca, "parca")

  # write parca
  parca_path <- seq_write(
    seq_parca,
    "v.seq.parca.poly",
    dirname = dirname,
    verbose = verbose,
    overwrite = overwrite
  )

  return(invisible(parca_path))
}

#' Check inconsistencies between cadastral and cartographic areas
#'
#' This function compares cadastral areas (`CONTENANCE`, in m²) with
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
#'   `AREA_ATOL` (absolute difference in m²),
#'   `AREA_RTOL` (relative difference),
#'   `AREA_CHECK` (logical flag).
#'
#' @importFrom sf st_area
#'
#' @export
parca_check_area <- function(parca,
                             atol = 500,
                             rtol = 0.05,
                             verbose = TRUE) {

  surf_cad <- seq_field("surf_cad")$name

  parca$AREA_SIG <- as.numeric(sf::st_area(parca)) / 10000
  parca$AREA_ATOL <- abs(parca[[surf_cad]] - parca$AREA_SIG * 10000)
  parca$AREA_RTOL <- parca$AREA_ATOL / parca[[surf_cad]]
  parca$AREA_CHECK <- (parca$AREA_ATOL >= atol & parca$AREA_RTOL >= rtol)

  bad_idu <- parca$IDU[parca$AREA_CHECK]
  n_bad_idu <- length(bad_idu)
  if (n_bad_idu > 0) {
    cli::cli_warn("Detected {n_bad_idu} inconsistent IDU{?s}: {.val {bad_idu}}")
  }

  if (verbose){
    if (n_bad_idu == 0){
      cli::cli_alert_success("No inconsistencies detected.")
    }
  }

  return(invisible(parca))
}
