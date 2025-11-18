#' Retrieve a BDP parcel geometry from BDP
#'
#' @param idu `character` Cadastral parcel identifier.
#'
#' @return An `sf` object containing the parcel geometry.
#' @export
get_parca_bdp <- function(idu){
  idu_parts <- idu_split(idu)

  bdp_geom <- happign::get_apicarto_cadastre(
    idu_parts$insee,
    code_abs = idu_parts$prefix,
    section = idu_parts$section,
    numero = idu_parts$numero,
    type = "parcelle",
    source = "bdp") |>
    transform(idu = idu)

  return(bdp_geom)
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
get_parca <- function(idu, bdp_geom = TRUE, lieu_dit = FALSE, verbose = TRUE){
  idu <- unique(idu)
  etalab <- get_parca_etalab(idu)

  if (bdp_geom){
    if (verbose) cli::cli_alert_info("Downloading BDP from IGN...")
    bdp <- get_parca_bdp(idu)
    idx <- match(etalab$idu, bdp$idu)
    etalab$geometry[!is.na(idx)] <- bdp$geometry[idx[!is.na(idx)]]
    if (verbose) {
      missing_bdp_idu <- intersect(etalab$idu, bdp$idu)
      if (length(missing_bdp_idu) > 0) {
        cli::cli_alert_success("{length(missing_bdp_idu)} IDU geometries replace with BDP geom.")
      }
    }
  }

  if (verbose) {
    missing_idu <- setdiff(idu, etalab$idu)
    if (length(missing_idu) > 0) {
      cli::cli_alert_warning("Geometry not found for {length(missing_idu)} IDU(s).")
      cli::cli_ul(missing_idu)
    }
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
  parca <- etalab |>
    merge(happign::com_2025[, c("COM", "NCC_COM", "DEP")], by.x = "commune", by.y = "COM") |>
    merge(happign::dep_2025[, c("DEP", "NCC_DEP", "REG")], all.x = TRUE) |>
    merge(happign::reg_2025[, c("REG", "NCC_REG")], all.x = TRUE) |>
    subset(select = c(
      "idu", "NCC_REG","REG", "NCC_DEP", "DEP", "NCC_COM", "commune",
      "prefixe", "section", "numero", "lieu_dit", "contenance", "geometry")
      ) |>
    setNames(
      c(
        "IDU", "REG_NOM", "REG_NUM", "DEP_NOM", "DEP_NUM", "COM_NOM", "COM_NUM",
        "PREFIXE", "SECTION", "NUMERO", "LIEU_DIT","CONTENANCE", "geometry"
      )
    )

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
seq_get_parca <- function(
    dirname = ".",
    bdp_geom = TRUE,
    verbose = TRUE,
    overwrite = FALSE){

  m <- read_matrice(dirname)
  m$IDU <- paste0(
    pad_left(m$INSEE, 5),
    pad_left(m$PREFIXE, 3),
    pad_left(m$SECTION, 2),
    pad_left(m$NUMERO, 4)
    )

  have_empty_lieu_dit <- any(is.na(m$LIEU_DIT))
  raw_parca <- get_parca(
    m$IDU,
    bdp_geom = bdp_geom,
    lieu_dit = have_empty_lieu_dit,
    verbose = verbose
  )

  seq_parca <- m[, c("IDU", "IDENTIFIANT", "PROPRIETAIRE", "LIEU_DIT", "TX_BOISEE")] |>
    merge(raw_parca, by = "IDU", all.x = TRUE)

  seq_parca$OCCUP_SOL <- ifelse(seq_parca$TX_BOISEE > 0.5, "BOISEE", "NON BOISEE")
  seq_parca$LIEU_DIT <- ifelse(
    is.na(seq_parca$LIEU_DIT.x), seq_parca$LIEU_DIT.y, seq_parca$LIEU_DIT.x
    )

  seq_parca <- seq_parca[, c(
    "IDU", "IDENTIFIANT", "PROPRIETAIRE", "REG_NOM", "REG_NUM", "DEP_NOM",
    "DEP_NUM", "COM_NOM", "COM_NUM", "PREFIXE", "SECTION", "NUMERO", "LIEU_DIT",
    "OCCUP_SOL", "CONTENANCE", "geometry")] |>
    sf::st_as_sf() |>
    sf::st_transform(2154)

  parca_path <- seq_write(
    seq_parca,
    "v.seq.parca.poly",
    dirname = dirname,
    verbose = verbose,
    overwrite = overwrite
  )

  return(seq_parca)
}

#' Check inconsistencies between cadastral and cartographic areas in a parca object
#'
#' This function evaluates discrepancies between cadastral areas (`SURF_CA`) and
#' cartographic areas computed from geometry (`st_area`) within a
#' parca object containing cadastral parcels.
#'
#' Discrepancies are assessed according to two criteria: an absolute threshold
#' expressed in square meters, and a relative threshold expressed as a percentage.
#' A parcel is considered inconsistent when both thresholds are exceeded.
#'
#' @param parca An `sf` object representing cadastral parcels.
#' It must contain the fields `IDU` (cadastral identifier)
#' and `SURF_CA` (cadastral area in hectares).
#' @param difference_threshold `integer`, default `500`.
#' Absolute difference threshold (in square meters) above which a parcel is flagged.
#' @param percent_threshold `numeric`, default `0.05`.
#' Relative difference threshold (expressed as a fraction) above which a parcel is flagged.
#' @param verbose `logical`, default `TRUE`.
#' Whether to display informational messages using the `cli` package.
#'
#' @return An `sf` object containing only the parcels identified as inconsistent.
#' If no inconsistencies are found, an empty `sf` (with the same structure) is returned.
#'
#' @details
#' For each parcel, the function computes:
#' * the cartographic area in hectares (from `st_area`)
#' * the absolute difference between cadastral and cartographic areas (converted to m2)
#' * the relative difference (absolute difference divided by cadastral area in m2)
#'
#' A parcel is retained in the output only if **both** thresholds
#' (`difference_threshold` and `percent_threshold`) are exceeded.
#'
#' When `verbose = TRUE`, the function outputs a summary of inconsistent IDU
#' values using CLI warnings.
#'
#' @importFrom sf st_area
#'
#' @export
parca_check_area <- function(parca,
                             difference_threshold = 500,
                             percent_threshold = 0.05,
                             verbose = TRUE) {

  # Compute SIG surface in ha
  parca$SIG_area <- as.numeric(sf::st_area(parca)) / 10000

  # Compute absolute difference (in m²) and percent difference
  difference <- abs(parca$SURF_CA * 10000 - parca$SIG_area * 10000)
  percent    <- difference / (parca$SURF_CA * 10000)

  # Determine inconsistent parcels
  inconsistent <- (difference >= difference_threshold & percent >= percent_threshold)

  # Build result sf (always returned, possibly empty)
  result <- parca[inconsistent, ]
  if (nrow(result) > 0) {
    result$difference <- difference[inconsistent]
    result$percent    <- percent[inconsistent]
  } else {
    # ensure the columns exist even when result is empty
    result$difference <- numeric(0)
    result$percent    <- numeric(0)
  }

  # Verbose output with cli
  if (verbose) {
    if (nrow(result) > 0) {
      bad_idu <- unique(result$IDU)
      cli::cli_alert_warning("Detected {length(bad_idu)} inconsistent IDU:")
      cli::cli_ul()
      for (id in bad_idu) cli::cli_li("{id}")
      cli::cli_end()
    } else {
      cli::cli_alert_success("No inconsistencies detected.")
    }
  }

  return(result)
}

# get_parca2 can remove 82 lines by using frcadastre
get_parca2 <- function(idu, bdp_geom = TRUE, lieu_dit = FALSE, verbose = TRUE){
  idu <- unique(idu)
  etalab <- frcadastre::idu_get_parcelle(idu, lieu_dit)

  if (bdp_geom){
    if (verbose) cli::cli_alert_info("Downloading BDP from IGN...")
    bdp <- get_parca_bdp(idu)
    idx <- match(etalab$idu, bdp$idu)
    etalab$geometry[!is.na(idx)] <- bdp$geometry[idx[!is.na(idx)]]
    if (verbose) {
      missing_bdp_idu <- intersect(etalab$idu, bdp$idu)
      if (length(missing_bdp_idu) > 0) {
        cli::cli_alert_success("{length(missing_bdp_idu)} IDU geometries replace with BDP geom.")
      }
    }
  }

  if (verbose) {
    missing_idu <- setdiff(idu, etalab$idu)
    if (length(missing_idu) > 0) {
      cli::cli_alert_warning("Geometry not found for {length(missing_idu)} IDU(s).")
      cli::cli_ul(missing_idu)
    }
  }

  parca <- etalab [, c("idu",
                       "code_reg", "reg_name",
                       "code_dep", "dep_name",
                       "code_com", "com_name",
                       "prefixe", "section", "numero", "lieudit", "contenance",
                       "geometry")]

  names(parca) <- c("IDU",
                    "REG_NUM", "REG_NOM",
                    "DEP_NUM", "DEP_NOM",
                    "COM_NUM", "COM_NOM",
                    "PREFIXE", "SECTION", "NUMERO", "LIEU_DIT", "CONTENANCE",
                    "geometry")

  return(parca)
}
