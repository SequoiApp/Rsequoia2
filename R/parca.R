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
  url <- "https://cadastre.data.gouv.fr/bundler/cadastre-etalab/communes/%s/geojson/parcelles"
  idu_parts <- idu_split(idu)

  urls <- sprintf(url, unique(idu_parts$insee))

  etalab <- lapply(urls, read_sf)
  etalab <- do.call(rbind, etalab)
  etalab$prefixe <- pad_left(etalab$prefixe, 3)
  etalab$section <- pad_left(etalab$section, 2)
  etalab$numero <- pad_left(etalab$numero, 4)

  etalab <- etalab[etalab$id %in% idu,]

  names(etalab)[names(etalab) == "id"] <- "idu"

  source <- seq_field("source")$name
  etalab[[source]] <- "etalab"
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

      source <- seq_field("source")$name
      etalab[[source]][!is.na(idx)] <- bdp[[source]][idx[!is.na(idx)]]

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

#' Remove Overlaps in a _PARCA_ Layer
#'
#' This function resolves overlapping geometries in a _PARCA_ layer by keeping
#' a single geometry for each overlapping area, based on INSEE total surface.
#' It then aggregates geometries by `IDU` and normalizes the result.
#'
#' @param parca An `sf` object, typically produced by [Rsequoia2::seq_parca()],
#'   containing cadastral parcels.
#'
#' @return An `sf` object corresponding to the input `parca` layer,
#'   with overlaps resolved, geometries aggregated by `IDU`, and normalized.
#'
#' @details
#' **Workflow:**
#' 1. **Clean topology**: Fix minor topological issues using `clean_topology()`
#'    with `snap` and `break`.
#' 2. **Detect overlaps**: Identify geometries that are duplicated due to overlaps.
#' 3. **Rank INSEE**: Compute total `SURF_CA` per INSEE and assign a rank,
#'    so that INSEE with largest total surface has highest priority.
#' 4. **Select overlaps**: For each overlapping geometry, keep only the entity
#'    with the highest INSEE rank.
#' 5. **Combine with non-overlaps**: Merge resolved overlaps with geometries
#'    that do not overlap.
#' 6. **Aggregate by IDU**: Merge geometries with the same `IDU` and preserve
#'    the first occurrence of non-numeric attributes.
#' 7. **Normalize**: Apply `seq_normalize()` to standardize the layer.
#'
#' Temporary columns used internally (`geom_key`, `overlap_id`, `insee_rank`)
#' are removed in the output.
#'
#' @export
remove_overlaps <- function(parca) {
  # retrieve field names
  idu_field     <- seq_field("idu")$name
  surf_field    <- seq_field("surf_cad")$name
  insee_field   <- seq_field("insee")$name

  # Clean topology
  cleaned <- clean_topology(parca, tool = c("snap", "break"))

  # Detect overlaps
  geom_key_bin <- st_as_binary(st_geometry(cleaned))
  is_overlap <- duplicated(geom_key_bin) | duplicated(geom_key_bin, fromLast = TRUE)

  overlaps <- cleaned[is_overlap, ]
  non_overlaps <- cleaned[!is_overlap, ]

  # Compute total surface per INSEE and rank
  total_surf <- tapply(cleaned[[surf_field]], cleaned[[insee_field]], sum)
  rank_df <- data.frame(INSEE = names(total_surf), total_surf = as.numeric(total_surf))
  rank_df <- rank_df[order(-rank_df$total_surf), ]
  insee_rank <- setNames(seq_len(nrow(rank_df)), rank_df$INSEE)

  overlaps$insee_rank <- insee_rank[as.character(overlaps[[insee_field]])]

  # Assign overlap ID
  overlaps$geom_key <- st_as_text(st_geometry(overlaps))
  overlaps$overlap_id <- as.integer(factor(overlaps$geom_key))

  # Keep the highest-rank INSEE per overlap
  keep_overlap <- do.call(rbind, lapply(
    split(overlaps, overlaps$overlap_id),
    function(x) x[which.min(x$insee_rank), ]
  ))

  # Combine with non-overlaps
  common_cols <- intersect(names(non_overlaps), names(keep_overlap))
  combined <- rbind(
    non_overlaps[, common_cols],
    keep_overlap[, common_cols]
  )

  # Aggregate by IDU
  attr_cols <- setdiff(names(combined), c("geometry", "overlap_id", "insee_rank"))

  agg_sf <- aggregate(
    combined[, attr_cols],
    by = list(IDU = combined[[idu_field]]),
    FUN = function(x) x[1]   # keep first for non-numeric attributes
  )

  # Normalize
  normalized <- seq_normalize(agg_sf, "parca")

  return(normalized)
}

#' Remove Holes in a _PARCA_ Layer
#'
#' This function fills holes in a _PARCA_ layer by adding geometries
#' corresponding to the missing areas.
#'
#' @param parca An `sf` object, typically produced by [Rsequoia2::seq_parca()],
#'   containing cadastral parcels.
#'
#' @return An updated `sf` object based on `parca`, with holes replaced
#'   by geometries without attributes.
#'
#' @export
remove_holes <- function(parca){
  # Step 1: Dissolve parcels and make valid
  dissolved <- dissolve(parca, 5) |>
    sf::st_sf() |>
    sf::st_make_valid()

  # Step 2: Extract interior holes and make valid
  holes <- extract_holes(dissolved) |>
    sf::st_make_valid()

  # Step 3: Union dissolved polygons with holes, clean topology
  large_dissolved <- sf::st_union(dissolved, holes) |>
    sf::st_union() |>
    sf::st_make_valid()

  # Step 4: Compute large holes as difference from original parcels
  large_holes <- sf::st_difference(large_dissolved,
                                   sf::st_union(parca)) |>
    sf::st_cast("POLYGON") |>
    quiet()

  # Step 5: Convert to sf with geometry column and normalize attributes
  large_holes_sf <- sf::st_sf(geometry = sf::st_geometry(large_holes)) |>
    seq_normalize("parca")

  # Step 6: Bind original parcels with large holes
  res <- rbind(parca, large_holes_sf)

  res
}

