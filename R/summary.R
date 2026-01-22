#' Summarize information for a Sequoia folder
#'
#' Generate synthetic table from Sequoia layers and write them as `.xlsx`
#' inside the current Sequoia dir. Useful for redaction of management document.
#'
#' @inheritParams seq_write
#'
#' @details
#' Available tables:
#'  - `"CAD"`: Cadastral parcels
#'  - `"CAD_COM"`: Surfaces by communes
#'  - `"PF"`: Surfaces by forest parcels
#'  - `"SSPF"`: Surfaces by forest sub-parcels
#'  - `"CAD_PLT"`: Link between cadastral parcels and forest parcels
#'  - `"OCCUPATION"`: Surfaces by land use
#'  - `"STATION"`: Surfaces by station
#'  - `"GEOL_BDCHARM50"`: Surfaces by geology (source: BDCHARM50)
#'  - `"GEOL_CARHAB"`: Surfaces by geology (source: CARHAB)
#'  - `"PEDO"`: Surfaces by pedology type
#'  - `"PLT_PF"`: Link between stand type and forest parcels
#'  - `"PF_PLT"`: Link between forest parcels and stand type
#'  - `"GESTION"`: Surfaces by management type
#'  - `"ALTI_PF"`: Altimetry recap by forest parcels (max, min, mean)
#'  - `"EXPO_PF"`: Exposition recap by forest parcels
#'  - `"PENTE_PF"`: Slope recap by forest parcels
#'
#' @return `list` of `data.frame`
#'
seq_summary <- function(dirname = ".", verbose = TRUE, overwrite = FALSE){

  tables <- list()
  ua <- seq_read("v.seq.ua.poly", dirname = dirname)
  pf <- ua_to_pf(ua)

  sum_surf_by <- function(x, ...){

    x <- sf::st_drop_geometry(x)
    cor_area <- seq_field("cor_area")$name

    by <- c(...)
    is_key <- by %in% names(seq_field())
    by_key <- vapply(by[is_key], \(x) seq_field(x)$name, character(1))
    by[is_key] <- by_key

    by_formula <- paste(by, collapse = " + ")

    aggregate(
      stats::as.formula(paste(cor_area, "~", by_formula)),
      data = x,
      FUN = sum,
      na.rm = TRUE,
      na.action = stats::na.pass
    )
  }
  order_by <- function(to_order, ..., decreasing = FALSE) {

    by <- vapply(
      c(...),
      function(x) seq_field(x)$name,
      character(1)
    )

    cols <- lapply(by, function(nm) to_order[[nm]])
    o <- do.call(order, c(cols, list(decreasing = decreasing)))
    to_order[o, , drop = FALSE]
  }
  pivot <- function(to_pivot, row, col, ...){

    row_vars <- vapply(row, \(x) seq_field(x)$name, character(1))

    pivoted <- stats::reshape(to_pivot,
            idvar = row_vars,
            timevar = seq_field(col)$name,
            sep = "___",
            direction = "wide"
    )
    names(pivoted) <- sub("^.*___", "", names(pivoted))

    return(pivoted)
  }
  add_total <- function(df, ..., name = "TOTAL"){
    cols <- c(...)
    is_key <- cols %in% names(seq_field())
    cols_key <- vapply(cols[is_key], \(x) seq_field(x)$name, character(1))
    cols[is_key] <- cols_key

    empty_row <- df[0, , drop = FALSE]
    empty_row[1, ] <- NA

    # total row
    total <- empty_row
    total[1, 1] <- name
    total[cols] <- colSums(df[cols], na.rm = TRUE)

    return(rbind(df, empty_row, total))
  }
  add_prop <- function(df, by, name = "PROPORTION"){
    by <- seq_field(by)$name
    df[[name]] <- if (nrow(df) > 0) df[[by]]/sum(df[[by]]) else numeric(0)
    return(df)
  }

  # OCCUPATION
  is_wooded <- seq_field("is_wooded")$name
  ua[[is_wooded]] <- ifelse(ua[[is_wooded]], "BOISEE", "NON BOISEE")
  ocs <- sum_surf_by(ua, "is_wooded")
  tables$OCCUPATION <- ocs

  # RAW UA
  tables$UA <- ua |> sf::st_drop_geometry()

  ua <- ua[ua[[is_wooded]] == "BOISEE", ]

  # PARCA
  parca <- sum_surf_by(ua, "reg_name", "dep_name", "com_name", "insee", "prefix", "section", "number", "locality")
  tables$PARCA <- parca

  # PARCA_COM
  parca_by_com <- sum_surf_by(ua, "reg_name", "dep_name", "com_name", "insee")
  tables$PARCA_COM <- parca_by_com

  # PF
  pf_raw <- sum_surf_by(ua, "pcl_code") |>
    order_by("pcl_code")
  tables$PF <- pf_raw

  # SSPF
  sspf_raw <- sum_surf_by(ua, "pcl_code", "sub_code") |>
    order_by("pcl_code", "sub_code")
  tables$SSPF <- sspf_raw

  # PF_PARCA
  parca_col <- c("com_name", "insee", "prefix", "section", "number")

  pf_by_parca <- sum_surf_by(ua, "pcl_code", parca_col) |>
    order_by("pcl_code", "insee", "prefix", "section", "number")
  tables$PF_PARCA <- pf_by_parca

  # PARCA_PF
  parca_by_pf <- sum_surf_by(ua, "pcl_code", parca_col) |>
    order_by("pcl_code") |>
    pivot(row = parca_col, "pcl_code", direction = "wide") |>
    order_by(parca_col)
  parca_by_pf$TOTAL <- rowSums(parca_by_pf[, -(1:5)], na.rm = T)
  tables$PARCA_PF <- parca_by_pf

  # PLT_TYPE
  plt_type <- sum_surf_by(ua, "std_type") |>
    order_by("std_type") |>
    add_prop("cor_area")
  tables$PLT_TYPE <- plt_type

  # PLT_STADE
  plt_stade <- sum_surf_by(ua, "std_stage") |>
    order_by("std_stage") |>
    add_prop("cor_area")
  tables$PLT_STADE <- plt_stade

  # PLT_ESS
  plt_ess <- sum_surf_by(ua, "res_spe1") |>
    order_by("res_spe1") |>
    add_prop("cor_area")
  tables$PLT_ESS <- plt_ess

  # PLT
  plt <- sum_surf_by(ua, "std_type", "std_stage", "res_spe1") |>
    order_by("std_type", "std_stage", "res_spe1") |>
    add_prop("cor_area")
  tables$PLT <- plt

  # PF_PLT
  pf_by_plt <- sum_surf_by(ua, "pcl_code", "std_type") |>
    order_by("std_type") |>
    pivot("pcl_code", "std_type", direction = "wide") |>
    order_by("pcl_code")
  pf_by_plt$TOTAL <- rowSums(pf_by_plt[, -1], na.rm = T)
  tables$PF_PLT <- pf_by_plt

  # PLT_PF
  plt_by_pf <- sum_surf_by(ua, "pcl_code", "std_type") |>
    order_by("pcl_code") |>
    pivot("std_type", "pcl_code", direction = "wide") |>
    order_by("std_type")
  plt_by_pf$TOTAL <- rowSums(plt_by_pf[, -1], na.rm = T)
  tables$PLT_PF <- plt_by_pf

  # GESTION
  gestion <- sum_surf_by(ua, "treatment") |>
    order_by("treatment") |>
    add_prop("cor_area")
  tables$GESTION <- gestion

  # GESTION_PLT
  gestion_by_plt <- sum_surf_by(ua, "treatment", "std_type") |>
    order_by("std_type") |>
    pivot("treatment", "std_type", direction = "wide") |>
    order_by("treatment")
  gestion_by_plt$TOTAL <- rowSums(gestion_by_plt[, -1], na.rm = T)
  tables$GESTION_PLT <- gestion_by_plt

  # PLT_GESTION
  plt_by_gestion <- sum_surf_by(ua, "std_type", "treatment") |>
    order_by("treatment") |>
    pivot("std_type", "treatment", direction = "wide") |>
    order_by("std_type")
  plt_by_gestion$TOTAL <- rowSums(plt_by_gestion[, -1], na.rm = T)
  tables$PLT_GESTION <- plt_by_gestion

  # STATION
  station <- sum_surf_by(ua, "station") |>
    add_prop("cor_area")
  tables$STATION <- station

  # BDCHARM50
  bdcharm50 <- seq_read("bdcharm50", dirname = dirname)
  geol_bdcharm50 <- ua |>
    sf::st_intersection(bdcharm50) |>
    suppressWarnings() |>
    ua_generate_area(verbose = FALSE) |> #if mutiple geol on one ua it's duplicated
    sf::st_drop_geometry() |>
    sum_surf_by("NOTATION", "DESCR")  |>
    order_by("cor_area", decreasing = TRUE) |>
    add_prop("cor_area")
  tables$BDCHARM50 <- geol_bdcharm50

  # CARHAB
  carhab <- seq_read("carhab", dirname = dirname)
  geol_carhab <- ua |>
    sf::st_intersection(carhab) |>
    suppressWarnings()  |>
    ua_generate_area(verbose = FALSE) |>
    sum_surf_by("CARHAB", "TYPE")  |>
    order_by("cor_area", decreasing = TRUE) |>
    add_prop("cor_area")
  tables$CARHAB <- geol_carhab

  # PEDO
  pedo <- seq_read("pedo", dirname = dirname)
  pedo <- ua |>
    sf::st_intersection(pedo) |>
    ua_generate_area(verbose = FALSE) |>
    suppressWarnings() |>
    sum_surf_by("id_ucs", "ger_nom", "nom_ucs") |>
    setNames(c("ID_UCS", "NOM_GER", "NOM_UCS", seq_field("cor_area")$name)) |>
    order_by("cor_area", decreasing = TRUE) |>
    add_prop("cor_area")
  tables$PEDO <- pedo

  # MNT
  pf_field <- seq_field("pcl_code")$name

  mnt <- seq_read("r.alt.mnt", dirname = dirname)
  fun <- list(mean = mean, min = min, max = max)
  mnt_by_pf <- Reduce(
    function(x, y) merge(x, y, by = "N_PARFOR"),
    lapply(
      names(fun),
      function(x){
        names(mnt) <- paste0("ALTITUDE_", toupper(x))
        as.data.frame(
          terra::extract(mnt, pf[pf_field], fun[[x]], na.rm = TRUE, bind = TRUE, ID = FALSE)
        )}
    ))
  tables$MNT_PF <- mnt_by_pf

  # MNH
  mnh <- seq_read("r.alt.mnh", dirname = dirname)
  fun <- list(mean = mean, min = min, max = max)
  mnh_by_pf <- Reduce(
    function(x, y) merge(x, y, by = "N_PARFOR"),
    lapply(
      names(fun),
      function(x){
        names(mnh) <- paste0("H_", toupper(x))
        as.data.frame(
          terra::extract(mnh, pf[pf_field], fun[[x]], na.rm = TRUE, bind = TRUE, ID = FALSE)
        )}
    ))
  tables$MNH_PF <- mnh_by_pf

  # EXPO
  expo <- seq_read("expo", dirname)
  m <- matrix(c(
    0,   45,  1, # Nord
    45,  135, 2, # Est
    135, 225, 3, # Sud
    225, 315, 4, # Ouest
    315, 360, 1  # Nord
  ), ncol = 3, byrow = TRUE)

  expo_class <- terra::classify(expo, m)

  classes <- c("NORD", "EST", "SUD", "OUEST")
  levels(expo_class) <- data.frame(value = 1:4, exposition = classes)
  freq <- terra::extract(expo_class, pf, "table", ID = FALSE) |> as.data.frame()

  expo_by_pf <- cbind(
    pf[pf_field] |> sf::st_drop_geometry(),
    setNames(freq / rowSums(freq) * 100, paste0("%_", names(freq)))
  )
  expo_by_pf$EXPO_MAJ <- classes[max.col(freq[classes])]
  tables$EXPO_PF <- expo_by_pf

  # PENTE
  pente <- seq_read("pente", dirname)
  pente_class <- terra::classify(pente, c(-Inf, 10, 40, 60, 80, Inf))
  freq <- terra::extract(pente_class, pf, "table", ID = FALSE) |> as.data.frame()

  pente_by_pf <- cbind(
    pf[pf_field] |> sf::st_drop_geometry(),
    setNames(freq / rowSums(freq) * 100, paste0("% ", names(freq)))
  )
  pente_by_pf$PENTE_MAJ <- names(freq)[max.col(freq)]
  tables$PENTE_PF <- pente_by_pf

  # SAVE
  filename <- dirname |> file.path(seq_layer("summary")$filename)

  seq_xlsx(
    tables,
    filename = filename,
    data_table = TRUE,
    overwrite = overwrite,
    verbose = verbose
  )

  return(tables)
}
