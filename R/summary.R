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

  # OCCUPATION
  is_wooded <- seq_field("is_wooded")$name
  ua[[is_wooded]] <- ifelse(ua[[is_wooded]], "BOISEE", "NON BOISEE")
  ocs <- sum_surf_by(ua, "is_wooded")
  tables$OCCUPATION <- ocs

  # RAW UA
  tables$UA <- ua |> sf::st_drop_geometry()

  # Remove wooded data from ua
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

  nb_pplmt <- ncol(pf_by_plt) - 1
  if (nb_pplmt > 1){
    pf_by_plt$TOTAL <- rowSums(pf_by_plt[, -1], na.rm = T)
  }
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

  nb_gestion <- ncol(gestion_by_plt) - 1
  if (nb_gestion > 1){
    gestion_by_plt$TOTAL <- rowSums(gestion_by_plt[, -1], na.rm = T)
  }
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
  bdcharm50 <- safe_seq_read("bdcharm50", dirname = dirname)
  if (!is.null(bdcharm50)){
    geol_bdcharm50 <- ua |>
      sf::st_intersection(bdcharm50) |>
      suppressWarnings() |>
      ua_generate_area(verbose = FALSE) |> #if mutiple geol on one ua it's duplicated
      sf::st_drop_geometry() |>
      sum_surf_by("NOTATION", "DESCR")  |>
      order_by("cor_area", decreasing = TRUE) |>
      add_prop("cor_area")
    tables$BDCHARM50 <- geol_bdcharm50
  }

  # CARHAB
  carhab <- safe_seq_read("carhab", dirname = dirname)
  if (!is.null(carhab)){
    geol_carhab <- ua |>
      sf::st_intersection(carhab) |>
      suppressWarnings()  |>
      ua_generate_area(verbose = FALSE) |>
      sum_surf_by("CARHAB", "TYPE")  |>
      order_by("cor_area", decreasing = TRUE) |>
      add_prop("cor_area")
    tables$CARHAB <- geol_carhab
  }

  # PEDO
  pedo <- safe_seq_read("pedo", dirname = dirname)
  if (!is.null(pedo)){
    pedo <- ua |>
      sf::st_intersection(pedo) |>
      ua_generate_area(verbose = FALSE) |>
      suppressWarnings() |>
      sum_surf_by("id_ucs", "ger_nom", "nom_ucs") |>
      setNames(c("ID_UCS", "NOM_GER", "NOM_UCS", seq_field("cor_area")$name)) |>
      order_by("cor_area", decreasing = TRUE) |>
      add_prop("cor_area")
    tables$PEDO <- pedo
  }

  pf_field <- seq_field("pcl_code")$name

  # ROUTES
  road <- safe_seq_read("v.road.topo.line", dirname = dirname)
  road_in <- road |>
    sf::st_intersection(ua$geom) |>
    suppressWarnings()

  if (nrow(road_in) > 1){
    road_map <- c(
      PN = "Pistes & layons",
      RF = "Route empierr\u00E9e",
      RD = "Route rev\u00EAtue",
      RC = "Route rev\u00EAtue"
    )

    type_field <- seq_field("type")$name
    road_in$length <- as.numeric(sf::st_length(road_in) |> units::set_units("km"))
    road_in$cat <- road_map[road_in[[type_field]]]
    road_in$cat[is.na(road_in$cat)] <- "Autre"

    # Force levels BEFORE aggregation
    road_in$PRIVE <- factor(
      road_in$PRIVE,
      levels = c(FALSE, TRUE),
      labels = c("PUBLIQUE", "PRIVEE")
    )

    road_by_type <- stats::xtabs(length ~ cat + PRIVE, data = road_in) |>
      as.data.frame.matrix()

    road_by_type$REVETEMENT <- rownames(road_by_type)
    rownames(road_by_type) <- NULL

    road_by_type <- road_by_type[,c("REVETEMENT", "PUBLIQUE", "PRIVEE")]
    road_by_type$TOTAL <- rowSums(road_by_type[, -1], na.rm = T)
    surface_ha <- sum(ua$SURF_CAD, na.rm = TRUE)
    road_by_type$KM_100HA <- road_by_type$TOTAL * 100 / surface_ha

    tables$ROAD <- road_by_type
  }

  # MNT
  mnt <- safe_seq_read("r.alt.mnt", dirname = dirname)
  if (!is.null(mnt)){
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
  }

  # MNH
  mnh <- safe_seq_read("r.alt.mnh", dirname = dirname)
  if (!is.null(mnh)){
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
  }

  # EXPO
  expo <- safe_seq_read("r.alt.expo", dirname = dirname)
  if (!is.null(expo)){
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
  }

  # PENTE
  pente <- safe_seq_read("r.alt.pente", dirname = dirname)
  if (!is.null(pente)){
    pente_class <- terra::classify(pente, c(-Inf, 10, 40, 60, 80, Inf))
    freq <- terra::extract(pente_class, pf, "table", ID = FALSE) |> as.data.frame()

    pente_by_pf <- cbind(
      pf[pf_field] |> sf::st_drop_geometry(),
      setNames(freq / rowSums(freq) * 100, paste0("% ", names(freq)))
    )
    pente_by_pf$PENTE_MAJ <- names(freq)[max.col(freq)]
    tables$PENTE_PF <- pente_by_pf
  }

  # SAVE
  filename <- dirname |> file.path(seq_layer("summary")$filename)
  secure_filename <- sprintf(
    "%s_%s.%s",
    tools::file_path_sans_ext(filename),
    format(Sys.time(), "%Y%m%dT%H%M%S"),
    tools::file_ext(filename)
  )
  seq_xlsx(
    tables,
    filename = secure_filename,
    data_table = TRUE,
    overwrite = overwrite,
    verbose = verbose
  )

  return(tables)
}
