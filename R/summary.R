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
seq_summary <- function(dirname = ".", verbose = TRUE){

  # HELPERS ----
  wb_add_seq_table <- function(wb, sheet, x, total_row = FALSE){

    wb |>
      openxlsx2::wb_add_worksheet(sheet = sheet) |>
      openxlsx2::wb_add_data_table(
        sheet = sheet,
        x = x,
        na.strings = NULL,
        total_row = total_row
      ) |>
      style_table(sheet = sheet, df = x) |>
      openxlsx2::wb_freeze_pane(
        sheet = sheet,
        first_row = TRUE
      )
  }

  wb <- wb_workbook()

  ua <- seq_read("v.seq.ua.poly", dirname = dirname)
  pf <- ua_to_pf(ua)

  # OCCUPATION ----
  is_wooded <- seq_field("is_wooded")$name
  ua[[is_wooded]] <- ifelse(ua[[is_wooded]], "BOISEE", "NON BOISEE")
  ocs <- sum_surf_by(ua, "is_wooded")
  wb <- wb_add_seq_table(wb, "OCCUPATION", ocs, total_row = c(text = "TOTAL", "sum"))

  # RAW UA ----
  raw_ua <- sf::st_drop_geometry(ua)
  wb <- wb_add_seq_table(wb, "UA", raw_ua)

  # Remove wooded data from ua
  ua <- ua[ua[[is_wooded]] == "BOISEE", ]

  # PARCA ----
  parca <- sum_surf_by(ua, "reg_name", "dep_name", "com_name", "insee", "prefix", "section", "number", "locality")
  wb <- wb_add_seq_table(
    wb, "PARCA", parca,
    total_row = c(text = "TOTAL", rep("none", ncol(parca) - 2), "sum")
  )

  # PARCA_COM ----
  parca_by_com <- sum_surf_by(ua, "reg_name", "dep_name", "com_name", "insee")
  wb <- wb_add_seq_table(
    wb, "PARCA_COM", parca_by_com,
    total_row = c(text = "TOTAL", rep("none", ncol(parca_by_com) - 2), "sum")
  )

  # PF ----
  pf_raw <- sum_surf_by(ua, "pcl_code") |>
    order_by("pcl_code")
  wb <- wb_add_seq_table(wb, "PF", pf_raw, total_row = c(text = "TOTAL", "sum"))

  # SSPF ----
  sspf_raw <- sum_surf_by(ua, "pcl_code", "sub_code") |>
    order_by("pcl_code", "sub_code")
  wb <- wb_add_seq_table(wb, "SSPF", sspf_raw, total_row = c(text = "TOTAL", "none", "sum"))

  # PF_PARCA ----
  parca_col <- c("com_name", "insee", "prefix", "section", "number", "locality")
  pf_by_parca <- sum_surf_by(ua, "pcl_code", parca_col) |>
    order_by("pcl_code", "insee", "prefix", "section", "number")
  wb <- wb_add_seq_table(
    wb, "PF_PARCA", pf_by_parca,
    total_row = c(text = "TOTAL", rep("none", ncol(pf_by_parca) - 2), "sum")
  )

  # PARCA_PF ----
  parca_by_pf <- sum_surf_by(ua, "pcl_code", parca_col) |>
    order_by("pcl_code") |>
    pivot(row = parca_col, "pcl_code", direction = "wide") |>
    order_by(parca_col)

  # Garde fou s'il n'y a qu'une parcelle
  nb_pcl <- ncol(parca_by_pf) - length(parca_col)
  if (nb_pcl > 1){
    parca_by_pf$TOTAL <- rowSums(parca_by_pf[, -seq_along(parca_col)], na.rm = T)
  }

  wb <- wb_add_seq_table(
    wb, "PARCA_PF", parca_by_pf,
    total_row = c(
      text = "TOTAL",
      rep("none", length(parca_col) - 1),
      rep("sum", ncol(parca_by_pf) - length(parca_col)))
  )

  # PLT_TYPE ----
  plt_type <- sum_surf_by(ua, "std_type") |>
    order_by("std_type") |>
    add_prop("cor_area")
  wb <- wb_add_seq_table(
    wb, "PLT_TYPE", plt_type,
    total_row = c(text = "TOTAL", "sum", "sum")
  )

  # PLT_RICH ----
  plt_rich <- sum_surf_by(ua, "std_wealth") |>
    order_by("std_wealth") |>
    add_prop("cor_area")
  wb <- wb_add_seq_table(
    wb, "PLT_RICH", plt_rich,
    total_row = c(text = "TOTAL", "sum", "sum")
  )

  # PLT_STADE ----
  plt_stade <- sum_surf_by(ua, "std_stage") |>
    order_by("std_stage") |>
    add_prop("cor_area")
  wb <- wb_add_seq_table(
    wb, "PLT_STADE", plt_stade,
    total_row = c(text = "TOTAL", "sum", "sum")
  )

  # PLT_ESS ----
  plt_ess <- sum_surf_by(ua, "res_spe1") |>
    order_by("res_spe1") |>
    add_prop("cor_area")
  wb <- wb_add_seq_table(
    wb, "PLT_ESS", plt_ess,
    total_row = c(text = "TOTAL", "sum", "sum")
  )

  # PLT ----
  plt <- sum_surf_by(ua, "std_type", "std_wealth", "std_stage", "res_spe1") |>
    order_by("std_type", "std_wealth", "std_stage", "res_spe1") |>
    add_prop("cor_area")
  wb <- wb_add_seq_table(
    wb, "PLT", plt,
    total_row = c(text = "TOTAL", rep("none", ncol(plt) - 3), "sum", "sum")
  )

  # PF_PLT ----
  pf_by_plt <- sum_surf_by(ua, "pcl_code", "std_type") |>
    order_by("std_type") |>
    pivot("pcl_code", "std_type", direction = "wide") |>
    order_by("pcl_code")

  nb_pplmt <- ncol(pf_by_plt) - 1
  if (nb_pplmt > 1){
    pf_by_plt$TOTAL <- rowSums(pf_by_plt[, -1], na.rm = T)
  }

  wb <- wb_add_seq_table(
    wb, "PF_PLT", pf_by_plt,
    total_row = c(text = "TOTAL", rep("sum", ncol(pf_by_plt) - 2), "sum")
  )

  # PLT_PF ----
  plt_by_pf <- sum_surf_by(ua, "pcl_code", "std_type") |>
    order_by("pcl_code") |>
    pivot("std_type", "pcl_code", direction = "wide") |>
    order_by("std_type")

  nb_pf <- ncol(plt_by_pf) - 1
  if (nb_pf > 1){
    plt_by_pf$TOTAL <- rowSums(plt_by_pf[, -1], na.rm = T)
  }
  wb <- wb_add_seq_table(
    wb, "PLT_PF", plt_by_pf,
    total_row = c(text = "TOTAL", rep("sum", ncol(plt_by_pf) - 1))
  )

  # PF_RICH ----
  pf_by_rich <- sum_surf_by(ua, "pcl_code", "std_wealth") |>
    order_by("std_wealth") |>
    pivot("pcl_code", "std_wealth", direction = "wide") |>
    order_by("pcl_code")

  nb_rich <- ncol(pf_by_rich) - 1
  if (nb_rich > 1){
    pf_by_rich$TOTAL <- rowSums(pf_by_rich[, -1], na.rm = T)
  }

  wb <- wb_add_seq_table(
    wb, "PF_RICH", pf_by_rich,
    total_row = c(text = "TOTAL", rep("sum", ncol(pf_by_rich) - 2), "sum")
  )

  # COUPE ----
  coupe <- sum_surf_by(ua, "mgmt_code", "std_type", "std_wealth", "std_stage", "res_spe1") |>
    order_by("mgmt_code", "std_type", "std_wealth", "std_stage", "res_spe1")
  actual_year <- Sys.Date() |> format("%Y") |> as.numeric()
  last_year <- actual_year + 15
  years <- as.character(actual_year:last_year)
  coupe_with_year <- coupe
  coupe_with_year[,years] <- NA

  wb <- wb_add_seq_table(
    wb, "COUPE", coupe_with_year,
    total_row = c(text = "TOTAL", rep("none", ncol(coupe) - 2), "sum", rep("count", 16))
  )

  # GESTION ----
  gestion <- sum_surf_by(ua, "treatment") |>
    order_by("treatment") |>
    add_prop("cor_area")
  wb <- wb_add_seq_table(
    wb, "GESTION", gestion,
    total_row = c(text = "TOTAL", rep("none", ncol(gestion) - 3), "sum", "sum")
  )

  # GESTION_PLT ----
  field_treatment <- seq_field("treatment")$name
  if (all(is.na(ua$AME_TYPE))) {
    cli::cli_alert_warning(
      "No stand type available in {.field {field_treatment}}"
    )
  } else {
    gestion_by_plt <- sum_surf_by(ua, "treatment", "std_type") |>
      order_by("std_type") |>
      pivot("treatment", "std_type", direction = "wide") |>
      order_by("treatment")

    value_cols <- setdiff(names(gestion_by_plt), field_treatment)

    if (length(value_cols) > 1) {
      gestion_by_plt$TOTAL <- rowSums(gestion_by_plt[value_cols], na.rm = TRUE)
    }

    wb <- wb_add_seq_table(
      wb,
      "GESTION_PLT",
      gestion_by_plt,
      total_row = c(text = "TOTAL", rep("sum", ncol(gestion_by_plt) - 1))
    )
  }

  # PLT_GESTION ----
  field_treatment <- seq_field("treatment")$name
  if (all(is.na(ua$AME_TYPE))) {
    cli::cli_alert_warning(
      "No stand type available in {.field {field_treatment}}"
    )
  }else{
    plt_by_gestion <- sum_surf_by(ua, "std_type", "treatment") |>
      order_by("treatment") |>
      pivot("std_type", "treatment", direction = "wide") |>
      order_by("std_type")

    nb_gestion <- ncol(plt_by_gestion) - 1
    if (nb_gestion > 1){
      plt_by_gestion$TOTAL <- rowSums(plt_by_gestion[, -1], na.rm = T)
    }

    wb <- wb_add_seq_table(
      wb, "PLT_GESTION", plt_by_gestion,
      total_row = c(text = "TOTAL", rep("sum", ncol(plt_by_gestion) - 1))
    )
  }

  # STATION ----
  station <- sum_surf_by(ua, "station") |>
    add_prop("cor_area")

  wb <- wb_add_seq_table(
    wb, "STATION", station,
    total_row = c(text = "TOTAL", rep("sum", 2))
  )

  # BDCHARM50 ----
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

    wb <- wb_add_seq_table(
      wb, "BDCHARM50", geol_bdcharm50,
      total_row = c(text = "TOTAL", "none", rep("sum", 2))
    )
  }

  # CARHAB ----
  carhab <- safe_seq_read("carhab", dirname = dirname)
  if (!is.null(carhab)){
    geol_carhab <- ua |>
      sf::st_intersection(carhab) |>
      suppressWarnings()  |>
      ua_generate_area(verbose = FALSE) |>
      sum_surf_by("CARHAB", "TYPE")  |>
      order_by("cor_area", decreasing = TRUE) |>
      add_prop("cor_area")

    wb <- wb_add_seq_table(
      wb, "CARHAB", geol_carhab,
      total_row = c(text = "TOTAL", "none", rep("sum", 2))
    )

  }

  # PEDO ----
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

    wb <- wb_add_seq_table(
      wb, "PEDO", pedo,
      total_row = c(text = "TOTAL", rep("none", 2), rep("sum", 2))
    )
  }

  pf_field <- seq_field("pcl_code")$name

  # ROUTES ----
  road <- safe_seq_read("v.road.line", dirname = dirname)

  if (!is.null(road)){
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

      wb <- wb_add_seq_table(
        wb, "ROAD", road_by_type,
        total_row = c(text = "TOTAL", rep("sum", 4))
      )
    }
  }

  # MNT ----
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

    wb <- wb_add_seq_table(
      wb, "MNT", mnt_by_pf,
      total_row = c(text = "TOTAL", rep("average", 3))
    )

  }

  # MNH ----
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

    wb <- wb_add_seq_table(
      wb, "MNH", mnh_by_pf,
      total_row = c(text = "TOTAL", rep("average", 3))
    )

  }

  # EXPO ----
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
    freq <- terra::extract(expo_class, pf, "table", ID = TRUE, wide = FALSE)
    freq <- stats::xtabs(count ~ ID + exposition, data = freq) |>
      as.data.frame.matrix()

    expo_by_pf <- cbind(
      pf[pf_field] |> sf::st_drop_geometry(),
      setNames(freq / rowSums(freq) * 100, paste0("%_", names(freq)))
    )
    expo_by_pf$EXPO_MAJ <- classes[max.col(freq[classes])]

    wb <- wb_add_seq_table(
      wb, "EXPO", expo_by_pf,
      total_row = c(text = "TOTAL", rep("average", 4), "none")
    )

  }

  # PENTE
  pente <- safe_seq_read("r.alt.pente", dirname = dirname)
  if (!is.null(pente)){
    m <- matrix(c(
      -Inf, 10, 1,
      10,  40,  2,
      40,  60,  3,
      60,  80,  4,
      80,  Inf, 5
    ), ncol = 3, byrow = TRUE)

    pente_class <- terra::classify(pente, m)
    classes <- c("< 10%", "[10% - 40%]", "[40% - 60%]", "[60% - 80%]", "> 80%")
    levels(pente_class) <- data.frame(value = 1:5, pentes = classes)
    freq <- terra::extract(pente_class, pf, "table", ID = TRUE, wide = FALSE)
    freq <- stats::xtabs(count ~ ID + pentes, data = freq) |>
      as.data.frame.matrix()
    names(freq) <- sub("^count\\.", "", names(freq))

    pente_by_pf <- cbind(
      pf[pf_field] |> sf::st_drop_geometry(),
      setNames(freq / rowSums(freq) * 100, paste0("% ", names(freq)))
    )
    pente_by_pf$PENTE_MAJ <- names(freq)[max.col(freq)]

    wb <- wb_add_seq_table(
      wb, "PENTE", pente_by_pf,
      total_row = c(text = "TOTAL", rep("average", 5), "none")
    )
  }

  # SAVE
  filename <- dirname |> file.path(seq_layer("summary")$filename)
  secure_filename <- sprintf(
    "%s_%s.%s",
    tools::file_path_sans_ext(filename),
    format(Sys.time(), "%Y%m%dT%H%M%S"),
    tools::file_ext(filename)
  )

  openxlsx2::wb_save(wb, secure_filename)

  if (verbose) {
    cli::cli_alert_success(
      "Summary saved to {.file {secure_filename}}."
    )
  }

  return(invisible(NULL))
}
