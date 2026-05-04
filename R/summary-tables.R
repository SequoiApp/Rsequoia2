# UA ----
build_summary_ua <- function(ua) {

  tbl <- sf::st_drop_geometry(ua)

  list(table = tbl, total_row = NULL)
}

# OCCUPATION ----
build_summary_occupation <- function(ua) {
  is_wooded <- seq_field("is_wooded")$name

  ua <- sf::st_drop_geometry(ua)
  ua[[is_wooded]] <- ifelse(ua[[is_wooded]] %in% TRUE, "BOISEE", "NON BOISEE")

  tbl <- sum_surf_by(ua, "is_wooded")

  tot <- c(text = "TOTAL", "sum")

  list(table = tbl, total_row = tot)
}

# PARCA ----
build_summary_parca <- function(ua) {
  tbl <- sum_surf_by(
    ua,
    "reg_name", "dep_name", "com_name",
    "insee", "prefix", "section", "number", "locality"
  )

  tot <- c(text = "TOTAL", rep("none", ncol(tbl) - 2), "sum")

  list(table = tbl, total_row = tot)
}

# PARCA_COM ----
build_summary_parca_com <- function(ua) {
  tbl <- sum_surf_by(ua, "reg_name", "dep_name", "com_name", "insee")

  tot <- c(text = "TOTAL", rep("none", ncol(tbl) - 2), "sum")

  list(table = tbl, total_row = tot)
}

# PF ----
build_summary_pf <- function(ua) {
  tbl <- sum_surf_by(ua, "pcl_code") |>
    order_by("pcl_code")

  tot <- c(text = "TOTAL", "sum")

  list(table = tbl, total_row = tot)
}

# SSPF ----
build_summary_sspf <- function(ua) {
  tbl <- sum_surf_by(ua, "pcl_code", "sub_code") |>
    order_by("pcl_code", "sub_code")

  tot <- c(text = "TOTAL", "none", "sum")

  list(table = tbl, total_row = tot)
}

# PF_PARCA ----
build_summary_pf_parca <- function(ua) {
  parca_col <- c("com_name", "insee", "prefix", "section", "number", "locality")

  tbl <- sum_surf_by(ua, "pcl_code", parca_col) |>
    order_by("pcl_code", "insee", "prefix", "section", "number")

  tot <- c(text = "TOTAL", rep("none", ncol(tbl) - 2), "sum")

  list(table = tbl, total_row = tot)
}

# PARCA_PF ----
build_summary_parca_pf <- function(ua) {
  parca_col <- c("com_name", "insee", "prefix", "section", "number", "locality")

  parca_by_pf <- sum_surf_by(ua, "pcl_code", parca_col) |>
    order_by("pcl_code") |>
    pivot(row = parca_col, "pcl_code") |>
    order_by(parca_col)

  # Garde-fou s'il n'y a qu'une parcelle
  nb_pcl <- ncol(parca_by_pf) - length(parca_col)
  if (nb_pcl > 1) {
    parca_by_pf$TOTAL <- rowSums(
      parca_by_pf[, -seq_along(parca_col), drop = FALSE],
      na.rm = TRUE
    )
  }

  tot <- c(
    text = "TOTAL",
    rep("none", length(parca_col) - 1),
    rep("sum", ncol(parca_by_pf) - length(parca_col))
  )

  list(table = parca_by_pf, total_row = tot)
}

# PLT_TYPE ----
build_summary_plt_type <- function(ua) {
  tbl <- sum_surf_by(ua, "std_type") |>
    order_by("std_type") |>
    add_prop("cor_area")

  tot <- c(text = "TOTAL", "sum", "sum")

  list(table = tbl, total_row = tot)
}

# PLT_RICH ----
build_summary_plt_rich <- function(ua) {
  tbl <- sum_surf_by(ua, "std_wealth") |>
    order_by("std_wealth") |>
    add_prop("cor_area")

  tot <- c(text = "TOTAL", "sum", "sum")

  list(table = tbl, total_row = tot)
}

# PLT_STADE ----
build_summary_plt_stade <- function(ua) {
  tbl <- sum_surf_by(ua, "std_stage") |>
    order_by("std_stage") |>
    add_prop("cor_area")

  tot <- c(text = "TOTAL", "sum", "sum")

  list(table = tbl, total_row = tot)
}

# PLT_ESS ----
build_summary_plt_ess <- function(ua) {
  tbl <- sum_surf_by(ua, "res_spe1") |>
    order_by("res_spe1") |>
    add_prop("cor_area")

  tot <- c(text = "TOTAL", "sum", "sum")

  list(table = tbl, total_row = tot)
}

# PLT ----
build_summary_plt <- function(ua) {
  tbl <- sum_surf_by(ua, "std_type", "std_wealth", "std_stage", "res_spe1") |>
    order_by("std_type", "std_wealth", "std_stage", "res_spe1") |>
    add_prop("cor_area")

  tot <- c(text = "TOTAL", rep("none", ncol(tbl) - 3), "sum", "sum")

  list(table = tbl, total_row = tot)
}

# PF_PLT ----
build_summary_pf_plt <- function(ua) {
  tbl <- sum_surf_by(ua, "pcl_code", "std_type") |>
    order_by("std_type") |>
    pivot("pcl_code", "std_type") |>
    order_by("pcl_code")

  nb_pplmt <- ncol(tbl) - 1
  if (nb_pplmt > 1) {
    tbl$TOTAL <- rowSums(tbl[, -1, drop = FALSE], na.rm = TRUE)
  }

  tot <- c(text = "TOTAL", rep("sum", ncol(tbl) - 2), "sum")

  list(table = tbl, total_row = tot)
}

# PLT_PF ----
build_summary_plt_pf <- function(ua) {
  plt_pf <- sum_surf_by(ua, "pcl_code", "std_type") |>
    order_by("pcl_code") |>
    pivot("std_type", "pcl_code") |>
    order_by("std_type")

  nb_pf <- ncol(plt_pf) - 1
  if (nb_pf > 1) {
    plt_pf$TOTAL <- rowSums(plt_pf[, -1, drop = FALSE], na.rm = TRUE)
  }

  tot <- c(text = "TOTAL", rep("sum", ncol(plt_pf) - 1))

  list(table = plt_pf, total_row = tot)
}

# PF_RICH ----
build_summary_pf_rich <- function(ua) {
  tbl <- sum_surf_by(ua, "pcl_code", "std_wealth") |>
    order_by("std_wealth") |>
    pivot("pcl_code", "std_wealth") |>
    order_by("pcl_code")

  nb_rich <- ncol(tbl) - 1
  if (nb_rich > 1) {
    tbl$TOTAL <- rowSums(tbl[, -1, drop = FALSE], na.rm = TRUE)
  }

  tot <- c(text = "TOTAL", rep("sum", ncol(tbl) - 2), "sum")

  list(table = tbl, total_row = tot)
}

# COUPE ----
build_summary_coupe <- function(ua) {
  coupe <- sum_surf_by(ua, "mgmt_code", "std_type", "std_wealth", "std_stage", "res_spe1") |>
    order_by("mgmt_code", "std_type", "std_wealth", "std_stage", "res_spe1")

  actual_year <- as.numeric(format(Sys.Date(), "%Y"))
  last_year <- actual_year + 20
  years <- as.character(actual_year:last_year)

  coupe_with_year <- coupe
  coupe_with_year[, years] <- NA

  tot <- c(text = "TOTAL", rep("none", ncol(coupe) - 2), "sum", rep("count", 16))

  list(table = coupe_with_year, total_row = tot)
}

# GESTION ----
build_summary_gestion <- function(ua) {
  tbl <- sum_surf_by(ua, "treatment") |>
    order_by("treatment") |>
    add_prop("cor_area")

  tot <- c(text = "TOTAL", rep("none", ncol(tbl) - 3), "sum", "sum")

  list(table = tbl, total_row = tot)
}

# GESTION_PLT ----
build_summary_gestion_plt <- function(ua) {
  tbl <- sum_surf_by(ua, "treatment", "std_type") |>
    order_by("std_type") |>
    pivot("treatment", "std_type") |>
    order_by("treatment")

  nb_pplmt <- ncol(tbl) - 1
  if (nb_pplmt > 1) {
    tbl$TOTAL <- rowSums(tbl[, -1, drop = FALSE], na.rm = TRUE)
  }

  tot <- c(text = "TOTAL", rep("sum", ncol(tbl) - 1))

  list(table = tbl, total_row = tot)
}

# PLT_GESTION ----
build_summary_plt_gestion <- function(ua) {
  tbl <- sum_surf_by(ua, "std_type", "treatment") |>
    order_by("treatment") |>
    pivot("std_type", "treatment") |>
    order_by("std_type")

  nb_gestion <- ncol(tbl) - 1
  if (nb_gestion > 1) {
    tbl$TOTAL <- rowSums(tbl[, -1, drop = FALSE], na.rm = TRUE)
  }

  tot <- c(text = "TOTAL", rep("sum", ncol(tbl) - 1))

  list(table = tbl, total_row = tot)
}

# STATION ----
build_summary_station <- function(ua) {
  tbl <- sum_surf_by(ua, "station") |>
    add_prop("cor_area")

  tot <- c(text = "TOTAL", rep("sum", 2))

  list(table = tbl, total_row = tot)
}

# BDCHARM50 ----
build_summary_bdcharm50 <- function(ua, dirname) {
  bdcharm50 <- seq_read("bdcharm50", dirname = dirname)

  tbl <- suppressWarnings(
    sf::st_intersection(ua, bdcharm50)
  ) |>
    ua_generate_area(verbose = FALSE) |> # if multiple geol on one ua, it is duplicated
    sf::st_drop_geometry() |>
    sum_surf_by("NOTATION", "DESCR") |>
    order_by("cor_area", decreasing = TRUE) |>
    add_prop("cor_area")

  tot <- c(text = "TOTAL", "none", rep("sum", 2))

  list(table = tbl, total_row = tot)
}

# CARHAB ----
build_summary_carhab <- function(ua, dirname) {
  carhab <- seq_read("carhab", dirname = dirname)

  tbl <- suppressWarnings(
    sf::st_intersection(ua, carhab)
  ) |>
    ua_generate_area(verbose = FALSE) |>
    sum_surf_by("CARHAB", "TYPE") |>
    order_by("cor_area", decreasing = TRUE) |>
    add_prop("cor_area")

  tot <- c(text = "TOTAL", "none", rep("sum", 2))

  list(table = tbl, total_row = tot)
}

# PEDO ----
build_summary_pedo <- function(ua, dirname) {
  pedo <- seq_read("pedo", dirname = dirname)

  tbl <- suppressWarnings(
    sf::st_intersection(ua, pedo)
  ) |>
    ua_generate_area(verbose = FALSE) |>
    sum_surf_by("id_ucs", "ger_nom", "nom_ucs")

  rename_map <- c(
    id_ucs = "ID_UCS",
    ger_nom = "NOM_GER",
    nom_ucs = "NOM_UCS"
  )

  old <- intersect(names(rename_map), names(tbl))
  names(tbl)[match(old, names(tbl))] <- unname(rename_map[old])

  tbl <- tbl |>
    order_by("cor_area", decreasing = TRUE) |>
    add_prop("cor_area")

  tot <- c(text = "TOTAL", rep("none", 2), rep("sum", 2))

  list(table = tbl, total_row = tot)
}

# ROUTES ----
build_summary_road <- function(ua, dirname) {
  road <- seq_read("v.road.line", dirname = dirname)

  road_in <- suppressWarnings(
    sf::st_intersection(road, ua$geom)
  )

  if (nrow(road_in) < 1) {
    cli::cli_abort("No road intersects the UA layer.")
  }

  road_map <- c(
    PN = "Pistes & layons",
    RF = "Route empierr\u00E9e",
    RN = "Route rev\u00EAtue",
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

  for (col in c("PUBLIQUE", "PRIVEE")) {
    if (!col %in% names(road_by_type)) {
      road_by_type[[col]] <- 0
    }
  }

  road_by_type <- road_by_type[, c("REVETEMENT", "PUBLIQUE", "PRIVEE")]
  road_by_type$TOTAL <- rowSums(road_by_type[, -1, drop = FALSE], na.rm = TRUE)

  surface_ha <- sum(ua$SURF_CAD, na.rm = TRUE)
  if (isTRUE(all.equal(surface_ha, 0))) {
    road_by_type$KM_100HA <- NA_real_
  } else {
    road_by_type$KM_100HA <- road_by_type$TOTAL * 100 / surface_ha
  }

  tot <- c(text = "TOTAL", rep("sum", 4))

  list(table = road_by_type, total_row = tot)
}

# MNT ----
build_summary_mnt <- function(pf, dirname) {
  mnt <- seq_read("r.alt.mnt", dirname = dirname)

  pf_field <- seq_field("pcl_code")$name
  fun <- list(mean = mean, min = min, max = max)

  tbl <- Reduce(
    function(x, y) merge(x, y, by = pf_field),
    lapply(
      names(fun),
      function(x) {
        r <- mnt
        names(r) <- paste0("ALTITUDE_", toupper(x))

        as.data.frame(
          terra::extract(r, pf[pf_field], fun[[x]], na.rm = TRUE, bind = TRUE, ID = FALSE)
        )
      }
    )
  )

  tot <- c(text = "TOTAL", rep("average", 3))

  list(table = tbl, total_row = tot)
}

# MNH ----
build_summary_mnh <- function(pf, dirname) {
  mnh <- seq_read("r.alt.mnh", dirname = dirname)

  pf_field <- seq_field("pcl_code")$name
  fun <- list(mean = mean, min = min, max = max)

  mnh_by_pf <- Reduce(
    function(x, y) merge(x, y, by = pf_field),
    lapply(
      names(fun),
      function(x) {
        r <- mnh
        names(r) <- paste0("H_", toupper(x))

        as.data.frame(
          terra::extract(r, pf[pf_field], fun[[x]], na.rm = TRUE, bind = TRUE, ID = FALSE)
        )
      }
    )
  )

  tot <- c(text = "TOTAL", rep("average", 3))

  list(table = mnh_by_pf, total_row = tot)
}

# EXPO ----
build_summary_expo <- function(pf, dirname) {
  expo <- seq_read("r.alt.expo", dirname = dirname)

  pf_field <- seq_field("pcl_code")$name

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

  missing_classes <- setdiff(classes, names(freq))
  freq[missing_classes] <- 0
  freq <- freq[classes]

  rs <- rowSums(freq)
  pct <- freq / rs * 100
  pct[rs == 0, ] <- NA_real_

  expo_by_pf <- cbind(
    sf::st_drop_geometry(pf[pf_field]),
    setNames(pct, paste0("%_", names(freq)))
  )

  expo_by_pf$EXPO_MAJ <- classes[max.col(freq, ties.method = "first")]
  expo_by_pf$EXPO_MAJ[rs == 0] <- NA_character_

  tot <- c(text = "TOTAL", rep("average", 4), "none")

  list(table = expo_by_pf, total_row = tot)
}

# PENTE ----
build_summary_pente <- function(pf, dirname) {
  pente <- seq_read("r.alt.pente", dirname = dirname)

  pf_field <- seq_field("pcl_code")$name

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

  missing_classes <- setdiff(classes, names(freq))
  freq[missing_classes] <- 0
  freq <- freq[classes]

  rs <- rowSums(freq)
  pct <- freq / rs * 100
  pct[rs == 0, ] <- NA_real_

  pente_by_pf <- cbind(
    sf::st_drop_geometry(pf[pf_field]),
    setNames(pct, paste0("% ", names(freq)))
  )

  pente_by_pf$PENTE_MAJ <- names(freq)[max.col(freq, ties.method = "first")]
  pente_by_pf$PENTE_MAJ[rs == 0] <- NA_character_

  tot <- c(text = "TOTAL", rep("average", 5), "none")

  list(table = pente_by_pf, total_row = tot)
}
