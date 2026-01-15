seq_summary <- function(dirname = ".", overwrite = FALSE, verbose = TRUE){

  # ua_path <- "C:\\Users\\PaulCarteron\\Desktop\\temp\\sequoia_test\\ESTREMONT_UA_polygon.shp"
  # ua_sf <- sf::st_read(ua_path) |> seq_normalize("ua")

  ua <- seq_read("v.seq.ua.poly", dirname = dirname)

  sum_surf_by <- function(ua, ...){

    ua <- sf::st_drop_geometry(ua)
    surf_cor <- seq_field("surf_cor")$name

    by <- list(...) |> lapply(\(x) seq_field(x)$name)
    by_formula <- paste(by, collapse = " + ")

    aggregate(
      stats::as.formula(paste(cor_area, "~", by_formula)),
      data = ua,
      FUN = sum,
      na.rm = TRUE,
      na.action = stats::na.pass
    )
  }
  order_by <- function(to_order, ..., decreasing = FALSE) {

    by <- vapply(
      list(...),
      function(x) seq_field(x)$name,
      character(1)
    )

    cols <- lapply(by, function(nm) to_order[[nm]])
    o <- do.call(order, c(cols, list(decreasing = decreasing)))
    to_order[o, , drop = FALSE]
  }
  pivot <- function(to_pivot, row, col, ...){
    pivoted <- stats::reshape(to_pivot,
            idvar = seq_field(row)$name,
            timevar = seq_field(col)$name,
            sep = "___",
            direction = "wide"
    )
    names(pivoted) <- sub("^.*___", "", names(pivoted))

    return(pivoted)
  }

  cor_area <- seq_field("cor_area")$name

  ocs <- sum_surf_by(ua, "land_use")

  pc <- sum_surf_by(ua, "com_nom", "section", "numero")
  pc_by_com <- sum_surf_by(ua, "com_nom")
  pc_by_dep <- sum_surf_by(ua, "dep_nom")

  pf <- sum_surf_by(ua, "parcelle", "com_num", "section", "numero")
  pf <- order_by(pf, "parcelle")

  sspf <- sum_surf_by(ua, "parcelle", "sous_parcelle", "com_num", "section", "numero")
  sspf <- order_by(sspf, "parcelle", "sous_parcelle")

  plt <- sum_surf_by(ua, "stand")
  plt <- order_by(plt, "surf_cor", decreasing = TRUE)
  plt$PROPORTION <- plt[[cor_area]]/sum(plt[[cor_area]])

  station <- sum_surf_by(ua, "sol")

  pf_by_plt <- sum_surf_by(ua, "parcelle", "stand")
  pf_by_plt <- order_by(pf_by_plt, "stand")
  pf_by_plt <- pivot(pf_by_plt, "parcelle", "stand", direction = "wide")
  pf_by_plt <- order_by(pf_by_plt, "parcelle")

  plt_by_pf <- sum_surf_by(ua, "parcelle", "stand")
  plt_by_pf <- order_by(plt_by_pf, "parcelle")
  plt_by_pf <- pivot(plt_by_pf, "stand", "parcelle", direction = "wide")
  plt_by_pf <- order_by(plt_by_pf, "stand")

  pc_by_plt <- sum_surf_by(ua, "stand", "idu")
  pc_by_plt <- order_by(pc_by_plt, "stand")
  pc_by_plt <- pivot(pc_by_plt, "idu", "stand", direction = "wide")
  pc_by_plt <- order_by(pc_by_plt, "idu")

  ame <- sum_surf_by(ua, "amenagement")
  ame <- order_by(ame, "amenagement")
  ame$PROPORTION <- ame[[cor_area]]/sum(ame[[cor_area]])

  elevation <- seq_read("r.alt.mnt", dirname = dirname)
  parcelle <- seq_field("parcelle")$name
  pf <- terra::vect(ua[, parcelle]) |> terra::aggregate(parcelle, count = FALSE)

  fun <- list(mean = mean, min = min, max = max)
  elevation_by_pf <- Reduce(
    function(x, y) merge(x, y, by = "N_PARFOR"),
    lapply(
      names(fun),
      function(x){
        names(elevation) <- paste0("ALTITUDE_", toupper(x))
        as.data.frame(
          terra::extract(elevation, pf, fun[[x]], na.rm = TRUE, bind = TRUE, ID = FALSE)
        )}
    ))


  pente <- seq_read("r.alt.pente", dirname = dirname)
  stand <- seq_field("stand")$name
  pente_by_plt <- as.data.frame(
    terra::extract(
      pente |> setNames("PENTE"),
      terra::vect(ua[, stand]) |> terra::aggregate(stand, count = FALSE),
      fun = mean,
      na.rm = TRUE,
      bind = TRUE,
      ID = FALSE
    )
  )

  expo <- seq_read("r.alt.expo", dirname = dirname)
  expo_by_plt <- as.data.frame(
    terra::extract(
      expo |> setNames("EXPOSITION"),
      terra::vect(ua[, stand]) |> terra::aggregate(stand, count = FALSE),
      fun = mean,
      na.rm = TRUE,
      bind = TRUE,
      ID = FALSE
    )
  )

}
