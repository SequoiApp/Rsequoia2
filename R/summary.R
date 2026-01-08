seq_summary <- function(dirname = ".", overwrite = FALSE, verbose = TRUE){

  ua <- seq_read("v.seq.ua.poly", dirname = dirname)
  ua_path <- "C:\\Users\\PaulCarteron\\Desktop\\temp\\sequoia_test\\ESTREMONT_UA_polygon.shp"
  ua <- sf::read_sf(ua_path) |> seq_normalize("ua") |> sf::st_drop_geometry()

  sum_surf_by <- function(ua, ...){
    surf_cor <- seq_field("surf_cor")$name

    by <- list(...) |> lapply(\(x) seq_field(x)$name)
    by_formula <- paste(by, collapse = " + ")

    aggregate(
      stats::as.formula(paste(surf_cor, "~", by_formula)),
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

  surf_cor <- seq_field("surf_cor")$name

  ocs <- sum_surf_by(ua, "land_use")

  pc <- sum_surf_by(ua, "com_nom", "section", "numero")
  pc_by_com <- sum_surf_by(ua, "com_nom")
  pc_by_dep <- sum_surf_by(ua, "dep_nom")

  pf <- sum_surf_by(ua, "parcelle", "com_num", "section", "numero")
  pf <- order_by(sspf, "parcelle")

  sspf <- sum_surf_by(ua, "parcelle", "sous_parcelle", "com_num", "section", "numero")
  sspf <- order_by(sspf, "parcelle", "sous_parcelle")

  plt <- sum_surf_by(ua, "peuplement")
  plt <- order_by(plt, "surf_cor", decreasing = TRUE)
  plt$PROPORTION <- plt[[surf_cor]]/sum(plt[[surf_cor]])

  station <- sum_surf_by(ua, "sol")

  pf_by_plt <- sum_surf_by(ua, "parcelle", "peuplement")
  pf_by_plt <- order_by(pf_by_plt, "peuplement")
  pf_by_plt <- pivot(pf_by_plt, "parcelle", "peuplement", direction = "wide")
  pf_by_plt <- order_by(pf_by_plt, "parcelle")

  plt_by_pf <- sum_surf_by(ua, "parcelle", "peuplement")
  plt_by_pf <- order_by(plt_by_pf, "parcelle")
  plt_by_pf <- pivot(plt_by_pf, "peuplement", "parcelle", direction = "wide")
  plt_by_pf <- order_by(plt_by_pf, "peuplement")

  pc_by_plt <- sum_surf_by(ua, "peuplement", "idu")
  pc_by_plt <- order_by(pc_by_plt, "peuplement")
  pc_by_plt <- pivot(pc_by_plt, "idu", "peuplement", direction = "wide")
  pc_by_plt <- order_by(pc_by_plt, "idu")

  ame <- sum_surf_by(ua, "amenagement")
  ame <- order_by(ame, "amenagement")
  ame$PROPORTION <- ame[[surf_cor]]/sum(ame[[surf_cor]])


}
