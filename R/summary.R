#' Summarize information for a Sequoia folder
#'
#' Generate synthetic table from Sequoia layers and write them as `.xlsx`
#' inside the current Sequoia dir. Useful for redaction of management document.
#'
#' @inheritParams seq_write
#'
#' @detail
#' Available tables:
#'  - `"CAD"`: Cadastral parcels
#'  - `"CAD_COM"`: Surfaces by communes
#'  - `"PF"`: Surfaces by forest parcels
#'  - `"SSPF"`: Surfaces by forest sub-parcels
#'  - `"CAD_PLT"`: Link between cadastral parcels and forest parcels
#'  - `"OCCUPATION"`: Surfaces by land use
#'  - `"GEOLOGY"`: Surfaces by geology
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

  ua <- seq_read("v.seq.ua.poly", dirname = dirname)
  pf <- ua_to_pf(ua)

  sum_surf_by <- function(ua, ...){

    ua <- sf::st_drop_geometry(ua)
    surf_cor <- seq_field("cor_area")$name

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

  cor_area <- seq_field("cor_area")$name

  ocs <- sum_surf_by(ua, "is_wooded")
  ocs <- add_total(ocs, "cor_area")

  ua <- ua[ua[[seq_field("is_wooded")$name]], ]

  pc_raw <- sum_surf_by(ua, "reg_name", "dep_name", "com_name", "insee", "prefix", "section", "number") |>
    add_total("cor_area")

  pc_by_com <- sum_surf_by(ua, "insee", "com_name") |>
    add_total("cor_area")

  pf_raw <- sum_surf_by(ua, "pcl_code") |>
    order_by("pcl_code") |>
    add_total("cor_area")

  sspf_raw <- sum_surf_by(ua, "pcl_code", "sub_code") |>
    order_by("pcl_code", "sub_code") |>
    add_total("cor_area")

  pf_by_cad <- sum_surf_by(ua, "pcl_code", "com_name", "insee", "prefix", "section", "number") |>
    order_by("pcl_code","insee", "prefix", "section", "number") |>
    add_total("cor_area")

  sspf_by_cad <- sum_surf_by(ua, "pcl_code", "sub_code", "com_name", "insee", "prefix", "section", "number") |>
    order_by("pcl_code", "sub_code","insee", "prefix", "section", "number") |>
    add_total("cor_area")

  plt <- sum_surf_by(ua, "std_type") |>
    order_by("cor_area", decreasing = TRUE) |>
    add_prop("cor_area") |>
    add_total("cor_area", "PROPORTION")

  geol <- sum_surf_by(ua, "soil") |>
    add_prop("cor_area") |>
    add_total("cor_area", "PROPORTION")

  pf_by_plt <- sum_surf_by(ua, "pcl_code", "std_type") |>
    order_by("std_type") |>
    pivot("pcl_code", "std_type", direction = "wide") |>
    order_by("pcl_code") |>
    add_total(unique(ua[[seq_field("std_type")$name]]))

  plt_by_pf <- sum_surf_by(ua, "pcl_code", "std_type") |>
    order_by("pcl_code") |>
    pivot("std_type", "pcl_code", direction = "wide") |>
    order_by("std_type") |>
    add_total(unique(ua[[seq_field("std_type")$name]]))

  ame <- sum_surf_by(ua, "treatment") |>
    order_by("treatment") |>
    add_prop("cor_area") |>
    add_total("cor_area", "PROPORTION")

  elevation <- seq_read("r.alt.mnt", dirname = dirname)
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
    pf[[pcl_code]],
    freq,
    setNames(freq / rowSums(freq) * 100, paste0("%_", names(freq)))
  )
  expo_by_pf$EXPO_MAJ <- classes[max.col(freq[classes])]

  pente <- seq_read("pente", dirname)
  pente_class <- terra::classify(pente, c(-Inf, 10, 40, 60, 80, Inf))
  freq <- terra::extract(pente_class, pf, "table", ID = FALSE) |> as.data.frame()

  pente_by_pf <- cbind(
    pf[[pcl_code]],
    freq,
    setNames(freq / rowSums(freq) * 100, paste0("% ", names(freq)))
  )
  pente_by_pf$PENTE_MAJ <- names(freq)[max.col(freq)]

  filename <- dirname |> file.path(seq_layer("summary")$filename)

  synthese <- list(
    CAD = pc_raw,
    CAD_COM = pc_by_com,
    PF = pf_raw,
    SSPF = sspf_raw,
    PF_CAD = pf_by_cad,
    SSPF_CAD = sspf_by_cad,
    OCCUPATION = ocs,
    GEOLOGY = geol,
    PLT_PF = plt_by_pf,
    PF_PLT = pf_by_plt,
    GESTION = ame,
    ALTI_PF = elevation_by_pf,
    EXPO_PF = expo_by_pf,
    PENTE_PF = pente_by_pf
  )

  seq_xlsx(
    synthese,
    filename = filename,
    overwrite = overwrite,
    verbose = verbose
  )

  return(synthese)
}
