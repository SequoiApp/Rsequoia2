#' Open old `R_SEQUOIA` vector file
#'
#' @param dirname `character` Directory where the file is located.
#' Defaults to the current working directory.
#' @param layer `character` Layer name.
#'
#' @return An `sf` object or an empty `sf` object if no file are matched.
#'
#' @export
seq1_read <- function(dirname = ".", layer) {

  valid_layers <- c(
    "com_line", "com_point",
    "infra_poly", "infra_line", "infra_point",
    "parca_poly",
    "route_poly", "route_line",
    "ua_poly"
  )

  if (length(layer) != 1 || !layer %in% valid_layers) {
    cli::cli_abort(c(
      "x" = "{.arg layer} is equal to {.val {layer}}.",
      "i" = "{.arg layer} must be one of {.val {valid_layers}}."
    ))
  }

  pattern <- switch(
    layer,
    "com_line"    = "_COM_line\\.shp$",
    "com_point"   = "_COM_point\\.shp$",
    "infra_poly"  = "_INFRA_polygon\\.shp$",
    "infra_line"  = "_INFRA_line\\.shp$",
    "infra_point" = "_INFRA_point\\.shp$",
    "parca_poly"  = "_PARCA_polygon\\.shp$",
    "route_poly"  = "_ROUTE_polygon\\.shp$",
    "route_line"  = "_ROUTE_line\\.shp$",
    "ua_poly"     = "_UA_polygon\\.shp$"
  )

  file <- list.files(
    path = dirname,
    pattern = pattern,
    full.names = TRUE,
    recursive = TRUE
  )

  if (length(file) == 0) {
    cli::cli_warn(c(
      "!" = "No matched file for {.val {layer}}."
    ))
    return(seq_empty)
  }

  if (length(file) > 1) {
    cli::cli_abort(c(
      "x" = "Multiple files detected for {.val {layer}}. The folder must contain exactly one matching file."
    ))
  }

  sf::read_sf(file)
}

#' Retrieve old `R_SEQUOIA` _PARCA_ identifier
#'
#' @param dirname `character` Directory where the file _PARCA_ is located.
#' Defaults to the current working directory.
#'
#' @return The `character` identifier
#'
#' @export
seq1_id <- function(dirname = "."){
  file <- list.files(
    path = normalizePath(dirname),
    pattern = "_PARCA_polygon\\.shp$",
    full.names = FALSE,
    all.files = FALSE,
    recursive = TRUE
  )

  if (length(file) == 0) {
    cli::cli_warn(c(
      "!" = "No _PARCA_ file detected."
    ))
    return(NULL)
  }

  if (length(file) > 1) {
    cli::cli_abort(c(
      "x" = "Multiple files _PARCA_ detected. The folder must contain exactly one matching file."
    ))
  }

  fname <- basename(file)
  return(sub("_PARCA_polygon\\.shp$", "", fname))
}

#' Update `R_SEQUOIA` _PARCA_ layer to `Rsequoia2`
#'
#' @param parca `sf` object containing cadastrals parcels
#' @param id `character` Identifier of the project
#'
#' @return An `sf` object containing cadastral parcels, normalized for
#' `Rsequoia2` use.
#'
#' @export
update_parca <- function(parca, id){
  id_field <- seq_field("identifier")$name
  dep_code_field <- seq_field("dep_code")$name
  com_code_field <- seq_field("com_code")$name
  insee_field <- seq_field("insee")$name

  norm_parca <- seq_normalize(parca, "parca")
  norm_parca[[id_field]] <- id
  norm_parca[[insee_field]] <- paste0(norm_parca[[dep_code_field]],
                                      norm_parca[[com_code_field]])
  norm_parca
}

#' Update `R_SEQUOIA` _UA_ layer to `Rsequoia2`
#'
#' @param ua `sf` object containing analysis units
#' @param id `character` Identifier of the project
#'
#' @return An `sf` object containing analysis units, normalized for
#' `Rsequoia2` use.
#'
#' @export
update_ua <- function(ua, id){
  id_field <- seq_field("identifier")$name
  dep_code_field <- seq_field("dep_code")$name
  com_code_field <- seq_field("com_code")$name
  insee_field <- seq_field("insee")$name
  is_wooded <- seq_field("is_wooded")$name

  if ("OCCUP_SOL" %in% names(ua)){
    ua[[is_wooded]] <- ifelse(!is.na(ua$OCCUP_SOL),
                              ifelse(ua$OCCUP_SOL == "BOISEE", TRUE, FALSE),
                              NA)
  }

  norm_ua <- seq_normalize(ua, "ua")

  norm_ua[[id_field]] <- id
  norm_ua[[insee_field]] <- paste0(norm_ua[[dep_code_field]],
                                   norm_ua[[com_code_field]])
  norm_ua
}

#' Update `R_SEQUOIA` _UA_ layer to `Rsequoia2`
#'
#' @param ua `sf` object containing analysis units
#' @param id `character` Identifier of the project
#'
#' @return An `sf` object containing analysis units, normalized for
#' `Rsequoia2` use.
#'
#' @noRd
update_infra <- function(x) {

  # normalize
  gtype <- sf::st_geometry_type(x, by_geometry = FALSE)
  target <- switch(
    as.character(gtype),
    "POLYGON"      = "vct_poly",
    "MULTIPOLYGON" = "vct_poly",
    "LINESTRING"      = "vct_line",
    "MULTILINESTRING" = "vct_line",
    "POINT"      = "vct_point",
    "MULTIPOINT" = "vct_point",
    cli::cli_abort("Invalid geometry : {gtype}")
  )
  x <- seq_normalize(x, target)

  # update
  type_field <- seq_field("type")$name

  map <- c(
    "BT"     = "BAT",
    "SP"     = "SPO",
    "RESO"   = "RSO",
    "SURFO"  = "SFP",
    "SFO" = "SFP",
    "SURFOi" = "SFI",
    "LE" = "LEL",
    "VF" = "VFE",
    "RU" = "RUP",
    "RUI" = "RUP",
    "RUi" = "RUI",
    "RIN" = "RUI",
    "PTSO" = "MAR",
    "FF" = "FLD",
    "FLE" = "FLD",
    "CX" = "CRX",
    "NOMO" = "HYD",
    "NOM" = "TYP",
    "VEG" = "FOR"
  )

  x[[type_field]] <- ifelse(
    x[[type_field]] %in% names(map),
    map[x[[type_field]]],
    x[[type_field]]
  )

  return(invisible(x))
}

#' Update `R_SEQUOIA` projet to `Rsequoia2`
#'
#' @param dirname `character` Directory where the `R_SEQUOIA` files are located.
#'   Defaults to the current working directory.
#' @param verbose `logical` If `TRUE`, display progress messages.
#'   Defaults to `TRUE`.
#' @param overwrite `logical`; whether to overwrite existing files.
#'   Defaults to `FALSE`.
#'
#' @return
#' Invisibly returns a named vector of file paths written by [seq_write()].
#' Returns `NULL` invisibly when no layer are found.
#'
#' @export
seq1_update <- function(
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE
) {
  # tiny helper ----
  seq_write2 <- function(x, key) {
    seq_write(x, key, dirname = dirname, id = id, verbose = verbose, overwrite = overwrite)
  }

  # id
  id_field <- seq_field("identifier")$name
  id <- seq1_id(dirname)

  # paths
  paths <- c()

  # parca
  parca <- seq1_read(dirname, "parca_poly")
  if (nrow(parca)>0){
    parca <- update_parca(parca, id)
    parca_path <- seq_write2(parca, "v.seq.parca.poly")
    paths <- c(paths, parca_path)
  }

  # ua
  ua <- seq1_read(dirname, "ua_poly")
  if (nrow(parca)>0){
    ua <- update_ua(ua, id)
    ua_path <- seq_write2(ua, "v.seq.ua.poly")
    paths <- c(paths, ua_path)
  }

  # com_line
  com_line <- seq1_read(dirname, "com_line")
  if (nrow(com_line)>0){
    com_line <- sf::st_sf(geometry = sf::st_geometry(com_line)) |>
      sf::st_make_valid()
    com_line[[id_field]] <- id
    com_line_path <- seq_write2(com_line, "v.com.graphic.line")
    paths <- c(paths, com_line_path)
  }

  # com_point
  com_point <- seq1_read(dirname, "com_point")
  if (nrow(com_line)>0){
    com_point <- seq_normalize(com_point, "vct_point")
    com_point[[id_field]] <- id
    com_point_path <- seq_write2(com_point, "v.com.graphic.point")
    paths <- c(paths, com_point_path)
  }

  # infra_poly
  type_field <- seq_field("type")$name # for all infra_

  infra_poly <- seq1_read(dirname, "infra_poly")
  if (nrow(infra_poly)>0){
    infra_poly <- seq_normalize(infra_poly, "vct_poly")
    infra_poly <- update_infra(infra_poly)
    infra_poly[[id_field]] <- id

    ## hydro
    hydro_poly <- infra_poly[infra_poly[[type_field]] %in%  c("RSO", "SFP", "SFI"), ]
    if (nrow(hydro_poly)>0){
      hydro_poly_path <- seq_write2(hydro_poly, "v.hydro.poly")
      paths <- c(paths, hydro_poly_path)
    }

    ## veg
    vege_poly <- infra_poly[infra_poly[[type_field]] %in%  c("FOR"), ]
    if (nrow(vege_poly)>0){
      vege_poly_path <- seq_write2(vege_poly, "v.vege.poly")
      paths <- c(paths, vege_poly_path)
    }

    ## infra
    infra_poly <- infra_poly[!infra_poly[[type_field]] %in%  c("RSO", "SFP", "SFI", "FOR"), ]
    if (nrow(infra_poly)>0){
      infra_poly_path <- seq_write2(infra_poly, "v.infra.poly")
      paths <- c(paths, infra_poly_path)
    }
  }

  # infra_line
  infra_line <- seq1_read(dirname, "infra_line")
  if (nrow(infra_line)>0){
    infra_line <- seq_normalize(infra_line, "vct_line")
    infra_line <- update_infra(infra_line)
    infra_line[[id_field]] <- id

    ## hydro
    hydro_line <- infra_line[infra_line[[type_field]] %in%  c("RUP", "RUI"), ]
    if (nrow(hydro_line)>0){
      hydro_line_path <- seq_write2(hydro_line, "v.hydro.line")
      paths <- c(paths, hydro_line_path)
    }

    ## veg
    vege_line <- infra_line[infra_line[[type_field]] %in%  c("FOR"), ]
    if (nrow(vege_line)>0){
      vege_line_path <- seq_write2(vege_line, "v.vege.line")
      paths <- c(paths, vege_line_path)
    }

    ## infra
    infra_line <- infra_line[!infra_line[[type_field]] %in%  c("RUP", "RUI", "FOR"), ]
    if (nrow(infra_line)>0){
      infra_line_path <- seq_write2(infra_line, "v.infra.line")
      paths <- c(paths, infra_line_path)
    }
  }

  # infra_point
  infra_point <- seq1_read(dirname, "infra_point")
  if (nrow(infra_point)>0){
    infra_point <- seq_normalize(infra_point, "vct_line")
    infra_point <- update_infra(infra_point)
    infra_point[[id_field]] <- id

    ## hydro
    hydro_point <- infra_point[infra_point[[type_field]] %in%  c("MAR"), ]
    if (nrow(hydro_point)>0){
      hydro_point_path <- seq_write2(hydro_point, "v.hydro.point")
      paths <- c(paths, hydro_point_path)
    }

    ## veg
    vege_point <- infra_point[infra_point[[type_field]] %in%  c("FEV", "REV", "FRV", "FEP", "REP"), ]
    if (nrow(vege_point)>0){
      vege_point_path <- seq_write2(vege_point, "v.vege.point")
      paths <- c(paths, vege_point_path)
    }

    ## topo
    topo_point <- infra_point[infra_point[[type_field]] %in%  c("TYP", "HYD"), ]
    if (nrow(topo_point)>0){
      topo_point_path <- seq_write2(topo_point, "v.toponyme.point")
      paths <- c(paths, topo_point_path)
    }

    ## infra
    infra_point <- infra_point[!infra_point[[type_field]] %in%  c("MAR", "FEV", "REV", "FRV", "FEP", "REP", "TYP", "HYD"), ]
    if (nrow(infra_point)>0){
      infra_point_path <- seq_write2(infra_point, "v.infra.point")
      paths <- c(paths, infra_point_path)
    }
  }

  # road_poly
  road_poly <- seq1_read(dirname, "route_poly")
  if (nrow(com_line)>0){
    road_poly <- seq_normalize(road_poly, "road_poly")
    road_poly[[id_field]] <- id
    road_poly_path <- seq_write2(road_poly, "v.road.graphic.poly")
    paths <- c(paths, road_poly_path)
  }

  # road_line
  road_line <- seq1_read(dirname, "route_line")
  if (nrow(road_line)>0){
    road_line <- seq_normalize(road_line, "road_line")
    road_line[[id_field]] <- id
    road_line_path <- seq_write2(road_line, "v.road.graphic.line")
    paths <- c(paths, road_line_path)
  }

  # result
  return(invisible(paths))
}
