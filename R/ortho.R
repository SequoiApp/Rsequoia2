
get_ortho <- function(
    x,
    type = c("irc", "rgb"),
    res = 1,
    crs = 2154,
    buffer = 200,
    overwrite = FALSE,
    verbose = TRUE){

  if (!inherits(x, c("sf", "sfc"))){
    cli::cli_abort(c(
      "x" = "{.arg x} is of class {.cls {class(x)}}.",
      "i" = "{.arg x} should be of class {.cls sf} or {.cls sfc}."
      ))
  }

  if (length(type) > 1 | !(type %in% c("irc", "rgb"))){
    cli::cli_abort(c(
      "x" = "{.arg type} is equal to {.val {type}}.",
      "i" = "{.arg type} should be one of {.val irc} or {.val rgb}."
      ))
  }

  x <- sf::st_transform(x, 2154)
  x_buff <- sf::st_buffer(x, buffer)

  layer <- switch(type,
                  "irc" = "ORTHOIMAGERY.ORTHOPHOTOS.IRC",
                  "rgb" = "ORTHOIMAGERY.ORTHOPHOTOS"
  )

  r <- happign::get_wms_raster(
    x_buff,
    layer = layer,
    crs = crs,
    res = res,
    verbose = verbose,
    overwrite = overwrite
  )

  r_mask <- terra::mask(r, terra::vect(x_buff))

  return(r_mask)
}

seq_ortho <- function(
    dirname = ".",
    type = c("irc", "rgb"),
    res = 1,
    crs = 2154,
    buffer = 200,
    overwrite = FALSE,
    verbose = TRUE){

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)

  path <- lapply(type, function(type) {
    r <- get_ortho(
      parca,
      type = type,
      res = res,
      crs = crs,
      buffer = buffer,
      overwrite = overwrite,
      verbose = verbose
    )

    path <- seq_write(
      r,
      key = switch(type, "irc" = "r.irc", "rgb" = "r.rgb"),
      dirname = dirname,
      overwrite = overwrite,
      verbose = verbose
    )
  })

  return(path)

}
