#' Download orthophotos from the IGN WMS (RGB or IRC)
#'
#' Downloads an orthophoto (RGB or infrared/IRC) from the IGN WMTS service for the
#' area covering `x` expanded with a buffer.
#' The result is returned as a masked `SpatRaster`, clipped to the input geometry
#' to keep file size minimal.
#'
#' @param x `sf` or `sfc`; Geometry located in France.
#' @param type `character`; Type of orthophoto to download. Must be one of:
#'   - `"rgb"` — true-color orthophoto
#'   - `"irc"` — near-infrared orthophoto
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#' the download area.
#' @param zoom `integer` between 0 and 21. The smaller the zoom level, the less
#' precise the resolution(see [happign::get_wmts()])
#' @param crs `numeric` or `character`; CRS of the returned raster (see
#' [happign::get_wmts()])
#' @param overwrite `logical`; If `TRUE`, file is overwritten.
#' @param verbose `logical`; If `TRUE`, display messages.
#'
#' @details
#' The orthophoto retrieved contains data for the whole bounding
#' box (bbox) of `x` (plus the buffer).
#' To reduce the final file size and avoid unnecessary pixels, the raster is
#' immediately masked with the buffered geometry.
#'
#' @return `SpatRaster` object from `terra` package
#'
#' @seealso [happign::get_wmts()]
#'
#' @examples
#' \dontrun{
#'
#' p <- sf::st_sfc(st_point(c(-4.372746579180652, 47.79820761331345)), crs = 4326)
#'
#' ortho <- get_ortho(p, type = "rgb", buffer = 50)
#' irc <- get_ortho(p, type = "irc", buffer = 50)
#'
#' terra::plotRGB(ortho)
#' terra::plotRGB(irc)
#'
#' }
#'
get_ortho <- function(
    x,
    type = c("irc", "rgb"),
    buffer = 200,
    zoom = 12,
    crs = 2154,
    overwrite = FALSE,
    verbose = TRUE){

  if (!inherits(x, c("sf", "sfc"))){
    cli::cli_abort(c(
      "x" = "{.arg x} is of class {.cls {class(x)}}.",
      "i" = "{.arg x} should be of class {.cls sf} or {.cls sfc}."
      ))
  }

  if (length(type) != 1 || !type %in% c("irc", "rgb")) {
    cli::cli_abort(c(
      "x" = "{.arg type} is equal to {.val {format(type)}}.",
      "i" = "{.arg type} must be equal to {.val irc} or {.val rgb}."
    ))
  }

  x <- sf::st_transform(x, 2154)
  x_env <- envelope(x, buffer)

  layer <- switch(
    type,
    "irc" = "ORTHOIMAGERY.ORTHOPHOTOS.IRC",
    "rgb" = "ORTHOIMAGERY.ORTHOPHOTOS.BDORTHO"
  )

  if (verbose) {cli::cli_alert_info("Downloading {toupper(type)} dataset...")}

  pb <- cli::cli_progress_bar(toupper(type), total = nrow(x_env), clear = TRUE)
  tmp <- tempdir()
  files <- c()
  for (i in seq_len(nrow(x_env))){

    if (verbose) {cli::cli_progress_update(id = pb)}

    file <- sprintf(file.path(tmp, sprintf("r_%03d.tif", i)))

    happign::get_wmts(
      x_env[i, ],
      layer = layer,
      zoom = zoom,
      crs = crs,
      filename = file,
      overwrite = TRUE,
      verbose = TRUE) |> suppressWarnings()

    files <- c(files, file)
  }
  cli::cli_progress_done(id = pb)

  v <- terra::vrt(files, options = c("-hidenodata"))

  if (verbose) {cli::cli_alert_info("Raster size optimization...")}
  r_mask <- terra::mask(v, x_env)
  terra::RGB(r_mask) <- c(1, 2, 3, 4)
  names(r_mask) <- c("red", "green", "blue", "alpha")

  return(invisible(r_mask))
}

#' Download RGB and/or IRC orthophotos for a Sequoia project
#'
#' Downloads one or several orthophotos (RGB and/or IRC) from the IGN WMTS service
#' for the `parca` layer of a Sequoia project.
#'
#' This function is a convenience wrapper looping over [get_ortho()], allowing
#' the user to download both products in one call and automatically write them
#' to the project directory using [seq_write()].
#'
#' @inheritParams get_ortho
#' @inheritParams seq_write
#'
#' @param type `character` One or several orthophoto types to download.
#' Must be one or both of:
#'   - `"rgb"` — true-color orthophoto
#'   - `"irc"` — near-infrared orthophoto
#'
#' @return A named list of file paths written by [seq_write()], one per `type`.
#'
#' @seealso [get_ortho()], [seq_write()]
#'
seq_ortho <- function(
    dirname = ".",
    type = c("irc", "rgb"),
    buffer = 200,
    zoom = 12,
    crs = 2154,
    overwrite = FALSE,
    verbose = TRUE){

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  if (verbose){
    cli::cli_h1("IMAGERY")
  }

  paths <- lapply(type, function(type) {
    r <- seq_retry(
      get_ortho(
        parca,
        type = type,
        zoom = zoom,
        crs = crs,
        buffer = buffer,
        overwrite = overwrite,
        verbose = verbose
      ),
      verbose = verbose
    )

    path <- seq_write(
      r,
      key = switch(type, "irc" = "r.ortho.irc", "rgb" = "r.ortho.rgb"),
      id = id,
      dirname = dirname,
      overwrite = overwrite,
      verbose = verbose
    )
  })

  return(invisible(paths))

}
