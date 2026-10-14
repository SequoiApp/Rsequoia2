#' Optimize downloaded orthophoto tiles
#'
#' Builds a virtual raster from downloaded tiles, masks it to the requested
#' area and assigns RGB/alpha bands.
#'
#' @param files `character`; Raster tile paths.
#' @param x `sf` or `sfc`; Geometry used to mask the raster.
#'
#' @return A `SpatRaster`.
#'
#' @keywords internal
#' @noRd
.ortho_transformer <- function(files, x, crs = 2154, crop = TRUE) {
  r <- terra::vrt(files, options = "-hidenodata")

  x <- sf::st_transform(x, terra::crs(r))
  x <- terra::vect(x)

  if (crop) r <- terra::crop(r, x)
  r <- terra::mask(r, x)

  target_crs <- sf::st_crs(crs)
  if (!terra::same.crs(r, target_crs$wkt)) {
    r <- terra::project(r, target_crs$wkt)
  }

  terra::RGB(r) <- c(1, 2, 3, 4)
  names(r) <- c("red", "green", "blue", "alpha")

  r
}

#' Download orthophotos from the IGN WMTS
#'
#' @inheritParams seq_write
#' @param x `sf` or `sfc`; Geometry located in France.
#' @param type `character`; One of `"irc"` or `"rgb"`.
#' @param buffer `numeric`; Buffer around `x`, in meters.
#' @param zoom `integer`; WMTS zoom level.
#' @param crs CRS of the returned raster.
#'
#' @return A `SpatRaster`.
#'
#' @export
get_ortho <- function(
    x,
    type = c("irc", "rgb"),
    buffer = 200,
    zoom = 12,
    crs = 2154,
    overwrite = FALSE,
    verbose = TRUE) {

  if (!inherits(x, c("sf", "sfc"))) {
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

  if (!is.numeric(buffer) || length(buffer) != 1L || !is.finite(buffer) || buffer < 0) {
    cli::cli_abort("{.arg buffer} must be a finite, non-negative number.")
  }

  x_clean <- x |>
    sf::st_geometry() |>
    sf::st_transform(crs) |>
    sf::st_make_valid()

  x_clean <- x_clean[!sf::st_is_empty(x_clean)]
  if (length(x_clean) == 0) {
    cli::cli_abort("{.arg x} has no valid geometry after cleaning.")
  }

  point_or_line <- sf::st_dimension(x_clean) < 2
  has_point_or_line <- any(point_or_line)
  if (buffer == 0 && has_point_or_line) {
    cli::cli_abort(c(
      "x" = "{.arg buffer} must be positive for points and lines.",
      "i" = "{cli::qty(sum(point_or_line))}Row{?s} {.val {which(point_or_line)}} {cli::qty(sum(point_or_line))}{?is/are} point or line."
    ))
  }

  x_env <- x_clean |>
    sf::st_buffer(buffer) |>
    sf::st_union() |>
    suppressWarnings(sf::st_collection_extract("POLYGON")) |>
    sf::st_cast("POLYGON", warn = FALSE)

  x_env <- x_env[!sf::st_is_empty(x_env)]
  if (length(x_env) == 0) {
    cli::cli_abort("{.arg x} has no valid polygon geometry after buffering.")
  }

  layer <- switch(
    type,
    irc = "ORTHOIMAGERY.ORTHOPHOTOS.IRC",
    rgb = "ORTHOIMAGERY.ORTHOPHOTOS.BDORTHO"
  )

  if (verbose) {
    pb <- cli::cli_progress_message(
      "Downloading {toupper(type)} dataset...",
      clear = TRUE
    )
    on.exit(cli::cli_progress_done(id = pb, result = "clear"), add = TRUE)
  }

  files <- vapply(seq_along(x_env), function(i) {
    file <- file.path(tempdir(), sprintf("r_%03d.tif", i))

    suppressWarnings(
      happign::get_wmts(
        x_env[i],
        layer = layer,
        zoom = zoom,
        crs = crs,
        filename = file,
        overwrite = TRUE,
        verbose = verbose
      )
    )

    file
  }, character(1))

  if (verbose) cli::cli_progress_done(pb)
  if (verbose) cli::cli_progress_message("Optimizing raster...", clear = TRUE)

  invisible(.ortho_transformer(files, x_env))
}


#' Fetch orthophoto layers
#'
#'
#' @inheritParams get_ortho
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#' @param type `character`; One or several orthophoto types.
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @keywords internal
#' @noRd
.ortho_fetcher <- function(
    x,
    dirname,
    id = NULL,
    type = c("irc", "rgb"),
    buffer = 200,
    zoom = 12,
    crs = 2154,
    overwrite = FALSE,
    verbose = TRUE) {

  allowed <- c("irc", "rgb")
  if (!all(type %in% allowed)) {
    cli::cli_abort("{.arg type} must be one or more of {.vals {allowed}}.")
  }

  if (verbose) cli::cli_h1("IMAGERY")

  keys <- c(
    irc = "r.ortho.irc",
    rgb = "r.ortho.rgb"
  )

  paths <- lapply(type, function(k) {
    tryCatch({
      r <- seq_retry(
        get_ortho(
          x,
          type = k,
          buffer = buffer,
          zoom = zoom,
          crs = crs,
          overwrite = overwrite,
          verbose = verbose
        ),
        verbose = verbose
      )

      seq_write(
        r,
        key = keys[[k]],
        dirname = dirname,
        id = id,
        overwrite = overwrite,
        verbose = verbose
      )

    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_danger(
          "Failed ORTHO layer {.val {k}}: {conditionMessage(e)}"
        )
      }
      NULL
    })
  })

  names(paths) <- unname(keys[type])
  invisible(Filter(Negate(is.null), paths))
}


#' Fetch orthophotos for an area
#'
#' Downloads RGB and/or IRC orthophotos around `x` and writes them to
#' `dirname`.
#'
#' @inheritParams get_ortho
#' @param dirname `character`; Output directory.
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @keywords internal
#' @noRd
fetch_ortho <- function(
    x,
    dirname,
    type = c("irc", "rgb"),
    buffer = 200,
    zoom = 12,
    crs = 2154,
    overwrite = FALSE,
    verbose = TRUE) {

  .ortho_fetcher(
    x = x,
    dirname = dirname,
    type = type,
    buffer = buffer,
    zoom = zoom,
    crs = crs,
    overwrite = overwrite,
    verbose = verbose
  )
}


#' Download orthophotos for a Sequoia project
#'
#' Downloads RGB and/or IRC orthophotos around the project area and writes
#' them to the Sequoia project directory.
#'
#' @inheritParams get_ortho
#' @inheritParams seq_write
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @seealso [get_ortho()], [seq_write()]
#'
#' @export
seq_ortho <- function(
    dirname = ".",
    type = c("irc", "rgb"),
    buffer = 200,
    zoom = 12,
    crs = 2154,
    overwrite = FALSE,
    verbose = TRUE) {

  ctx <- .seq_context(dirname)

  .ortho_fetcher(
    x = ctx$parca,
    dirname = dirname,
    id = ctx$id,
    type = type,
    buffer = buffer,
    zoom = zoom,
    crs = crs,
    overwrite = overwrite,
    verbose = verbose
  )
}
