#' Retrieve forest vegetation polygons around an area
#'
#' @param x `sf` or `sfc`; Geometry located in France.
#' @param buffer `numeric`; Buffer around `x`, in meters.
#' @param clip `logical`; If `TRUE`, remove the forest area itself.
#' @param tol `numeric`; Minimum area threshold in square meters.
#'
#' @return An `sf` polygon layer. An empty standardized layer is returned
#'   when no vegetation is found.
#'
#' @export
get_vege_poly <- function(x, buffer = 1000, clip = TRUE, tol = 500) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}.")
  }

  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- seq_envelope(x, buffer)
  control_envelope <- seq_envelope(x, buffer + 500)

  forest_mask <- happign::get_wfs(
    fetch_envelope,
    layer = "IGNF_MASQUE-FORET.2021-2023:masque_foret",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  empty <- function() {
    cli::cli_warn("No vegetation data found. Empty {.cls sf} is returned.")
    invisible(create_empty_sf("POLYGON") |> seq_normalize("vct_poly"))
  }

  if (is.null(forest_mask) || !nrow(forest_mask)) return(empty())

  vege <- suppressWarnings(
    forest_mask |>
      sf::st_transform(crs) |>
      sf::st_intersection(control_envelope) |>
      sf::st_cast("MULTIPOLYGON") |>
      sf::st_cast("POLYGON", warn = FALSE)
  )

  if (clip) {
    forest <- seq_dissolve(sf::st_buffer(x, 5), 5)

    vege <- suppressWarnings(
      vege |>
        sf::st_difference(sf::st_union(forest)) |>
        sf::st_cast("MULTIPOLYGON") |>
        sf::st_cast("POLYGON", warn = FALSE) |>
        sf::st_make_valid()
    )
  }

  vege <- remove_small_geometries(vege, tol) |> sf::st_make_valid()
  if (!nrow(vege)) return(empty())

  vege[[seq_field("type")$name]] <- "FOR"
  vege[[seq_field("source")$name]] <- "ignf_masque_foret"

  invisible(vege)
}


#' Remove geometries below a minimum area threshold
#'
#' @param x `sf` or `sfc`; Input polygon geometries.
#' @param tol `numeric`; Minimum area threshold in square meters.
#' @param crs `numeric`; Metric CRS used for area calculation.
#'
#' @return An object of the same class as `x`.
#'
#' @keywords internal
#' @noRd
remove_small_geometries <- function(x, tol, crs = 2154) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}.")
  }

  if (!is.numeric(tol) || length(tol) != 1 || is.na(tol) || tol < 0) {
    cli::cli_abort("{.arg tol} must be a single non-negative numeric value.")
  }

  original_crs <- sf::st_crs(x)

  x <- x |>
    sf::st_transform(crs) |>
    sf::st_make_valid() |>
    quiet()

  keep <- as.numeric(sf::st_area(x)) >= tol
  x <- x[keep, , drop = FALSE]
  sf::st_transform(x, original_crs)

}


#' Retrieve forest vegetation lines around an area
#'
#' @inheritParams get_vege_poly
#' @param poly Optional preloaded vegetation polygon layer.
#'
#' @return An `sf` line layer.
#'
#' @export
get_vege_line <- function(
    x,
    buffer = 1000,
    clip = TRUE,
    tol = 500,
    poly = NULL) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}.")
  }

  if (!is.numeric(buffer) || length(buffer) != 1 || is.na(buffer) || buffer < 5) {
    cli::cli_abort("{.arg buffer} must be a single numeric value >= 5.")
  }

  crs <- 2154
  x <- sf::st_transform(x, crs)
  cleaning_envelope <- seq_envelope(x, buffer - 5)

  vege_poly <- if (is.null(poly)) {
    get_vege_poly(x, buffer = buffer, clip = clip, tol = tol)
  } else {
    sf::st_transform(poly, crs)
  }

  if (!nrow(vege_poly)) {
    cli::cli_warn("No vegetation data found. Empty {.cls sf} is returned.")
    return(invisible(create_empty_sf("LINE") |> seq_normalize("vct_line")))
  }

  vege_line <- suppressWarnings(
    vege_poly |>
      poly_to_line() |>
      sf::st_intersection(cleaning_envelope) |>
      sf::st_cast("LINESTRING") |>
      seq_normalize("vct_line")
  )

  vege_line[[seq_field("type")$name]] <- "FOR"
  vege_line[[seq_field("source")$name]] <- "ignf_masque_foret"

  invisible(vege_line)
}


#' Generate vegetation point features around an area
#'
#' @inheritParams get_vege_poly
#'
#' @return An `sf` point layer.
#'
#' @export
get_vege_point <- function(x, buffer = 1000, clip = TRUE) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}.")
  }

  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- seq_envelope(x, buffer)
  control_envelope <- seq_envelope(x, buffer + 500)

  empty <- function() {
    cli::cli_warn("No vegetation data found. Empty {.cls sf} is returned.")
    invisible(create_empty_sf("POINT") |> seq_normalize("vct_point"))
  }

  fv <- happign::get_wfs(
    fetch_envelope,
    layer = "LANDCOVER.FORESTINVENTORY.V2:formation_vegetale",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if (is.null(fv) || !nrow(fv)) return(empty())

  fv <- suppressWarnings(
    fv |>
      sf::st_transform(crs) |>
      sf::st_intersection(control_envelope) |>
      sf::st_make_valid()
  )

  map <- c(
    FF1 = "FEV", FO1 = "FEV",
    FF2 = "REV", FO2 = "REV",
    FF3 = "FRV", FO3 = "FRV",
    FP = "PEV", LA = "LAV"
  )

  type <- seq_field("type")$name
  fv[[type]] <- unname(map[substr(fv$code_tfv, 1, 3)])
  fv <- fv[!is.na(fv[[type]]), ]

  if (!nrow(fv)) return(empty())

  if (clip) {
    forest <- seq_dissolve(sf::st_buffer(x, 5), 5)

    fv <- suppressWarnings(
      fv |>
        sf::st_difference(sf::st_union(forest)) |>
        sf::st_make_valid() |>
        sf::st_collection_extract("POLYGON")
    )

    if (!nrow(fv)) return(empty())
  }

  n_points <- round(sum(as.numeric(sf::st_area(fv))) / 10000 / 4)
  if (n_points < 1) return(empty())

  point <- sf::st_sample(fv, n_points, type = "hexagonal") |>
    sf::st_as_sf() |>
    sf::st_join(fv, join = sf::st_intersects) |>
    seq_normalize("vct_point")

  point[[seq_field("source")$name]] <- "ignf_bd_foretv2"

  invisible(point)
}


#' Build vegetation layers
#'
#' Retrieves vegetation polygons once, derives vegetation lines from them,
#' and retrieves vegetation points separately.
#'
#' @inheritParams get_vege_poly
#'
#' @return A named list of vegetation layers.
#'
#' @keywords internal
#' @noRd
.build_vege_layers <- function(
    x,
    buffer = 1000,
    clip = TRUE,
    tol = 500) {

  poly <- get_vege_poly(x, buffer = buffer, clip = clip, tol = tol)

  list(
    "v.vege.poly" = poly,
    "v.vege.line" = get_vege_line(x, buffer = buffer, clip = clip, tol = tol, poly = poly),
    "v.vege.point" = get_vege_point(x, buffer = buffer, clip = clip)
  )
}


#' Fetch vegetation layers
#'
#' @inheritParams get_vege_poly
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#' @param overwrite `logical`; If `TRUE`, overwrite existing layers.
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @keywords internal
#' @noRd
.vege_fetcher <- function(
    x,
    dirname,
    id = NULL,
    buffer = 1000,
    clip = TRUE,
    tol = 500,
    verbose = TRUE,
    overwrite = FALSE) {

  if (verbose) cli::cli_h1("VEGETATION")

  layers <- .build_vege_layers(x, buffer = buffer, clip = clip, tol = tol)
  identifier <- if (!is.null(id)) seq_field("identifier")$name else NULL

  paths <- lapply(names(layers), function(k) {
    tryCatch({
      f <- layers[[k]]

      if (!is.null(id)) f[[identifier]] <- rep(id, nrow(f))

      seq_write(
        f, key = k, dirname = dirname, id = id,
        verbose = verbose, overwrite = overwrite
      )
    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_danger(
          "Failed VEGETATION layer {.val {k}}: {conditionMessage(e)}"
        )
      }
      NULL
    })
  })

  names(paths) <- names(layers)
  invisible(Filter(Negate(is.null), paths))
}


#' Fetch vegetation layers for an area
#'
#' @inheritParams get_vege_poly
#' @param dirname `character`; Output directory.
#' @param overwrite `logical`; If `TRUE`, overwrite existing layers.
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @keywords internal
#' @noRd
fetch_vege <- function(
    x,
    dirname,
    buffer = 1000,
    clip = TRUE,
    tol = 500,
    verbose = TRUE,
    overwrite = FALSE) {

  .vege_fetcher(
    x = x,
    dirname = dirname,
    buffer = buffer,
    clip = clip,
    tol = tol,
    verbose = verbose,
    overwrite = overwrite
  )
}


#' Generate vegetation layers for a Sequoia project
#'
#' Retrieves vegetation polygon, line and point layers around the project area
#' and writes them to the Sequoia project directory.
#'
#' @inheritParams get_vege_poly
#' @inheritParams seq_write
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @seealso [get_vege_poly()], [get_vege_line()], [get_vege_point()]
#'
#' @export
seq_vege <- function(
    dirname = ".",
    buffer = 1000,
    clip = TRUE,
    tol = 500,
    verbose = TRUE,
    overwrite = FALSE) {

  ctx <- .seq_context(dirname)

  .vege_fetcher(
    x = ctx$parca, dirname = dirname, id = ctx$id,
    buffer = buffer, clip = clip, tol = tol,
    verbose = verbose, overwrite = overwrite
  )
}
