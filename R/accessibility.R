#' Retrieve forest accessibility features (porter or skidder)
#'
#' Builds a convex buffer around the input geometry and retrieves accessibility
#' features for the requested machine type.
#'
#' @param x An `sf` object defining the input area of interest.
#' @param type `character` Accessibility type. One of `"porter"` or `"skidder"`.
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#' the download area.
#' @param verbose `logical`; If `TRUE`, display progress and informational
#'   messages.
#'
#' @return An `sf` object containing accessibility features, or `NULL` if none found.
#'
#' @export
get_accessibility <- function(
    x,
    type = c("porteur", "skidder"),
    buffer = 1000,
    verbose = TRUE){

  if (!inherits(x, c("sf", "sfc"))){
    cli::cli_abort(c(
      "x" = "{.arg x} is of class {.cls {class(x)}}.",
      "i" = "{.arg x} should be of class {.cls sf} or {.cls sfc}."
    ))
  }

  if (length(type) != 1) {
    cli::cli_abort(c(
      "x" = "{.arg type} must contain exactly one element.",
      "i" = "You supplied {length(type)}."
    ))
  }

  if (!type %in% c("porteur", "skidder")){
    cli::cli_abort(c(
      "x" = "{.code type = {.val {type}}} isn't valid.",
      "i" = "{.arg type} should be one of {.val porteur} or  {.val skidder}"
    ))
  }

  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- seq_envelope(x, buffer)

  layer <- switch(
    type,
    "porteur" = "IGNF_ACCESSIBILITE-PHYSIQUE-FORETS-:acces_porteur",
    "skidder" = "IGNF_ACCESSIBILITE-PHYSIQUE-FORETS-:acces_skidder"
  )

  if (verbose){
    cli::cli_alert_info("Downloading forest {.val {type}} accessibility ...")
  }

  access <- happign::get_wfs(
    x = fetch_envelope,
    layer = layer,
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if (!nrow(access)) {
    return(NULL)
  }

  return(invisible(sf::st_transform(access, crs)))
}

#' Generate access point layer for a Sequoia project
#'
#' Create an empty `sf` for access point features, and writes the resulting
#' layer to disk.
#'
#' @inheritParams seq_write
#'
#' @details
#' The access point layer is an empty layer : user must point access themselves.
#' The layer is written to disk using [seq_write()] with the key `"v.access.entry.point"`.
#'
#' @return
#' Invisibly returns a named list of file paths written by [seq_write()].
#'
#' @seealso
#' [seq_write()]
#'
#' @export
seq_access <- function(
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE
) {

  if (verbose) {
    cli::cli_h1("ACCESSIBILITY")
  }

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  layers <- c(
    porteur = "v.access.porteur.poly",
    skidder = "v.access.skidder.poly"
  )

  pb <- NULL
  if (verbose) {
    pb <- cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_spin} ACCESSIBILITY layer: {.val {k}} | ",
        "[{cli::pb_current}/{cli::pb_total}]"
      ),
      total = length(layers),
      auto_terminate = FALSE
    )
  }

  path <- list()

  for (k in names(layers)) {

    if (verbose) {
      cli::cli_progress_update(id = pb, force = TRUE)
    }

    f_path <- tryCatch({
      f <- get_accessibility(
        x = parca,
        buffer = 0,
        type = k,
        verbose = FALSE
      )

      if (is.null(f) || nrow(f) == 0) {
        NULL
      } else {
        f <- f |>
          sf::st_transform(sf::st_crs(parca)) |>
          sf::st_intersection(sf::st_union(sf::st_geometry(parca))) |>
          sf::st_cast("POLYGON") |>
          suppressWarnings()

        f[[identifier]] <- id

        seq_write(
          f,
          layers[[k]],
          dirname,
          id,
          verbose = verbose,
          overwrite = overwrite
        )
      }
    }, error = function(e) NULL)

    if (!is.null(f_path)) {
      path <- c(path, f_path)
    }

  }

  # Empty access-entry layer
  access <- create_empty_sf("POINT") |>
    seq_normalize("vct_point")

  access_path <- seq_write(
    access,
    "v.access.entry.point",
    dirname,
    id,
    verbose = verbose,
    overwrite = overwrite
  )

  path <- c(path, access_path)

  invisible(path)
}
