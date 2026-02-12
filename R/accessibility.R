#' Retrieve forest accessibility features (porter or skidder)
#'
#' Builds a convex buffer around the input geometry and retrieves accessibility
#' features for the requested machine type.
#'
#' @param x An `sf` object defining the input area of interest.
#' @param type `character` Accessibility type. One of `"porter"` or `"skidder"`.
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#' the download area.
#' @param verbose `logical` If `TRUE`, display messages.
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
  fetch_envelope <- envelope(x, buffer)

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
    verbose = TRUE
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
#' @param dirname `character` Path to the project directory.
#'   Defaults to the current working directory.
#' @param verbose `logical`; whether to display informational messages.
#'   Defaults to `TRUE`.
#' @param overwrite `logical`; whether to overwrite existing files.
#'   Defaults to `FALSE`.
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

  if (verbose){
    cli::cli_h1("Access")
  }

  # read PARCA
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  id_field <- seq_field("identifier")$name
  id <- unique(parca[[id_field]])

  paths <- list()
  # vehicle acces
  porteur <- get_accessibility(x = parca, buffer = 0, type = "porteur", verbose = verbose)
  if (!is.null(porteur)){
    porteur <- porteur |>
      sf::st_transform(sf::st_crs(parca)) |>
      sf::st_intersection(parca) |>
      suppressWarnings()
    porteur[[id_field]] <- id
    porteur <- seq_write(
      porteur,
      "v.access.porteur.poly",
      dirname = dirname,
      id = id,
      verbose = verbose,
      overwrite = overwrite
    )
    paths <- c(paths, porteur)
  }

  # vehicle acces
  skidder <- get_accessibility(x = parca, buffer = 0, type = "skidder", verbose = verbose)
  if (!is.null(skidder)){
    skidder <- skidder |>
      sf::st_transform(sf::st_crs(parca)) |>
      sf::st_intersection(parca) |>
      suppressWarnings()
    skidder[[id_field]] <- id
    skidder <- seq_write(
      skidder,
      "v.access.skidder.poly",
      dirname = dirname,
      id = id,
      verbose = verbose,
      overwrite = overwrite
    )
    paths <- c(paths, skidder)
  }

  # Create access
  access <- create_empty_sf("POINT") |>
    seq_normalize("vct_point")

  # Write access
  access <- seq_write(
    access,
    "v.access.entry.point",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )
  paths <- c(paths, access)

  return(invisible(paths))
}
