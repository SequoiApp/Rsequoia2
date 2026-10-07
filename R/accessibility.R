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
      "i" = "{.arg type} should be one of {.val porteur} or {.val skidder}"
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

#' Clip access data to an area
#'
#' @param access `sf`; Raw access data.
#' @param x `sf` or `sfc`; Area used to clip access data.
#'
#' @return An `sf` polygon layer, or `NULL` if there is no intersection.
#'
#' @keywords internal
#' @noRd
.access_transformer <- function(access, x) {

  if (is.null(access) || !nrow(access)) {
    return(NULL)
  }

  access <- suppressWarnings(
    access |>
      sf::st_transform(sf::st_crs(x)) |>
      sf::st_intersection(sf::st_union(sf::st_geometry(x))) |>
      sf::st_cast("POLYGON")
  )

  if (!nrow(access)) {
    return(NULL)
  }

  access
}

#' Fetch accessibility layers
#'
#' @inheritParams get_accessibility
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#' @param overwrite `logical`; If `TRUE`, overwrite existing layers.
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @keywords internal
#' @noRd
.access_fetcher <- function(
    x,
    dirname,
    id = NULL,
    buffer = 0,
    verbose = TRUE,
    overwrite = FALSE) {

  if (verbose){
    cli::cli_h1("ACCESSIBILITY")
  }

  layers <- c(
    porteur = "v.access.porteur.poly",
    skidder = "v.access.skidder.poly"
  )

  pb <- NULL
  if (verbose) {
    pb <- cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_spin} Searching ACCESSIBILITY layer: {.val {k}} | ",
        "[{cli::pb_current}/{cli::pb_total}]"
      ),
      total = length(layers)
    )
  }

  paths <- lapply(names(layers), function(k) {

    if (verbose) {
      cli::cli_progress_update(id = pb, set = list(k = k), force = TRUE)
    }

    tryCatch({
      access <- get_accessibility(x, type = k, buffer = buffer, verbose = FALSE)
      access <- .access_transformer(access, x)

      if (is.null(access)) {
        if (verbose) {
          cli::cli_alert_info("No pedology layer found.")
        }
        return(invisible(NULL))
      }

      if (!is.null(id)) {
        identifier <- seq_field("identifier")$name
        access[[identifier]] <- id
      }

      seq_write(
        access,
        key = layers[[k]],
        dirname = dirname,
        id = id,
        verbose = verbose,
        overwrite = overwrite
      )
    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_danger(
          "Failed ACCESSIBILITY layer {.val {k}}: {conditionMessage(e)}"
        )
      }
      NULL
    })
  })

  names(paths) <- unname(layers)
  paths <- Filter(Negate(is.null), paths)

  # Empty access-entry layer, completed manually by the user
  entry <- create_empty_sf("POINT") |> seq_normalize("vct_point")
  if (!is.null(id)){
    identifier <- seq_field("identifier")$name
    entry[[identifier]] <- rep(id, nrow(entry))
  }

  paths[["v.access.entry.point"]] <- seq_write(
    x = entry,
    key = "v.access.entry.point",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  invisible(paths)
}


#' Fetch accessibility layers for an area
#'
#' Retrieves accessibility layers around `x` and writes them to `dirname`.
#'
#' @inheritParams get_accessibility
#' @param dirname `character`; Output directory.
#' @param overwrite `logical`; If `TRUE`, overwrite existing layers.
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @keywords internal
#' @noRd
fetch_access <- function(
    x,
    dirname,
    buffer = 0,
    verbose = TRUE,
    overwrite = FALSE) {

  .access_fetcher(
    x = x,
    dirname = dirname,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )
}


#' Generate accessibility layers for a Sequoia project
#'
#' Retrieves porter and skidder accessibility polygons for the project area
#' and creates an empty access-entry point layer.
#'
#' @inheritParams get_accessibility
#' @inheritParams seq_write
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @seealso [get_accessibility()], [seq_write()]
#'
#' @export
seq_access <- function(
    dirname = ".",
    buffer = 0,
    verbose = TRUE,
    overwrite = FALSE) {

  ctx <- .seq_context(dirname)

  .access_fetcher(
    x = ctx$parca,
    dirname = dirname,
    id = ctx$id,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )
}
