#' Download one GPU vector layer
#'
#' Downloads a single Geoportail de l'Urbanisme layer intersecting `x`.
#'
#' @param x `sf` or `sfc`; Geometry located in France.
#' @param layer `character`; GPU API layer identifier.
#' @param verbose `logical`; If `TRUE`, display messages.
#'
#' @return An `sf` object, or `NULL` if no feature is found.
#'
#' @export
get_gpu <- function(x, layer, verbose = TRUE) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}.")
  }

  allowed <- c(
    "municipality", "document", "zone-urba",
    "prescription-surf", "prescription-lin", "prescription-pct",
    "assiette-sup-s", "assiette-sup-l", "assiette-sup-p",
    "generateur-sup-s", "generateur-sup-l", "generateur-sup-p"
  )

  layer <- tryCatch(
    match.arg(layer, allowed),
    error = function(e) cli::cli_abort(
      "Invalid {.arg layer}. Allowed values are: {.vals {allowed}}."
    )
  )

  gpu <- suppressWarnings(happign::get_apicarto_gpu(x, layer))

  if (is.null(gpu) || !nrow(gpu)) {
    if (verbose) cli::cli_alert_warning("GPU layer {.val {layer}}: no intersecting features.")
    return(invisible(NULL))
  }

  invisible(gpu)
}


#' Fetch GPU layers
#'
#' @param x `sf` or `sfc`; Area of interest.
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#' @param key `character`; GPU Sequoia layer identifiers.
#' @param verbose `logical`; If `TRUE`, display messages.
#' @param overwrite `logical`; If `TRUE`, overwrite existing layers.
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @keywords internal
#' @noRd
.gpu_fetcher <- function(
    x,
    dirname,
    id = NULL,
    key = get_keys("gpu", reduce = FALSE),
    verbose = TRUE,
    overwrite = FALSE) {

  sources <- list(
    "v.gpu.municipality.poly"  = "municipality",
    "v.gpu.document.poly"      = "document",
    "v.gpu.zone.poly"          = "zone-urba",
    "v.gpu.prescription.poly"  = "prescription-surf",
    "v.gpu.prescription.line"  = "prescription-lin",
    "v.gpu.prescription.point" = "prescription-pct",
    "v.gpu.supa.poly"          = c("assiette-sup-s", "assiette-sup-l", "assiette-sup-p"),
    "v.gpu.supg.poly"          = "generateur-sup-s",
    "v.gpu.supg.line"          = "generateur-sup-l",
    "v.gpu.supg.point"         = "generateur-sup-p"
  )

  if (!all(key %in% names(sources))) {
    cli::cli_abort("{.arg key} must be one or more of {.val {names(sources)}}.")
  }

  if (verbose){
    cli::cli_h1("GPU")
  }

  pb <- NULL
  if (verbose) {
    pb <- cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_spin} Searching GPU layer: {.val {k}} | ",
        "[{cli::pb_current}/{cli::pb_total}]"
      ),
      total = length(key)
    )
  }

  paths <- lapply(key, function(k) {
    if (verbose){
      cli::cli_progress_update(id = pb, force = TRUE)
    }

    tryCatch({
      gpu <- lapply(sources[[k]], \(layer) get_gpu(x, layer, verbose = FALSE))
      gpu <- Filter(\(x) !is.null(x) && nrow(x), gpu)

      if (!length(gpu)){
        return(NULL)
      }

      gpu <- do.call(rbind, gpu) |> sf::st_transform(2154)
      if (!is.null(id)){
        identifier <- seq_field("identifier")$name
        gpu[[identifier]] <- id
      }

      seq_write(
        gpu,
        key = k,
        dirname = dirname,
        id = id,
        verbose = verbose,
        overwrite = overwrite
      )

    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_danger(
          "Failed GPU layer {.val {k}}: {conditionMessage(e)}"
        )
      }
      NULL
    })
  })

  names(paths) <- key
  invisible(Filter(Negate(is.null), paths))
}


#' Fetch GPU layers for an area
#'
#' @param x `sf` or `sfc`; Area of interest.
#' @param dirname `character`; Output directory.
#' @param key `character`; GPU Sequoia layer identifiers.
#' @param verbose `logical`; If `TRUE`, display messages.
#' @param overwrite `logical`; If `TRUE`, overwrite existing layers.
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @keywords internal
#' @noRd
fetch_gpu <- function(
    x,
    dirname,
    key = get_keys("gpu", reduce = FALSE),
    verbose = TRUE,
    overwrite = FALSE) {

  .gpu_fetcher(
    x = x, dirname = dirname, key = key,
    verbose = verbose, overwrite = overwrite
  )
}


#' Generate GPU layers for a Sequoia project
#'
#' Retrieves GPU layers intersecting the project area and writes them to the
#' Sequoia project directory.
#'
#' @inheritParams seq_write
#' @param key `character`; GPU layer identifiers. Defaults to
#'   `get_keys("gpu", reduce = FALSE)`.
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @seealso [get_gpu()], [seq_write()]
#'
#' @export
seq_gpu <- function(
    dirname = ".",
    key = get_keys("gpu", reduce = FALSE),
    verbose = TRUE,
    overwrite = FALSE) {

  ctx <- .seq_context(dirname)

  .gpu_fetcher(
    x = sf::st_union(ctx$parca),
    dirname = dirname,
    id = ctx$id,
    key = key,
    verbose = verbose,
    overwrite = overwrite
  )
}
