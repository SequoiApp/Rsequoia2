#' Create _UA_ sf object from _PARCA_ sf object
#'
#' @param parca `sf` Object from [Rsequoia2::seq_parca()] containing cadastral
#' parcels.
#'
#' @return An `sf` object containing the analysis units.
#'
#' @export
parca_to_ua <- function(parca) {
  ua <- seq_normalize(parca, "ua")

  defaults <- c(
    is_dgd = TRUE, # DGD_SOUMIS
    is_wooded = TRUE, # DGD_BOISE
    is_damaged = FALSE, # SINISTRE
    is_available = TRUE, # DISPONIBLE
    is_compartmented = FALSE, # CLOISONNE
    is_subsidized = FALSE # SUBVENTION
  )

  for (k in names(defaults)) {
    ua[[seq_field(k)$name]] <- defaults[[k]]
  }

  return(ua)
}

#' Create _PARCA_ sf object from _UA_ sf object
#'
#' @param ua `sf` object containing analysis units
#'
#' @return An `sf` object containing cadastral parcels.
#'
#' @details
#' This function deviates from the traditional process.
#' It allows the _PARCA_ layer to be recreated from a _UA_ layer
#' in the case of a folder where the first layer is missing.
#'
#' @export
ua_to_parca <- function(ua) {
  idu_field <- seq_field("idu")$name

  parca <- seq_normalize(ua, "parca")

  aggregate(
    parca,
    by = setNames(list(parca[[idu_field]]), idu_field),
    FUN = function(x) x[1],
    do_union = TRUE
  )
}

#' Create analysis units layer
#'
#' This function reads the cadastral layer file from a project directory
#' and create the analysis units layer.
#'
#' The resulting object is returned invisibly as an `sf` polygons layer.
#' The output file is automatically written into the working directory defined
#' by `dirname`.
#'
#' @inheritParams create_matrice
#'
#' @return An `sf` object
#'
#' @export
seq_parca_to_ua <- function(
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE){

  # create ua
  parca <- seq_read("v.seq.parca.poly", dirname = dirname, verbose = verbose)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  ua <- parca_to_ua(parca)

  # write ua
  ua_path <- seq_write(
    ua,
    "v.seq.ua.poly",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  return(invisible(ua_path))
}
