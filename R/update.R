#' Open old `R_SEQUOIA` vector file
#'
#' @param dirname `character` Directory where the file is located.
#' Defaults to the current working directory.
#' @param layer `character` Layer name.
#'
#' @return An `sf` object.
#'
#' @export
seq1_read <- function(dirname = ".", layer) {

  valid_layers <- c(
    "coms_line", "coms_point",
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

  if (length(file) != 1) {
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
  path <- file <- list.files(
    path = dirname,
    pattern = "_PARCA_polygon\\.shp$",
    full.names = FALSE,
    all.files = FALSE,
    recursive = TRUE
  )

  if (length(file) != 1) {
    cli::cli_abort(
      "x" = "Multiple files _PARCA_ detected. The folder must contain exactly one matching file."
    )
  }

  fname <- basename(path)
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

