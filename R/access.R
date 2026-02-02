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
#' The layer is written to disk using [seq_write()] with the key `"v.acces.point"`.
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

  # Create access
  access <- create_empty_sf("POINT") |>
    seq_normalize("vct_point")

  # Write access
  access <- seq_write(
    access,
    "v.acces.point",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  return(invisible(c(access) |> as.list()))
}
