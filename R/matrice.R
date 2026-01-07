#' Create a  forest matrice
#'
#' Generates a default forest matrice as excel file used to store general forest
#' information (e.g., `IDENTIFIANT`, `PROPRIETAIRE`) and cadastral attributes
#' (`CODE_INSEE`, `PREFIXE`, `SECTION`, `NUMERO`, `LIEU_DIT`).
#'
#' @param dirname `character` Path to the directory. Defaults to the current
#' working directory.
#' @param id `character` Identifier of the forest. Typically the name of the
#' forest
#' @param overwrite `logical` If `TRUE`, filename is overwritten.
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @return Invisibly returns the path to the created file.
#' @export
#'
#' @examples
#' \dontrun{
#' create_matrice("~/matrice.xlsx", overwrite = TRUE)
#' }
create_matrice <- function(dirname = ".", id = "MY_FOREST", overwrite = FALSE, verbose = TRUE){

  matrice <- data.frame(
    "identifiant" = id,
    "prop" = "NAME OF THE OWNER",
    "insee" = "33103",
    "prefixe" = "000",
    "section" = "AB",
    "numero" = "60",
    "lieu_dit" = "NAME OF LIEU DIT"
  ) |> seq_normalize("matrice")

  seq_xlsx(
    x = list("MATRICE" = matrice),
    filename = file.path(dirname, paste0(id, "_matrice.xlsx")),
    overwrite = overwrite,
    verbose = verbose
    )
}

#' Read matrice and format for Rsequoia2
#'
#' @param dirname `character` Directory where the matrice file is located.
#' Defaults to the current working directory.
#'
#' @importFrom openxlsx2 read_xlsx
#'
#' @return `data.frame` formated as Rsequoia2 matrice
#'
read_matrice <- function(dirname = "."){

  m_path <- list.files(dirname, pattern = "_matrice\\.xlsx$", full.names = TRUE)

  # No file
  if (length(m_path) == 0) {
    cli::cli_abort(c(
      "!" = "No {.val *_matrice.xlsx} file found in {.path {normalizePath(dirname)}}.",
      "i" = "See {.fn Rsequoia2::create_matrice} to generate one."
    ))
  }

  # Multiple files
  if (length(m_path) > 1) {
    cli::cli_abort(c(
      "!" = "Multiple {.val *_matrice.xlsx} files found in {.path {normalizePath(dirname)}}.",
      "x" = "Only one matrice file is allowed.",
      "v" = "Files found: {paste(basename(m_path), collapse = ', ')}"
    ))
  }

  m <- openxlsx2::read_xlsx(
    m_path,
    skip_empty_rows = TRUE,
    skip_empty_cols = TRUE,
    na.strings = c("", " ",  "#N/A"),
    convert = FALSE
  )

  # name_check
  matrice_keys <- c(
    "identifiant", "proprietaire", "insee", "prefix",
    "section", "numero", "lieu_dit"
  )

  required <- sapply(matrice_keys, \(x) seq_field(x)$name)
  missing  <- setdiff(required, names(m))

  if (length(missing) > 0) {
    cli::cli_abort("Missing column in {.file {m_path}} : {.val {missing}}")
  }

  # Extract ID
  identifiant <- seq_field("identifiant")$name
  id <- unique(m[[identifiant]])
  id <- id[!is.na(id) & nzchar(trimws(id))]   # remove NA + empty + spaces-only

  # Empty IDs
  if (length(id) == 0) {
    cli::cli_abort(c(
      "!" = "Column {.field IDENTIFIANT} is empty."
    ))
  }

  # Multiple distinct IDs
  if (length(id) > 1) {
    cli::cli_abort(c(
      "!" = "Multiple IDs detected in column {.field IDENTIFIANT}.",
      "x" = "Only one unique ID is expected.",
      "v" = "IDs found: {paste(id, collapse = ', ')}"
    ))
  }

  # Resolve field names once
  f <- function(x) seq_field(x)$name

  m[[f("identifiant")]] <- id
  m[[f("insee")]] <- pad_left(m[[f("insee")]], 5)
  m[[f("prefix")]] <- pad_left(m[[f("prefix")]], "0")
  m[[f("section")]] <- pad_left(m[[f("section")]], "0")
  m[[f("numero")]] <- pad_left(m[[f("numero")]], "0")

  return(m)

}
