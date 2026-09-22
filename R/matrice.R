#' Create a  forest matrice
#'
#' Generates a default forest matrice as excel file used to store general forest
#' information (e.g., `IDENTIFIANT`, `PROPRIETAIRE`) and cadastral attributes
#' (`CODE_INSEE`, `PREFIXE`, `SECTION`, `NUMERO`, `LIEU_DIT`).
#'
#' @inheritParams seq_write
#' @param id `character` Identifier of the forest. Typically the name of the
#' forest
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
    "IDENTIFIANT" = id,
    "PROPRIETAIRE" = "",
    "INSEE" = "",
    "PREFIX" = "",
    "SECTION" = "",
    "NUMERO" = "",
    "LIEU_DIT" = ""
  ) |> seq_normalize("matrice")

  seq_xlsx(
    MATRICE = matrice,
    filename = file.path(dirname, paste0(id, "_matrice.xlsx")),
    overwrite = overwrite,
    verbose = verbose
    )
}

#' Read matrice and format for Rsequoia2
#'
#' @inheritParams seq_read
#'
#' @importFrom openxlsx2 read_xlsx
#'
#' @return `data.frame` formated as Rsequoia2 matrice
#'
read_matrice <- function(dirname = "."){

  m_path <- list.files(
    dirname,
    pattern = "_matrice\\.xlsx$",
    ignore.case = TRUE,
    full.names = TRUE,
    recursive = TRUE
  )

  # Exclude Excel temporary lock files like "~$_matrice.xlsx"
  xlsx_lock_file <- m_path[grepl("^~\\$.*_matrice\\.xlsx$", basename(m_path), ignore.case = TRUE)]
  if (length(xlsx_lock_file) > 0) {
    cli::cli_abort(c(
      "!" = "The matrice file appears to be open in Excel.",
      "x" = "Please close the matrice workbook first.",
      "i" = "Temporary file detected: {paste(basename(xlsx_lock_file), collapse = ', ')}"
    ))
  }

  # No file
  if (length(m_path) == 0) {
    cli::cli_abort(c(
      "!" = "No {.val *_matrice.xlsx} file found in {.path {dirname}}.",
      "i" = "See {.fn Rsequoia2::create_matrice} to generate one."
    ))
  }

  # Multiple files
  if (length(m_path) > 1) {
    cli::cli_abort(c(
      "!" = "Multiple {.val *_matrice.xlsx} files found in {.path {dirname}}.",
      "x" = "Only one matrice file is allowed.",
      "v" = "Files found: {paste(basename(m_path), collapse = ', ')}"
    ))
  }

  m <- openxlsx2::read_xlsx(
    m_path,
    skip_empty_rows = TRUE,
    skip_empty_cols = TRUE,
    na = c("", " ",  "#N/A"),
    convert = FALSE
  )

  m <- seq_normalize(m, "matrice")

  # name_check
  matrice_keys <- c(
    "identifier", "owner", "insee", "prefix",
    "section", "number", "locality"
  )

  required <- sapply(matrice_keys, \(x) seq_field(x)$name)
  missing <- setdiff(required, names(m))

  if (length(missing) > 0) {
    cli::cli_abort("Missing column in {.file {m_path}} : {.val {missing}}")
  }

  # Extract ID
  identifier <- seq_field("identifier")$name
  id <- unique(m[[identifier]])
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

  # Check empty field
  fields <- c("insee", "section", "number")
  errors <- character()

  for (field in fields) {
    name <- seq_field(field)$name
    empty_rows <- which(is.na(m[[name]]) | m[[name]] == "" | m[[name]] == "00000")

    if (length(empty_rows)) {
      errors <- c(
        errors,
        cli::format_inline(
          "Empty {.field {name}} found at lines: {.values {empty_rows}}"
        )
      )
    }
  }

  if (length(errors)) {
    cli::cli_abort(c(
      "!" = "Missing required values:",
      setNames(errors, rep("x", length(errors)))
    ))
  }

  # Resolve field names once
  f <- function(x) seq_field(x)$name

  m[[f("identifier")]] <- id
  m[[f("insee")]] <- pad_left(m[[f("insee")]], 5)
  m[[f("prefix")]] <- pad_left(m[[f("prefix")]], 3)
  m[[f("section")]] <- pad_left(m[[f("section")]], 2)
  m[[f("number")]] <- pad_left(m[[f("number")]], 4)

  m <- seq_normalize(m, "matrice")

  return(m)

}
