#' Right-pad a string with zeros
#'
#' Internal helper used to format fixed-width codes.
#'
#' @param x `character` String to pad
#' @param width `integer` Desired total width
#' @param fill `character` Filler to pad with
#'
#' @return A zero-padded character string.
#' @keywords internal
#'
pad_left <- function(x, width, fill = "0") {
  gsub(" ", fill, sprintf(paste0("%", width, "s"), as.character(x)))
}

#' Split IDU
#'
#' Internal helper used to split idu
#'
#' @param idu `character` IDU(s) to pad
#'
#' @keywords internal
#'
idu_split <- function(idu) {

  code_dep <- substr(idu, 1, 2)
  code_com <- substr(idu, 3, 5)
  insee <- paste0(code_dep, code_com)

  data.frame(
    idu     = idu,
    code_dep = code_dep,
    code_com = code_com,
    prefix   = substr(idu, 6, 8),
    section  = substr(idu, 9, 10),
    numero   = substr(idu, 11, 14),
    insee    = insee,
    stringsAsFactors = FALSE
  )
}
#' Build IDU
#'
#' Internal helper used to build idu
#'
#' @param idu `character` IDU(s) to pad
#'
#' @keywords internal
#'
idu_build <- function(dep, com, prefix, section, numero) {

  dep <- pad_left(dep, 2)
  com <- pad_left(com, 3)
  prefix <- pad_left(prefix, 3)
  section <- pad_left(section, 2)
  numero <- pad_left(numero, 4)

  return(paste0(dep, com, prefix, section, numero))
}
#' Force silent function
#'
#' Internal helper used to silent function
#'
#' @param expr Code to capture
#'
#' @keywords internal
#'
quiet <- function(expr) {
  utils::capture.output(result <- suppressMessages(suppressWarnings(expr)))
  result
}
