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
