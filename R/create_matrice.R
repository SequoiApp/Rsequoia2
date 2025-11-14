#' Create a  forest matrice
#'
#' Generates a default forest matrice as excel file used to store general forest
#' information (e.g., `IDENTIFIANT`, `PROPRIETAIRE`) and cadastral attributes
#' (`CODE_INSEE`, `PREFIXE`, `SECTION`, `NUMERO`, `LIEU_DIT`).
#'
#' @param filename `character` Path to the output `.xlsx` file. Defaults
#' to `"~/matrice.xlsx"`.
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
create_matrice <- function(filename = "~/matrice.xlsx", overwrite = FALSE, verbose = TRUE){

  matrice <- data.frame(
    "IDENTIFIANT" = "NOM DE LA FORET",
    "PROPRIETAIRE" = "NOM DU PROPRIETAIRE",
    "CODE_INSEE" = "08170",
    "PREFIXE" = "123",
    "SECTION" = "A",
    "NUMERO" = "1",
    "LIEU_DIT" = "NOM DU LIEU DIT",
    "TX_BOISEE" = "0,8"
  )

  seq_xlsx(
    x = list("MATRICE" = matrice),
    filename = filename,
    overwrite = overwrite,
    verbose = verbose
    )
}
