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
    "IDENTIFIANT" = id,
    "PROPRIETAIRE" = "NAME OF THE OWNER",
    "CODE_INSEE" = "08170",
    "PREFIXE" = "123",
    "SECTION" = "A",
    "NUMERO" = "1",
    "LIEU_DIT" = "NAME OF LIEU DIT",
    "TX_BOISEE" = "0,8"
  )

  seq_xlsx(
    x = list("MATRICE" = matrice),
    filename = file.path(dirname, paste0(id, "_matrice.xlsx")),
    overwrite = overwrite,
    verbose = verbose
    )
}
