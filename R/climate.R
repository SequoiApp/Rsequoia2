#' Generate meteorological outputs
#'
#' Downloads and formats meteorological data from Météo-France and DRIAS.
#' This includes download of the nearest climatological station fiche (PDF),
#' ombrothermic summaries, precipitation statistics. Results are written
#' to an Excel workbook based on the internal template `CLIMAT_MF.xlsx`.
#'
#' @inheritParams seq_write
#' @inheritParams mf_get_climatology
#'
#' @return
#' A named `list` of file paths :
#' - `"fiche.meteo"``: Path to the downloaded climatological station PDF
#' - `"meteofrance"``: Path to the generated Excel workbook
#'
#' @export
seq_climat <- function(dirname = ".", cache = NULL, verbose = TRUE, overwrite = FALSE){

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  if (verbose){
    cli::cli_h1("CLIMATE")
  }

  path <- list()
  # Fiche climatologique
  meteo_dir <- file.path(dirname, "METEO")
  dir.create(meteo_dir, showWarnings = FALSE, recursive = TRUE)

  pdf_path <- mf_get_climate_fiche(parca, dirname = meteo_dir, verbose = verbose)
  path <- c(path, pdf_path$pdf |> setNames("fiche.meteo"))

  raw_clim <- mf_get_climatology(parca, cache = cache, verbose = verbose)
  ombro <- mf_ombro(raw_clim, periods = c(30, 5))
  precipitation <- mf_precipitation(raw_clim)

  filepath <- file.path(meteo_dir, sprintf("%s_CLIMAT_MF.xlsx", id))

  if (verbose) {cli_alert_info("Writing meteorological data to: {.path {filepath}}")}

  wb <- openxlsx2::wb_load(system.file("xlsx/CLIMAT_MF.xlsx", package = "Rsequoia2"))

  wb <- openxlsx2::wb_clean_sheet(wb, sheet = 1, styles = FALSE) |>
    openxlsx2::wb_add_data(sheet = 1, raw_clim)
  wb <- openxlsx2::wb_clean_sheet(wb, sheet = 2, dims = "A1:F30", styles = FALSE) |>
    openxlsx2::wb_add_data(sheet = 2, ombro)
  wb <- openxlsx2::wb_clean_sheet(wb, sheet = 3, dims = "A1:B2000", styles = FALSE) |>
    openxlsx2::wb_add_data(sheet = 3, precipitation)

  openxlsx2::wb_save(wb, file = filepath,  overwrite = overwrite)

  path <- c(path, filepath |> setNames("meteofrance"))

  return(invisible(path))
}
