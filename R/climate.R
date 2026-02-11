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
seq_climate <- function(dirname = ".", cache = NULL, verbose = TRUE, overwrite = FALSE){

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  if (verbose){
    cli::cli_h1("CLIMATE")
  }

  path <- list()

  meteo_dir <- file.path(dirname, "4_METEO")
  dir.create(meteo_dir, showWarnings = FALSE, recursive = TRUE)
  filepath <- file.path(meteo_dir, sprintf("%s_CLIMAT_MF.xlsx", id))

  # Fiche climatologique
  pdf_path <- mf_get_climate_fiche(parca, dirname = meteo_dir, verbose = verbose)
  path <- c(path, pdf_path)

  # Meteo france
  raw_clim <- mf_get_climatology(parca, cache = cache, verbose = verbose)
  ombro <- mf_ombro(raw_clim, periods = c(30, 5))
  precipitation <- mf_precipitation(raw_clim)

  wb <- openxlsx2::wb_load(system.file("xlsx/CLIMAT_MF.xlsx", package = "Rsequoia2"))

  if (verbose) {cli_alert_info("Writing Meteo-France data to: {.path {filepath}}")}

  wb <- openxlsx2::wb_clean_sheet(wb, sheet = 2, styles = FALSE) |>
    openxlsx2::wb_add_data(sheet = 2, raw_clim)
  wb <- openxlsx2::wb_clean_sheet(wb, sheet = 3, dims = "A1:F30", styles = FALSE) |>
    openxlsx2::wb_add_data(sheet = 3, ombro)
  wb <- openxlsx2::wb_clean_sheet(wb, sheet = 4, dims = "A1:B2000", styles = FALSE) |>
    openxlsx2::wb_add_data(sheet = 4, precipitation)

  # Drias
  txt <- list.files(meteo_dir, pattern = ".txt", full.names = TRUE)
  if (length(txt) == 0) {
    cli::cli_warn(
      c(
        "No DRIAS {.file .txt} file found in: {.path {meteo_dir}}.",
        "i" = "DRIAS projections will be skipped.",
        "i" = "See vignette {.emph 'seq_climate'} for instructions."
      )
    )

    # clean sheets only (remove existing values)
    wb <- openxlsx2::wb_clean_sheet(wb, sheet = 5, styles = FALSE) |>
      openxlsx2::wb_clean_sheet(sheet = 6, styles = FALSE) |>
      openxlsx2::wb_clean_sheet(sheet = 7, styles = FALSE) |>
      openxlsx2::wb_clean_sheet(sheet = 8, styles = FALSE)
  } else {
    txt <- txt[1]  # ensure single file
    drias_metadata <- drias_read_metadata(txt)
    drias_raw <- drias_read_table(txt)
    drias_ombro <- drias_ombro(txt)
    drias_etp <- drias_etp(txt)

    wb <- openxlsx2::wb_clean_sheet(wb, sheet = 5, styles = FALSE) |>
      openxlsx2::wb_add_data(sheet = 5, drias_metadata$indices)

    wb <- openxlsx2::wb_clean_sheet(wb, sheet = 6, styles = FALSE) |>
      openxlsx2::wb_add_data(sheet = 6, drias_raw)

    wb <- openxlsx2::wb_clean_sheet(wb, sheet = 7, dims = "A1:F60", styles = FALSE) |>
      openxlsx2::wb_add_data(sheet = 7, drias_ombro)

    wb <- openxlsx2::wb_clean_sheet(wb, sheet = 8, dims = "A1:E60", styles = FALSE) |>
      openxlsx2::wb_add_data(sheet = 8, drias_etp)
  }

  openxlsx2::wb_save(wb, file = filepath,  overwrite = overwrite)
  path <- c(path, filepath |> setNames("meteofrance"))

  return(invisible(path))
}
