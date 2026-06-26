#' Summarize information for a Sequoia folder
#'
#' Generate synthetic table from Sequoia layers and write them as `.xlsx`
#' inside the current Sequoia dir. Useful for redaction of management document.
#'
#' @inheritParams seq_write
#'
#' @details
#' Available tables:
#'  - `"CAD"`: Cadastral parcels
#'  - `"CAD_COM"`: Surfaces by communes
#'  - `"PF"`: Surfaces by forest parcels
#'  - `"SSPF"`: Surfaces by forest sub-parcels
#'  - `"CAD_PLT"`: Link between cadastral parcels and forest parcels
#'  - `"OCCUPATION"`: Surfaces by land use
#'  - `"STATION"`: Surfaces by station
#'  - `"GEOL_BDCHARM50"`: Surfaces by geology (source: BDCHARM50)
#'  - `"GEOL_CARHAB"`: Surfaces by geology (source: CARHAB)
#'  - `"PEDO"`: Surfaces by pedology type
#'  - `"PLT_PF"`: Link between stand type and forest parcels
#'  - `"PF_PLT"`: Link between forest parcels and stand type
#'  - `"GESTION"`: Surfaces by management type
#'  - `"ALTI_PF"`: Altimetry recap by forest parcels (max, min, mean)
#'  - `"EXPO_PF"`: Exposition recap by forest parcels
#'  - `"PENTE_PF"`: Slope recap by forest parcels
#'
#' @return `list` of `data.frame`
#'
seq_summary <- function(dirname = ".", verbose = TRUE) {

  # BUILD CONTEXT ----
  ua <- seq_read("v.seq.ua.poly", dirname = dirname)
  ua <- seq_normalize(ua, "ua")
  ua <- ua_repair_dgd(ua, verbose = verbose)

  is_dgd <- seq_field("is_dgd")$name
  is_wooded <- seq_field("is_wooded")$name
  cor_area <- seq_field("cor_area")$name

  cor_area_is_empty <- all(is.na(ua[[cor_area]]) | ua[[cor_area]] == 0)
  if (cor_area_is_empty) {
    cli::cli_abort(c(
      "x" = "Invalid corrected area column.",
      "!" = "{.field {cor_area}} contains only missing or zero values.",
      "i" = "Run the UA correction step before building the summary."
    ))
  }

  if (!is.logical(ua[[is_wooded]])) {
    cli::cli_alert_warning(
      "{.field {is_wooded}} is not logical. Current type: {.cls {class(ua[[is_wooded]])}}."
    )
  }

  if (!is.logical(ua[[is_dgd]])) {
    cli::cli_alert_warning(
      "{.field {is_dgd}} is not logical. Current type: {.cls {class(ua[[is_dgd]])}}."
    )
  }

  pf <- ua_to_pf(ua)
  ua_dgd <- ua[ua[[is_dgd]] %in% TRUE, , drop = FALSE]

  # TABLE BUILDERS ----
  builders <- list(
    ua = function() build_summary_ua(ua),
    occupation = function() build_summary_occupation(ua),
    parca = function() build_summary_parca(ua_dgd),
    parca_com = function() build_summary_parca_com(ua_dgd),
    pf = function() build_summary_pf(ua_dgd),
    sspf = function() build_summary_sspf(ua_dgd),
    sspf_descr = function() build_summary_sspf_descr(ua_dgd),
    pf_parca = function() build_summary_pf_parca(ua_dgd),
    parca_pf = function() build_summary_parca_pf(ua_dgd),
    plt_type = function() build_summary_plt_type(ua_dgd),
    plt_rich = function() build_summary_plt_rich(ua_dgd),
    plt_stade = function() build_summary_plt_stade(ua_dgd),
    plt_ess = function() build_summary_plt_ess(ua_dgd),
    plt_str = function() build_summary_plt_str(ua_dgd),
    plt_type_rich = function() build_summary_plt_type_rich(ua_dgd),
    plt_type_rich_stade = function() build_summary_plt_type_rich_stade(ua_dgd),
    plt_type_rich_str = function() build_summary_plt_type_rich_str(ua_dgd),
    plt_type_rich_stade_ess = function() build_summary_plt_type_rich_stade_ess(ua_dgd),
    plt_type_rich_str_ess = function() build_summary_plt_type_rich_str_ess(ua_dgd),
    plt_type_stade_ess = function() build_summary_plt_type_stade_ess(ua_dgd),
    pf_plt_type = function() build_summary_pf_plt_type(ua_dgd),
    pf_plt_type_rich = function() build_summary_pf_plt_type_rich(ua_dgd),
    pf_plt_type_rich_stade = function() build_summary_pf_plt_type_rich_stade(ua_dgd),
    pf_plt_type_rich_str = function() build_summary_pf_plt_type_rich_str(ua_dgd),
    plt_pf = function() build_summary_plt_pf(ua_dgd),
    pf_rich = function() build_summary_pf_rich(ua_dgd),
    coupe = function() build_summary_coupe(ua_dgd),
    gestion = function() build_summary_gestion(ua_dgd),
    gestion_plt = function() build_summary_gestion_plt(ua_dgd),
    plt_gestion = function() build_summary_plt_gestion(ua_dgd),
    station = function() build_summary_station(ua_dgd),
    bdcharm50 = function() build_summary_bdcharm50(ua_dgd, dirname),
    carhab = function() build_summary_carhab(ua_dgd, dirname),
    pedo = function() build_summary_pedo(ua_dgd, dirname),
    road = function() build_summary_road(ua_dgd, dirname),
    mnt = function() build_summary_mnt(pf, dirname),
    mnh = function() build_summary_mnh(pf, dirname),
    expo = function() build_summary_expo(pf, dirname),
    pente = function() build_summary_pente(pf, dirname)
  )

  # BUILD + WRITE ON THE FLY ----
  wb <- openxlsx2::wb_workbook()
  tables <- list()

  if (verbose) {
    cli::cli_h2("Summary tables")
  }

  for (name in names(builders)) {
    sheet <- toupper(name)

    res <- safe_add_seq_table(
      wb = wb,
      sheet = sheet,
      fun = builders[[name]],
      verbose = verbose
    )

    wb <- res$wb
    if (isTRUE(res$ok)) {
      tables[[sheet]] <- res$table
    }
  }

  # SAVE ----
  filename <- file.path(dirname, seq_layer("summary")$filename)

  secure_filename <- sprintf(
    "%s_%s.%s",
    tools::file_path_sans_ext(filename),
    format(Sys.time(), "%Y%m%dT%H%M%S"),
    tools::file_ext(filename)
  )

  tryCatch(
    {
      openxlsx2::wb_save(wb, secure_filename)
    },
    error = function(e) {
      cli::cli_abort(c(
        "Failed to save summary workbook.",
        "x" = conditionMessage(e),
        "i" = "Check that the file is not already open in Excel."
      ))
    }
  )

  if (verbose) {
    cli::cli_alert_success("Summary saved to {.file {secure_filename}}.")
  }

  invisible(tables)
}
