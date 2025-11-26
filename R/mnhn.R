

get_mnhn <- function(
    x,
    key,
    buffer = 500,
    overwrite = FALSE,
    verbose = TRUE){

  if (!inherits(x, c("sf", "sfc"))){
    cli::cli_abort(c(
      "x" = "{.arg x} is of class {.cls {class(x)}}.",
      "i" = "{.arg x} should be of class {.cls sf} or {.cls sfc}."
    ))
  }

  if (length(key) != 1) {
    cli::cli_abort(
      "{.arg key} should be length one not {.val {length(key)}}."
    )
  }

  if (!key %in% get_keys("mnhn")){
    cli::cli_abort(c(
      "x" = "{.arg key} {.val {key}} isn't valid.",
      "i" = "Run {.run Rsequoia2::get_keys(\"mnhn\")} for available layers."
    ))
  }

  x <- sf::st_transform(x, 2154)
  x_buff <- sf::st_buffer(x, buffer)

  layer <- switch(key,
                  "apb" = "patrinat_apb:apb",
                  "apg" = "patrinat_apg:apg",
                  "aphn" = "patrinat_aphn:aire_protection_habitats_naturels",
                  "aplg" = "patrinat_aplg:apg",
                  "bios" = "patrinat_bios:bios",
                  "unesco" = "patrinat_bpm:Bien_patrimoine_mondial_UNESCO",
                  "cdl" = "patrinat_cdl:conservatoire_littoral",
                  "cen" = "patrinat_cen:cen",
                  "geoparc" = "patrinat_geoparc:geoparc",
                  "gsf" = "patrinat_gsf:gsf",
                  "inpg" = "patrinat_inpg:inpg",
                  "ospar" = "patrinat_ospar:pgsf",
                  "pgsf" = "patrinat_pgsf:pgsf",
                  "pn" = "patrinat_pn:parc_national",
                  "pn2" = "patrinat_pn2:pn",
                  "pnm" = "patrinat_pnm:pnm",
                  "pnr" = "patrinat_pnr:pnr",
                  "pprnn" = "patrinat_pprnn:pprnn",
                  "ramsar" = "patrinat_ramsar:pnm",
                  "rb" = "patrinat_rb:reserve_biologique",
                  "ripn" = "patrinat_ripn:ripn",
                  "rnc" = "patrinat_rnc:pnm",
                  "rncfs" = "patrinat_rncfs:rncfs",
                  "rnn" = "patrinat_rnn:rnn",
                  "rnr" = "patrinat_rnr:rnr",
                  "sc" = "patrinat_sc:sc",
                  "sic" = "patrinat_sic:sic",
                  "znieff1" = "patrinat_znieff1:znieff1",
                  "znieffmer1" = "patrinat_znieff1_mer:znieff1_mer",
                  "znieff2" = "patrinat_znieff2:znieff2",
                  "znieffmer2" = "patrinat_znieff2_mer:znieff2_mer",
                  "zpr" = "patrinat_zpr:zpr",
                  "zps" = "patrinat_zps:zps"
  )

  f <- happign::get_wfs(
    x_buff,
    layer = layer,
    overwrite = overwrite,
    spatial_filter = "intersects"
  ) |> suppressWarnings() |> suppressMessages()


  return(invisible(f))
}

seq_mnhn <- function(
    dirname = ".",
    buffer = 500,
    key = get_keys("mnhn"),
    verbose = TRUE,
    overwrite = FALSE){

  # read matrice
  parca <- read_sf(get_path("v.seq.parca.poly"))

  pb <- cli::cli_progress_bar(
    format = "{cli::pb_spin} Querying MNHN layer: {.val {k}} | [{cli::pb_current}/{cli::pb_total}]",
    total  = length(key)
  )

  quiet <- function(expr) {
    utils::capture.output(result <- suppressMessages(suppressWarnings(expr)))
    result
  }

  valid <- character()
  empty <- character()
  path <- list()
  for (k in key) {
    cli::cli_progress_update(id = pb)
    f <- quiet(get_mnhn(parca, k, buffer = buffer))
    if (!is.null(f)) {
      valid <- c(valid, k)
      f_path <- seq_write(f, key, dirname, verbose = FALSE, overwrite = overwrite)
      path <- c(path, f_path)
    } else {
      empty <- c(empty, k)
    }
  }
  cli::cli_progress_done(id = pb)

  if (length(valid) > 0) {
    cli::cli_alert_success(
      "{length(valid)} non-empty layer{?s} found: {.val {valid}}"
    )
  } else {
    cli::cli_warn("All layers are empty.")
  }

  return(path)
}
