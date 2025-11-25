get_inpn_keys <- function(){
  cfg_path <- system.file("config/seq_layers.yaml", package = "Rsequoia2")
  cfg <- yaml::read_yaml(cfg_path)
  inpn_key <- grepv("inpn", names(cfg))
  inpn_reduce_key <- sapply(strsplit(inpn_key, "\\."), `[`, 3)

  return(inpn_reduce_key)
}

get_inpn <- function(
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

  if (!key %in% get_inpn_keys()){
    cli::cli_abort(c(
      "x" = "{.arg key} {.val {key}} isn't valid.",
      "i" = "Run {.run Rsequoia2::get_inpn_keys()} for available layers."
    ))
  }

  x <- sf::st_transform(x, 2154)
  x_buff <- sf::st_buffer(x, buffer)

  wfs <- switch(key,
      apb = "PROTECTEDAREAS.APB:apb",
      aphn = "PROTECTEDAREAS.APHN:aphn",
      bios = "PROTECTEDAREAS.BIOS:bios",
      cdl = "PROTECTEDAREAS.MNHN.CDL.PARCELS:cdl",
      cen = "PROTECTEDAREAS.MNHN.CONSERVATOIRES:cen",
      gp = "PROTECTEDAREAS.GP:gp",
      inpg = "PROTECTEDAREAS.INPG:inpg",
      pn = "PROTECTEDAREAS.PN:pn",
      pnm = "PROTECTEDAREAS.PNM:pnm",
      pnr = "PROTECTEDAREAS.PNR:pnr",
      pprnn = "PROTECTEDAREAS.MNHN.RN.PERIMETER:pprnn",
      ramsar = "PROTECTEDAREAS.RAMSAR:ramsar",
      rb = "PROTECTEDAREAS.RB:rb",
      ripn = "PROTECTEDAREAS.RIPN:ripn",
      rnc = "PROTECTEDAREAS.RNC:rnc",
      rncfs = "PROTECTEDAREAS.RNCF:rncfs",
      rnn = "PROTECTEDAREAS.RNN:rnn",
      rnr = "PROTECTEDSITES.MNHN.RESERVES-REGIONALES:rnr",
      sic = "PROTECTEDAREAS.SIC:sic",
      zps = "PROTECTEDAREAS.ZPS:zps",
      zpr = "PROTECTEDAREAS.ZPR:zpr",
      znieff1 = "PROTECTEDAREAS.ZNIEFF1:znieff1",
      znieff1_mer = "PROTECTEDAREAS.ZNIEFF1.SEA:znieff1_mer",
      znieff2 = "PROTECTEDAREAS.ZNIEFF2:znieff2",
      znieff2_mer = "PROTECTEDAREAS.ZNIEFF2.SEA:znieff2_mer",
    )

  f <- happign::get_wfs(
    x_buff,
    layer = wfs,
    overwrite = overwrite,
    spatial_filter = "intersects"
  ) |> suppressWarnings() |> suppressMessages() |> invisible()


  return(invisible(f))
}

seq_inpn <- function(
    dirname = ".",
    buffer = 500,
    verbose = TRUE,
    overwrite = FALSE){

  # read matrice
  parca <- read_sf(get_path("v.seq.parca.poly"))
  keys <- get_inpn_keys()

  pb <- cli::cli_progress_bar(
    format = "{cli::pb_bar} {.val {key}} | [{pb_current}/{pb_total}]",
    total = length(keys)
  )

  quiet <- function(expr) {
    utils::capture.output(result <- suppressMessages(suppressWarnings(expr)))
    result
  }

  time_start <- Sys.time()
  res <- lapply(keys, function(key){
    cli::cli_progress_update(id = pb)
    quiet(get_inpn(parca, key, buffer = buffer))
  })
  cli::cli_progress_done(id = pb)
  cli::cli_alert_success(
    "Downloaded {length(keys)} INPN layers in {Sys.time() - time_start}."
  )

}
