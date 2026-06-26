#' List BDCharm50 archive URLs
#'
#' Scrapes the BDCharm50 download directory and returns available ZIP archive
#' URLs, named by archive filename.
#'
#' @param dep French num department
#'
#' @return A named `character` vector. Names are ZIP filenames and values are
#'   download URLs.
#'
#' @keywords internal
get_bdcharm50_url <- function(dep) {
  base_url <- "http://infoterre.brgm.fr/telechargements/BDCharm50/"

  dep <- check_dep(dep)

  dep3 <- pad_left(dep, 3)
  zip_name <- sprintf("GEO050K_HARM_%s.zip", dep3)

  zip_name[dep3 %in% c("059", "062")] <-"GEO050K_HARM_059_062.zip"

  zip_name[dep3 %in% c("075", "077", "078", "091", "092", "093", "094", "095")] <-
    "GEO050K_HARM_075_077_078_091_092_093_094_095.zip"

  urls <- paste0(base_url, zip_name)
  stats::setNames(urls, zip_name)
}

#' Download BRGM BD Charm 50 geology archives
#'
#' Downloads BD Charm 50 ZIP archives for one or more French departments and
#' stores them in the Rsequoia2 cache.
#'
#' BD Charm 50 is a harmonised 1:50,000 geological map database produced by
#' BRGM from vectorised geological maps. The downloaded ZIP files are cached and
#' reused on later calls unless `overwrite = TRUE`.
#'
#' @param dep `character` or `numeric`; Department code(s), such as `"08"` or
#' `8`. Codes are checked against [Rsequoia2::get_cog()].
#' @param cache `character`; Directory where ZIP archives are stored.
#' Defaults to the Rsequoia2 BD Charm 50 cache directory, see
#' [Rsequoia2::seq_cache()].
#' @param verbose `logical`; If `TRUE`, display progress messages.
#' @param overwrite `logical`; If `TRUE`, re-download archives even when
#' they already exist in `cache`.
#'
#' @return Invisibly returns a named `character` vector of local ZIP file paths,
#' with department codes as names.
#'
#' @export
download_bdcharm50 <- function(
    dep,
    cache = seq_cache("bdcharm50")$path,
    verbose = TRUE,
    overwrite = FALSE
){

  dep <- check_dep(dep)

  urls <- get_bdcharm50_url(dep)
  urls <- urls[!duplicated(names(urls))]

  files <- lapply(names(urls), function(zip_name) {
    zip_url <- unname(urls[zip_name])
    zip_local <- file.path(cache, zip_name)

    if (!file.exists(zip_local) || overwrite) {
      if (verbose) {
        cli::cli_progress_step(
          "Downloading BDCharm50: {.val {zip_name}}",
          msg_done = "Downloading BDCharm50: {.val {zip_name}}",
          spinner = TRUE
        )
      }

      curl::curl_download(
        url = zip_url,
        destfile = zip_local,
        quiet = TRUE
      )
    } else if (verbose) {
      cli::cli_alert_success(
        "Using cached BDCharm50 archive for department {.val {one_dep}}: {.file {basename(zip_local)}}"
      )
    }

    return(zip_local)
  })

  files <- unlist(files, use.names = FALSE)
  names(files) <- dep

  return(invisible(files))
}
