#' List BDCharm50 archive URLs
#'
#' Scrapes the BDCharm50 download directory and returns available ZIP archive
#' URLs, named by archive filename.
#'
#' @return A named `character` vector. Names are ZIP filenames and values are
#'   download URLs.
#'
#' @keywords internal
get_bdcharm50_url <- function() {
  base_url <- "http://data.cquest.org/brgm/bd_charm_50/2019/"

  html <- readLines(base_url, warn = FALSE)

  href <- regmatches(html, gregexpr('href="[^"]+"', html))
  href <- unlist(href, use.names = FALSE)
  href <- gsub('^href="|"$', "", href)

  zip_name <- href[grepl("\\.zip$", href, ignore.case = TRUE)]
  zip_url <- paste0(base_url, zip_name)

  stats::setNames(zip_url, basename(zip_url))
}

#' Find BDCharm50 archive for a department
#'
#' Finds the unique BDCharm50 ZIP archive matching a department code.
#'
#' @param dep Department code.
#' @param urls Named `character` vector of archive URLs, usually returned by
#'   [get_bdcharm50_url()].
#'
#' @return A `character(1)` ZIP filename.
#'
#' @keywords internal
find_bdcharm50_zip <- function(dep, urls) {
  dep3 <- pad_left(dep, 3)

  zip_name <- grep(dep3, names(urls), value = TRUE)

  if (length(zip_name) == 0) {
    cli::cli_abort(c(
      "No BDCharm50 archive found for department {.val {dep}}.",
      "i" = "Expected to find a filename containing department code {.val {dep3}}."
    ))
  }

  if (length(zip_name) > 1) {
    cli::cli_abort(c(
      "Several BDCharm50 archives found for department {.val {dep}}.",
      "x" = "Matches: {.vals {zip_name}}",
      "i" = "Please make the archive matching rule stricter."
    ))
  }

  zip_name
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
) {
  dep <- check_dep(dep)

  urls <- get_bdcharm50_url()

  files <- lapply(dep, function(one_dep) {
    zip_name <- find_bdcharm50_zip(one_dep, urls)

    zip_url <- unname(urls[zip_name])
    zip_local <- file.path(cache, zip_name)

    if (!file.exists(zip_local) || overwrite) {
      if (verbose) {
        cli::cli_progress_step(
          "Downloading BDCharm50 for department {.val {one_dep}}",
          msg_done = "Downloading BDCharm50 for department {.val {one_dep}}",
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
