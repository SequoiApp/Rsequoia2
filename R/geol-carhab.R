#' Download CarHab geology archive
#'
#' Downloads CarHab ZIP archives for one or more French departments and
#' stores them in the Rsequoia2 cache.
#'
#' CarHab is a harmonised geological map database produced by
#' BRGM used to created the `CarHab` dataset of PatriNat. The downloaded
#' ZIP files are cached and reused on later calls unless `overwrite = TRUE`.
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
download_carhab <- function(
    dep,
    cache = seq_cache("carhab")$path,
    verbose = TRUE,
    overwrite = FALSE
    ) {

  dep <- check_dep(dep)
  base_url <- "http://infoterre.brgm.fr/telechargements/BDCharm50/"

  dep_ref <- get_cog(verbose = FALSE)$dep
  dep_ref <- dep_ref[dep_ref$DEP %in% dep, c("DEP", "NCC_DEP")]

  dep_ref$filename <- sprintf(
    "CARHAB_%s_%s.zip",
    dep_ref$DEP,
    gsub("\\s+", "-", dep_ref$NCC_DEP)
  )

  files <- lapply(seq_len(nrow(dep_ref)), function(i) {
    one_dep <- dep_ref$DEP[i]
    zip_name <- dep_ref$filename[i]

    zip_url <- paste0(base_url, zip_name)
    zip_local <- file.path(cache, zip_name)

    if (!file.exists(zip_local) || overwrite) {
      if (verbose) {
        cli::cli_progress_step(
          "Downloading CarHab for department {.val {one_dep}}",
          msg_done = "Downloading CarHab for department {.val {one_dep}}",
          spinner = TRUE
        )
      }

      curl::curl_download(
        url = zip_url,
        destfile = zip_local,
        quiet = TRUE
      )

    } else if (verbose) {
      cli::cli_alert_info(
        "Using cached CarHab archive for department {.val {one_dep}}: {.file {basename(zip_local)}}"
      )
    }

    zip_local
  })

  files <- unlist(files, use.names = FALSE)
  names(files) <- dep_ref$DEP

  return(invisible(files))
}
