#' Get COG administrative datasets
#'
#' Download and cache French COG datasets (communes, departments, regions)
#' from data.gouv.fr. Cached data are reused unless `update = TRUE`.
#'
#' @param cache Cache directory. If `NULL`, uses package cache directory.
#' @param update Force re-download even if cached.
#' @param verbose Display progress messages.
#'
#' @return Named list of data frames:
#' - `com`: communes
#' - `dep`: departments
#' - `reg`: regions
#'
#' @examples
#' \dontrun{
#' cog <- get_cog()
#' cog <- get_cog(update = TRUE)
#' }
#'
#' @export
get_cog <- function(cache = NULL, update = FALSE, verbose = TRUE) {

  if (is.null(cache)) {
    cache <- tools::R_user_dir("Rsequoia2", which = "cache")
  }

  cog_dir <- file.path(cache, "cog")
  dir.create(cog_dir, recursive = TRUE, showWarnings = FALSE)

  paths <- list(
    com = file.path(cog_dir, "com.rds"),
    dep = file.path(cog_dir, "dep.rds"),
    reg = file.path(cog_dir, "reg.rds")
  )

  if (!update && all(file.exists(unlist(paths)))) {
    if (verbose) cli::cli_alert_success("COG loaded from cache")
    return(lapply(paths, readRDS))
  }

  if (verbose) cli::cli_alert_info("Downloading COG datasets...")

  df <- dg_get_dataset("58c984b088ee386cdb1261f3")
  r <- df$resources
  r$last_modified <- as.Date(substr(r$last_modified, 1, 10))
  r <- r[order(r$last_modified, decreasing = TRUE), ]

  read_latest <- function(pattern) {
    url <- r[grepl(pattern, r$title), "url"][[1]]
    utils::read.csv(url, fileEncoding = "UTF-8", encoding = "UTF-8")
  }

  suffix_cols <- function(x, cols, suffix) {
    n <- names(x)
    names(x) <- replace(n, n %in% cols, paste0(cols, "_", suffix))
    x
  }

  # Communes
  com <- read_latest("Liste des communes, arrondissements municipaux")
  com <- suffix_cols(com, c("TNCC", "NCC", "NCCENR", "LIBELLE"), "COM")
  com <- com[com$TYPECOM == "COM", ]

  # Départements
  dep <- read_latest("Liste des d.{1}partements") #.{1} avoid ascii
  dep <- suffix_cols(
    dep,
    c("CHEFLIEU", "TNCC", "NCC", "NCCENR", "LIBELLE"),
    "DEP"
  )

  # Régions
  reg <- read_latest("Liste des r.{1}gions") #.{1} avoid ascii
  reg <- suffix_cols(
    reg,
    c("CHEFLIEU", "TNCC", "NCC", "NCCENR", "LIBELLE"),
    "REG"
  )

  saveRDS(com, paths$com)
  saveRDS(dep, paths$dep)
  saveRDS(reg, paths$reg)

  if (verbose) {
    cli::cli_alert_success("COG cached in {.path {normalizePath(cog_dir)}}")
  }

  list(com = com, dep = dep, reg = reg)
}
