.re_main <- list(
  an        = "([0-9]{2})",
  sec       = "(?:\\s+([0-9]{3}))?",
  section   = "\\s+([A-Za-z]{1,2})",
  numero    = "\\s+([0-9]+)",
  voirie    = "\\s+(.+?)(?=\\s+(?:[A-Z][0-9]{3}|[0-9]{4}))",
  rivoli    = "\\s+((?:[A-Z][0-9]{3})|(?:[0-9]{4}))",
  parc_prim = "(?:\\s+([0-9]{4}))?",
  fp        = "\\s+([0-9])",
  spar      = "(?:\\s+([0-9]{3}[A-Z]))?",
  gr        = "(?:\\s+([A-Z]{1,2}))?",
  cls_nat   = "(?:(?:\\s+([0-9]{2}))?\\s+([A-Za-z][A-Za-z ']*))?",
  rest      = "\\s+(.*)$"
)

.re_detail <- list(
  spar    = "^([0-9]{3}[A-Z])",
  suf     = "\\s+([A-Z]{1,2})",
  gr      = "\\s+([A-Z]{1,2})",
  classe  = "(?:\\s+([0-9]{2}))?",
  nature  = "\\s+([A-Za-z][A-Za-z ']*)",
  ha      = "(?:\\s+([0-9]{1,3}))?",
  a       = "\\s+([0-9]{2})",
  ca      = "\\s+([0-9]{2})\\s+"
)

parse_surface <- function(x) {

  # Find first French decimal token (e.g. "123,1", "510,46")
  m <- regexpr("[0-9]+,[0-9]+", x)
  if (m[1] == -1){
    n <- as.integer(regmatches(x, gregexpr("\\b[0-9]{1,3}\\b", x))[[1]])
    n <- tail(n, 3)
    s <- c(
      ha = ifelse(length(n) == 3, n[1], 0),
      a  = ifelse(length(n) >= 2, n[length(n) - 1], 0),
      ca = ifelse(length(n) >= 1, n[length(n)], NA)
    )
    return(s)
  }

  dec <- regmatches(x, m)         # the decimal token
  pre <- substr(x, 1, m[1] - 1)   # everything before the decimal token

  # Integer tokens before revenue (keep as character to preserve leading zeros)
  tok <- regmatches(pre, gregexpr("\\b[0-9]{1,3}\\b", pre))[[1]]

  # If revenue is written with a thousands prefix like "1 510,46",
  # remove that prefix ONLY when it's a 1-digit token and the decimal has 3 digits before comma.
  # This matches your examples with "... 1 123,1" but not "... 11 123,1".
  decimal_has_3digits <- grepl("^[0-9]{3},", dec)
  last_is_1digit      <- length(tok) >= 1 && nchar(tail(tok, 1)) == 1

  if (decimal_has_3digits && last_is_1digit) {
    tok <- tok[-length(tok)]
  }

  # Surface = last up to 3 remaining integer tokens, right-aligned as ha/a/ca
  tok <- tail(tok, 3)
  tok <- c(rep("0", 3 - length(tok)), tok)

  setNames(as.integer(tok), c("ha", "a", "ca"))
}

parse_main <- function(x) {

  regex_prf <- paste0("^", paste(.re_main, collapse = ""))

  rm <- regmatches(x, regexec(regex_prf, x, perl = TRUE))[[1]]

  df <- as.data.frame(as.list(rm[-1]), stringsAsFactors = FALSE)
  names(df) <- c(
    "an", "sec", "section", "numero", "voirie", "rivoli",
    "parc_prim","fp", "spar","gr","classe","nature",
    "rest"
  )

  surf <- parse_surface(df$rest)

  df$rest <- NULL
  df$parc_prim <- NULL
  df$sec  <- ifelse(is.na(df$sec), "000", df$sec)
  df$ha <- surf["ha"]
  df$a  <- surf["a"]
  df$ca <- surf["ca"]
  df$surface <- df$ha + df$a * 0.01 + df$ca * 0.0001
  df$type <- "main"

  df[, c("rest", "parc_prim", "ha", "a", "ca")] <- NULL

  return(df)
}

parse_detail <- function(x) {

  regex_prf <- paste0("^", paste(.re_detail, collapse = ""))
  rm <- regmatches(x, regexec(regex_prf, x))[[1]] |> as.list()
  df <- data.frame(rm[-1], stringsAsFactors = FALSE)
  df <- setNames(df, names(.re_detail))

  df$ha <- ifelse(df$ha == "", 0, as.numeric(df$ha))
  df$a <- as.numeric(df$a)
  df$ca <- as.numeric(df$ca)
  df$surface <- df$ha + df$a * 0.01 + df$ca * 0.0001
  df$type <- "detail"

  df[,c("suf", "ha", "a", "ca")] <- NULL
  df[,c("an", "sec", "section", "numero", "voirie", "rivoli", "fp")] <- NA

  return(df)
}

parse_rp <- function(pdf){

  # Normalize txt ----
  txt <- pdftools::pdf_text(pdf) |> paste0(collapse = "\n")
  txt <- iconv(txt, "UTF-8", "ASCII//TRANSLIT")
  txt <- gsub(" +", " ", txt)

  if (nchar(txt) < 500){
    cli::cli_abort("pdf is probably an image (scan).")
  }

  # Extract COG info ----
  dep <- sub(".*departement\\s*:\\s*([0-9]{2}).*", "\\1", txt, ignore.case = T)
  com <- sub(".*commune\\s*:\\s*([0-9]{3}).*", "\\1", txt, ignore.case = T)

  # Split into lines ----
  lines <- trimws(unlist(strsplit(txt, "\n", fixed = TRUE)))
  idx_non_bati <- which(grepl("non batie", lines))[1]
  lines <- lines[idx_non_bati:length(lines)]
  lines <- lines[lines != ""]

  # differentiate between main and detail row ----
  is_main <- \(x) grepl("^\\d{2}(?:\\s+([0-9]{3}))?\\s+[A-Za-z]{1,2}\\s+\\d+", x)
  is_detail <- \(x) grepl("^[0-9A-Z]{3,4}\\s+[A-Z]{1,2}\\s+[A-Z]{1,2}", x)

  prf_df <- lapply(
    lines,
    \(x) {
      if (is_main(x)) {parse_main(x)}
      else if (is_detail(x)) {parse_detail(x)}
      else {NULL}
    }
  )

  prf_df <- Filter(Negate(is.null), prf_df)
  df <- do.call(rbind, prf_df)
  df$dep <- dep
  df$com <- com

  fill_down <- function(x) {
    ok <- !is.na(x)
    idx <- which(ok)
    x[idx[cumsum(ok)]]
  }

  cols_to_fill <- c(
    "an", "sec", "section", "numero", "voirie", "rivoli", "fp", "spar"
  )

  df[cols_to_fill] <- lapply(df[cols_to_fill], fill_down)

  return(invisible(df))
}

