#' Parse a French cadastral property record (RP) PDF
#'
#' `parse_rp()` extracts and parses non-built parcels from a French cadastral
#' property record PDF (Relevé de Propriété – RP).
#'
#' The function reads the PDF, identifies main parcels and detail rows,
#' computes parcel surfaces, and returns a tidy data frame ready for analysis.
#'
#' @param pdf Path to a cadastral RP PDF file.
#'
#' @return A data frame with one row per parcel (main and detail).
#' Columns include parcel identifiers, land use, surface (in hectares),
#' and administrative codes.
#'
#' @details
#' Only the *non-built* section of the RP is parsed.
#' Image-only (scanned) PDFs are not supported.
#'
#' @examples
#' \dontrun{
#' df <- parse_rp("releve_propriete.pdf")
#' head(df)
#' }
#'
#' @export
parse_rp <- function(pdf){

  # Normalize txt ----
  txt <- pdftools::pdf_text(pdf) |> paste0(collapse = "\n")
  txt <- iconv(txt, "UTF-8", "ASCII//TRANSLIT")
  txt <- gsub(" +", " ", txt)

  if (nchar(txt) < 500){
    cli::cli_abort("pdf is probably an image (scan): {basename(pdf)}")
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
  df[, c("an", "rivoli", "fp", "s_par", "gr", "classe")] <- NULL

  fill_down <- function(x) {
    ok <- !is.na(x)
    idx <- which(ok)
    x[idx[cumsum(ok)]]
  }

  cols_to_fill <- c("prefix", "section", "numero", "lieu_dit")

  df[cols_to_fill] <- lapply(df[cols_to_fill], fill_down)

  df$com <- pad_left(com, 3)
  df$insee <- paste0(pad_left(dep, 2), pad_left(com, 3))
  df$prefix <- ifelse(is.na(df$prefix), "000", pad_left(df$prefix, 3))
  df$section <- pad_left(df$section, 2)
  df$numero <- pad_left(df$numero, 4)

  df$idu <- paste0(df$insee, df$prefix, df$section, df$numero)
  df$source <- basename(pdf)

  m_all <- df |>
    merge(happign::com_2025[, c("COM", "NCC_COM", "DEP")], by.x = "insee", by.y = "COM") |>
    merge(happign::dep_2025[, c("DEP", "NCC_DEP", "REG")], all.x = TRUE) |>
    merge(happign::reg_2025[, c("REG", "NCC_REG")], all.x = TRUE) |>
    field_rename() |>
    field_order("parca")

  m <- m_all[m_all$TYPE == "main", !(names(m_all) %in% c("NATURE", "TYPE"))]

  return(invisible(list(m_all = m_all, m = m)))
}

#' Main parcel parsing regex (internal)
#'
#' Internal specification used to parse main cadastral parcel rows.
#'
#' @noRd
.re_main <- list(
  an        = "([0-9]{2})",
  prefix    = "(?:\\s+([0-9]{3}))?",
  section   = "\\s+([A-Za-z]{1,2})",
  numero    = "\\s+([0-9]+)",
  lieu_dit    = "\\s+(.+?)(?=\\s+(?:[A-Z][0-9]{3}|[0-9]{4}))",
  rivoli    = "\\s+((?:[A-Z][0-9]{3})|(?:[0-9]{4}))",
  parc_prim = "(?:\\s+([0-9]{4}))?",
  fp        = "\\s+([0-9])",
  s_par      = "(?:\\s+([0-9]{3}[A-Z]))?",
  gr        = "(?:\\s+([A-Z]{1,2}))?",
  cls_nat   = "(?:(?:\\s+([0-9]{2}))?\\s+([A-Za-z][A-Za-z ']*))?",
  rest      = "\\s+(.*)$"
)

#' Detail parcel parsing regex (internal)
#'
#' Internal specification used to parse detail parcel rows.
#'
#' @noRd
.re_detail <- list(
  s_par    = "^([0-9]{3}[A-Z])",
  suf     = "\\s+([A-Z]{1,2})",
  gr      = "\\s+([A-Z]{1,2})",
  classe  = "(?:\\s+([0-9]{2}))?",
  nature  = "\\s+([A-Za-z][A-Za-z ']*)",
  ha      = "(?:\\s+([0-9]{1,3}))?",
  a       = "\\s+([0-9]{2})",
  ca      = "\\s+([0-9]{2})\\s+"
)

#' Parse cadastral surface components from a text fragment (internal)
#'
#' Extracts surface components (ha / a / ca) from a cadastral line fragment
#' and returns them as integers.
#'
#' @param x Character string containing surface information.
#'
#' @return An integer vector of length 3 with names `ha`, `a`, and `ca`.
#'
#' @noRd
parse_surface <- function(x) {

  # locate first French decimal token (revenue)
  m <- regexpr("[0-9]+,[0-9]+", x)

  # part to inspect for surface integers
  pre <- if (m[1] == -1) x else substr(x, 1, m[1] - 1)

  # extract integer tokens (keep as character for leading zeros)
  tok <- regmatches(pre, gregexpr("\\b[0-9]{1,3}\\b", pre))[[1]]

  # remove 1-digit thousands prefix like "... 1 510,46"
  if (m[1] != -1 &&
      grepl("^[0-9]{3},", regmatches(x, m)) &&
      length(tok) > 0 &&
      nchar(utils::tail(tok, 1)) == 1) {
    tok <- tok[-length(tok)]
  }

  # take last up to 3 tokens and right-align as ha / a / ca
  tok <- utils::tail(tok, 3)
  tok <- c(rep("0", 3 - length(tok)), tok)

  setNames(as.integer(tok), c("ha", "a", "ca"))
}

#' Parse a main cadastral parcel row (internal)
#'
#' Parses a main (non-built) parcel row extracted from a cadastral RP PDF.
#'
#' @param x A single line of text corresponding to a main parcel.
#'
#' @return A one-row data frame describing the parcel.
#'
#' @noRd
parse_main <- function(x) {

  regex_prf <- paste0("^", paste(.re_main, collapse = ""))

  rm <- regmatches(x, regexec(regex_prf, x, perl = TRUE))[[1]]

  df <- as.data.frame(as.list(rm[-1]), stringsAsFactors = FALSE)
  names(df) <- c(
    "an", "prefix", "section", "numero", "lieu_dit", "rivoli",
    "parc_prim","fp", "s_par","gr","classe","nature",
    "rest"
  )

  surf <- parse_surface(df$rest)

  df$rest <- NULL
  df$parc_prim <- NULL
  df$prefix  <- ifelse(is.na(df$prefix), "000", df$prefix)
  df$ha <- surf["ha"]
  df$a  <- surf["a"]
  df$ca <- surf["ca"]
  df$contenance <- df$ha + df$a * 0.01 + df$ca * 0.0001
  df$type <- "main"

  df[, c("rest", "parc_prim", "ha", "a", "ca")] <- NULL

  return(df)
}

#' Parse a detail cadastral parcel row (internal)
#'
#' Parses a detail row associated with a main cadastral parcel.
#'
#' @param x A single line of text corresponding to a detail parcel.
#'
#' @return A one-row data frame describing the detail parcel.
#'
#' @noRd
parse_detail <- function(x) {

  regex_prf <- paste0("^", paste(.re_detail, collapse = ""))
  rm <- regmatches(x, regexec(regex_prf, x))[[1]] |> as.list()
  df <- data.frame(rm[-1], stringsAsFactors = FALSE)
  df <- setNames(df, names(.re_detail))

  df$ha <- ifelse(df$ha == "", 0, as.numeric(df$ha))
  df$a <- as.numeric(df$a)
  df$ca <- as.numeric(df$ca)
  df$contenance <- df$ha + df$a * 0.01 + df$ca * 0.0001
  df$type <- "detail"

  df[,c("suf", "ha", "a", "ca")] <- NULL
  df[,c("an", "prefix", "section", "numero", "lieu_dit", "rivoli", "fp")] <- NA

  return(df)
}

