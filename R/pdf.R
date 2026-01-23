# HELPER FUNCTIONS ----
#' Extract visual data from PDF with page numbers
#'
#' Wrapper around [pdftools::pdf_data()] that adds page numbers
#' to each extracted text element.
#'
#' @param pdf `character`. Path to PDF file
#' @return A `data.frame` with columns from `pdf_data()` plus `page` (integer)
#' @seealso [pdftools::pdf_data()].
#' @importFrom pdftools pdf_data
#' @keywords internal
extract_visual_data <- function(pdf) {
  pdf_data_list <- pdftools::pdf_data(pdf)

  res <- vector("list", length(pdf_data_list))

  for (i in seq_along(pdf_data_list)) {
    df <- as.data.frame(pdf_data_list[[i]], stringsAsFactors = FALSE)
    df$page <- i
    res[[i]] <- df
  }

  out <- do.call(rbind, res)
  rownames(out) <- NULL
  out$page <- as.integer(out$page)

  out
}

#' Identify "non bâtie(s)" section anchors
#'
#' Locates the "non" markers that indicate the start of unbuilt property
#' sections in French cadastral PDFs, with context validation.
#'
#' @param all_visual_data A `data.frame` from [extract_visual_data()]
#' @return A `data.frame` of anchor positions with same columns as input
#' @keywords internal
identify_non_anchors <- function(all_visual_data) {

  # 1. Pre-filter "non" candidates
  non_candidates <- all_visual_data[
    all_visual_data$text == "non" &
      all_visual_data$x >= 415 & all_visual_data$x <= 425,
  ]

  if (nrow(non_candidates) == 0) return(non_candidates)

  # 2. Add columns to check context words
  line_matches <- merge(
    non_candidates,
    all_visual_data[all_visual_data$text %in% c("Propriété(s)", "bâtie(s)"),
                    c("page", "y", "text", "x")],
    by = "page",
    suffixes = c("", "_ctx")
  )

  # 3. Check if context words are on the same line ±10px
  line_matches$y_diff <- abs(line_matches$y - line_matches$y_ctx)
  line_matches$valid <- (line_matches$text_ctx == "Propriété(s)" &
                           line_matches$x_ctx >= 363 & line_matches$x_ctx <= 373) |
    (line_matches$text_ctx == "bâtie(s)" &
       line_matches$x_ctx >= 436 & line_matches$x_ctx <= 446)

  line_matches$valid <- line_matches$valid & (line_matches$y_diff <= 10)

  # 4. Keep only one valid "non" per page
  valid_non <- subset(line_matches, valid)
  if (nrow(valid_non) > 0) {
    non_anchors <- valid_non[!duplicated(valid_non$page), names(non_candidates)]
  } else {
    # fallback: first "non" per page
    non_anchors <- do.call(
      rbind,
      by(non_candidates, non_candidates$page, function(x) x[1, ])
    )
    rownames(non_anchors) <- NULL
    warning("'Propriété(s) non bâtie(s)' context not verified")
  }

  return(non_anchors)
}

#' Identify "totale" section anchors
#'
#' Locates the "totale" markers that indicate section ends in French
#' cadastral PDFs, validated by proximity to "Contenance".
#'
#' @param all_visual_data A `data.frame` from [extract_visual_data()]
#' @return A `data.frame` of anchor positions with same columns as input
#' @keywords internal
identify_total_anchors <- function(all_visual_data) {

  # 1. Pre-filter "totale" candidates
  total_candidates <- all_visual_data[
    all_visual_data$text == "totale" & all_visual_data$x >= 153 & all_visual_data$x <= 163,
  ]

  if (nrow(total_candidates) == 0) return(total_candidates)

  # 2. Merge with "Contenance" on the same page
  contenance <- all_visual_data[
    all_visual_data$text == "Contenance" &
      all_visual_data$x >= 96 & all_visual_data$x <= 106,
  ]

  merged <- merge(total_candidates, contenance, by = "page", suffixes = c("", "_ctx"))
  merged$y_diff <- abs(merged$y - merged$y_ctx)

  # 3. Keep only if ±15px vertical tolerance
  valid_total <- merged[merged$y_diff <= 15, names(total_candidates)]

  # 4. Keep only one anchor per page
  total_anchors <- valid_total[!duplicated(valid_total$page), ]

  total_anchors
}

#' Extract table section from page
#'
#' Extracts the data table section from a PDF page using anchor positions.
#' Removes repeated column headers.
#'
#' @param page_data A `data.frame` for a single pdf page
#' @param non_anchor Anchor row from [identify_non_anchors()]
#' @param has_total `logical`, whether page contains "totale" anchor
#' @return Filtered `data.frame` containing only table rows
#' @keywords internal
extract_table_section <- function(page_data, non_anchor, has_total) {
  # Section start: 20px after "non"
  start_y <- non_anchor$y + 20

  # Section end
  if(has_total) {
    # Last page: stop before "totale"
    total_y <- page_data[
      page_data$text == "totale" & page_data$x == 158L,
      "y"
    ]
    if(length(total_y) > 0) {
      end_y <- total_y[1] - 5
    } else {
      end_y <- 545
    }
  } else {
    # Intermediate pages: stop at y=545
    end_y <- 545
  }

  # Extract section
  section_data <- page_data[
    page_data$y >= start_y & page_data$y <= end_y,
  ]

  # Remove repeated column headers
  section_data <- section_data[
    !grepl("An|Sec|N°|Plan|Voirie|Adresse|Code", section_data$text),
  ]

  return(section_data)
}

#' Remove column headers from table data
#'
#' Filters out repeated column headers based on position and text patterns.
#' Uses predefined position ranges for known header elements.
#'
#' @param table_data A `data.frame` of extracted table content
#' @return Filtered `data.frame` without header rows
#' @keywords internal
filter_column_headers <- function(table_data) {
  if(nrow(table_data) == 0) return(table_data)

  # Filter by height
  table_data <- table_data[table_data$height < 10, ]

  # List of filters to apply
  filters <- list(
    list(x_min = 252, x_max = 272, pattern = "Code"),
    list(x_min = 252, x_max = 272, pattern = "Rivoli"),
    list(x_min = 275, x_max = 295, pattern = "N°"),
    list(x_min = 284, x_max = 304, pattern = "Parc"),
    list(x_min = 279, x_max = 299, pattern = "Prim"),
    list(x_min = 301, x_max = 321, pattern = "FP/"),
    list(x_min = 301, x_max = 321, pattern = "DP"),
    list(x_min = 319, x_max = 339, pattern = "Tar"),
    list(x_min = 319, x_max = 339, pattern = "S"),
    list(x_min = 243, x_max = 263, pattern = "SUF"),
    list(x_min = 366, x_max = 386, pattern = "SSGR"),
    list(x_min = 369, x_max = 389, pattern = "GR/"),
    list(x_min = 393, x_max = 413, pattern = "CL"),
    list(x_min = 452, x_max = 472, pattern = "Nat"),
    list(x_min = 453, x_max = 473, pattern = "Cult"),
    list(x_min = 515, x_max = 535, pattern = "contenance"),
    list(x_min = 515, x_max = 535, pattern = "HA"),
    list(x_min = 536, x_max = 556, pattern = "A"),
    list(x_min = 547, x_max = 567, pattern = "CA"),
    list(x_min = 559, x_max = 579, pattern = "Revenu"),
    list(x_min = 582, x_max = 602, pattern = "cadastral"),
    list(x_min = 619, x_max = 639, pattern = "Coll"),
    list(x_min = 644, x_max = 664, pattern = "Nat"),
    list(x_min = 644, x_max = 664, pattern = "Exo"),
    list(x_min = 663, x_max = 683, pattern = "AN"),
    list(x_min = 663, x_max = 683, pattern = "Ret"),
    list(x_min = 685, x_max = 705, pattern = "Fraction"),
    list(x_min = 687, x_max = 707, pattern = "RC"),
    list(x_min = 697, x_max = 717, pattern = "Exo"),
    list(x_min = 718, x_max = 738, pattern = "%EXO"),
    list(x_min = 742, x_max = 762, pattern = "TC"),
    list(x_min = 774, x_max = 794, pattern = "Feuillet"),
    list(x_min = 714, x_max = 734, pattern = "Délivré")
  )

  # Apply all filters
  for(filt in filters) {
    idx <- which(
      table_data$x >= filt$x_min &
        table_data$x <= filt$x_max &
        grepl(filt$pattern, table_data$text, ignore.case = TRUE)
    )
    if(length(idx) > 0) {
      table_data <- table_data[-idx, ]
    }
  }

  return(table_data)
}

#' Group text elements into logical lines
#'
#' Groups elements on the same visual line using vertical proximity.
#' Elements within tolerance pixels are considered part of the same line.
#'
#' @param table_data A `data.frame` from [filter_column_headers()]
#' @param tolerance `Numeric`. Vertical tolerance in pixels (default 8)
#' @return A `data.frame` with added `y_group` column
#' @keywords internal
group_lines_by_y <- function(table_data, tolerance = 8) {
  if (nrow(table_data) == 0) return(table_data)

  # Order by page and y
  table_data <- table_data[order(table_data$page, table_data$y), ]

  # Compute line groups per page
  table_data$y_group <- ave(
    table_data$y, table_data$page,
    FUN = function(y) {
      # Compute difference between consecutive y values
      diffs <- c(Inf, diff(y))
      # Start a new group if difference > tolerance
      cumsum(diffs > tolerance)
    }
  )

  # Reorder table_data by page, y_group, x
  table_data <- table_data[order(table_data$page, table_data$y_group, table_data$x), ]

  return(table_data)
}


#' Validate if line contains parcel data
#'
#' Checks if a line contains valid parcel data based on text patterns.
#'
#' @param line_data A `data.frame` for a single line from [group_lines_by_y()]
#' @return `logical`. `TRUE` if line contains valid data patterns
#' @keywords internal
is_valid_row <- function(line_data) {
  if(nrow(line_data) < 3) return(FALSE)

  # Check if it's a real data line
  has_number <- any(grepl("^[0-9]+$", line_data$text))
  has_code <- any(grepl("^[A-Z][0-9]+", line_data$text))
  has_letter <- any(grepl("^[A-Z]$", line_data$text))

  return(has_number | has_code | has_letter)
}

#' Extract single column from line data
#'
#' Extracts text for a specific column based on x-position ranges.
#'
#' @param line_data A `data.frame` for a single line
#' @param col_name `character`. Column name from [define_column_positions()]
#' @param col_positions List of column position definitions
#' @return Extracted text or NA if no elements found
#' @keywords internal
extract_column <- function(line_data, col_name, col_positions) {
  pos <- col_positions[[col_name]]

  elements <- line_data[
    line_data$x >= pos$x - 3 & line_data$x <= pos$end + 3,  # 3px tolerance
  ]

  if(nrow(elements) == 0) {
    return(NA)
  }

  elements <- elements[order(elements$x), "text"] |>
    paste(collapse = " ") |>
    trimws()

  return(ifelse(elements == "", NA, elements))
}

#' Extract complete row from line data
#'
#' Applies [extract_column()] to all columns to build a complete data row.
#'
#' @param line_data A `data.frame` for a single line
#' @param col_positions List from [define_column_positions()]
#' @return A `data.frame` with one row containing all column values
#' @keywords internal
extract_row <- function(line_data, col_positions) {
  # Extract each column for this line
  row_values <- lapply(names(col_positions), function(col_name) {
    extract_column(line_data, col_name, col_positions)
  })

  # Convert to dataframe
  names(row_values) <- names(col_positions)
  row_df <- as.data.frame(row_values, stringsAsFactors = FALSE)

  return(row_df)
}

#' Extract all data rows from table
#'
#' Processes all lines in table data, extracting valid rows.
#'
#' @param table_data A `data.frame` from [group_lines_by_y()]
#' @param col_positions List from [define_column_positions()]
#' @return List of `data.frame`, one per extracted row
#' @keywords internal
extract_rows <- function(table_data, col_positions) {
  all_rows <- list()
  pages <- unique(table_data$page)

  for(current_page in pages) {
    page_data <- table_data[table_data$page == current_page, ]
    y_groups <- unique(page_data$y_group)

    for(current_y_group in y_groups) {
      line_data <- page_data[page_data$y_group == current_y_group, ]
      line_data <- line_data[order(line_data$x), ]

      if(nrow(line_data) == 0) next

      # Check if valid data line
      if(!is_valid_row(line_data)) next

      # Extract the row
      row_df <- extract_row(line_data, col_positions)
      all_rows[[length(all_rows) + 1]] <- row_df
    }
  }

  return(all_rows)
}

#' Define column x-position ranges
#'
#' Returns a list of column positions for parsing cadastral tables.
#' Each column has x and end coordinates defining its horizontal range.
#'
#' @return List with named column position definitions
#' @keywords internal
define_column_positions <- function() {
  list(
    an = list(x = 18, end = 20),
    section = list(x = 30, end = 43),
    num_plan = list(x = 59, end = 71),
    num_voirie = list(x = 85, end = 102),
    lieu_dit = list(x = 117, end = 194),
    rivoli = list(x = 262, end = 277),
    num_prim = list(x = 289, end = 302),
    dp = list(x = 311, end = 319),
    star = list(x = 329, end = 338),
    suf = list(x = 353, end = 364),
    gr = list(x = 376, end = 391),
    cl = list(x = 403, end = 409),
    natcult = list(x = 420, end = 474),
    ha = list(x = 525, end = 534),
    a = list(x = 546, end = 550),
    ca = list(x = 557, end = 565),
    rev_ca = list(x = 569, end = 616),
    coll = list(x = 629, end = 640),
    nat_exo = list(x = 654, end = 664),
    an_ret = list(x = 673, end = 682),
    fract_exo = list(x = 695, end = 718),
    pct_exo = list(x = 728, end = 745),
    tc = list(x = 752, end = 760),
    feuillet = list(x = 784, end = 805)
  )
}

#' Extract raw parcel data from PDF
#'
#' Main function that orchestrates the extraction of unbuilt parcel data
#' from French cadastral PDFs. Calls all internal processing functions.
#'
#' @param pdf `character`. Path to PDF file
#' @return A `data.frame` with raw parcel data, column names from
#'   [define_column_positions()]
#' @keywords internal
get_raw_parcels <- function(pdf) {
  cat("Extracting unbuilt parcels...\n")

  # 1. Extract PDF visual data
  all_visual_data <- extract_visual_data(pdf)

  if(nrow(all_visual_data) == 0) {
    cat("No visual data found.\n")
    return(data.frame())
  }

  # 2. Identify section boundaries
  cat("Identifying section boundaries...\n")
  non_anchors <- identify_non_anchors(all_visual_data)

  if(nrow(non_anchors) == 0) {
    cat("No sections found.\n")
    return(data.frame())
  }

  total_anchors <- identify_total_anchors(all_visual_data)

  # 3. Extract table sections efficiently
  cat("Extracting table data...\n")

  # Split visual data by page to avoid repeated subsetting
  pages_split <- split(all_visual_data, all_visual_data$page)

  table_data_list <- vector("list", nrow(non_anchors))

  for(i in seq_len(nrow(non_anchors))) {
    current_non <- non_anchors[i, ]
    current_page <- current_non$page

    page_data <- pages_split[[as.character(current_page)]]

    has_total <- any(page_data$text == "totale" & page_data$x == 158L)

    # Extract section (already handles empty)
    section_data <- extract_table_section(page_data, current_non, has_total)
    table_data_list[[i]] <- section_data
  }

  # Combine all sections at once
  table_data <- do.call(rbind, table_data_list)

  if(nrow(table_data) == 0) {
    cat("No data detected.\n")
    col_positions <- define_column_positions()
    return(data.frame(matrix(ncol = length(col_positions), nrow = 0,
                             dimnames = list(NULL, names(col_positions)))))
  }

  # 4. Define column positions
  col_positions <- define_column_positions()

  # 5. Filter headers and group lines
  cat("Cleaning and structuring data...\n")
  table_data <- table_data |>
    filter_column_headers() |>
    group_lines_by_y()

  # 6. Extract all structured rows
  all_rows <- extract_rows(table_data, col_positions)
  cat(sprintf("Rows detected: %d\n", length(all_rows)))

  # 7. Create final table
  if(length(all_rows) > 0) {
    final_table <- do.call(rbind, all_rows)
  } else {
    final_table <- data.frame(matrix(ncol = length(col_positions), nrow = 0,
                                     dimnames = list(NULL, names(col_positions))))
  }

  cat("Extraction complete.\n")
  return(final_table)
}

#' Process raw parcel data into structured format
#'
#' Cleans, normalizes, and structures raw parcel data by identifying main parcels,
#' subdivisions, and adding calculated fields like surface area.
#'
#' @param df Raw `data.frame` from [get_raw_parcels()]
#' @return Processed `data.frame` with normalized fields, row types, IDs,
#'   and calculated columns
#' @keywords internal
process_raw_parcels <- function(df) {
  # --- Initialize columns ---
  df[c("id", "row_type", "reference_row_index")] <- list(NA_character_, NA_character_, NA_integer_)

  # --- STEP 0: Field normalization ---
  normalize_num_plan <- function(val) {
    if(is.na(val) || val == "") return(NA_character_)
    digits <- gsub("[^0-9]", "", gsub("\\s+", "", as.character(val)))
    if(digits == "" || nchar(digits) > 4) return(NA_character_)
    sprintf("%04d", as.integer(digits))
  }
  df$num_plan <- vapply(df$num_plan, normalize_num_plan, character(1))

  normalize_section <- function(val) {
    if(is.na(val) || val == "") return(NA_character_)
    val <- gsub("\\s+", "", as.character(val))
    if(grepl("^[A-Z]$", val)) return(paste0("0000", val))
    if(grepl("^[A-Z]{2}$", val)) return(paste0("000", val))
    if(grepl("^[0-9]{3}[A-Z]{2}$", val)) return(val)
    return(NA_character_)
  }
  df$section <- vapply(df$section, normalize_section, character(1))

  # Split section into prefix and section_code
  df$prefix <- substr(df$section, 1, 3)
  df$section <- substr(df$section, 4, 5)

  # Reorder columns to put prefix second
  other_cols <- setdiff(names(df), "prefix")
  new_order <- c(other_cols[1], "prefix", other_cols[-1])
  df <- df[, new_order]

  # --- STEP 1: Identify main parcels ---
  main_idx <- which(!is.na(df$an) & !is.na(df$prefix) & !is.na(df$section) &
                      !is.na(df$num_plan) & !is.na(df$lieu_dit) &
                      !is.na(df$rivoli) & !is.na(df$dp))

  if(length(main_idx) > 0) {
    df$id[main_idx] <- paste0(df$prefix[main_idx], df$section[main_idx], df$num_plan[main_idx])
    df$row_type[main_idx] <- "main"
    df$reference_row_index[main_idx] <- main_idx

    # Define propagation blocks
    block_id <- rep(NA_integer_, nrow(df))
    block_starts <- main_idx
    block_ends <- c(main_idx[-1]-1, nrow(df))
    for(i in seq_along(block_starts)) {
      block_id[block_starts[i]:block_ends[i]] <- i
    }

    # Propagate main parcel fields vectorially
    main_fields <- c("an","prefix","section","num_plan","lieu_dit","rivoli","dp")
    optional_fields <- c("num_voirie","num_prim")

    for(i in seq_along(block_starts)) {
      rows <- which(block_id == i)
      for(field in c(main_fields, optional_fields)) {
        if(!is.null(df[[field]])) {
          mask <- is.na(df[[field]][rows])
          df[[field]][rows][mask] <- df[[field]][rows[1]]
        }
      }
      df$id[rows] <- df$id[rows[1]]
      df$reference_row_index[rows] <- df$reference_row_index[rows[1]]
    }
  }

  # --- STEP 2 & 3: Process subdivisions ---
  sub_idx <- which(!is.na(df$suf) & is.na(df$row_type) & !is.na(df$id))
  if(length(sub_idx) > 0) {
    for(idx in sub_idx) {
      parcel <- df$id[idx]
      # Propagate until next row with different id or filled row_type
      end_idx <- idx
      for(j in (idx+1):nrow(df)) {
        if(df$id[j] != parcel || !is.na(df$row_type[j])) break
        end_idx <- j
      }
      rows <- (idx:end_idx)
      df$row_type[rows] <- "subdivision"

      sub_fields <- c("star","suf","gr","cl","natcult","ha","a","ca","rev_ca")
      for(field in sub_fields) {
        if(!is.null(df[[field]]) && !is.na(df[[field]][idx])) {
          mask <- is.na(df[[field]][rows])
          df[[field]][rows][mask] <- df[[field]][idx]
        }
      }
    }
  }

  # --- STEP 4: Process tax subdivisions (main) ---
  main_rows <- which(df$row_type == "main")
  for(idx in main_rows) {
    parcel <- df$id[idx]
    end_idx <- idx
    for(j in (idx+1):nrow(df)) {
      if(df$id[j] != parcel || (!is.na(df$row_type[j]) && df$row_type[j] != "main")) break
      end_idx <- j
    }
    rows <- (idx+1):end_idx
    target_rows <- rows[is.na(df$row_type[rows])]
    if(length(target_rows) > 0) {
      df$row_type[target_rows] <- "main"
      main_fields <- c("star","gr","cl","natcult","ha","a","ca","rev_ca")
      for(field in main_fields) {
        if(!is.null(df[[field]]) && !is.na(df[[field]][idx])) {
          mask <- is.na(df[[field]][target_rows])
          df[[field]][target_rows][mask] <- df[[field]][idx]
        }
      }
    }
  }

  # Mark orphan rows
  df$row_type[is.na(df$row_type)] <- "orphan"

  # --- STEP 5: Calculated columns ---
  df$is_subdivision <- FALSE
  if(!is.null(df$id) && !is.null(df$suf)) {
    parcels <- unique(na.omit(df$id))
    for(parcel in parcels) {
      parcel_rows <- df$id == parcel
      if(any(!is.na(df$suf[parcel_rows]))) df$is_subdivision[parcel_rows] <- TRUE
    }
  }

  # Surface calculation
  if(!is.null(df$ha) && !is.null(df$a) && !is.null(df$ca)) {
    conv <- function(x) { as.numeric(gsub(",", ".", x)); }
    ha_num <- suppressWarnings(conv(df$ha)); ha_num[is.na(ha_num)] <- 0
    a_num <- suppressWarnings(conv(df$a)); a_num[is.na(a_num)] <- 0
    ca_num <- suppressWarnings(conv(df$ca)); ca_num[is.na(ca_num)] <- 0
    df$surface_m2 <- round(ha_num*10000 + a_num*100 + ca_num, 2)
    df$surface_ha <- df$surface_m2 / 10000
  } else {
    df$surface_m2 <- NA_real_; df$surface_ha <- NA_real_
  }

  return(df[, -28]) # keep compatibility
}

# MAIN FUNCTION ----
#' Extract reference information from PDF header
#'
#' Parses the first page of a French cadastral PDF to extract commune,
#' department, and other reference information from the header section.
#'
#' @param pdf_path `character`. Path to PDF file
#' @return A `data.frame` with columns: year, department_code, commune_code,
#'   commune_name, tres, communal_num, file_name, file_path, insee
#' @importFrom pdftools pdf_text
#' @export
get_reference <- function(pdf_path) {
  pdf_text <- tryCatch(pdftools::pdf_text(pdf_path)[1],
                       error = function(e) { warning(paste("PDF read error:", e$message)); return(NULL) })
  text <- pdf_text[1]

  # Helper to extract pattern
  extract <- \(pattern) {
    m <- regmatches(text, regexec(pattern, text, perl = TRUE))[[1]]
    if(length(m) >= 2) trimws(m[2]) else NA_character_
  }

  # Extract all values
  commune_name_match <- regmatches(text, regexec("Commune\\s*:\\s*\\d{2,3}\\s+([A-ZÉÈÊÀÂÎÏÔÙÛÇ'\\s-]+?)(?=\\s+TRES|$)", text, perl = TRUE))

  data.frame(
    year = extract("Année de référence\\s*:\\s*(\\d{4})"),
    department_code = extract("Département\\s*:\\s*([0-9A-Z]{2,3})"),
    commune_code = extract("Commune\\s*:\\s*(\\d{2,3})"),
    commune_name = if(length(commune_name_match[[1]]) >= 2) trimws(commune_name_match[[1]][2]) else NA_character_,
    tres = extract("TRES\\s*:\\s*([0-9A-Z]+)"),
    communal_num = extract("(?<!\\d)([A-Z]\\d{5})(?!\\d)"),
    file_name = basename(pdf_path),
    file_path = pdf_path,
    stringsAsFactors = FALSE
  ) |>
    transform(insee = as.character(paste0(department_code, commune_code)))
}

#' Extract owner information from PDF
#'
#' Parses French cadastral PDFs to extract owner details including type,
#' name, and address from property rights sections.
#'
#' @param pdf_path `character`. Path to PDF file
#' @return A `data.frame` with columns: type, type_code, code, name, firstname,
#'   address, seq_name, file_name, file_path
#' @importFrom pdftools pdf_text
#' @export
get_owner <- function(pdf_path) {
  text <- tryCatch(pdftools::pdf_text(pdf_path)[1],
                   error = function(e) { warning(paste("PDF read error:", e$message)); return("") })

  blocks <- strsplit(text, "Droit réel\\s*:")[[1]]
  if (length(blocks) < 2) return(data.frame())

  type_map <- c("Propriétaire" = "pp", "Propriétaire/Indivision" = "ind_pp",
                "Usufruitier" = "us", "Nu-propriétaire/Indivision" = "ind_np",
                "Usufruitier/Indivision" = "ind_us", "Nu-propriétaire" = "np")

  extract <- function(pattern, text) {
    m <- regmatches(text, regexec(pattern, text, perl = TRUE))
    if(length(m[[1]]) > 1) trimws(m[[1]][2]) else NA_character_
  }

  owners_df <- data.frame()
  file_name <- basename(pdf_path)

  for (block in blocks[-1]) {
    block <- sub("Propriété\\(s\\).*", "", block)

    type <- if(grepl("Propriétaire/Indivision", block)) "Propriétaire/Indivision"
    else if(grepl("Propriétaire", block)) "Propriétaire"
    else if(grepl("Usufruitier/Indivision", block)) "Usufruitier/Indivision"
    else if(grepl("Usufruitier", block)) "Usufruitier"
    else if(grepl("Nu-propriétaire/Indivision", block)) "Nu-propriétaire/Indivision"
    else if(grepl("Nu-propriétaire", block)) "Nu-propriétaire"
    else NA_character_

    type_code <- if(!is.na(type) && type %in% names(type_map)) type_map[type] else "NA"
    code <- extract("Numéro propriétaire\\s*:\\s*([A-Z0-9]+)", block)
    name <- extract("Nom\\s*:\\s*([A-ZÉÈÊÀÂÎÏÔÙÛÇ'\\s-]+)(?=\\s+Prénom|\\s+Adresse|$)", block)
    firstname <- extract("Prénom\\s*:\\s*([A-ZÉÈÊÀÂÎÏÔÙÛÇ'\\s-]+)(?=\\s+Adresse|$)", block)

    # Address extraction
    addr_start <- regexpr("Adresse\\s*:", block)
    address <- if(addr_start > 0) {
      addr <- substr(block, addr_start + attr(addr_start, "match.length"), nchar(block))
      addr <- trimws(gsub("\\s+", " ", addr))
      sub("\\s*(Propriété\\(s\\)|Droit réel).*$", "", addr)
    } else NA_character_

    seq_name <- if(!is.na(firstname) && !is.na(name) && !is.na(type_code)) {
      paste(name, " ", firstname, " (", type_code, ")", sep = "")
    } else if(!is.na(name) && !is.na(type_code)) {
      paste(name, " (", type_code, ")", sep = "")
    } else NA_character_

    owners_df <- rbind(owners_df, data.frame(
      type, type_code, code, name, firstname, address, seq_name,
      file_name, file_path = pdf_path,
      stringsAsFactors = FALSE
    ))
  }

  return(owners_df)
}

#' Extract and process complete parcel data from PDF
#'
#' Orchestrates the full extraction pipeline: raw parcel data, reference info,
#' owner info, and combines everything into a final structured data frame.
#'
#' @param pdf `character`. Path to PDF file
#' @return A complete parcel `data.frame` with all processed fields and
#'   commune/owner information
#' @export
get_parcels <- function(pdf){
  raw_parcels <- get_raw_parcels(pdf)
  proc_parcels <- process_raw_parcels(raw_parcels)

  refs <- get_reference(pdf)
  owners <- get_owner(pdf)

  # Add new columns
  proc_parcels$dep_code <- refs$department_code
  proc_parcels$com_code <- refs$commune_code
  proc_parcels$com_name <- refs$commune_name
  proc_parcels$insee <- refs$insee
  proc_parcels$owner <- if(nrow(owners) > 0) paste(owners$seq_name, collapse = " & ") else NA
  proc_parcels$idu <- paste0(proc_parcels$com_code,
                             proc_parcels$id)

  # Define the desired order of columns
  first_cols <- c("owner", "dep_code", "com_code", "com_name", "insee", "idu")
  all_cols <- names(proc_parcels)
  other_cols <- setdiff(all_cols, first_cols)
  new_order <- c(first_cols, other_cols)
  parcels <- proc_parcels[, new_order]

  return(parcels)
}

#' Process multiple PDFs and combine results
#'
#' Applies the extraction pipeline to multiple PDF files and combines
#' results into a single list with three data frames.
#'
#' @param pdf_files `character` vector. Paths to PDF file
#' @return List with three `data.frame`: `refs`, `owners`, `parcels`
#' @export
get_matrice <- function(pdf_files){
  n <- length(pdf_files)
  refs_list <- vector("list", n)
  owners_list <- vector("list", n)
  parcels_list <- vector("list", n)

  for(i in seq_along(pdf_files)) {
    pdf <- pdf_files[i]
    cat("Treatment :", pdf, "\n")

    refs_list[[i]] <- get_reference(pdf)
    owners_list[[i]] <- get_owner(pdf)
    parcels_list[[i]] <- get_parcels(pdf)
  }

  list(
    refs = do.call(rbind, refs_list),
    owners = do.call(rbind, owners_list),
    parcels = do.call(rbind, parcels_list)
  )
}
