#' Normalize a data frame according to Sequoia table definitions
#'
#' `seq_normalize()` standardizes the structure of a `data.frame` using the
#' Sequoia configuration stored in `inst/config/seq_fields.yaml` and
#' `seq_tables.yaml`.
#'
#' The normalization process applies, in order:
#'
#' 1. **Field renaming** using configured aliases
#' 2. **Field selection**: add missing required fields and drop invalid ones
#' 3. **Type coercion** to the classes defined in the configuration
#' 4. **Column reordering** according to the table definition
#'
#' This ensures that the resulting data frame matches exactly the schema
#' expected for the selected Sequoia table.
#'
#' @param x `data.frame` to normalize.
#' @param table `character` A single table name as defined in
#' `config/seq_tables.yaml`.
#'
#' @details
#' **Field aliases** defined in `seq_fields.yaml` allow multiple column names
#' (e.g. `"foret"`, `"id"`) to be mapped to the canonical field name
#' (e.g. `"IDENTIFIANT"`).
#' Aliases are automatically detected and replaced during normalization.
#'
#' For example, if the configuration contains:
#' ```yaml
#' id:
#'   name: "IDENTIFIANT"
#'   alias: ["id", "foret"]
#' ```
#' Then:
#' ```r
#' names(df)
#' #> c("foret", "contenance")
#'
#' df <- seq_normalize(df, "parca")
#'
#' names(df)
#' #> c("IDENTIFIANT", "SURF_CAD")
#' ```
#'
#' @return A normalized `data.frame` matching the structure required by the
#'   selected Sequoia table.
#'
#' @export
seq_normalize <- function(x, table){

  x <- field_rename(x)
  x <- field_add_drop(x, table)
  x <- field_check_class(x)
  x <- field_order(x, table)

  return(x)
}

#' @noRd
field_rename <- function(x){

  fields <- seq_field()
  alias_list <- lapply(fields, \(x) setNames(rep(x$name, length(x$alias)), x$alias))
  alias_map <- Reduce(c, alias_list, c())

  # Columns eligible for renaming
  hits <- names(x) %in% names(alias_map)
  new_names <- replace(names(x), hits, alias_map[names(x)[hits]])
  duplicated_name <- unique(new_names[duplicated(new_names)])

  if (length(duplicated_name) > 0) {
    conflicts <- names(x)[hits][new_names[hits] %in% duplicated_name]
    targets <- new_names[hits][new_names[hits] %in% duplicated_name]
    conflict_lines <- sprintf("  - %s -> %s", conflicts, targets)

    cli::cli_abort(c(
      "x" = "Multiple columns would be renamed to the same field name.",
      "!" = c("Conflicting renames:\n", conflict_lines),
      "i" = "Fix your input or update your alias definitions."
    ))
  }

  names(x) <- new_names

  return(x)
}

#' @noRd
field_check_class <- function(x){

  fields <- seq_field()
  class_list <- lapply(fields, \(x) setNames(x$class, x$name))
  class_map <- Reduce(c, class_list, c())

  to_check <- intersect(names(class_map), names(x))

  coerce_one <- function(v, tgt) {
    tgt <- tolower(tgt)
    if (is.factor(v)) v <- as.character(v)  # safe baseline
    switch(tgt,
           "character" = as.character(v),
           "numeric"   = as.numeric(v),
           "double"    = as.numeric(v),
           "integer"   = as.integer(as.numeric(v)),
           "logical"   = as.logical(v),
           "factor"    = factor(v),
           "date"      = as.Date(v),
           v # unknown -> leave as is
    ) |> suppressWarnings()
  }

  for (cols in to_check) {
    if (class_map[[cols]] %in% c("numeric", "double")){
      x[[cols]] <- gsub(",", ".", x[[cols]], fixed = TRUE)
    }
    x[[cols]] <- coerce_one(x[[cols]], class_map[[cols]])
  }

  return(x)
}

#' @noRd
field_order <- function(x, table){

  fields <- seq_field()[seq_table(table)]
  ordered_name <- unlist(lapply(fields, \(x) x$name))

  to_reorder <- intersect(ordered_name, names(x))
  other <- setdiff(names(x), to_reorder)

  x <- x[c(to_reorder, other)]

  return(x)
}

#' @noRd
field_add_drop <- function(x, table){
  fields <- seq_field()
  table_fields <- fields[seq_table(table)]
  table_names <- unlist(lapply(table_fields, `[[`, "name"))

  to_add <- setdiff(table_names, names(x))
  x[to_add] <- NA_character_

  keep <- intersect(names(x), table_names)
  x <- x[keep]

  return(x)
}

