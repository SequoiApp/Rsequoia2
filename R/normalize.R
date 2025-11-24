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
  x <- field_order(x)

  return(x)
}

#' Load field definitions from the Sequoia configuration
#'
#' Internal helper used by `seq_normalize()` to access field metadata
#' defined in `inst/config/seq_fields.yaml`. When `field` is `NULL`, the full
#' configuration list is returned. Otherwise, the corresponding field
#' definition is extracted.
#'
#' @param field `character` or `NULL`; If provided, must match a key in
#' `inst/config/seq_fields.yaml`.
#'
#' @return A list describing all fields, or a single field definition.
#'
#' @examples
#' \dontrun{
#' # Return all field definitions
#' seq_field()
#'
#' # All avaialbe fields
#' names(seq_field())
#'
#' # Return the definition of a specific field
#' seq_field("identifiant")
#' }
#'
seq_field <- function(field = NULL){

  cfg_path <- system.file("config/seq_fields.yaml", package = "Rsequoia2")
  cfg <- yaml::read_yaml(cfg_path)

  if (is.null(field)){
    return(cfg)
  }

  bad_field_name <- !(field %in% names(cfg))
  if (bad_field_name){
    cli::cli_abort(c(
      "x" = "Bad {.arg field} value : {.val {field}}",
      "i" = "Run {.run  Rsequoia2:::seq_field()} for all available fields."
    ))
  }

  return(cfg[[field]])

}

#' Load table definitions from the Sequoia configuration
#'
#' Internal helper used by `seq_normalize()` to determine which fields
#' belong to a given table. Table definitions are stored in
#' `inst/config/seq_tables.yaml`.
#'
#' @param table `character` or `NULL`; If provided, must match a key in
#' `inst/config/seq_tables.yaml`.
#'
#' @return A character vector of field keys for the selected table.
#'
#' @examples
#' \dontrun{
#' # List all available tables
#' names(seq_table())
#'
#' # Load field keys for the "parcelle" table
#' seq_table("parca")
#' }
#'
seq_table <- function(table = NULL){

  cfg_path <- system.file("config/seq_tables.yaml", package = "Rsequoia2")
  cfg <- yaml::read_yaml(cfg_path)

  if (is.null(table)){
    return(cfg)
  }

  table_names <- names(cfg)
  if (length(table) != 1){
    cli::cli_abort("{.arg table} should be length one character from : {.val {table_names}}")
  }

  bad_table_name <- !(table %in% table_names)
  if (bad_table_name){
    cli::cli_abort(c(
      "x" = "Bad {.arg table} value : {.val {table}}",
      "i" = "Available tables are : {.val {table_names}}"
    ))
  }

  return(cfg[[table]])
}

#' @noRd
field_rename <- function(x){

  fields <- seq_field()
  alias_list <- lapply(fields, \(x) setNames(rep(x$name, length(x$alias)), x$alias))
  alias_map <- Reduce(c, alias_list, c())

  to_rename <- names(x) %in% names(alias_map)

  if (length(to_rename) > 0){
    names(x)[to_rename] <- alias_map[names(x)[to_rename]]
  }

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



