#' Load field definitions from the Sequoia configuration
#'
#' Internal helper used by `seq_normalize()` to access field metadata
#' defined in `inst/config/seq_fields.yaml`. When `field` is `NULL`, the full
#' configuration list is returned. Otherwise, the corresponding field
#' definition is extracted.
#'
#' @param field `character` or `NULL`; If provided, must match a key in
#' `inst/config/seq_fields.yaml`.
#' @param filepath `character` or `NULL`; override for the path to the default
#' YAML configuration file (`inst/config/seq_fields.yaml`). Used mainly for
#' testing.
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
#' seq_field("identifier")
#' }
#'
seq_field <- function(field = NULL, filepath = NULL){

  if (is.null(filepath)) {
    filepath <- system.file("config/seq_fields.yaml", package = "Rsequoia2")
  }

  cfg <- yaml::read_yaml(filepath)

  if (is.null(field)){
    return(cfg)
  }

  bad_field_name <- !(field %in% names(cfg))
  if (bad_field_name){
    cli::cli_abort(c(
      "x" = "Bad {.arg field} value : {.val {field}}",
      "i" = "Run {.run  Rsequoia2::seq_field()} for all available fields."
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
#' @param filepath `character` or `NULL`; override for the path to the default
#' YAML configuration file (`inst/config/seq_fields.yaml`). Used mainly for
#' testing.
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
seq_table <- function(table = NULL, filepath = NULL){

  # Use test path if provided
  if (is.null(filepath)) {
    filepath <- system.file("config/seq_tables.yaml", package = "Rsequoia2")
  }

  cfg <- yaml::read_yaml(filepath)

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
check_config <- function(filepath = NULL) {

  field_key <- names(seq_field(filepath = filepath))
  table <- seq_table()

  # Check 1: all fields in seq_tables.yaml exist in seq_fields.yaml
  find_bad_key <- function(tbl, tbl_name) {
    bad_key <- setdiff(tbl, field_key)
    n_bad_key <- length(bad_key)

    if (n_bad_key > 0) {
      cli::cli_warn(
        "{n_bad_key} bad key{?s} found in table {.val {tbl_name}}: {.val {bad_key}}"
      )
    }

    return(bad_key)
  }

  table_bad_keys <- mapply(find_bad_key, table, names(table), SIMPLIFY = FALSE)

  # Check 2: all fields in seq_fields.yaml are used in seq_tables.yaml
  used_keys <- unique(unlist(table, use.names = FALSE))
  unused_fields <- setdiff(field_key, used_keys)
  n_unused <- length(unused_fields)

  if (n_unused > 0) {
    cli::cli_warn(
      "{n_unused} field{?s} defined in seq_fields but not used in any table: {.val {unused_fields}}"
    )
  }

  # Success message if no issues
  total_bad <- sum(lengths(table_bad_keys)) + length(unused_fields)

  if (total_bad == 0) {
    cli::cli_alert_success("Well done, Configuration is consistent !")
  }

  invisible(list(
    table_bad_keys = table_bad_keys,
    unused_fields  = unused_fields
  ))
}


