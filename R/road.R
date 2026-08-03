#' Retrieve road sections around an area
#'
#' @param x An `sf` object defining the input area.
#' @param buffer `numeric`; Buffer around `x`, in meters, used to define the
#'   download area.
#' @param private_in `logical`; If `TRUE`, roads fully contained in `x` are
#'   marked as private.
#'
#' @return An `sf` object containing road sections with standardized fields,
#'   or `NULL` if no roads are found.
#'
#'   The main fields are:
#'
#'   * `TYPE`: Simplified road type:
#'     - `RN`: national roads, motorways, European roads, `D9xx` roads,
#'       and major slip roads;
#'     - `RD`: other departmental roads and intermediate slip roads;
#'     - `RC`: other paved roads;
#'     - `RF`: gravel roads;
#'     - `PN`: natural paths and tracks.
#'   * `NATURE`: Simplified surface type (`revetue`, `empierree`, `naturel`).
#'   * `NOM`: Road number, or road name when no number is available.
#'   * `PRIVE`: Original private status. Roads fully contained in `x` are
#'     forced to `TRUE` when `private_in = TRUE`.
#'   * `SOURCE`: Data source (`BDTOPO V3`).
#'
#'   Other fields standardized by `seq_normalize()` are retained when
#'   available, including road importance and weight restrictions.
#'
#' @details
#' Road sections are retrieved from the IGN BDTOPO V3 dataset within a
#' buffered envelope around `x`.
#'
#' The function uses CRS EPSG:2154 for buffering and spatial operations.
#'
#' @export
get_road <- function(x, buffer = 1000, private_in = TRUE) {

  crs <- 2154
  x <- sf::st_transform(x, crs)

  # Retrieve roads around the input area.
  roads <- happign::get_wfs(
    seq_envelope(x, buffer),
    "BDTOPO_V3:troncon_de_route",
    verbose = FALSE
  )

  if (is.null(roads) || !nrow(roads)) {
    return(NULL)
  }

  roads <- roads |>
    sf::st_transform(crs) |>
    sf::st_zm()

  road_nature <- roads$nature
  road_number <- roads$cpx_numero
  road_number[!nzchar(road_number)] <- NA_character_

  importance <- suppressWarnings(as.numeric(roads$importance))

  # Simplify the road surface.
  paved_natures <- c(
    "Type autoroutier",
    "Bretelle",
    "Rond-point",
    "Route \u00e0 1 chauss\u00e9e",
    "Route \u00e0 2 chauss\u00e9es"
  )

  surface <- rep("naturel", nrow(roads))
  surface[road_nature %in% paved_natures] <- "revetue"
  surface[road_nature == "Route empierr\u00e9e"] <- "empierree"

  # Identify national and departmental roads.
  numbered <- !is.na(road_number)
  bretelle <- road_nature == "Bretelle"

  is_rn <- surface == "revetue" & (
    numbered & grepl("^(A|E|N|D9[0-9]{2})", road_number) |
      bretelle & !is.na(importance) & importance <= 2
  )

  is_rd <- surface == "revetue" & (
    numbered & grepl("^D", road_number) |
      bretelle & !is.na(importance) &
      importance > 2 & importance <= 4
  )

  # Assign types from the most general to the most specific.
  road_type <- rep("PN", nrow(roads))
  road_type[surface == "empierree"] <- "RF"
  road_type[surface == "revetue"] <- "RC"
  road_type[is_rd] <- "RD"
  road_type[is_rn] <- "RN"

  # Use the road number first, then the road name.
  road_name <- road_number
  missing_name <- is.na(road_name)

  road_name[missing_name] <- roads$cpx_toponyme_route_nommee[missing_name]
  road_name[!nzchar(road_name)] <- NA_character_

  # Normalize the output fields.
  out <- seq_normalize(roads, "road_line")

  out[[seq_field("type")$name]] <- road_type
  out[[seq_field("nature")$name]] <- surface
  out[[seq_field("name")$name]] <- road_name
  out[[seq_field("source")$name]] <- "BDTOPO V3"

  # Preserve existing status and force contained roads to private.
  if (private_in) {
    inside <- lengths(sf::st_within(roads, sf::st_union(x))) > 0
    private_field <- seq_field("is_private")$name
    out[[private_field]][inside] <- TRUE
  }

  unique(out)
}

#' Generate road section layer for a Sequoia project
#'
#' Retrieves road section line features intersecting and surrounding
#' the project area and writes the resulting layer to disk.
#'
#' @inheritParams get_road
#' @inheritParams seq_write
#'
#' @details
#' Road section line features are retrieved using [get_road()].
#'
#' If no features are found, the function returns `NULL` invisibly and no file
#' is written.
#'
#' When features are present, the layer is written to disk using
#' [seq_write()] with the key `"v.road.line"`.
#'
#' @return
#' Invisibly returns a named list of file paths written by [seq_write()].
#' Returns `NULL` invisibly when no features are found.
#'
#' @seealso
#' [get_road()], [seq_write()]
#'
#' @export
seq_road <- function(
    dirname = ".",
    buffer = 1000,
    private_in = TRUE,
    verbose = TRUE,
    overwrite = FALSE
) {

  # Read project area (PARCA)
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  id_field <- seq_field("identifier")$name
  id <- unique(parca[[id_field]])

  if (verbose){
    cli::cli_h1("ROAD LINES")
  }

  # Retrieve road section
  roads <- get_road(parca, buffer = buffer, private_in = private_in)

  # Exit early if nothing to write
  if (!is.null(roads) ) {
    roads[[id_field]] <- id

    roads <- seq_write(
      roads,
      "v.road.line",
      dirname = dirname,
      id = id,
      verbose = verbose,
      overwrite = overwrite
    )
  }

  return(invisible(c(roads) |> as.list()))
}
