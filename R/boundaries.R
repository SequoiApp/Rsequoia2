#' Create _FOREST_ and _OWNER_ object from _PARCA_ for a Sequoia project
#'
#' Aggregates parcel geometries to build forest and owner boundaries.
#'
#' @inheritParams seq_write
#' @param tol `numeric`; buffer tolerance in meter used to close small gaps.
#' Any gap narrower than `2 * tol` is filled, causing polygons on both sides to
#' be merged.
#'
#' @details
#' **About `tol`**
#' The gap-closing logic is based on a French forestry rule stating that a forest
#' split by a path or narrow road is legally considered a single continuous stand
#'
#' To reflect this, small linear gaps are removed using the sequence
#' *buffer → union → unbuffer*, applied with the distance given by `tol`.
#' Any gap narrower than `2 * tol` is closed.
#'
#' **Output layers**
#' The function produces six layers:
#'
#' - `FOREST_poly`   : Forest boundaries with aggregated surface.
#'   **`tol` is applied** to remove small gaps.
#' - `FOREST_line`   : Forest boundaries as linestrings.
#' - `FOREST_point`  : Centroids of forest polygons.
#'
#' - `OWNER_poly`    : Boundaries aggregated per owner.
#'   **`tol` is *not* applied** to avoid merging polygons across owners.
#' - `OWNER_line`    : Owner boundaries as linestrings.
#' - `OWNER_point`   : Centroids of owner polygons.
#'
#' @return A named list
#'
#' @export

seq_boundaries <- function(
    dirname = ".",
    tol = 10,
    verbose = TRUE,
    overwrite = FALSE){

  # tiny helper ----
  seq_write2 <- function(x, key) {
    seq_write(x, key, dirname = dirname, verbose = verbose, overwrite = overwrite)
  }

  # Resolve field and layer ----
  identifiant <- seq_field("identifiant")$name
  owner <- seq_field("proprietaire")$name
  surf_cad <- seq_field("surf_cad")$name

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)

  # Forest boudaries ----
  forest <- aggregate(
    parca[surf_cad],
    by = list(parca[[identifiant]]) |> setNames(identifiant),
    FUN = sum,
    do_union = TRUE
  )

  forest$geometry <- sf::st_sfc(
    lapply(forest$geometry, dissolve, tol = tol),
    crs = sf::st_crs(forest)
  )

  forest_line <- poly_to_line(forest) |> suppressWarnings()

  forest_point <- sf::st_centroid(forest) |> suppressWarnings()

  f_poly <- seq_write2(forest, "v.seq.forest.poly")
  f_line <- seq_write2(forest, "v.seq.forest.line")
  f_point <- seq_write2(forest, "v.seq.forest.point")

  # Owner boudaries ----
  by_id_owner <- list(parca[[identifiant]], parca[[owner]]) |>
    setNames(c(identifiant, owner))

  owner <- aggregate(
    parca[surf_cad],
    by = by_id_owner,
    FUN = sum,
    do_union = TRUE
  )

  owner_line <- poly_to_line(owner) |> suppressWarnings()

  owner_point <- sf::st_centroid(owner) |> suppressWarnings()

  o_poly <- seq_write2(owner, "v.seq.owner.poly")
  o_line <- seq_write2(owner_line, "v.seq.owner.line")
  o_point <- seq_write2(owner_point, "v.seq.owner.point")

  return(c(f_poly, f_line, f_point, o_poly, o_line , o_point) |> as.list())

}

