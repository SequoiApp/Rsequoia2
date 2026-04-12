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
#' *buffer -> union -> unbuffer*, applied with the distance given by `tol`.
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
  seq_write2 <- function(x, key, id) {
    seq_write(x, key, dirname = dirname, id = id, verbose = verbose, overwrite = overwrite)
  }

  # Resolve field and layer ----
  identifier <- seq_field("identifier")$name
  owner <- seq_field("owner")$name
  cad_area <- seq_field("cad_area")$name

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  id <- unique(parca[[identifier]])

  # Forest polygon ----
  forest <- aggregate(
    parca[cad_area],
    by = list(parca[[identifier]]) |> setNames(identifier),
    FUN = sum,
    do_union = FALSE
  )

  forest_attr <- sf::st_drop_geometry(forest)

  forest_geom <- lapply(seq_len(nrow(forest)), function(i) {

    g <- sf::st_geometry(forest)[[i]]

    tmp <- seq_dissolve(
      sf::st_sf(geometry = sf::st_sfc(g, crs = sf::st_crs(forest))),
      buffer = tol
    )

    sf::st_geometry(tmp)[[1]]

  })

  forest_polygon <- sf::st_sf(
    forest_attr,
    geometry = sf::st_sfc(forest_geom, crs = sf::st_crs(forest))
  )

  # Forest line ----
  forest_line <- poly_to_line(forest_polygon) |> suppressWarnings()
  forest_line[[identifier]] <- id

  # Forest point ----
  forest_point <- sf::st_centroid(forest_polygon) |> suppressWarnings()

  f_poly <- seq_write2(forest_polygon, "v.seq.forest.poly", id)
  f_line <- seq_write2(forest_line, "v.seq.forest.line", id)
  f_point <- seq_write2(forest_point, "v.seq.forest.point", id)

  # Owner boudaries ----
  by_id_owner <- list(parca[[identifier]], parca[[owner]]) |>
    setNames(c(identifier, owner))

  owner <- aggregate(
    parca[cad_area],
    by = by_id_owner,
    FUN = sum,
    do_union = TRUE
  )

  owner_line <- poly_to_line(owner) |> suppressWarnings()
  owner_line[[identifier]] <- id

  owner_point <- sf::st_centroid(owner) |> suppressWarnings()

  o_poly <- seq_write2(owner, "v.seq.owner.poly", id)
  o_line <- seq_write2(owner_line, "v.seq.owner.line", id)
  o_point <- seq_write2(owner_point, "v.seq.owner.point", id)

  return(invisible(c(f_poly, f_line, f_point, o_poly, o_line , o_point) |> as.list()))

}
