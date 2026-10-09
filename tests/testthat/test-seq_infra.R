test_that("seq_infra() returns expected paths", {
  with_seq_cache({
    local_mocked_bindings(
      get_infra_poly = function(...) Rsequoia2:::seq_poly,
      get_infra_line = function(...) Rsequoia2:::seq_line,
      get_infra_point = function(...) Rsequoia2:::seq_point
    )

    paths <- seq_infra(seq_cache, verbose = FALSE, overwrite = TRUE)

    expect_named(paths, c("v.infra.point", "v.infra.line", "v.infra.poly"))
    expect_length(paths, 3)
    expect_all_true(file.exists(unlist(paths)))
  })
})

test_that("seq_infra() returns correct geometries", {
  with_seq_cache({
    local_mocked_bindings(
      get_infra_poly = function(...) Rsequoia2:::seq_poly,
      get_infra_line = function(...) Rsequoia2:::seq_line,
      get_infra_point = function(...) Rsequoia2:::seq_point
    )

    infra <- seq_infra(seq_cache, verbose = FALSE, overwrite = TRUE) |>
      lapply(read_sf)

    poly <- infra[["v.infra.poly"]]
    expect_all_true(sf::st_geometry_type(poly) %in% c("POLYGON", "MULTIPOLYGON"))
    expect_true(sf::st_crs(poly) == sf::st_crs(2154))

    line <- infra[["v.infra.line"]]
    expect_all_true(sf::st_geometry_type(line) %in% c("LINESTRING", "MULTILINESTRING"))
    expect_true(sf::st_crs(line) == sf::st_crs(2154))

    point <- infra[["v.infra.point"]]
    expect_all_true(sf::st_geometry_type(point) %in% c("POINT", "MULTIPOINT"))
    expect_true(sf::st_crs(point) == sf::st_crs(2154))
  })
})

test_that("seq_infra() layers contain project id", {
  with_seq_cache({
    local_mocked_bindings(
      get_infra_poly = function(...) Rsequoia2:::seq_poly,
      get_infra_line = function(...) Rsequoia2:::seq_line,
      get_infra_point = function(...) Rsequoia2:::seq_point
    )

    infra <- seq_infra(seq_cache, verbose = FALSE, overwrite = TRUE) |>
      lapply(read_sf)

    identifier <- seq_field("identifier")$name
    expect_all_true(vapply(infra, \(x) identifier %in% names(x), logical(1)))
    expect_all_true(vapply(infra, \(x) all(x[[identifier]] == "ECKMUHL"), logical(1)))
  })
})

test_that("seq_infra() writes empty layers", {
  with_seq_cache({
    local_mocked_bindings(
      get_infra_poly = function(...) Rsequoia2:::seq_empty,
      get_infra_line = function(...) Rsequoia2:::seq_empty,
      get_infra_point = function(...) Rsequoia2:::seq_empty
    )

    paths <- seq_infra(seq_cache, verbose = FALSE, overwrite = TRUE)
    infra <- lapply(paths, read_sf)

    expect_named(paths, c("v.infra.point", "v.infra.line", "v.infra.poly"))
    expect_length(paths, 3)
    expect_all_true(file.exists(unlist(paths)))
    expect_all_true(vapply(infra, \(x) nrow(x) == 0, logical(1)))
  })
})

test_that("seq_infra() continues when one layer fails", {
  with_seq_cache({
    local_mocked_bindings(
      get_infra_poly = function(...) Rsequoia2:::seq_poly,
      get_infra_line = function(...) stop("Test error"),
      get_infra_point = function(...) Rsequoia2:::seq_point
    )

    paths <- seq_infra(seq_cache, verbose = FALSE, overwrite = TRUE)

    expect_named(paths, c("v.infra.point", "v.infra.poly"))
    expect_length(paths, 2)
    expect_all_true(file.exists(unlist(paths)))
  })
})
