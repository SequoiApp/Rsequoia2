fake_gpkg <- function() {

  guides <- Rsequoia2:::seq_poly |>
    transform(
      id = 1:3,
      nom = c("Guide A", "Guide B", "Guide C")
    )

  guides_urls <- data.frame(
    guide_id = 1:3,
    url = sprintf("https://example.org/%s.pdf", LETTERS[1:3])
  )

  stations <- data.frame(
    guide_id = rep(1:3, 3),
    nom = rep(sprintf("Station %s", LETTERS[1:3]), 3)
  )

  path <- tempfile(fileext = ".gpkg")

  sf::write_sf(guides, path, layer = "guides")
  sf::write_sf(guides_urls, path, layer = "guides_urls")
  sf::write_sf(stations, path, layer = "stations")

  path
}

test_that("get_guides accept only sf object", {

  expect_error(get_guides(1), "must be of class")
  expect_error(get_guides(NA), "must be of class")
  expect_error(get_guides(NULL), "must be of class")
  expect_error(get_guides("char"), "must be of class")

})

test_that("get_guides accepts polygons, points and lines", {

  path <- fake_gpkg()

  local_mocked_bindings(
    download_guides = function(...) path
  )

  inputs <- list(
    Rsequoia2:::seq_poly,
    Rsequoia2:::seq_point,
    Rsequoia2:::seq_line
  )

  res <- lapply(inputs, get_guides, verbose = FALSE)

  lapply(res, \(x){
    expect_type(x, "list")
    expect_s3_class(x$guides, "sf")
    expect_named(x, c("guides", "guides_urls", "stations"))
  })

})


test_that("get_guides filters related tables", {

  path <- fake_gpkg()

  local_mocked_bindings(
    download_guides = function(...) path
  )

  result <- get_guides(Rsequoia2:::seq_poly[1, ], buffer = 0, verbose = FALSE)

  expect_named(result, c("guides", "guides_urls", "stations"))
  expect_all_true(result$guides$id == 1L)
  expect_all_true(result$guides_urls$guide_id == 1L)
  expect_all_true(result$stations$guide_id == 1L)
})


test_that("get_guides returns NULL when no guide is found", {

  path <- fake_gpkg()

  local_mocked_bindings(
    download_guides = function(...) path
  )

  result <- get_guides(Rsequoia2:::seq_point, buffer = 0, verbose = FALSE)

  expect_null(result)
})
