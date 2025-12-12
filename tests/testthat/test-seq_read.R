test_that("seq_read() reads vector layers correctly", {

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  m_path <- create_matrice(seq_cache, "MY_TEST", verbose = F, overwrite = T)

  v <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  v_path <- get_path("parca", dirname = seq_cache)

  sf::write_sf(v, v_path, append = FALSE)

  v_read <- seq_read("parca", seq_cache)

  expect_s3_class(v_read, "sf")
  expect_equal(nrow(v_read), nrow(v))
  expect_true("geometry" %in% names(v_read))

  expect_message(
    seq_read("parca", seq_cache, verbose = TRUE),
    "Loaded vector layer"
  )
})

test_that("seq_read() reads raster layers correctly", {

  skip_on_os("mac")

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  m_path <- create_matrice(seq_cache, "MY_TEST", verbose = F, overwrite = T)

  r <- rast(nrows=5, ncols=5, vals=1:25)
  r_path <- get_path("irc", dirname = seq_cache)

  terra::writeRaster(r, r_path, overwrite = TRUE)

  r_read <- seq_read("irc", seq_cache)

  expect_s4_class(r_read, "SpatRaster")
  expect_equal(dim(r), dim(r_read))

  expect_message(
    seq_read("irc", seq_cache, verbose = TRUE),
    "Loaded raster layer"
  )

})

test_that("seq_read() returns error file does not exist", {

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  m_path <- create_matrice(seq_cache, "MY_TEST", verbose = FALSE, overwrite = TRUE)

  # Path for a layer that does NOT exist
  missing_path <- get_path("parca", dirname = seq_cache)

  # Expect a warning + NULL result
  expect_error(
    seq_read("parca", seq_cache, verbose = TRUE),
    "doesn't exist"
  )

})
