test_that("seq_read() reads vector layers correctly", {

  d <- tempdir()
  m_path <- create_matrice(d, "MY_TEST", verbose = F, overwrite = T)

  v <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  v_path <- get_path("parca", dirname = d)

  sf::write_sf(v, v_path, append = FALSE)

  on.exit(unlink(c(v_path, m_path)))

  v_read <- seq_read("parca", d)

  expect_s3_class(v_read, "sf")
  expect_equal(nrow(v_read), nrow(v))
  expect_true("geometry" %in% names(v_read))

  expect_message(
    seq_read("parca", d, verbose = TRUE),
    "Loaded vector layer"
  )
})

test_that("seq_read() reads raster layers correctly", {

  skip_on_os("mac")

  d <- tempdir()
  m_path <- create_matrice(d, "MY_TEST", verbose = F, overwrite = T)

  r <- rast(nrows=5, ncols=5, vals=1:25)
  r_path <- get_path("irc", dirname = d)

  on.exit(unlink(c(r_path, m_path)))

  terra::writeRaster(r, r_path, overwrite = TRUE)

  r_read <- seq_read("irc", d)

  expect_s4_class(r_read, "SpatRaster")
  expect_equal(dim(r), dim(r_read))

  expect_message(
    seq_read("irc", d, verbose = TRUE),
    "Loaded raster layer"
  )

})

test_that("seq_read() returns error file does not exist", {

  d <- tempdir()
  m_path <- create_matrice(d, "MY_TEST", verbose = FALSE, overwrite = TRUE)

  # Path for a layer that does NOT exist
  missing_path <- get_path("parca", dirname = d)

  # Ensure file is absent
  if (file.exists(missing_path)) unlink(missing_path)

  on.exit(unlink(m_path))

  # Expect a warning + NULL result
  expect_error(
    seq_read("parca", d, verbose = TRUE),
    "doesn't exist"
  )

})
