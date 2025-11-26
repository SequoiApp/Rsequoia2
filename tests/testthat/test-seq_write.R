test_that("seq_write() writes vector layers correctly", {
  tmp <- tempdir()

  m <- create_matrice(tmp, "MY_TEST", verbose = F, overwrite = T)

  v <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  seq_write(v, "v.seq.parca.poly", tmp)
  v_path <- file.path(tmp,"MY_TEST_PARCA_poly.geojson")

  on.exit(unlink(c(v_path, m)))
  expect_true(file.exists(v_path))

})

test_that("seq_write() writes raster layers correctly", {
  tmp <- tempdir()

  m <- create_matrice(tmp, "MY_TEST", verbose = F, overwrite = T)

  r <- rast(nrows=5, ncols=5, vals=1:25)
  seq_write(r, "r.ortho.irc", tmp)
  r_path <- file.path(tmp, "MY_TEST_IRC.tif")
  on.exit(unlink(c(r_path, m)))

  expect_true(file.exists(r_path))

})

test_that("seq_write() overwrite vector properly correctly", {
  tmp <- tempdir()

  m <- create_matrice(tmp, "MY_TEST", verbose = F, overwrite = T)

  v <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  v_path <- file.path(tmp, "MY_TEST_PARCA_poly.geojson")
  seq_write(v, "v.seq.parca.poly", tmp)

  on.exit(unlink(c(v_path, m)))

  expect_silent(seq_write(v, "v.seq.parca.poly", tmp, overwrite = TRUE))
  expect_warning(seq_write(v, "v.seq.parca.poly", tmp, overwrite = FALSE))

})

test_that("seq_write() overwrite raster properly correctly", {
  tmp <- tempdir()

  m <- create_matrice(tmp, "MY_TEST", verbose = F, overwrite = T)

  r <- rast(nrows=5, ncols=5, vals=1:25)
  r_path <- file.path(tmp, "MY_TEST_IRC.tif")
  seq_write(r, "r.ortho.irc", tmp)

  on.exit(unlink(c(r_path, m)))

  expect_silent(seq_write(r, "r.ortho.irc", tmp, overwrite = TRUE))
  expect_warning(seq_write(r, "r.ortho.irc", tmp, overwrite = FALSE))

})
