test_that("seq_write() writes vector layers correctly", {

  d <- tempdir()
  m_path <- create_matrice(d, "MY_TEST", verbose = F, overwrite = T)

  v <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  v_path <- get_path("parca", d)

  on.exit(unlink(c(v_path, m_path)))

  seq_write(v, "parca", d)

  expect_true(file.exists(v_path))

})

test_that("seq_write() writes raster layers correctly", {

  d <- tempdir()
  m_path <- create_matrice(d, "MY_TEST", verbose = F, overwrite = T)

  r <- rast(nrows=5, ncols=5, vals=1:25)
  r_path <- get_path("irc", d)

  on.exit(unlink(c(r_path, m_path)))

  seq_write(r, "irc", d)

  expect_true(file.exists(r_path))

})

test_that("seq_write() overwrite vector properly correctly", {

  d <- tempdir()
  m_path <- create_matrice(d, "MY_TEST", verbose = F, overwrite = T)

  v <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  v_path <- get_path("parca", d)
  seq_write(v, "parca", d)

  on.exit(unlink(c(v_path, m_path)))

  expect_silent(seq_write(v, "parca", d, overwrite = TRUE))
  expect_warning(seq_write(v, "parca", d, overwrite = FALSE))

})

test_that("seq_write() overwrite raster properly correctly", {
  d <- tempdir()

  m <- create_matrice(d, "MY_TEST", verbose = F, overwrite = T)

  r <- rast(nrows=5, ncols=5, vals=1:25)
  r_path <- get_path("irc", d)
  seq_write(r, "irc", d)

  on.exit(unlink(c(r_path, m)))

  expect_silent(seq_write(r, "irc", d, overwrite = TRUE))
  expect_warning(seq_write(r, "irc", d, overwrite = FALSE))

})
