square <- list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))

test_that("seq_parcels() works in full case", {
  ua <- sf::st_sf(
    data.frame(
      PARFOR      = c("01.01", "01.01", "01.03", "01.02", "02.01", "02.02"),
      N_PARFOR    = c("01", "01", "01", "01", "02", "02"),
      N_SSPARFOR  = c("01", "01", "03", "02", "01", "02"),
      SURF_COR    = rep(1, 6)
    ),
    geometry = sf::st_sfc(
      replicate(6, sf::st_polygon(square), simplify = FALSE),
      crs = 2154
    )
  )

  d <- tempdir()
  m_path <- create_matrice(d, "MY_TEST", verbose = F)
  ua_path <- seq_write(ua, "v.seq.ua.poly", dirname = d)
  paths <- seq_parcels(dirname = d, verbose = FALSE)

  on.exit(unlink(c(m_path, ua_path, paths)))

  expect_length(paths, 4)
  expect_all_true(vapply(paths, file.exists, T))

  all_sf <- lapply(paths, read_sf) |> setNames(names(paths))
  invisible(lapply(all_sf, function(x) expect_s3_class(x, "sf")))

  pf_poly <- all_sf$v.seq.pf.poly
  expect_shape(pf_poly, dim = c(2, 3))
  expect_equal(pf_poly$SURF_COR, c(4, 2))

  expect_equal(dim(pf_poly), c(2, 3))
  expect_named(pf_poly, c("N_PARFOR", "SURF_COR", "geometry"))

  pf_line <- all_sf$v.seq.pf.line
  expect_shape(pf_line, ncol = 2)

  sspf_poly <- all_sf$v.seq.sspf.poly
  expect_shape(sspf_poly, dim = c(5, 4))
  expect_equal(sspf_poly$SURF_COR, c(2, 1, 1, 1, 1))

  sspf_line <- all_sf$v.seq.pf.line
  expect_shape(pf_line, ncol = 2)
})

test_that("seq_parcels() does not overwrite existing layers when overwrite=FALSE", {

  ua <- sf::st_sf(
    data.frame(
      PARFOR = "01.01",
      N_PARFOR = "01",
      N_SSPARFOR = "01",
      SURF_COR = 1
    ),
    geometry = sf::st_sfc(sf::st_polygon(square), crs = 2154)
  )

  d <- tempdir()
  m_path <- create_matrice(d, "MY_TEST", verbose = F)
  ua_path <- seq_write(ua, "v.seq.ua.poly", dirname = d)

  # first call OK
  paths <- seq_parcels(dirname = d, overwrite = FALSE)

  # second call should **fail**
  warn <- capture_warnings(seq_parcels(dirname = d, overwrite = FALSE))
  expect_length(warn, 4)
  expect_all_true(grepl("already exists", warn))

  on.exit(unlink(c(m_path, ua_path, paths)))
})

test_that("seq_parcels() works with a single parcel", {

  ua <- sf::st_sf(
    data.frame(
      PARFOR      = "01.01",
      N_PARFOR    = "01",
      N_SSPARFOR  = "01",
      SURF_COR    = 10
    ),
    geometry = sf::st_sfc(sf::st_polygon(square), crs = 2154)
  )

  d <- tempdir()
  m_path <- create_matrice(d, "MY_TEST4", verbose = FALSE)
  ua_path <- seq_write(ua, "v.seq.ua.poly", dirname = d)

  paths <- seq_parcels(dirname = d, verbose = FALSE)

  all_sf <- lapply(paths, read_sf)
  pf_poly <- all_sf$v.seq.pf.poly
  sspf_poly <- all_sf$v.seq.sspf.poly

  expect_equal(nrow(pf_poly), 1)
  expect_equal(nrow(sspf_poly), 1)
  expect_equal(pf_poly$SURF_COR, 10)
  expect_equal(sspf_poly$SURF_COR, 10)

  on.exit(unlink(c(ua_path, m_path, paths)))
})

test_that("seq_parcels() correctly splits SSPF inside a PF", {

  ua <- sf::st_sf(
    data.frame(
      PARFOR      = c("01.01", "01.01", "01.01"),
      N_PARFOR    = c("01", "01", "01"),
      N_SSPARFOR  = c("01", "02", "03"),
      SURF_COR    = c(1, 2, 3)
    ),
    geometry = sf::st_sfc(
      replicate(3, sf::st_polygon(square), simplify = FALSE),
      crs = 2154
    )
  )

  d <- tempdir()
  m_path <- create_matrice(d, "MY_TEST5", verbose = FALSE)
  ua_path <- seq_write(ua, "v.seq.ua.poly", dirname = d)

  paths <- seq_parcels(dirname = d, verbose = FALSE)
  all_sf <- lapply(paths, read_sf)

  pf_poly <- all_sf$v.seq.pf.poly
  sspf_poly <- all_sf$v.seq.sspf.poly

  # PF is one row, SSPF is 3 rows
  expect_equal(nrow(pf_poly), 1)
  expect_equal(nrow(sspf_poly), 3)

  # Aggregations correct
  expect_equal(pf_poly$SURF_COR, 6)
  expect_equal(sspf_poly$SURF_COR, c(1, 2, 3))

  on.exit(unlink(c(ua_path, m_path, paths)))
})
