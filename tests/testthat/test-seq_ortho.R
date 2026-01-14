test_that("seq_ortho writes one raster per type", {

  # seq dir creation ----
  seq_dir <- file.path(tempdir(), "seq")
  dir.create(seq_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(seq_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # get MATRICE & PARCA----
  matrice_path <- system.file("extdata", package = "Rsequoia2") |>
    list.files(full.names = TRUE, pattern = "matrice", ignore.case = TRUE) |>
    file.copy(seq_dir)

  parca_path <- system.file("extdata", package = "Rsequoia2") |>
    list.files(full.names = TRUE, pattern = "parca", ignore.case = TRUE) |>
    sf::read_sf() |>
    seq_write("v.seq.parca.poly", seq_dir)

  # fake raster ----
  fake_raster <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 10, ymin = 0, ymax = 10,
    crs = "EPSG:2154"
  )

  # ---- capture calls ----
  called <- list(get_ortho = 0, seq_write = 0)

  testthat::local_mocked_bindings(
    get_ortho = function(...) {
      called$get_ortho <<- called$get_ortho + 1
      fake_raster
    },
    seq_write = function(x, key, ...) {
      called$seq_write <<- called$seq_write + 1
    }
  )

  res <- seq_ortho(dirname = seq_dir, type = c("irc", "rgb"), verbose = FALSE)

  expect_type(res, "list")
  expect_length(res, 2)

  expect_equal(called$get_ortho, 2)
  expect_equal(called$seq_write, 2)
})
