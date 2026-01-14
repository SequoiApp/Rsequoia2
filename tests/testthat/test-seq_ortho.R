test_that("seq_ortho writes one raster per type", {

  with_seq_cache({
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

    res <- seq_ortho(dirname = seq_cache, type = c("irc", "rgb"), verbose = FALSE)
    expect_length(res, 2)

    expect_equal(called$get_ortho, 2)
    expect_equal(called$seq_write, 2)
  })

})
