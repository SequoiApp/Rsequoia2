with_seq_cache <- function(code, env = parent.frame()) {
  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  assign("seq_cache", seq_cache, envir = env)
  force(code)
}


test_that("seq_read() reads vector layers", {

  with_seq_cache({

    seq_write(Rsequoia2:::seq_poly, "parca", seq_cache)

    expect_s3_class(
      seq_read("parca", seq_cache),
      "sf"
    )

    expect_message(
      seq_read("parca", seq_cache, verbose = TRUE),
      "Loaded vector layer"
    )
  })

})

test_that("seq_read() reads raster layers", {

  skip_on_os("mac")

  with_seq_cache({

    r <- terra::rast(nrows = 5, ncols = 5, vals = 1:25)
    seq_write(r, "irc", seq_cache)

    r_read <- seq_read("irc", seq_cache)

    expect_s4_class(r_read, "SpatRaster")
    expect_identical(dim(r_read), dim(r))

    expect_message(
      seq_read("irc", seq_cache, verbose = TRUE),
      "Loaded raster layer"
    )
  })

})

test_that("seq_read() errors when file does not exist", {

  with_seq_cache({
    expect_error(
      seq_read("parca", seq_cache, verbose = TRUE),
      "doesn't exist"
    )
  })

})
