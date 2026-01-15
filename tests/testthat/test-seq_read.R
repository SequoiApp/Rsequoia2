test_that("seq_read() reads vector layers", {
  with_seq_cache({

    parca <- seq_read("parca", seq_cache)
    expect_s3_class(parca, "sf")

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
      seq_read("prsf", seq_cache, verbose = TRUE),
      "doesn't exist"
    )
  })

})

test_that("seq_read() errors when multiple matching files are found", {

  with_seq_cache({

    layer_info <- seq_layer("parca")
    dir.create(file.path(seq_cache, layer_info$path), recursive = TRUE, showWarnings = FALSE)

    f1 <- file.path(seq_cache, layer_info$path, paste0(layer_info$name, ".gpkg"))
    f2 <- file.path(seq_cache, layer_info$path, paste0(layer_info$name, "_copy.gpkg"))

    file.create(f1)
    file.create(f2)

    expect_error(
      seq_read("parca", seq_cache),
      "Multiple layer"
    )
  })
})
