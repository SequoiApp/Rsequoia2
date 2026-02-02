
test_that("seq_write() writes vector layers correctly", {
  with_seq_cache({
    # prsf is use because parca us already in seq_cache
    path <- seq_write(Rsequoia2:::seq_poly, "prsf", dirname = seq_cache)
    expect_true(file.exists(path))
    expect_s3_class(sf::read_sf(path), "sf")
  })
})

test_that("seq_write() writes raster layers correctly", {

  skip_on_os(c("mac", "linux"))

  with_seq_cache({
    r <- rast(nrows=5, ncols=5, vals=1:25)

    path <- seq_write(r, "irc", dirname = seq_cache)
    expect_true(file.exists(path))
    expect_s4_class(terra::rast(path), "SpatRaster")
  })

})

test_that("seq_write() writes xlsx layers correctly", {

  with_seq_cache({
    x <- data.frame(col1 = 1:10, col2 = 1:10)
    path <- seq_write(x, "matrice", dirname = seq_cache, overwrite = TRUE)
    expect_true(file.exists(path))
    expect_s3_class(openxlsx2::read_xlsx(path), "data.frame")
  })

})

test_that("seq_write() overwrite vector properly correctly", {

  with_seq_cache({
    v <- Rsequoia2:::seq_poly
    seq_write(v, "prsf", seq_cache)
    expect_silent(seq_write(v, "prsf", seq_cache, overwrite = TRUE))
    expect_warning(seq_write(v, "prsf", seq_cache, overwrite = FALSE))
  })

})

test_that("seq_write() overwrite raster properly correctly", {

  skip_on_os(c("mac", "linux"))

  with_seq_cache({
    r <- rast(nrows=5, ncols=5, vals=1:25)

    seq_write(r, "irc", seq_cache)
    expect_silent(seq_write(r, "irc", seq_cache, overwrite = TRUE))
    expect_warning(seq_write(r, "irc", seq_cache, overwrite = FALSE))
  })

})

test_that("seq_write() creates target directory when needed", {
  with_seq_cache({
    layer_info <- seq_layer("prsf")

    rel_path <- layer_info$full_path
    abs_path <- file.path(seq_cache, rel_path)

    # directory must not exist beforehand
    expect_false(dir.exists(dirname(abs_path)))

    path <- seq_write(Rsequoia2:::seq_poly, "prsf", seq_cache)

    expect_true(file.exists(path))
    expect_true(dir.exists(dirname(path)))
    expect_s3_class(sf::read_sf(path), "sf")
  })
})

test_that("seq_write() aborts if vector key is used with non-sf object", {
  with_seq_cache({
    expect_error(
      seq_write(data.frame(a = 1), "prsf", dirname = seq_cache),
      "not an .*sf"
    )
  })
})

test_that("seq_write() aborts if raster key is used with non-raster object", {
  with_seq_cache({
    expect_error(
      seq_write(Rsequoia2:::seq_poly, "irc", dirname = seq_cache),
      "not a .*SpatRaster"
    )
  })
})

test_that("seq_write() prefixes filename with id when provided", {
  with_seq_cache({
    path <- seq_write(
      Rsequoia2:::seq_poly, "prsf", dirname = seq_cache, id = "TEST"
    )

    expect_true(grepl("TEST_", basename(path)))
    expect_true(file.exists(path))
  })
})

test_that("seq_write() prefixes filename with id from x when id = NULL", {
  with_seq_cache({
    x <- Rsequoia2:::seq_poly
    x[[seq_field("identifier")$name]] <- "TEST"

    path <- seq_write(x, "prsf", dirname = seq_cache, id = NULL)

    expect_true(grepl("TEST_", basename(path)))
    expect_true(file.exists(path))
  })
})

test_that("seq_write() prefixes filename with id from matrice when id = NULL", {
  with_seq_cache({
    path <- seq_write(Rsequoia2:::seq_poly, "prsf", dirname = seq_cache, id = NULL)

    expect_true(grepl("ECKMUHL_", basename(path)))
    expect_true(file.exists(path))
  })
})

