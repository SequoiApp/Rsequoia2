with_seq_cache <- function(code) {
  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)
  force(code)
}

test_that("seq_write() writes vector layers correctly", {
  with_seq_cache({
    path <- seq_write(Rsequoia2:::seq_poly, "parca", dirname = seq_cache)
    expect_true(file.exists(path))
    expect_s3_class(sf::read_sf(path), "sf")
  })
})

test_that("seq_write() writes raster layers correctly", {

  skip_on_os("mac")

  with_seq_cache({
    r <- rast(nrows=5, ncols=5, vals=1:25)
    path <- seq_write(r, "irc", dirname = seq_cache)
    expect_true(file.exists(path))
    expect_s4_class(terra::rast(path), "SpatRaster")
  })

})

test_that("seq_write() overwrite vector properly correctly", {

  with_seq_cache({
    v <- Rsequoia2:::seq_poly
    seq_write(v, "parca", seq_cache)
    expect_silent(seq_write(v, "parca", seq_cache, overwrite = TRUE))
    expect_warning(seq_write(v, "parca", seq_cache, overwrite = FALSE))
  })

})

test_that("seq_write() overwrite raster properly correctly", {

  skip_on_os("mac")

  with_seq_cache({
    r <- rast(nrows=5, ncols=5, vals=1:25)
    seq_write(r, "irc", seq_cache)
    expect_silent(seq_write(r, "irc", seq_cache, overwrite = TRUE))
    expect_warning(seq_write(r, "irc", seq_cache, overwrite = FALSE))
  })

})

test_that("seq_write() creates target directory when needed", {

  with_seq_cache({
    rel_path <- get_path("parca")
    abs_path <- file.path(seq_cache, rel_path)

    # directory must not exist beforehand
    expect_false(dir.exists(dirname(abs_path)))

    path <- seq_write(Rsequoia2:::seq_poly, "parca", seq_cache)

    expect_true(file.exists(path))
    expect_true(dir.exists(dirname(path)))
    expect_s3_class(sf::read_sf(path), "sf")
  })

})
