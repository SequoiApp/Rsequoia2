test_that("get_path() throw error when no matching key", {
  expect_error(
    get_path("badkey"),
    "This file is part of the package and must not be modified"
    )
})

test_that("get_path() throw error when multiple matching key", {
  expect_error(
    get_path("mnhn"),
    "Multiple match for"
  )
})

test_that("get_path() returns expected path", {

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  m <- create_matrice(seq_cache, "MY_ID", verbose = F, overwrite = T)

  key <- "v.mnhn.znieff1.poly"
  expected <- file.path(seq_cache, "MY_ID_MNHN_ZNIEFF1_poly.geojson")
  names(expected) <- key

  expect_equal(get_path(key, seq_cache), expected)

})

test_that("get_path() partial key matching", {

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  m <- create_matrice(seq_cache, "MY_ID", verbose = F, overwrite = T)

  key <- "v.mnhn.znieff1.poly"
  expected <- file.path(seq_cache, "MY_ID_MNHN_ZNIEFF1_poly.geojson")
  names(expected) <- key

  expect_equal(get_path("znieff1", seq_cache), expected)

})

