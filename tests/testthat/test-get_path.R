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

  d <- tempdir()
  m <- create_matrice(d, "MY_ID", verbose = F, overwrite = T)

  on.exit(unlink(m))

  key <- "v.mnhn.znieff1.poly"
  expected <- file.path(d, "MY_ID_MNHN_ZNIEFF1_poly.geojson")
  names(expected) <- key

  expect_equal(get_path(key, d), expected)

})

test_that("get_path() partial key matching", {

  d <- tempdir()
  m <- create_matrice(d, "MY_ID", verbose = F, overwrite = T)
  on.exit(unlink(m))

  key <- "v.mnhn.znieff1.poly"
  expected <- file.path(d, "MY_ID_MNHN_ZNIEFF1_poly.geojson")
  names(expected) <- key

  expect_equal(get_path("znieff1", d), expected)

})

