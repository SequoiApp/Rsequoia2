test_that("get_path() throw error when no matching key", {

  expect_error(
    get_path("badkey"),
    "This file is part of the package and must not be modified"
    )

})

test_that("get_path() throw error when multiple matching key", {

  expect_error(
    get_path("inpn"),
    "Multiple match for"
  )

})

test_that("get_path() returns expected path", {

  tmp <- tempdir()
  m <- create_matrice(tmp, "MY_ID", verbose = F, overwrite = T)
  on.exit(unlink(m))

  key <- "v.inpn.znieff1.poly"
  expected <- file.path(tmp, "MY_ID_INPN_ZNIEFF1_poly.geojson")
  names(expected) <- key

  expect_equal(get_path(key, tmp), expected)

})

test_that("get_path() partial key matching", {

  tmp <- tempdir()
  m <- create_matrice(tmp, "MY_ID", verbose = F, overwrite = T)
  on.exit(unlink(m))

  key <- "v.inpn.znieff1.poly"
  expected <- file.path(tmp, "MY_ID_INPN_ZNIEFF1_poly.geojson")
  names(expected) <- key

  expect_equal(get_path("znieff1", tmp), expected)

})

