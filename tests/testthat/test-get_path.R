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

  path <- get_path("v.inpn.znieff1.poly", tmp)
  expect_equal(path, file.path(tmp, "MY_ID_INPN_ZNIEFF1_poly.geojson"))

})

test_that("get_path() partial key matching", {

  tmp <- tempdir()
  m <- create_matrice(tmp, "MY_ID", verbose = F, overwrite = T)
  expected_path <- file.path(tmp, "MY_ID_INPN_ZNIEFF1_poly.geojson")

  on.exit(unlink(c(m, expected_path)))

  expect_equal(get_path("znieff1", tmp), expected_path)

})

