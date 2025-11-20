
test_that("parca_check_area() returns expected object", {

  parca <- fake_parca(contenance = 0)
  check <- parca_check_area(parca, verbose = FALSE)

  expect_s3_class(check, "sf")
  expect_shape(check, dim = c(1, 20))
  expect_all_true(
    c("AREA_SIG", "AREA_ATOL", "AREA_RTOL", "AREA_CHECK") %in% names(check)
  )
})

test_that("parca_check_area() warns when inconsistencies are detected", {

  parca <- fake_parca(idu = c("bad_idu", "bad_idu2"), contenance = 1000)
  expect_warning(
    parca_check_area(parca, verbose = TRUE),
    "Detected .* inconsistent IDU.*bad_idu.*bad_idu2"
  )
})

test_that("parca_check_area() produces no warning when areas are consistent", {

  parca <- fake_parca(contenance = 0)
  expect_no_warning(parca_check_area(parca, verbose = FALSE))

})

test_that("parca_check_area() prints details when verbose = TRUE", {

  parca <- fake_parca(contenance = 0)
  expect_message(
    parca_check_area(parca, verbose = TRUE),
    "No inconsistencies detected"
  )

})
