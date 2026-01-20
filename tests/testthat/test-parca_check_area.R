test_that("parca_check_area() returns expected object", {
  with_seq_cache({
    check <- parca_check_area(p, verbose = FALSE)

    expect_s3_class(check, "sf")

    expect_true("CHECK_AREA" %in% names(check))
  })
})

test_that("parca_check_area() warns when inconsistencies are detected", {

  with_seq_cache({
    cad_area <- seq_field("cad_area")$name
    p[[cad_area]] <- 1000

    expect_warning(
      parca_check_area(p, verbose = TRUE),
      "Detected.*IDU"
    )
  })
})

test_that("parca_check_area() produces no warning when areas are consistent", {
  with_seq_cache({
    cad_area <- seq_field("cad_area")$name
    p[[cad_area]] <- 150 / 10000

    local_mocked_bindings(
      st_area = function(...) 150,
      .package = "sf"
    )

    expect_no_warning(parca_check_area(p, verbose = FALSE))
  })
})

test_that("parca_check_area() prints details when verbose = TRUE", {
  with_seq_cache({
    cad_area <- seq_field("cad_area")$name
    p[[cad_area]] <- 150 / 10000

    local_mocked_bindings(
      st_area = function(...) 150,
      .package = "sf"
    )

    expect_message(
      parca_check_area(p, verbose = TRUE),
      "No area inconsistencies"
    )

  })
})
