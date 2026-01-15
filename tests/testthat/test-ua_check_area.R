test_that("ua_check_area() updates inconsistent surfaces with warning", {
  with_seq_cache({

    cad_area <- seq_field("cad_area")$name
    p[[cad_area]] <- 10

    ua <- parca_to_ua(p)
    ua[[cad_area]] <- 11

    expect_warning(res <- ua_check_area(ua, p), "area values corrected")
    expect_all_equal(res[[cad_area]], 10)

  })
})

test_that("ua_check_area() keeps ua unchanged when no difference", {
  with_seq_cache({

    cad_area <- seq_field("cad_area")$name

    ua <- seq_normalize(p, "ua")
    expect_no_warning(res <- ua_check_area(ua, p, verbose = FALSE))
    expect_identical(res[[cad_area]], ua[[cad_area]])

  })
})

test_that("ua_check_area() prints success message when verbose = TRUE", {
  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    expect_message(
      ua_check_area(ua, p, verbose = TRUE),
      "No cadastral area discrepancies detected"
    )
  })
})

test_that("ua_check_area() ignores NA values from parca", {
  with_seq_cache({

    cad_area <- seq_field("cad_area")$name
    p[[cad_area]] <- 10
    p[[cad_area]][1] <- NA

    ua <- parca_to_ua(p)
    ua[[cad_area]] <- 10

    res <- ua_check_area(ua, p, verbose = FALSE)
    expect_all_equal(res[[cad_area]], 10)
  })
})

