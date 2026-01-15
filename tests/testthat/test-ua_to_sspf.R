test_that("ua_to_sspf() aggregates surfaces by UG correctly", {
  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    mgmt_code <- seq_field("mgmt_code")$name
    ua[[mgmt_code]] <- "A"
    ua[[mgmt_code]][1:2] <- "B"

    cor_area <- seq_field("cor_area")$name
    ua[[cor_area]] <- 10

    sspf <- ua_to_sspf(ua)

    expect_setequal(sspf[[mgmt_code]], c("A", "B"))
    expect_gt(sspf[[cor_area]][sspf[[mgmt_code]] == "A"], 10)
    expect_gt(sspf[[cor_area]][sspf[[mgmt_code]] == "B"], 10)
  })
})

test_that("ua_to_sspf() ignores NA cor_area values", {
  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    mgmt_code <- seq_field("mgmt_code")$name
    ua[[mgmt_code]] <- "A"

    cor_area <- seq_field("cor_area")$name
    ua[[cor_area]] <- 10
    ua[[cor_area]][1] <- NA

    sspf <- ua_to_sspf(ua)

    expect_equal(sspf[[cor_area]], 10 * sum(!is.na(ua[[cor_area]])))
  })
})

test_that("ua_to_sspf() errors when UG field is all NA", {
  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    mgmt_code <- seq_field("mgmt_code")$name
    ua[[mgmt_code]] <- NA

    expect_error(ua_to_sspf(ua), "Failed to generate SSPF")
  })
})

test_that("ua_to_sspf() errors when UG field is missing entirely", {

  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    mgmt_code <- seq_field("mgmt_code")$name
    ua[[mgmt_code]] <- NULL

    expect_error(ua_to_sspf(ua), "Failed to generate SSPF")
  })
})


test_that("ua_to_sspf() preserves descriptive fields correctly", {
  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    mgmt_code <- seq_field("mgmt_code")$name
    ua[[mgmt_code]] <- "A"

    desc <- seq_desc_fields()[1:2]
    ua[[desc[1]]] <- "DESC1"
    ua[[desc[2]]] <- "DESC2"

    sspf <- ua_to_sspf(ua)

    # All descriptive fields should appear once
    expect_true(all(desc %in% names(sspf)))

    # Values should be preserved (unique)
    expect_equal(sspf[[desc[1]]], "DESC1")
    expect_equal(sspf[[desc[2]]], "DESC2")
  })
})
