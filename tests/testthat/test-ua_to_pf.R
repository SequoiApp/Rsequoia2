
test_that("ua_to_pf() aggregates surfaces by UG correctly", {
  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    pcl_code <- seq_field("pcl_code")$name
    ua[[pcl_code]] <- "A"
    ua[[pcl_code]][1:2] <- "B"

    cor_area <- seq_field("cor_area")$name
    ua[[cor_area]] <- 10

    pf <- ua_to_pf(ua)

    expect_setequal(pf[[pcl_code]], c("A", "B"))
    expect_gt(pf[[cor_area]][pf[[pcl_code]] == "A"], 10)
    expect_gt(pf[[cor_area]][pf[[pcl_code]] == "B"], 10)
  })
})

test_that("ua_to_pf() ignores NA cor_area values", {
  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    pcl_code <- seq_field("pcl_code")$name
    ua[[pcl_code]] <- "A"

    cor_area <- seq_field("cor_area")$name
    ua[[cor_area]] <- 10
    ua[[cor_area]][1] <- NA

    pf <- ua_to_pf(ua)

    expect_equal(pf[[cor_area]], 10 * sum(!is.na(ua[[cor_area]])))
  })
})

test_that("ua_to_pf() errors when PF field is all NA", {
  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    pcl_code <- seq_field("pcl_code")$name
    ua[[pcl_code]] <- NA

    expect_error(ua_to_pf(ua), "Failed to generate PF")
  })
})

test_that("ua_to_pf() errors when PF field is missing entirely", {

  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    pcl_code <- seq_field("pcl_code")$name
    ua[[pcl_code]] <- NULL

    expect_error(ua_to_pf(ua), "Failed to generate PF")
  })
})
