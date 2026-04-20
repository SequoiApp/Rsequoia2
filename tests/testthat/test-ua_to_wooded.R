test_that("ua_to_wooded() aggregates surfaces by wooded status", {
  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    is_wooded <- seq_field("is_wooded")$name
    ua[[is_wooded]] <- TRUE
    ua[[is_wooded]][1:2] <- FALSE

    cor_area <- seq_field("cor_area")$name
    ua[[cor_area]] <- 10

    wooded <- ua_to_wooded(ua)

    expect_setequal(wooded[[is_wooded]], c(TRUE, FALSE))
    expect_equal(wooded[[cor_area]][wooded[[is_wooded]]], 70)
    expect_equal(wooded[[cor_area]][!wooded[[is_wooded]]], 20)
  })
})

test_that("ua_to_wooded() ignores NA cor_area values", {
  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    is_wooded <- seq_field("is_wooded")$name
    ua[[is_wooded]] <- TRUE

    cor_area <- seq_field("cor_area")$name
    ua[[cor_area]] <- 10
    ua[[cor_area]][1] <- NA

    wooded <- ua_to_wooded(ua)

    expect_equal(wooded[[cor_area]], 10 * sum(!is.na(ua[[cor_area]])))
  })
})

test_that("ua_to_wooded() errors when is_wooded field is all NA", {
  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    is_wooded <- seq_field("is_wooded")$name
    ua[[is_wooded]] <- NA

    expect_error(ua_to_wooded(ua), "Failed to generate")
  })
})

test_that("ua_to_wooded() errors when is_wooded field is missing entirely", {

  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    is_wooded <- seq_field("is_wooded")$name
    ua[[is_wooded]] <- NULL

    expect_error(ua_to_wooded(ua), "Failed to generate")
  })
})
