test_that("ua_to_ua() abort when IDU are invalid", {
  with_seq_cache({

    idu <- seq_field("idu")$name
    p[[idu]] <- "A"

    ua <- parca_to_ua(p)
    ua[[idu]] <- "B"

    expect_error(
      ua_to_ua(ua, p, verbose = FALSE),
      "Please correct IDU inconsistency before going further"
    ) |> suppressWarnings()

  })
})
