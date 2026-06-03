test_that("ua_repair_idu() updates inconsistent IDU with warning", {
  with_seq_cache({

    idu <- seq_field("idu")$name
    ua <- parca_to_ua(p)

    # Force a wrong IDU
    ua[[idu]][1:3] <- "WRONG_IDU"
    res <- ua_repair_idu(ua, p, verbose = FALSE)

    expect_identical(res[[idu]], p[[idu]])

  })
})

test_that("ua_repair_idu() keeps ua unchanged when IDU is correct", {
  with_seq_cache({

    idu <- seq_field("idu")$name
    ua <- parca_to_ua(p)

    res <- ua_repair_idu(ua, p, verbose = FALSE)

    expect_identical(res[[idu]], ua[[idu]])

  })
})

test_that("ua_repair_idu() prints message when verbose = TRUE and invalid IDU", {
  with_seq_cache({

    idu <- seq_field("idu")$name
    ua <- parca_to_ua(p)
    ua[[idu]][1:3] <- "WRONG_IDU"

    expect_message(
      ua_repair_idu(ua, p, verbose = TRUE),
      "incorrect IDU and were corrected"
    ) |> suppressMessages()

  })
})

test_that("ua_repair_idu() prints message when verbose = TRUE and IDU are all valid", {
  with_seq_cache({

    ua <- parca_to_ua(p)

    expect_message(
      ua_repair_idu(ua, p, verbose = TRUE),
      "UA IDU values are correct"
    )

  })
})
