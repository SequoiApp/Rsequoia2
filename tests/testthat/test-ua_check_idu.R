test_that("ua_check_idu returns TRUE when all IDU match", {

  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    expect_true(ua_check_idu(ua, p, verbose = FALSE))
  })

})

test_that("ua_check_idu print message when verbose = TRUE", {

  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    expect_message(
      ua_check_idu(ua, p, verbose = TRUE),
      "UA and PARCA IDU values are consistent"
    )
  })

})

test_that("ua_check_idu returns FALSE when IDU are missing", {

  with_seq_cache({
    ua <- seq_normalize(p, "ua")
    idu_field <- seq_field("idu")$name
    p[[idu_field]] <- "A"
    ua[[idu_field]] <- "B"

    expect_message(res <- ua_check_idu(ua, p), "PARCA IDU missing in UA") |>
      suppressMessages()
    expect_message(res <- ua_check_idu(ua, p), "UA IDU unknown in PARCA")|>
      suppressMessages()
    expect_false(res)
  })

})

