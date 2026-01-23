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
      "All cadastral IDUs from"
    )
  })

})

test_that("ua_check_idu returns FALSE when IDU are missing", {

  with_seq_cache({
    ua <- seq_normalize(p, "ua")
    idu_field <- seq_field("idu")$name
    ua[[idu_field]] <- 999

    expect_warning(res <- ua_check_idu(ua, p), "Some cadastral IDUs from `parca` are missing in `ua`")
    expect_false(res)
  })

})

