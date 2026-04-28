test_that("ua_check_coverage succeeds when UA and PARCA cover each other", {
  with_seq_cache({
    ua <- parca_to_ua(p)

    expect_no_error(
      res <- ua_check_coverage(ua, p)
    )

    expect_s3_class(res, "sf")
    expect_equal(nrow(res), nrow(ua))
  })
})

test_that("ua_check_coverage abort when there UA coverage inconsistency", {
  with_seq_cache({
    ua <- parca_to_ua(p)
    idu_field <- seq_field("idu")$name

    extra_ua <- ua[1:3, ]
    sf::st_geometry(extra_ua) <- sf::st_geometry(Rsequoia2:::seq_poly)
    extra_ua[[idu_field]] <- "BAD_IDU"

    ua <- rbind(ua, extra_ua)

    expect_error(
      ua_check_coverage(ua, p), "UA features do not intersect PARCA"
    )

  })
})

test_that("ua_check_coverage abort when there PARCA coverage inconsistency", {
  with_seq_cache({
    ua <- parca_to_ua(p)
    idu_field <- seq_field("idu")$name

    extra_parca <- p[1:3, ]
    sf::st_geometry(extra_parca) <- sf::st_geometry(Rsequoia2:::seq_poly)
    extra_parca[[idu_field]] <- "BAD_IDU"

    p <- rbind(p, extra_parca)

    expect_error(
      ua_check_coverage(ua, p), "PARCA features do not intersect UA"
    )

  })
})
