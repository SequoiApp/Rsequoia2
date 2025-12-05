ug   <- seq_field("ug")$name
surf <- seq_field("surf_cor")$name

test_that("ua_to_sspf() aggregates surfaces by UG correctly", {

  # Minimal UA with 2 SSPF groups: A and B
  ua <- sf::st_sf(
    "ug"   = c("A", "A", "B"),
    "surf" = c(10, 20, 5),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2))
    )
  )

  # rename to match real canonical names
  names(ua) <- c(ug, surf, "geometry")

  sspf <- ua_to_sspf(ua)

  expect_setequal(sspf[[ug]], c("A", "B"))
  expect_equal(sspf[[surf]][sspf[[ug]] == "A"], 30)
  expect_equal(sspf[[surf]][sspf[[ug]] == "B"], 5)
})

test_that("ua_to_sspf() ignores NA surf_cor values", {

  ua <- sf::st_sf(
    "ug"   = c("A", "A"),
    "surf" = c(10, NA),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1))
    )
  )

  names(ua) <- c(ug, surf, "geometry")

  sspf <- ua_to_sspf(ua)

  expect_equal(sspf[[surf]], 10)
})

test_that("ua_to_sspf() errors when UG field is all NA", {

  ua <- sf::st_sf(
    "ug"   = c(NA, NA),
    "surf" = c(1, 2),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1))
    )
  )
  names(ua) <- c(ug, surf, "geometry")

  expect_error(
    ua_to_sspf(ua),
    "Field used to group by UG is missing"
  )
})

test_that("ua_to_sspf() errors when UG field is missing entirely", {

  ua <- sf::st_sf(
    "wrong_name" = c("A", "A"),
    "surf"       = c(1, 2),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1))
    )
  )

  # rename only surf properly, but remove UG entirely
  names(ua) <- c("wrong_name", surf, "geometry")

  expect_error(
    ua_to_sspf(ua),
    "Field used to group by UG is missing"
  )
})

test_that("ua_to_sspf() preserves descriptive fields correctly", {

  # Pick the first 2 desc fields to make test small
  desc <- seq_desc_fields()[1:2]

  ua <- sf::st_sf(
    "ug"   = c("A", "A"),
    "surf" = c(10, 20),
    "desc1" = "desc1",
    "desc2" = "desc2",
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1))
    )
  )
  names(ua) <- c(ug, surf, desc, "geometry")  # Align canonical UG & surf_cor

  sspf <- ua_to_sspf(ua)

  # All descriptive fields should appear once
  expect_true(all(desc %in% names(sspf)))

  # Values should be preserved (unique)
  expect_equal(sspf[[desc[1]]], "desc1")
  expect_equal(sspf[[desc[2]]], "desc2")
})
