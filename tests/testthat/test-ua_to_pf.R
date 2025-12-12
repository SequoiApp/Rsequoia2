pf <- seq_field("parcelle")$name
surf <- seq_field("surf_cor")$name

test_that("ua_to_pf() aggregates surfaces correctly", {

  ua <- sf::st_sf(
    "pf" = c("A", "A", "B"),
    "surf" = c(10, 20, 5),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2))
    )
  )
  names(ua) <- c(pf, surf, "geometry")

  pf_poly <- ua_to_pf(ua)

  expect_shape(ua, dim = c(3, 3))
  expect_setequal(pf_poly[[pf]], c("A", "B"))
  expect_equal(pf_poly[[surf]][pf_poly[[pf]] == "A"], 30)
  expect_equal(pf_poly[[surf]][pf_poly[[pf]] == "B"], 5)
})

test_that("ua_to_pf() ignores NA surf_cor values", {

  ua <- sf::st_sf(
    "pf" = c("X", "X"),
    "surf" = c(10, NA),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1))
    )
  )
  names(ua) <- c(pf, surf, "geometry")

  pf_poly <- ua_to_pf(ua)

  expect_equal(pf_poly[[surf]], 10)
})

test_that("ua_to_pf() errors when parcel field exists but is all NA", {

  # bad alias
  ua1 <- sf::st_sf(
    pf = c(NA, NA),
    surf = c(1, 2),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1))
    )
  )
  names(ua1) <- c(pf, surf, "geometry")
  expect_error(ua_to_pf(ua1), "Field used to group by parcels is missing")

  # No prf col at all
  ua2 <- sf::st_sf(
    pf = c(NA, NA),
    surf = c(1, 2),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1))
    )
  )
  names(ua2) <- c("bad_name", surf, "geometry")
  expect_error(ua_to_pf(ua2), "Field used to group by parcels is missing")


})
