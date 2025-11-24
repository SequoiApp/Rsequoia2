test_that("ua_check_idu_with_parca returns TRUE when all IDU match", {
  skip_if_not_installed("sf")
  library(sf)

  idu <- seq_field("idu")$name

  ua <- st_sf(
    setNames(list(1:3), idu),
    geometry = st_sfc(
      st_point(c(0,0)), st_point(c(1,1)), st_point(c(2,2))
    )
  )

  parca <- st_sf(
    setNames(list(1:3), idu),
    geometry = st_sfc(
      st_point(c(0,0)), st_point(c(1,1)), st_point(c(2,2))
    )
  )

  expect_true(ua_check_idu(ua, parca, verbose = FALSE))
})

test_that("ua_check_idu_with_parca returns FALSE when IDU are missing", {
  skip_if_not_installed("sf")
  library(sf)

  idu <- seq_field("idu")$name

  ua <- st_sf(
    setNames(list(1:3), idu),
    geometry = st_sfc(
      st_point(c(0,0)), st_point(c(1,1)), st_point(c(2,2))
    )
  )

  parca <- st_sf(
    setNames(list(c(1,2,3,4)), idu),
    geometry = st_sfc(
      st_point(c(0,0)), st_point(c(1,1)),
      st_point(c(2,2)), st_point(c(3,3))
    )
  )

  expect_false(ua_check_idu(ua, parca, verbose = FALSE))
})

test_that("ua_check_idu emits CLI warning when verbose = TRUE", {
  skip_if_not_installed("sf")
  library(sf)

  idu <- seq_field("idu")$name

  ua <- st_sf(
    setNames(list(1:2), idu),
    geometry = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
  )

  parca <- st_sf(
    setNames(list(c(1,2,3)), idu),
    geometry = st_sfc(
      st_point(c(0,0)), st_point(c(1,1)), st_point(c(2,2))
    )
  )

  msg <- testthat::capture_messages(
    res <- ua_check_idu(ua, parca, verbose = TRUE)
  )

  expect_false(res)
  expect_true(any(grepl("missing", msg, ignore.case = TRUE)))
})

test_that("ua_check_idu_with_parca detects multiple missing IDU", {
  skip_if_not_installed("sf")
  library(sf)

  idu <- seq_field("idu")$name

  ua <- st_sf(
    setNames(list(1:2), idu),
    geometry = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
  )

  parca <- st_sf(
    setNames(list(c(1,2,3,4,5)), idu),
    geometry = st_sfc(
      st_point(c(0,0)), st_point(c(1,1)),
      st_point(c(2,2)), st_point(c(3,3)), st_point(c(4,4))
    )
  )

  out <- ua_check_idu(ua, parca, verbose = FALSE)

  expect_false(out)
})
