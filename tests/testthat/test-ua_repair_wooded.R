make_ua_occupation <- function(dgd, wooded, area = 1, mgmt_code = seq_along(dgd)) {
  data.frame(setNames(
    list(dgd, wooded, area, as.character(mgmt_code)),
    c(
      seq_field("is_dgd")$name,
      seq_field("is_wooded")$name,
      seq_field("cor_area")$name,
      seq_field("mgmt_code")$name
    )
  ))
}

test_that("ua_repair_wooded() sets wooded to FALSE when DGD is FALSE", {
  wooded <- seq_field("is_wooded")$name

  ua <- make_ua_occupation(
    dgd = FALSE,
    wooded = TRUE,
    mgmt_code = "01.01"
  )

  ua <- ua_repair_wooded(ua, verbose = FALSE)

  expect_false(ua[[wooded]])
})


test_that("ua_repair_wooded() keeps valid wooded submitted surfaces unchanged", {
  ua <- make_ua_occupation(
    dgd = TRUE,
    wooded = TRUE,
    mgmt_code = "01.01"
  )

  expect_equal(
    ua_repair_wooded(ua, verbose = FALSE),
    ua
  )
})


test_that("ua_repair_wooded() keeps non-submitted non-wooded surfaces unchanged", {
  ua <- make_ua_occupation(
    dgd = FALSE,
    wooded = FALSE,
    mgmt_code = "01.01"
  )

  expect_equal(
    ua_repair_wooded(ua, verbose = FALSE),
    ua
  )
})


test_that("ua_repair_wooded() repairs only invalid rows", {
  wooded <- seq_field("is_wooded")$name

  ua <- make_ua_occupation(
    dgd = c(TRUE, TRUE, FALSE, FALSE),
    wooded = c(TRUE, FALSE, TRUE, FALSE),
    mgmt_code = c("01.01", "01.02", "02.01", "02.02")
  )

  ua <- ua_repair_wooded(ua, verbose = FALSE)

  expect_equal(
    ua[[wooded]],
    c(TRUE, FALSE, FALSE, FALSE)
  )
})


test_that("ua_repair_wooded() warns when invalid rows are repaired", {
  ua <- make_ua_occupation(
    dgd = FALSE,
    wooded = TRUE,
    mgmt_code = "01.01"
  )

  expect_message(
    ua_repair_wooded(ua, verbose = TRUE),
    "wooded status corrected"
  )
})


test_that("ua_repair_wooded() is silent when verbose is FALSE", {
  ua <- make_ua_occupation(
    dgd = FALSE,
    wooded = TRUE,
    mgmt_code = "01.01"
  )

  expect_silent(
    ua_repair_wooded(ua, verbose = FALSE)
  )
})


test_that("ua_repair_wooded() warns with affected management code", {
  ua <- make_ua_occupation(
    dgd = c(FALSE, FALSE),
    wooded = c(TRUE, TRUE),
    mgmt_code = c("01.01", "02.01")
  )

  expect_message(
    ua_repair_wooded(ua, verbose = TRUE),
    "01.01|02.01"
  )
})
