make_ua_occupation <- function(dgd, wooded, area, mgmt_code = seq_along(dgd)) {
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

test_that("ua_check_non_wooded_threshold() returns TRUE when threshold is respected", {
  ua <- make_ua_occupation(
    dgd = c(TRUE, TRUE),
    wooded = c(TRUE, FALSE),
    area = c(9, 1)
  )

  expect_true(
    ua_check_non_wooded_threshold(ua, threshold = 0.10, verbose = FALSE)
  )
})


test_that("ua_check_non_wooded_threshold() returns FALSE when threshold is exceeded", {
  ua <- make_ua_occupation(
    dgd = c(TRUE, TRUE),
    wooded = c(TRUE, FALSE),
    area = c(8, 2)
  )

  expect_false(
    ua_check_non_wooded_threshold(ua, threshold = 0.10, verbose = FALSE)
  )
})


test_that("ua_check_non_wooded_threshold() ignores non-submitted surfaces", {
  ua <- make_ua_occupation(
    dgd = c(TRUE, TRUE, FALSE),
    wooded = c(TRUE, FALSE, FALSE),
    area = c(9, 1, 100)
  )

  expect_true(
    ua_check_non_wooded_threshold(ua, threshold = 0.10, verbose = FALSE)
  )
})


test_that("ua_check_non_wooded_threshold() accepts custom threshold", {
  ua <- make_ua_occupation(
    dgd = c(TRUE, TRUE),
    wooded = c(TRUE, FALSE),
    area = c(8, 2)
  )

  expect_true(
    ua_check_non_wooded_threshold(ua, threshold = 0.20, verbose = FALSE)
  )
})


test_that("ua_check_non_wooded_threshold() return false when submitted DGD surface is zero", {
  ua <- make_ua_occupation(
    dgd = c(FALSE, FALSE),
    wooded = c(FALSE, FALSE),
    area = c(10, 20)
  )

  expect_false(
    ua_check_non_wooded_threshold(ua, verbose = FALSE),
    "Submitted DGD surface is empty or equal to zero"
  )
})


test_that("ua_check_non_wooded_threshold() warns when verbose and threshold is exceeded", {
  ua <- make_ua_occupation(
    dgd = c(TRUE, TRUE),
    wooded = c(TRUE, FALSE),
    area = c(8, 2)
  )

  expect_message(
    ua_check_non_wooded_threshold(ua, threshold = 0.10, verbose = TRUE),
    "[DANGER]"
  )
})


test_that("ua_check_non_wooded_threshold() is silent when verbose is FALSE", {
  ua <- make_ua_occupation(
    dgd = c(TRUE, TRUE),
    wooded = c(TRUE, FALSE),
    area = c(8, 2)
  )

  expect_silent(
    ua_check_non_wooded_threshold(ua, threshold = 0.10, verbose = FALSE)
  )
})

test_that("ua_check_non_wooded_threshold() messages when threshold is respected", {
  ua <- make_ua_occupation(
    dgd = c(TRUE, TRUE),
    wooded = c(TRUE, FALSE),
    area = c(9, 0.5)
  )

  expect_message(
    expect_true(
      ua_check_non_wooded_threshold(ua, threshold = 0.10, verbose = TRUE)
    ),
    "Non-wooded submitted surface is"
  )
})

test_that("ua_check_non_wooded_threshold() warns with ratio when threshold is exceeded", {
  ua <- make_ua_occupation(
    dgd = c(TRUE, TRUE),
    wooded = c(TRUE, FALSE),
    area = c(8, 2)
  )

  expect_message(
    expect_false(
      ua_check_non_wooded_threshold(ua, threshold = 0.10, verbose = TRUE)
    ),
    "[DANGER]"
  )
})
