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

test_that("ua_to_occupation() aggregates UA by occupation", {
  ua <- make_ua_occupation(
    dgd = c(TRUE, TRUE, FALSE),
    wooded = c(TRUE, FALSE, FALSE),
    area = c(8, 2, 5),
    mgmt_code = c("01.01", "01.02", "02.01")
  )

  res <- ua_to_occupation(ua, verbose = FALSE)

  expect_equal(sort(res[["OCCUPATION"]]), sort(c(
    "SOUMIS_BOISE",
    "SOUMIS_NON_BOISE",
    "NON_SOUMIS"
  )))
})


test_that("ua_to_occupation() sums corrected area by occupation", {
  cor_area <- seq_field("cor_area")$name

  ua <- make_ua_occupation(
    dgd = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    wooded = c(TRUE, TRUE, FALSE, FALSE, FALSE),
    area = c(5, 3, 2, 4, 1),
    mgmt_code = c("01.01", "01.02", "02.01", "03.01", "03.02")
  )

  res <- ua_to_occupation(ua, verbose = FALSE)
  area_by_occ <- setNames(res[[cor_area]], res[["OCCUPATION"]])

  expect_equal(area_by_occ[["SOUMIS_BOISE"]], 8)
  expect_equal(area_by_occ[["SOUMIS_NON_BOISE"]], 2)
  expect_equal(area_by_occ[["NON_SOUMIS"]], 5)
}
)

test_that("ua_to_occupation() repairs non-submitted wooded surfaces before aggregation", {
  is_wooded <- seq_field("is_wooded")$name
  cor_area <- seq_field("cor_area")$name

  ua <- make_ua_occupation(
    dgd = c(FALSE, FALSE),
    wooded = c(TRUE, FALSE),
    area = c(2, 3),
    mgmt_code = c("01.01", "01.02")
  )

  res <- ua_to_occupation(ua, verbose = FALSE)

  expect_equal(nrow(res), 1)
  expect_equal(res[["OCCUPATION"]], "NON_SOUMIS")
  expect_false(res[[is_wooded]])
  expect_equal(res[[cor_area]], 5)
})

test_that("ua_to_occupation() aborts when DGD field is empty", {
  ua <- make_ua_occupation(
    dgd = c(NA, NA),
    wooded = c(TRUE, FALSE),
    area = c(1, 2)
  )

  expect_error(
    ua_to_occupation(ua, verbose = FALSE),
    "Field .* is missing or is empty"
  )
})

test_that("ua_to_occupation() aborts when wooded field is empty", {
  ua <- make_ua_occupation(
    dgd = c(TRUE, FALSE),
    wooded = c(NA, NA),
    area = c(1, 2)
  )

  expect_error(
    ua_to_occupation(ua, verbose = FALSE),
    "Field .* is missing or is empty"
  )
})
