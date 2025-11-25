test_that("ua_check_idu returns TRUE when all IDU match", {

  parca <- fake_parca(numero = c("0001", "0002"))
  ua <- seq_normalize(parca, "ua")

  expect_true(ua_check_idu(ua, parca, verbose = FALSE))
})

test_that("ua_check_idu print message when verbose = TRUE", {

  parca <- fake_parca(numero = c("0001", "0002"))
  ua <- seq_normalize(parca, "ua")

  expect_message(
    ua_check_idu(ua, parca, verbose = TRUE),
    "All cadastral IDUs from"
  )
})

test_that("ua_check_idu returns FALSE when IDU are missing", {

  parca <- fake_parca(1, 1, 1, 1)
  ua <- seq_normalize(fake_parca(2, 2, 2, 2), "ua")

  expect_warning(res <- ua_check_idu(ua, parca), "1111")
  expect_false(res)
})


test_that("ua_check_idu detects multiple missing IDU", {

  parca <- fake_parca(1:3, 1:3, 1:3, 1:3)
  ua <- seq_normalize(fake_parca(3:5, 3:5, 3:5, 3:5), "ua")
  expect_warning(res <- ua_check_idu(ua, parca), "1111.*2222")
  expect_false(res)

})
