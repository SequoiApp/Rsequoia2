test_that("get_parca_bdp() aborts on empty idu", {

  expect_error(get_parca_bdp(character()), "idu.*non-empty")
  expect_error(get_parca_bdp(NULL), "idu.*non-empty")
  expect_error(get_parca_bdp(numeric()), "idu.*non-empty")

  expect_error(get_parca_bdp(Rsequoia2:::seq_poly), "must be")
  expect_error(get_parca_bdp(list("aaaa")), "must be")

})
