test_that("get_lieux_dits reads lieux_dits for unique INSEE codes", {

  call <- 0
  local_mocked_bindings(
    read_etalab = function(...) {
      call <<- call + 1
      data.frame(nom = c("A", "B"))
    },
  )

  res <- get_lieux_dits("010010000A0001")

  expect_equal(call, 1)

  locality <- seq_field("locality")$name
  expect_named(res, locality)
  expect_equal(res[[locality]], c("A", "B"))
})
