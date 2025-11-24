test_that("seq_normalize() works", {

  fake_field <- list(
    field1 = list(name = "FIELD1", alias = "alias1", class = "character"),
    field2 = list(name = "FIELD2", alias = "alias2", class = "numeric"),
    field3 = list(name = "FIELD3", alias = "alias3", class = "numeric"),
    field4 = list(name = "FIELD4", alias = "alias4", class = "character")
  )

  local_mocked_bindings(
    seq_field = function() fake_field,
    seq_table = function(table) c("field1", "field2", "field3"),
  )

  # Data using aliases
  df <- data.frame(
    alias2 = "1,2",
    alias1 = 1.2,
    other = NA_character_
  )

  res <- seq_normalize(df ,"table1")

  expect_equal(
    res,
    data.frame(
      FIELD1 = "1.2",
      FIELD2 = 1.2,
      FIELD3 = NA_real_
  ))

})
