test_that("ua_generate_ug() creates UG field correctly", {

  ua <- data.frame(
    "N_PARFOR" = c(1,   1, "a", "a", 1,  NA),
    "N_SSPARFOR" = c("a", 1,  1,  "a", NA, 1)
  )

  ug_field <- "PARFOR"
  local_mocked_bindings(
    seq_field = function(x){
      switch(x,
        "parcelle" = list(name = "N_PARFOR"),
        "sous_parcelle" = list(name = "N_SSPARFOR"),
        "ug" = list(name = ug_field)
      )
    }
  )

  res <- ua_generate_ug(
    ua, ug_keys = c("parcelle", "sous_parcelle"), separator = ".", verbose = FALSE
  )

  expect_true(ug_field %in% names(res))
  expect_equal(res[[ug_field]], c("01.a", "01.01", "a.01", "a.a", "01.00", "00.01"))
})

test_that("ua_generate_ug() throw expected message when verbose = TRUE", {

  ua <- data.frame(
    "N_PARFOR" = c(1,   1, "a", "a", 1,  NA),
    "N_SSPARFOR" = c("a", 1,  1,  "a", NA, 1)
  )

  ug_field <- "PARFOR"
  local_mocked_bindings(
    seq_field = function(x){
      switch(x,
             "parcelle" = list(name = "N_PARFOR"),
             "sous_parcelle" = list(name = "N_SSPARFOR"),
             "ug" = list(name = ug_field)
      )
    }
  )

  expect_message(
    ua_generate_ug(ua, ug_keys = c("parcelle", "sous_parcelle"), separator = ".", verbose = TRUE),
    paste("UG field", ug_field, "created", sep = ".*")
  )

})

test_that("ua_generate_ug() separator arg is working", {

  ua <- data.frame(
    "N_PARFOR" = c(1,   1, "a", "a", 1,  NA),
    "N_SSPARFOR" = c("a", 1,  1,  "a", NA, 1)
  )

  ug_field <- "PARFOR"
  local_mocked_bindings(
    seq_field = function(x){
      switch(x,
             "parcelle" = list(name = "N_PARFOR"),
             "sous_parcelle" = list(name = "N_SSPARFOR"),
             "ug" = list(name = ug_field)
      )
    }
  )

  res <- ua_generate_ug(
    ua, ug_keys = c("parcelle", "sous_parcelle"), separator = "_", verbose = FALSE
  )

  expect_true(ug_field %in% names(res))
  expect_equal(res[[ug_field]], c("01_a", "01_01", "a_01", "a_a", "01_00", "00_01"))

})

test_that("ua_generate_ug() empty separator is working", {

  ua <- data.frame(
    "N_PARFOR" = c(1,   1, "a", "a", 1,  NA),
    "N_SSPARFOR" = c("a", 1,  1,  "a", NA, 1)
  )

  ug_field <- "PARFOR"
  local_mocked_bindings(
    seq_field = function(x){
      switch(x,
             "parcelle" = list(name = "N_PARFOR"),
             "sous_parcelle" = list(name = "N_SSPARFOR"),
             "ug" = list(name = ug_field)
      )
    }
  )

  res <- ua_generate_ug(
    ua, ug_keys = c("parcelle", "sous_parcelle"), separator = "", verbose = FALSE
  )

  expect_true(ug_field %in% names(res))
  expect_equal(res[[ug_field]], c("01a", "0101", "a01", "aa", "0100", "0001"))

})
