test_that("get_path() throw error when no matching key", {
  expect_error(
    get_path("badkey"),
    "This file is part of the package and must not be modified"
    )
})

test_that("get_path() throw error when multiple matching key", {
  expect_error(
    get_path("mnhn"),
    "Multiple match for"
  )
})

test_that("get_path() returns a named file path for a valid key", {

    path <- get_path("parca")

    expect_type(path, "character")
    expect_length(path, 1)
    expect_named(path, get_keys("parca", FALSE))

})

test_that("get_path() uses layer name and extension from config", {

    path <- get_path("parca")
    expect_match(basename(path),"\\.")

})

test_that("get_path() emits message in verbose mode", {

    expect_message(
      get_path("parca", verbose = TRUE),
      "Resolved"
    )

})

