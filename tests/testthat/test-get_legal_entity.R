fake_legal_entity_csv <- function(dir, dep = "29", com = "158") {
  f <- file.path(dir, paste0(dep, com, "_legal.csv"))
  writeLines(
    c(
      "DEP;COM;PREFIX;SECTION;NUMERO;LIEU_DIT;SURF_TOT;NATURE;CONTENANCE;TYPE;PROP",
      "29;158;;ZR;0001;TEST;10;;;;P;OWNER_A",
      "29;158;;ZR;0001;TEST;10;;;;P;OWNER_B",
      "29;158;;ZR;0002;;5;;;;X;IGNORED"
    ),
    f
  )
  f
}

test_that("get_legal_entity() errors on invalid input", {

  expect_error(
    get_legal_entity("99999", verbose = FALSE),
    regexp = "Invalid INSEE"
  )

  expect_error(
    get_legal_entity("99", verbose = FALSE),
    regexp = "Invalid department"
  )

})

test_that("get_legal_entity() works with valid INSEE code", {

  cache <- file.path(tempdir(), "legal_entity")
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  write_fake_legal_csv(cache, dep = "29", com = "158") |> invisible()

  local_mocked_bindings(
    download_legal_entity = function(cache, verbose) cache
  )

  res <- get_legal_entity("29158", cache = cache, verbose = FALSE)

  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) == 1)

  expect_equal(res$insee, "29158")
  expect_equal(res$section, "ZR")
  expect_equal(res$numero, "0001")
  expect_equal(res$proprietaire, "OWNER_A \\ OWNER_B")
})
