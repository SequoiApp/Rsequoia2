test_that("ua_generate_ug() creates UG field correctly", {
  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    mgmt_code <- seq_field("mgmt_code")$name
    pcl_code <- seq_field("pcl_code")$name
    sub_code <- seq_field("sub_code")$name

    expected <- c(
      num_char  = "01.a",
      num_num   = "01.01",
      char_num  = "a.01",
      char_char = "a.a",
      num_na    = "01.00",
      na_num    = "00.01",
      na_na_1   = "00.00",
      na_na_2   = "00.00",
      na_na_3   = "00.00"
    )

    cases <- list(
      num_char  = c(1,  "a",  "01.a" ),
      num_num   = c(1,   1,   "01.01"),
      char_num  = c("a", 1,   "a.01" ),
      char_char = c("a", "a", "a.a"  ),
      num_na    = c(1,   NA,  "01.00"),
      na_num    = c(NA,  1,   "00.01"),
      na_na     = c(NA,  NA,  "00.00")
    )

    pcl_vals <- lapply(cases, `[`, 1) |> unlist()
    sub_vals <- lapply(cases, `[`, 2) |> unlist()
    expected <- lapply(cases, `[`, 3) |> unlist()

    n <- length(cases)
    ua[[pcl_code]][seq_len(n)] <- pcl_vals
    ua[[sub_code]][seq_len(n)] <- sub_vals

    res <- ua_generate_ug(
      ua, ug_keys = c("pcl_code", "sub_code"), separator = ".", verbose = FALSE
    )

    expect_true(mgmt_code %in% names(res))
    expect_equal(res[[mgmt_code]][seq_len(n)], unname(expected))
  })
})

test_that("ua_generate_ug() throw expected message when verbose = TRUE", {
  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    mgmt_code <- seq_field("mgmt_code")$name
    pcl_code <- seq_field("pcl_code")$name
    sub_code <- seq_field("sub_code")$name

    ua[[pcl_code]] <- 1
    ua[[sub_code]] <- "a"

    expect_message(
      ua_generate_ug(ua, ug_keys = c("pcl_code", "sub_code"), verbose = TRUE),
      sprintf("UG field *%s created", mgmt_code)
    )
  })
})

test_that("ua_generate_ug() separator arg is working", {
  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    mgmt_code <- seq_field("mgmt_code")$name
    pcl_code <- seq_field("pcl_code")$name
    sub_code <- seq_field("sub_code")$name

    ua[[pcl_code]] <- 1
    ua[[sub_code]] <- "a"

    sep = "_"
    res <- ua_generate_ug(
      ua, ug_keys = c("pcl_code", "sub_code"), separator = sep, verbose = FALSE
    )

    expect_true(mgmt_code %in% names(res))
    expect_all_true(grepl(sep, res[[mgmt_code]]))
  })
})

test_that("ua_generate_ug() empty separator is working", {
  with_seq_cache({
    ua <- seq_normalize(p, "ua")

    mgmt_code <- seq_field("mgmt_code")$name
    pcl_code <- seq_field("pcl_code")$name
    sub_code <- seq_field("sub_code")$name

    ua[[pcl_code]] <- "a"
    ua[[sub_code]] <- "a"

    sep = ""
    res <- ua_generate_ug(
      ua, ug_keys = c("pcl_code", "sub_code"), separator = sep, verbose = FALSE
    )

    expect_true(mgmt_code %in% names(res))
    expect_equal(res[[mgmt_code]][1], "aa")
  })
})
