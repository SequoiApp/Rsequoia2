test_that("seq_parca_to_ua() write ua to expected path", {

  with_seq_cache({
    ua_path <- seq_parca_to_ua(dirname = seq_cache, verbose = F)
    expect_true(file.exists(ua_path))
  })

})

test_that("seq_parca_to_ua() return correctly formated ua", {

  with_seq_cache({
    id <- seq_field("identifier")$name

    ua_path <- seq_parca_to_ua(dirname = seq_cache, verbose = F)
    ua <- sf::read_sf(ua_path)

    expect_s3_class(ua, "sf")
    expect_equal(ua[[id]], p[[id]])

    col_name <- lapply(seq_table("ua"), \(x) seq_field(x)$name)
    expect_all_true(col_name %in% names(ua))

  })

})
