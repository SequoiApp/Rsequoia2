test_that("seq_mnhn() return expected path", {

  with_seq_cache({
    local_mocked_bindings(
      get_mnhn = function(...) Rsequoia2:::seq_poly
    )

    layer <- c("pn", "pnr")
    mnhn_path <- seq_mnhn(dirname = seq_cache, key = layer, verbose = FALSE)

    expect_length(mnhn_path, length(layer))
    expect_all_true(file.exists(unlist(mnhn_path)))
  })
})

test_that("seq_mnhn() return sf", {

  with_seq_cache({
    local_mocked_bindings(
      get_mnhn = function(...) Rsequoia2:::seq_poly
    )

    mnhn_path <- seq_mnhn(dirname = seq_cache, key = "pn", verbose = FALSE)
    pn <- sf::read_sf(mnhn_path)

    expect_s3_class(pn, "sf")

  })
})

test_that("seq_mnhn() layers contain id", {
  with_seq_cache({

    local_mocked_bindings(
      get_mnhn = function(...) Rsequoia2:::seq_poly
    )

    paths <- seq_mnhn(dirname = seq_cache, key = get_keys("mnhn"), verbose = FALSE)
    mnhn <- lapply(paths, read_sf)

    identifier <- seq_field("identifier")$name
    expect_all_true(vapply(mnhn, \(x) identifier %in% names(x), TRUE))
  })
})

test_that("seq_mnhn() is silent when verbose is FALSE", {

  with_seq_cache({
    local_mocked_bindings(
      get_mnhn = function(...) Rsequoia2:::seq_empty
    )

    expect_silent(
      seq_mnhn(dirname = seq_cache, key = "pn", verbose = FALSE)
    )
  })
})
