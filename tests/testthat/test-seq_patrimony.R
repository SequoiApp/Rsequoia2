test_that("seq_patrimony() return expected path", {

  with_seq_cache({
    local_mocked_bindings(
      get_patrimony = function(...) Rsequoia2:::seq_poly
    )

    layer <- c("immh", "pamh")
    patrimony_path <- seq_patrimony(dirname = seq_cache,
                                    key = layer,
                                    verbose = FALSE)

    expect_length(patrimony_path, length(layer))
    expect_all_true(file.exists(unlist(patrimony_path)))
  })
})

test_that("seq_patrimony() return sf", {

  with_seq_cache({
    local_mocked_bindings(
      get_patrimony = function(...) Rsequoia2:::seq_poly
    )

    patrimony_path <- seq_patrimony(dirname = seq_cache,
                                    key = "imdn",
                                    verbose = FALSE)
    pn <- sf::read_sf(patrimony_path)

    expect_s3_class(pn, "sf")

  })
})

test_that("seq_com() layers contain id", {
  with_seq_cache({

    local_mocked_bindings(
      get_patrimony = function(...) Rsequoia2:::seq_poly
    )

    paths <- seq_patrimony(dirname = seq_cache, key = get_keys("pat"), verbose = FALSE)
    patrimony <- lapply(paths, read_sf)

    identifier <- seq_field("identifier")$name
    expect_all_true(vapply(patrimony, \(x) identifier %in% names(x), TRUE))
  })
})

test_that("seq_patrimony() is silent when verbose is FALSE", {

  with_seq_cache({
    local_mocked_bindings(
      get_patrimony = function(...) NULL
    )

    expect_silent(
      seq_patrimony(dirname = seq_cache, key = "imdn", verbose = FALSE)
    )
  })
})
