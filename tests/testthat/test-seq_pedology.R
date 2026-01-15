test_that("seq_pedology() returns path", {
  with_seq_cache({

    local_mocked_bindings(
      get_pedology = function(...) Rsequoia2:::seq_poly,
      get_pedology_pdf = function(out_dir, ...) out_dir,
    )

    path <- seq_pedology(seq_cache, verbose = FALSE)
    expect_true(file.exists(path))
  })
})

test_that("seq_pedology() returns valid sf", {
  with_seq_cache({

    called <- 0
    local_mocked_bindings(
      get_pedology = function(...) Rsequoia2:::seq_poly,
      get_pedology_pdf = function(...) called <<- called + 1,
    )

    path <- seq_pedology(seq_cache, verbose = FALSE)
    expect_s3_class(sf::read_sf(path), "sf")
    expect_equal(called, 1)
  })

})

test_that("seq_pedology() writes nothing when no features exist", {
  with_seq_cache({

    called <- 0
    local_mocked_bindings(
      get_pedology = function(...) NULL,
      get_pedology_pdf = function(...) called <<- called + 1
    )

    path <- seq_pedology(dirname = seq_cache, verbose = FALSE)
    expect_length(path, 0)
    expect_equal(called, 0)
  })

})

