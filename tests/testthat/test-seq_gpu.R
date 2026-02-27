test_that("seq_gpu() returns existing paths", {
  with_seq_cache({

    poly <- Rsequoia2:::seq_poly

    local_mocked_bindings(
      get_gpu   = function(...) poly,
      seq_write = function(...) {
        path <- tempfile(fileext = ".gpkg")
        file.create(path)
        path
      }
    )

    out <- seq_gpu(seq_cache, verbose = FALSE)

    expect_type(out, "list")
    expect_true(length(out) > 0)
    expect_true(all(vapply(out, file.exists, logical(1))))
  })
})

test_that("seq_gpu() calls seq_write once per output", {
  with_seq_cache({

    poly <- Rsequoia2:::seq_poly

    called <- 0L

    local_mocked_bindings(
      get_gpu = function(...) poly,
      seq_write = function(...) {
        called <<- called + 1L
        path <- tempfile(fileext = ".gpkg")
        file.create(path)
        path
      }
    )

    out <- seq_gpu(seq_cache, verbose = FALSE)

    expect_equal(called, length(out))
  })
})

test_that("seq_gpu() writes nothing when no features exist", {
  with_seq_cache({

    called <- 0

    local_mocked_bindings(
      get_gpu = function(...) NULL,
      seq_write = function(...) {
        called <<- called + 1
      }
    )

    out <- seq_gpu(seq_cache, verbose = FALSE)

    expect_null(out)
    expect_equal(called, 0)
  })
})
