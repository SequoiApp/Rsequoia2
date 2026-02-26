test_that("seq_ifn() returns existing paths", {
  with_seq_cache({

    poly <- Rsequoia2:::seq_poly |>
      transform(codeser = "B33")

    local_mocked_bindings(
      get_ifn   = function(...) poly,
      get_ser_pdf = function(...) "ser_pdf_path",
      seq_write = function(...) {
        path <- tempfile(fileext = ".gpkg")
        file.create(path)
        path
      }
    )

    out <- seq_ifn(seq_cache, verbose = FALSE)

    expect_type(out, "list")
    expect_true(length(out) > 0)
    expect_true(all(vapply(out, file.exists, logical(1))))
  })
})

test_that("seq_ifn() calls seq_write once per output", {
  with_seq_cache({

    poly <- Rsequoia2:::seq_poly |>
      transform(codeser = "B33")

    called <- 0L

    local_mocked_bindings(
      get_ifn = function(...) poly,
      get_ser_pdf = function(...) "ser_pdf_path",
      seq_write = function(...) {
        called <<- called + 1L
        path <- tempfile(fileext = ".gpkg")
        file.create(path)
        path
      }
    )

    out <- seq_ifn(seq_cache, verbose = FALSE)

    expect_equal(called, length(out))
  })
})

test_that("seq_ifn() respects key argument", {
  with_seq_cache({

    poly <- Rsequoia2:::seq_poly |>
      transform(codeser = "B33")

    seen <- character()

    local_mocked_bindings(
      get_ifn = function(parca, key, ...) {
        seen <<- c(seen, key)
        poly
      },
      get_ser_pdf = function(...) "ser_pdf_path",
      seq_write = function(...) tempfile(fileext = ".gpkg")
    )

    seq_ifn(seq_cache, key = c("ser", "zp"), verbose = FALSE)

    expect_setequal(seen, c("ser", "zp"))
  })
})

test_that("seq_ifn() writes nothing when no features exist", {
  with_seq_cache({

    called <- 0

    local_mocked_bindings(
      get_ifn = function(...) NULL,
      get_ser_pdf = function(...) "ser_pdf_path",
      seq_write = function(...) {
        called <<- called + 1
      }
    )

    out <- seq_ifn(seq_cache, verbose = FALSE)

    expect_null(out)
    expect_equal(called, 0)
  })
})

test_that("seq_ifn() returns only written layers when some key are empty", {
  with_seq_cache({

    poly <- Rsequoia2:::seq_poly |>
      transform(codeser = "B33")

    local_mocked_bindings(
      get_ifn = function(parca, key, ...) {
        if (key == "ser") return(poly)
        NULL
      },
      get_ser_pdf = function(...) "ser_pdf_path",
      seq_write = function(...) tempfile(fileext = ".gpkg")
    )

    out <- seq_ifn(seq_cache, key = c("ser", "rfn"), verbose = FALSE)

    expect_named(out, "ser")
    expect_length(out, 1)
  })
})

test_that("seq_ifn() errors on invalid key", {

  expect_error(
    seq_ifn(key = "invalid", verbose = FALSE),
    "key"
  )
})
