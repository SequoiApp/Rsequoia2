test_that("seq_ifn() returns written paths", {
  with_seq_cache({
    poly <- transform(Rsequoia2:::seq_poly, codeser = "B33")

    local_mocked_bindings(
      get_ifn = function(...) poly,
      get_ser_pdf = function(...) NULL,
      seq_write = function(...) {
        path <- tempfile(fileext = ".gpkg")
        file.create(path)
        path
      }
    )

    out <- seq_ifn(seq_cache, verbose = FALSE)

    expect_type(out, "list")
    expect_length(out, length(get_keys("ifn")))
    expect_all_true(vapply(out, file.exists, logical(1)))
  })
})

test_that("seq_ifn() respects key argument", {
  with_seq_cache({
    poly <- transform(Rsequoia2:::seq_poly, codeser = "B33")
    seen <- character()

    local_mocked_bindings(
      get_ifn = function(x, key, ...) {
        seen <<- c(seen, key)
        poly
      },
      get_ser_pdf = function(...) NULL,
      seq_write = function(...) tempfile(fileext = ".gpkg")
    )

    out <- seq_ifn(seq_cache, key = c("ser", "zp"), verbose = FALSE)

    expect_setequal(seen, c("ser", "zp"))
    expect_length(out, 2)
  })
})

test_that("seq_ifn() downloads SER pdf", {
  with_seq_cache({
    poly <- transform(Rsequoia2:::seq_poly, codeser = "B33")
    called <- 0L

    local_mocked_bindings(
      get_ifn = function(...) poly,
      get_ser_pdf = function(...) called <<- called + 1L,
      seq_write = function(...) tempfile(fileext = ".gpkg")
    )

    seq_ifn(seq_cache, key = "ser", verbose = FALSE)

    expect_equal(called, 1)
  })
})

test_that("seq_ifn() writes nothing when no features exist", {
  with_seq_cache({
    called <- 0L

    local_mocked_bindings(
      get_ifn = function(...) NULL,
      get_ser_pdf = function(...) NULL,
      seq_write = function(...) called <<- called + 1L
    )

    out <- seq_ifn(seq_cache, verbose = FALSE)

    expect_length(out, 0)
    expect_equal(called, 0)
  })
})

test_that("seq_ifn() returns only non-empty layers", {
  with_seq_cache({
    poly <- transform(Rsequoia2:::seq_poly, codeser = "B33")

    local_mocked_bindings(
      get_ifn = function(x, key, ...) if (key == "ser") poly else NULL,
      get_ser_pdf = function(...) NULL,
      seq_write = function(...) tempfile(fileext = ".gpkg")
    )

    out <- seq_ifn(seq_cache, key = c("ser", "rfn"), verbose = FALSE)

    expect_length(out, 1)
    expect_named(out, "v.ifn.ser.poly")
  })
})

test_that("seq_ifn() rejects invalid key", {
  with_seq_cache({
    expect_error(
      seq_ifn(seq_cache, key = "invalid", verbose = FALSE),
      "key"
    )
  })
})
