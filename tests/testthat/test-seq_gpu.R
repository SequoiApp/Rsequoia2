test_that("seq_gpu() writes expected layers", {
  with_seq_cache({
    local_mocked_bindings(get_gpu = function(...) Rsequoia2:::seq_poly)

    key <- c("v.gpu.document.poly", "v.gpu.zone.poly")
    paths <- seq_gpu(seq_cache, key = key, verbose = FALSE)

    expect_named(paths, key)
    expect_length(paths, 2)
    expect_all_true(file.exists(unlist(paths)))
  })
})

test_that("seq_gpu() writes sf with project id", {
  with_seq_cache({
    local_mocked_bindings(get_gpu = function(...) Rsequoia2:::seq_poly)

    path <- seq_gpu(
      seq_cache,
      key = "v.gpu.document.poly",
      verbose = FALSE
    )[[1]]

    gpu <- sf::read_sf(path)
    identifier <- seq_field("identifier")$name

    expect_s3_class(gpu, "sf")
    expect_true(identifier %in% names(gpu))
    expect_equal(unique(gpu[[identifier]]), "ECKMUHL")
  })
})

test_that("seq_gpu() combines SUPA sources", {
  with_seq_cache({
    seen <- character()

    local_mocked_bindings(
      get_gpu = function(x, layer, ...) {
        seen <<- c(seen, layer)
        Rsequoia2:::seq_poly
      }
    )

    paths <- seq_gpu(
      seq_cache,
      key = "v.gpu.supa.poly",
      verbose = FALSE
    )

    expect_setequal(
      seen,
      c("assiette-sup-s", "assiette-sup-l", "assiette-sup-p")
    )
    expect_named(paths, "v.gpu.supa.poly")
  })
})

test_that("seq_gpu() skips empty layers", {
  with_seq_cache({
    local_mocked_bindings(
      get_gpu = function(x, layer, ...) {
        if (layer == "document") Rsequoia2:::seq_poly else NULL
      }
    )

    paths <- seq_gpu(
      seq_cache,
      key = c("v.gpu.document.poly", "v.gpu.zone.poly"),
      verbose = FALSE
    )

    expect_length(paths, 1)
    expect_named(paths, "v.gpu.document.poly")
  })
})

test_that("seq_gpu() writes nothing when no features exist", {
  with_seq_cache({
    called <- 0L

    local_mocked_bindings(
      get_gpu = function(...) NULL,
      seq_write = function(...) called <<- called + 1L
    )

    out <- seq_gpu(seq_cache, verbose = FALSE)

    expect_length(out, 0)
    expect_equal(called, 0)
  })
})
