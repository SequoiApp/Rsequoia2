test_that("seq_gpu() returns existing paths", {

  with_seq_cache({
    local_mocked_bindings(
      get_gpu   = function(...) Rsequoia2:::seq_poly
    )

    layer <- c("v.gpu.document.poly", "v.gpu.zone.poly")
    gpu_path <- seq_gpu(dirname = seq_cache, key = layer, verbose = FALSE)

    expect_type(gpu_path, "list")
    expect_true(length(gpu_path) > 0)
    expect_true(all(vapply(gpu_path, file.exists, logical(1))))
  })
})

test_that("seq_gpu() return expected path", {

  with_seq_cache({
    local_mocked_bindings(
      get_gpu = function(...) Rsequoia2:::seq_poly
    )

    layer <- c("v.gpu.document.poly", "v.gpu.zone.poly")
    gpu_path <- seq_gpu(dirname = seq_cache, key = layer, verbose = FALSE)

    expect_length(gpu_path, length(layer))
    expect_all_true(file.exists(unlist(gpu_path)))
  })
})

test_that("seq_gpu() return sf", {

  with_seq_cache({
    local_mocked_bindings(
      get_gpu = function(...) Rsequoia2:::seq_poly
    )

    gpu_path <- seq_gpu(dirname = seq_cache,
                   key = "v.gpu.document.poly",
                   verbose = FALSE)

    document <- sf::read_sf(gpu_path)

    expect_s3_class(document, "sf")

  })
})

test_that("seq_gpu() layers contain id", {
  with_seq_cache({

    local_mocked_bindings(
      get_gpu = function(...) Rsequoia2:::seq_poly
    )

    layer <- c("v.gpu.document.poly", "v.gpu.zone.poly")
    paths <- seq_gpu(dirname = seq_cache, key = layer, verbose = FALSE)
    gpu_path <- lapply(paths, read_sf)

    identifier <- seq_field("identifier")$name
    expect_all_true(vapply(gpu_path, \(x) identifier %in% names(x), TRUE))
  })
})

test_that("seq_gpu() calls seq_write once per output", {
  with_seq_cache({

    called <- 0L

    local_mocked_bindings(
      get_gpu = function(...) Rsequoia2:::seq_poly,
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
