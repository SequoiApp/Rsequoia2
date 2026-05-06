make_lidar_test_raster <- function() {
  r <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:2154"
  )

  terra::values(r) <- 1:terra::ncell(r)
  r
}


make_lidar_parca <- function() {
  sf::st_sf(
    ID_SEQ = "SEQ001",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(
          0, 0,
          10, 0,
          10, 10,
          0, 10,
          0, 0
        ),
        ncol = 2,
        byrow = TRUE
      ))),
      crs = 2154
    )
  )
}


mock_lidar_meta <- function(key) {
  switch(
    key,
    "r.alt.mnt.lidar" = list(path = "RASTER/ALT", name = "MNT_LIDAR", ext = "tif"),
    "r.alt.mns.lidar" = list(path = "RASTER/ALT", name = "MNS_LIDAR", ext = "tif"),
    "r.alt.mnh.lidar" = list(path = "RASTER/ALT", name = "MNH_LIDAR", ext = "tif"),
    stop("Unexpected key: ", key)
  )
}

test_that("seq_lidar() generates one output per requested key", {

  with_seq_cache({

    dirname <- tempfile("seq_")
    dir.create(dirname, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(dirname, recursive = TRUE, force = TRUE), add = TRUE)

    written <- list()

    testthat::local_mocked_bindings(
      seq_read = function(key, dirname) {
        expect_identical(key, "v.seq.parca.poly")
        make_lidar_parca()
      },
      seq_field = function(key) {
        expect_identical(key, "identifier")
        list(name = "ID_SEQ")
      },
      seq_layer = function(key, verbose = FALSE) {
        mock_lidar_meta(key)
      },
      get_lidar = function(x, key, buffer, crs, cache, overwrite, verbose) {
        r <- make_lidar_test_raster()
        names(r) <- paste0(key, "_lidar")
        r
      },
      seq_write = function(r, key, id, dirname, overwrite, verbose) {
        written[[length(written) + 1L]] <<- list(
          key = key,
          id = id,
          dirname = dirname,
          overwrite = overwrite,
          name = names(r)
        )

        meta <- mock_lidar_meta(key)

        normalizePath(
          file.path(
            dirname,
            meta$path,
            sprintf("%s_%s.%s", id, meta$name, meta$ext)
          ),
          winslash = "/",
          mustWork = FALSE
        )
      },
      .package = "Rsequoia2"
    )

    res <- seq_lidar(
      dirname = dirname,
      key = c("mnt", "mns", "mnh"),
      verbose = FALSE
    )

    expect_type(res, "list")
    expect_identical(names(res), c("mnt", "mns", "mnh"))

    expect_length(written, 3)

    expect_identical(
      vapply(written, `[[`, character(1), "key"),
      c(
        "r.alt.mnt.lidar",
        "r.alt.mns.lidar",
        "r.alt.mnh.lidar"
      )
    )

    expect_identical(
      vapply(written, `[[`, character(1), "id"),
      rep("SEQ001", 3)
    )

    expect_identical(
      vapply(written, `[[`, character(1), "name"),
      c("mnt_lidar", "mns_lidar", "mnh_lidar")
    )
  })
})

test_that("seq_lidar() skips existing output when overwrite is FALSE", {

  with_seq_cache({

    dirname <- tempfile("seq_")
    dir.create(dirname, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(dirname, recursive = TRUE, force = TRUE), add = TRUE)

    existing_dir <- file.path(dirname, "RASTER/ALT")
    dir.create(existing_dir, recursive = TRUE, showWarnings = FALSE)

    existing_path <- file.path(existing_dir, "SEQ001_MNT_LIDAR.tif")
    file.create(existing_path)

    get_lidar_called <- FALSE
    seq_write_called <- FALSE

    testthat::local_mocked_bindings(
      seq_read = function(...) make_lidar_parca(),
      seq_field = function(...) list(name = "ID_SEQ"),
      seq_layer = function(key, verbose = FALSE) mock_lidar_meta(key),
      get_lidar = function(...) {
        get_lidar_called <<- TRUE
        make_lidar_test_raster()
      },
      seq_write = function(...) {
        seq_write_called <<- TRUE
        existing_path
      },
      .package = "Rsequoia2"
    )

    expect_warning(
      res <- seq_lidar(
        dirname = dirname,
        key = "mnt",
        overwrite = FALSE,
        verbose = FALSE
      ),
      "already exists"
    )

    expect_false(get_lidar_called)
    expect_false(seq_write_called)

    expect_identical(names(res), "mnt")
    expect_identical(
      res[[1]],
      normalizePath(existing_path, winslash = "/", mustWork = FALSE)
    )
  })
})

test_that("seq_lidar() recomputes existing output when overwrite is TRUE", {

  with_seq_cache({

    dirname <- tempfile("seq_")
    dir.create(dirname, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(dirname, recursive = TRUE, force = TRUE), add = TRUE)

    existing_dir <- file.path(dirname, "RASTER/ALT")
    dir.create(existing_dir, recursive = TRUE, showWarnings = FALSE)

    existing_path <- file.path(existing_dir, "SEQ001_MNT_LIDAR.tif")
    file.create(existing_path)

    get_lidar_called <- FALSE
    seq_write_called <- FALSE

    testthat::local_mocked_bindings(
      seq_read = function(...) make_lidar_parca(),
      seq_field = function(...) list(name = "ID_SEQ"),
      seq_layer = function(key, verbose = FALSE) mock_lidar_meta(key),
      get_lidar = function(...) {
        get_lidar_called <<- TRUE
        r <- make_lidar_test_raster()
        names(r) <- "mnt_lidar"
        r
      },
      seq_write = function(r, key, id, dirname, overwrite, verbose) {
        seq_write_called <<- TRUE

        expect_identical(key, "r.alt.mnt.lidar")
        expect_identical(id, "SEQ001")
        expect_true(overwrite)

        normalizePath(existing_path, winslash = "/", mustWork = FALSE)
      },
      .package = "Rsequoia2"
    )

    res <- seq_lidar(
      dirname = dirname,
      key = "mnt",
      overwrite = TRUE,
      verbose = FALSE
    )

    expect_true(get_lidar_called)
    expect_true(seq_write_called)

    expect_identical(names(res), "mnt")
    expect_identical(
      res[[1]],
      normalizePath(existing_path, winslash = "/", mustWork = FALSE)
    )
  })
})

test_that("seq_lidar() rejects invalid key", {
  expect_error(
    seq_lidar(key = "invalid", verbose = FALSE),
    "'arg' should be one of"
  )
})
