test_that("seq_elevation() works", {

  skip_on_os(c("mac", "linux"))

  with_seq_cache({
    fake_dem <- terra::rast(nrows=50, ncols=50, xmin=0, xmax=50, ymin=0, ymax=50, crs = "epsg:2154")
    terra::values(fake_dem) <- as.integer(runif(50*50, 0, 100))

    fake_dsm <- terra::rast(nrows=50, ncols=50, xmin=0, xmax=50, ymin=0, ymax=50, crs = "epsg:2154")
    terra::values(fake_dsm) <- as.integer(runif(50*50, 0, 100))

    local_mocked_bindings(
      get_dem = function(x, ...) fake_dem,
      get_dsm = function(x, ...) fake_dsm,
    )

    paths <- seq_elevation(dirname = seq_cache, verbose = FALSE)

    expect_length(paths, 5)
    expect_all_true(lapply(paths, file.exists) |> unlist())

    required <- c("mnt", "mns", "pente", "expo")
    expect_all_true(sapply(required, function(k) any(grepl(k, names(paths)))))
  })

})
