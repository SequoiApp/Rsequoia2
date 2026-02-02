test_that("seq_elevation() works", {

  skip_on_os("mac")

  with_seq_cache({
    fake_dem <- terra::rast(nrows=50, ncols=50, xmin=0, xmax=50, ymin=0, ymax=50, crs = "epsg:2154")
    terra::values(fake_dem) <- as.integer(runif(50*50, 0, 100))
    # Raster should be save as tiff so gdal can understand drive = GTIFF not MEM
    tmp <- file.path(seq_cache, "test.tif")
    terra::writeRaster(fake_dem, tmp, overwrite = TRUE)
    fake_dem <- terra::rast(tmp)

    fake_dsm <- terra::rast(nrows=50, ncols=50, xmin=0, xmax=50, ymin=0, ymax=50, crs = "epsg:2154")
    terra::values(fake_dsm) <- as.integer(runif(50*50, 0, 100))
    # Raster should be save as tiff so gdal can understand drive = GTIFF not MEM
    tmp <- file.path(seq_cache, "test.tif")
    terra::writeRaster(fake_dsm, tmp, overwrite = TRUE)
    fake_dsm <- terra::rast(tmp)

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
