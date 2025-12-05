test_that("seq_elevation() works", {

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE))

  m <- fake_matrice(id = "TEST")
  m_path <- file.path(seq_cache, "TEST_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  p <- fake_parca(dep = 29)
  p_path <- seq_write(p, "parca", dirname = seq_cache)

  fake_dem <- terra::rast(nrows=50, ncols=50, xmin=0, xmax=50, ymin=0, ymax=50, crs = "epsg:2154")
  terra::values(fake_dem) <- as.integer(runif(50*50, 0, 100))

  fake_dsm <- terra::rast(nrows=50, ncols=50, xmin=0, xmax=50, ymin=0, ymax=50, crs = "epsg:2154")
  terra::values(fake_dsm) <- as.integer(runif(50*50, 0, 100))

  local_mocked_bindings(
    get_dem = function(x, ...) fake_dem,
    get_dsm = function(x, ...) fake_dsm,
  )

  path <- seq_elevation(dirname = seq_cache, verbose = FALSE)

  expect_length(path, 5)
  expect_equal(class(path), "list")
  expect_all_true(lapply(path, file.exists) |> unlist())

  required <- c("mnt", "mns", "pente", "expo")
  expect_all_true(sapply(required, function(k) any(grepl(k, names(path)))))

})
