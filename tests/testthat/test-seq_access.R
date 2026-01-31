test_that("seq_access() returned expected path", {
  with_seq_cache({
    path <- seq_access(seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_length(path, 1)
    expect_all_true(file.exists(unlist(path)))
  })
})

test_that("seq_access() creates an empty sf layer with CRS", {
  with_seq_cache({
    path <- seq_access(seq_cache, verbose = FALSE, overwrite = TRUE)
    acces <- read_sf(path)

    expect_s3_class(acces, "sf")
    expect_equal(nrow(acces), 0)
    expect_equal(sf::st_crs(acces)$epsg, 2154)
  })
})

# test_that("seq_access() layers contain id", {
#   with_seq_cache({
#     path <- seq_access(seq_cache, verbose = FALSE, overwrite = TRUE)
#     acces <- read_sf(path)
#     identifier <- seq_field("identifier")$name
#     expect_true(identifier %in% names(acces))
#   })
# })
