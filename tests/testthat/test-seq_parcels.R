test_that("seq_parcels() works", {
  with_seq_cache({

    ua <- parca_to_ua(p)
    pcl_code <- seq_field("pcl_code")$name
    sub_code <- seq_field("sub_code")$name
    ua[[pcl_code]] <- "00"
    ua[[sub_code]] <- "00"
    ua[[sub_code]][1] <- "01"
    ua <- ua_to_ua(ua, p, verbose = F)
    seq_write(ua, "ua", seq_cache, overwrite = TRUE)

    parcels <- seq_parcels(dirname = seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_all_true(vapply(parcels, file.exists, TRUE))

    pf_poly <- sf::read_sf(parcels[["v.seq.pf.poly"]])
    expect_shape(pf_poly, nrow = 1)
    expect_true(sf::st_geometry_type(pf_poly) %in% c("MULTIPOLYGON", "POLYGON"))

    sspf_poly <- sf::read_sf(parcels[["v.seq.sspf.poly"]])
    expect_shape(sspf_poly, nrow = 2)
    expect_all_true(sf::st_geometry_type(sspf_poly) %in% c("MULTIPOLYGON", "POLYGON"))

  })
})
