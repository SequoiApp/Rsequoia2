test_that("seq_parcels() generates expected parcel layers", {

  skip_if_not(
    qgisprocess::has_qgis(),
    "QGIS not available on this system"
  )

  with_seq_cache({

    ua <- parca_to_ua(p)

    pcl_code <- seq_field("pcl_code")$name
    sub_code <- seq_field("sub_code")$name

    ua[[pcl_code]] <- "00"
    ua[[sub_code]] <- "00"
    ua[[sub_code]][1] <- "01"  # force SSPF split

    ua <- ua_to_ua(ua, p, verbose = FALSE)
    seq_write(ua, "ua", seq_cache, overwrite = TRUE)

    # ---- Run function ----
    parcels <- seq_parcels(dirname = seq_cache, verbose = FALSE, overwrite = TRUE)

    # ---- Returned structure ----
    expect_type(parcels, "list")
    expect_named(
      parcels,
      c(
        "v.seq.pf.poly",
        "v.seq.pf.line",
        "v.seq.pf.point",
        "v.seq.sspf.poly",
        "v.seq.sspf.line",
        "v.seq.sspf.point"
      )
    )

    # ---- Files exist ----
    expect_true(all(vapply(parcels, file.exists, logical(1))))

    # ---- PF checks ----
    pf_poly <- sf::read_sf(parcels[["v.seq.pf.poly"]])
    expect_shape(pf_poly, nrow = 1)
    expect_true(
      all(sf::st_geometry_type(pf_poly) %in% c("POLYGON", "MULTIPOLYGON"))
    )

    # ---- SSPF checks ----
    sspf_poly <- sf::read_sf(parcels[["v.seq.sspf.poly"]])
    expect_shape(sspf_poly, nrow = 2)
    expect_true(
      all(sf::st_geometry_type(sspf_poly) %in% c("POLYGON", "MULTIPOLYGON"))
    )

    # ---- Line geometry check ----
    pf_line <- sf::read_sf(parcels[["v.seq.pf.line"]])
    expect_true(
      all(sf::st_geometry_type(pf_line) %in% c("MULTILINESTRING", "LINESTRING"))
    )

    # ---- Point geometry check ----
    pf_point <- sf::read_sf(parcels[["v.seq.pf.point"]])
    expect_true(
      all(sf::st_geometry_type(pf_point) == "POINT")
    )
  })
})

test_that("seq_parcels() respects overwrite argument", {

  skip_if_not(
    qgisprocess::has_qgis(),
    "QGIS not available on this system"
  )

  with_seq_cache({

    ua <- parca_to_ua(p)

    pcl_code <- seq_field("pcl_code")$name
    sub_code <- seq_field("sub_code")$name
    ua[[pcl_code]] <- "00"
    ua[[sub_code]] <- "00"

    ua <- ua_to_ua(ua, p, verbose = FALSE)
    seq_write(ua, "ua", seq_cache, overwrite = TRUE)

    # First run
    seq_parcels(dirname = seq_cache, overwrite = TRUE)

    # Second run without overwrite should error
    expect_warning(
      seq_parcels(dirname = seq_cache, overwrite = FALSE)
    ) |> suppressWarnings()

  })
})

test_that("seq_parcels() fails if UA layer is missing", {
  with_seq_cache({
    expect_error(seq_parcels(dirname = seq_cache), "doesn't exist")
  })
})
