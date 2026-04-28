test_that("ua_update_parca_fields() updates IDU-linked fields from parca", {
  with_seq_cache({

    idu_field <- seq_field("idu")$name
    reg_name <- seq_field("reg_name")$name
    com_name <- seq_field("com_name")$name
    cad_area <- seq_field("cad_area")$name

    p[[reg_name]] <- "REGION_OK"
    p[[com_name]] <- "COMMUNE_OK"
    p[[cad_area]] <- 10

    ua <- parca_to_ua(p)

    ua[[reg_name]] <- "WRONG_REGION"
    ua[[com_name]] <- "WRONG_COMMUNE"
    ua[[cad_area]] <- 99

    res <- ua_update_parca_fields(ua, p)

    expect_s3_class(res, "sf")
    expect_identical(res[[idu_field]], ua[[idu_field]])
    expect_identical(res[[reg_name]], p[[reg_name]])
    expect_identical(res[[com_name]], p[[com_name]])
    expect_equal(res[[cad_area]], p[[cad_area]])

  })
})

test_that("ua_update_parca_fields() keeps non-IDU-linked fields unchanged", {
  with_seq_cache({

    mgmt_field <- seq_field("mgmt_code")$name

    ua <- parca_to_ua(p)
    ua[[mgmt_field]] <- "MGMT_TEST"

    res <- ua_update_parca_fields(ua, p)

    expect_identical(res[[mgmt_field]], ua[[mgmt_field]])

  })
})

test_that("ua_update_parca_fields() keeps row count and geometry", {
  with_seq_cache({

    ua <- parca_to_ua(p)

    res <- ua_update_parca_fields(ua, p)

    expect_s3_class(res, "sf")
    expect_equal(nrow(res), nrow(ua))
    expect_equal(sf::st_geometry(res), sf::st_geometry(ua))

  })
})

test_that("ua_update_parca_fields() requires IDU field in ua and parca", {
  with_seq_cache({

    idu_field <- seq_field("idu")$name

    ua <- parca_to_ua(p)

    ua_without_idu <- ua
    ua_without_idu[[idu_field]] <- NULL

    expect_error(
      ua_update_parca_fields(ua_without_idu, p)
    )

    parca_without_idu <- p
    parca_without_idu[[idu_field]] <- NULL

    expect_error(
      ua_update_parca_fields(ua, parca_without_idu)
    )

  })
})
