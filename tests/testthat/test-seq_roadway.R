# mock_voids ----
mock_voids <- st_sf(
  IDENTIFIANT = NA_character_,
  TYPE = c("RC","RD","RF","CC","CR"),
  NATURE = NA_character_,
  NOM = NA_character_,
  SOURCE = NA_character_,
  geometry = st_sfc(
    sf::st_multipolygon(list(list(rbind(
      c(500680, 6749000),
      c(500980, 6749000),
      c(500980, 6749006),
      c(500680, 6749006),
      c(500680, 6749000)
    )))),

    sf::st_multipolygon(list(list(rbind(
      c(500820, 6749006),
      c(500826, 6749006),
      c(500826, 6749026),
      c(500820, 6749026),
      c(500820, 6749006)
    )))),

    sf::st_multipolygon(list(list(rbind(
      c(500700, 6749026),
      c(500900, 6749026),
      c(500900, 6749032),
      c(500700, 6749032),
      c(500700, 6749026)
    )))),

    sf::st_multipolygon(list(list(rbind(
      c(500974, 6749000),
      c(500980, 6749000),
      c(500980, 6749040),
      c(500974, 6749040),
      c(500974, 6749000)
    )))),

    sf::st_multipolygon(list(list(rbind(
      c(500900, 6749006),
      c(500906, 6749006),
      c(500926, 6749026),
      c(500920, 6749026),
      c(500900, 6749006)
    )))),

    crs = 2154
  )
)

# mock_roads ----
mock_roads <- st_sf(
  IDENTIFIANT = paste0("road_", 1:2),
  TYPE = c("PN","PN"),
  NATURE = NA_character_,
  IMPORTANCE = c(3L,2L),
  IS_PRIVATE = c(FALSE, FALSE),
  RESTRICTION = c(NA, "3.5t"),
  NOM = c("Rue A", "Rue B"),
  SOURCE = "mock",
  OFFSET = NA_character_,
  geometry = st_sfc(
    st_linestring(rbind(c(500680, 6749020), c(500980, 6749020))),
    st_linestring(rbind(c(500830, 6749000), c(500830, 6749060)))
  ),
  crs = 2154
)

# tests ----
test_that("seq_roadway() writes correct geometry types", {
  with_seq_cache({

    local_mocked_bindings(
      seq_read = function(key, ...) {
        if (key == "parca") return(p)
        if (key == "v.cad.vides.poly") return(mock_voids)
      }
    )

    paths <- seq_roadway(
      source = "v.cad.vides.poly",
      dirname = seq_cache,
      verbose = FALSE,
      overwrite = TRUE
    )

    line <- sf::read_sf(paths[[1]])
    poly <- sf::read_sf(paths[[2]])

    expect_all_true(sf::st_geometry_type(line) %in% "LINESTRING")
    expect_all_true(sf::st_geometry_type(poly) %in% c("POLYGON", "MULTIPOLYGON"))

    expect_true(sf::st_crs(line) == sf::st_crs(2154))
    expect_true(sf::st_crs(poly) == sf::st_crs(2154))
  })
})

test_that("seq_roadway() layers contain identifier", {
  with_seq_cache({

    local_mocked_bindings(
      seq_read = function(key, ...) {
        if (key == "parca") return(p)
        if (key == "v.cad.vides.poly") return(mock_voids)
      }
    )

    paths <- seq_roadway(
      source = "v.cad.vides.poly",
      dirname = seq_cache,
      verbose = FALSE,
      overwrite = TRUE
    )

    layers <- lapply(paths, sf::read_sf)

    identifier <- seq_field("identifier")$name

    expect_all_true(
      vapply(layers, \(x) identifier %in% names(x), TRUE)
    )
  })
})

test_that("seq_roadway() doesn't write when no features exist", {
  with_seq_cache({

    called <- 0

    local_mocked_bindings(
      seq_read = function(key, ...) {
        if (key == "parca") return(p)
        if (key == "v.cad.vides.poly") return(Rsequoia2:::seq_poly)
      },
      get_roadway_poly = function(...) {
        sf::st_sf(
          geometry = sf::st_sfc(crs = 2154)
        )
      },
      seq_write = function(...) called <<- called + 1
    )

    paths <- seq_roadway(
      source = "v.cad.vides.poly",
      dirname = seq_cache,
      verbose = FALSE
    )

    expect_null(paths)
    expect_equal(called, 0)
  })
})

test_that("seq_roadway() fails with invalid source", {
  with_seq_cache({

    expect_error(
      seq_roadway(
        source = "invalid.layer",
        dirname = seq_cache,
        verbose = FALSE
      ),
      "source"
    )

  })
})

test_that("seq_roadway() appends PN road lines when available", {
  with_seq_cache({

    pn <- Rsequoia2:::seq_line
    pn[[seq_field("type")$name]] <- "PN"

    local_mocked_bindings(
      seq_read = function(key, ...) {
        if (key == "parca") return(p)
        if (key == "v.cad.vides.poly") return(mock_voids)
        if (key == "v.road.line") return(mock_roads)
      }
    )

    paths <- seq_roadway(
      source = "v.cad.vides.poly",
      dirname = seq_cache,
      verbose = FALSE,
      overwrite = TRUE
    )

    line <- sf::read_sf(paths[[1]])

    expect_true(nrow(line) > 0)
  })
})
