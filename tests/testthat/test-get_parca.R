test_that("get_parca() works (local only)", {

  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  idu <- "33103000AB0060"  # Tiny commune: 33103 (Castelmoron-d'Albret)

  out <- get_parca(idu, verbose = FALSE)
  expect_s3_class(out, "sf")
  expect_snapshot(str(sf::st_drop_geometry(out)))

  out <- get_parca(idu, lieu_dit = TRUE, verbose = FALSE)
  expect_s3_class(out, "sf")
  expect_snapshot(str(sf::st_drop_geometry(out)))

  out <- get_parca(idu, bdp_geom = FALSE, verbose = FALSE)
  expect_s3_class(out, "sf")
  expect_snapshot(str(sf::st_drop_geometry(out)))

})

test_that("get_parca() add bdp geom when possible", {

  fake_etalab <- sf::st_sf(
    idu = 1:2,
    commune = "29158",
    geometry = sf::st_sfc(replicate(2, sf::st_point(c(1, 1)), FALSE))
  )

  geom_bdp <- sf::st_sfc(sf::st_point(c(2, 2)))
  replaced_idu <- 2
  fake_bdp <- sf::st_sf(
    idu = replaced_idu,
    geometry = geom_bdp
  )

  local_mocked_bindings(
    get_parca_etalab = function(idu) fake_etalab ,
    get_parca_bdp = function(idu) fake_bdp
  )

  res <- get_parca(idu = 1:2, bdp_geom = TRUE, verbose = FALSE)
  expect_identical(res$geometry[replaced_idu,], geom_bdp)
})

test_that("get_parca() does not replace geometry when BDP has no matching IDU", {

  fake_etalab <- sf::st_sf(
    idu = 1,
    commune = "29158",
    geometry = sf::st_sfc(sf::st_point(c(1,1)))
  )

  fake_bdp <- sf::st_sf(
    idu = 999,
    geometry = sf::st_sfc(sf::st_point(c(9,9)))
  )

  local_mocked_bindings(
    get_parca_etalab = function(idu) fake_etalab,
    get_parca_bdp = function(idu) fake_bdp,
  )

  res <- get_parca(idu = 1, bdp_geom = TRUE, verbose = FALSE)
  expect_true(all(sf::st_coordinates(res$geometry) == c(1,1)))
})

test_that("get_parca() warns about missing IDU", {

  fake_etalab <- sf::st_sf(
    idu = 1,
    commune = "29158",
    geometry = sf::st_sfc(sf::st_point(c(1,1)))
  )

  local_mocked_bindings(
    get_parca_etalab = function(idu) fake_etalab
  )

  # missing idu = 2
  expect_warning(
    get_parca(idu = 1:2, bdp_geom = FALSE, verbose = FALSE),
    regexp = "Geometry not found"
  )

})

test_that("get_parca() joins lieux-dits only when enabled", {

  fake_etalab <- sf::st_sf(
    idu = 1,
    commune = "29158",
    geometry = sf::st_sfc(sf::st_point(c(1,1)))
  )

  fake_lieux <- sf::st_sf(
    idu = 1,
    lieu_dit = "TEST",
    geometry = sf::st_sfc(sf::st_point(c(1,1)))
  )

  local_mocked_bindings(
    get_parca_etalab = function(idu) fake_etalab,
    get_lieux_dits = function(idu) fake_lieux
  )

  # When TRUE
  res1 <- get_parca(idu = 1, bdp_geom = FALSE, lieu_dit = TRUE, verbose = FALSE)
  lieu_dit_field <- seq_field("lieu_dit")$name
  expect_equal(res1[[lieu_dit_field]], "TEST")

  # When FALSE
  res2 <- get_parca(idu = 1, bdp_geom = FALSE,lieu_dit = FALSE, verbose = FALSE)
  expect_true(is.na(res2[[lieu_dit_field]]))
})

test_that("get_parca() updates source field when BDP geometry replaces ETALAB", {

  source_field <- seq_field("source")$name

  fake_etalab <- sf::st_sf(
    idu = c(1, 2),
    commune = "29158",
    geometry = sf::st_sfc(
      sf::st_point(c(1, 1)),
      sf::st_point(c(1, 1))
    )
  )
  fake_etalab[[source_field]] <- "etalab"

  fake_bdp <- sf::st_sf(
    idu = 2,
    geometry = sf::st_sfc(sf::st_point(c(2, 2)))
  )
  fake_bdp[[source_field]] <- "bdp"

  local_mocked_bindings(
    get_parca_etalab = function(idu) fake_etalab,
    get_parca_bdp    = function(idu) fake_bdp
  )

  res <- get_parca(idu = c(1, 2), bdp_geom = TRUE, verbose = FALSE)

  # IDU 2 → replaced
  expect_equal(res[[source_field]], c("etalab", "bdp"))

})
