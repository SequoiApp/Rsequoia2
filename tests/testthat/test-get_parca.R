test_that("get_parca() works with bdp_geom = F & get_lieux_dit = F", {

  etalab <- Rsequoia2:::seq_poly
  idu <- c("idu1", "idu2", "idu3")

  idu_field <- seq_field("idu")$name
  etalab[[idu_field]] <- idu

  source_field <- seq_field("source")$name
  etalab[[source_field]] <- "etalab"

  calls <- list(etalab = 0, bdp = 0)
  local_mocked_bindings(
    get_parca_etalab = function(idu) {calls$etalab <<- calls$etalab + 1; etalab},
    get_parca_bdp = function(idu) {calls$bdp <<- calls$bdp + 1}
  )

  res <- get_parca(idu = idu, bdp_geom = FALSE, lieu_dit = FALSE, verbose = FALSE)

  expect_s3_class(res, "sf")
  expect_shape(res, nrow = 3)

  expect_equal(sf::st_crs(res)$epsg, 2154)

  expect_equal(idu, res[[idu_field]])
  expect_all_equal(res[[source_field]], "etalab")

  expect_equal(calls$etalab, 1)
  expect_equal(calls$bdp, 0)

})

test_that("get_parca() works with bdp_geom = T & get_lieux_dit = F", {

  idu_field <- seq_field("idu")$name
  source_field <- seq_field("source")$name
  idu <- c("idu1", "idu2", "idu3")

  etalab <- Rsequoia2:::seq_poly
  etalab[[idu_field]] <- idu
  etalab[[source_field]] <- "etalab"

  bdp <- Rsequoia2:::seq_point
  bdp$idu <- c(idu[1:2], "other_idu") # /!\ "idu" is name returned by get_parca_bdp
  bdp[[source_field]] <- "bdp"

  calls <- list(etalab = 0, bdp = 0)
  local_mocked_bindings(
    get_parca_etalab = function(idu) {calls$etalab <<- calls$etalab + 1; etalab},
    get_parca_bdp = function(idu) {calls$bdp <<- calls$bdp + 1; bdp}
  )

  res <- get_parca(idu = idu, bdp_geom = TRUE, lieu_dit = FALSE, verbose = FALSE)

  expect_s3_class(res, "sf")
  expect_shape(res, nrow = 3)

  expect_equal(idu, res[[idu_field]])
  expect_equal(res[[source_field]], c("bdp", "bdp", "etalab"))

  expect_equal(
    as.character(sf::st_geometry_type(res)),
    c("POINT", "POINT", "POLYGON")
  )

  expect_equal(calls$etalab, 1)
  expect_equal(calls$bdp, 1)

})

test_that("get_parca() is verbose with bdp_geom = T & get_lieux_dit = F", {

  idu_field <- seq_field("idu")$name
  source_field <- seq_field("source")$name
  idu <- c("idu1", "idu2", "idu3")

  etalab <- Rsequoia2:::seq_poly
  etalab[[idu_field]] <- idu
  etalab[[source_field]] <- "etalab"

  bdp <- Rsequoia2:::seq_point
  bdp$idu <- c(idu[1:2], "other_idu") # /!\ "idu" is name returned by get_parca_bdp
  bdp[[source_field]] <- "bdp"

  calls <- list(etalab = 0, bdp = 0)
  local_mocked_bindings(
    get_parca_etalab = function(idu) {calls$etalab <<- calls$etalab + 1; etalab},
    get_parca_bdp = function(idu) {calls$bdp <<- calls$bdp + 1; bdp}
  )

  expect_message(
    get_parca(idu = idu, bdp_geom = TRUE, lieu_dit = FALSE, verbose = TRUE),
    "Downloading BDP from IGN..."
  ) |> suppressMessages()

  expect_message(
    get_parca(idu = idu, bdp_geom = TRUE, lieu_dit = FALSE, verbose = TRUE),
    "2 of 3 ETALAB geom successfully replaced with BDP geom"
  ) |> suppressMessages()

})

test_that("get_parca() works with bdp_geom = F & get_lieux_dit = T", {

  idu_field <- seq_field("idu")$name
  locality_field <- seq_field("locality")$name
  idu <- c("idu1", "idu2", "idu3")

  etalab <- Rsequoia2:::seq_poly
  etalab[[idu_field]] <- idu

  lieu <-  c("lieu1", "lieu2", "lieu3")
  lieu_dit <- Rsequoia2:::seq_poly
  lieu_dit[[locality_field]] <- lieu

  calls <- list(etalab = 0, lieu_dit = 0)
  local_mocked_bindings(
    get_parca_etalab = function(idu) {calls$etalab <<- calls$etalab + 1; etalab},
    get_lieux_dits = function(idu) {calls$lieu_dit <<- calls$lieu_dit + 1; lieu_dit}
  )

  res <- get_parca(idu = idu, bdp_geom = FALSE, lieu_dit = TRUE, verbose = FALSE)

  expect_s3_class(res, "sf")
  expect_shape(res, nrow = 3)

  expect_equal(lieu, res[[locality_field]])

  expect_equal(calls$etalab, 1)
  expect_equal(calls$lieu_dit, 1)

})

test_that("get_parca() is verbose with bdp_geom = F & get_lieux_dit = T", {

  idu_field <- seq_field("idu")$name
  locality_field <- seq_field("locality")$name
  idu <- c("idu1", "idu2", "idu3")

  etalab <- Rsequoia2:::seq_poly
  etalab[[idu_field]] <- idu

  lieu <-  c("lieu1", "lieu2", "lieu3")
  lieu_dit <- Rsequoia2:::seq_poly
  lieu_dit[[locality_field]] <- lieu

  calls <- list(etalab = 0, lieu_dit = 0)
  local_mocked_bindings(
    get_parca_etalab = function(idu) {calls$etalab <<- calls$etalab + 1; etalab},
    get_lieux_dits = function(idu) {calls$lieu_dit <<- calls$lieu_dit + 1; lieu_dit}
  )

  expect_message(
    get_parca(idu = idu, bdp_geom = FALSE, lieu_dit = TRUE, verbose = TRUE),
    "Downloading and joining Lieux dits..."
  ) |> suppressMessages()

  expect_message(
    get_parca(idu = idu, bdp_geom = FALSE, lieu_dit = TRUE, verbose = TRUE),
    "Lieux dits joined."
  ) |> suppressMessages()

})

test_that("get_parca() warn when get_parca_bdp() return error", {

  idu_field <- seq_field("idu")$name
  source_field <- seq_field("source")$name
  idu <- c("idu1", "idu2", "idu3")

  etalab <- Rsequoia2:::seq_poly
  etalab[[idu_field]] <- idu
  etalab[[source_field]] <- "etalab"

  calls <- list(etalab = 0, bdp = 0)
  local_mocked_bindings(
    get_parca_etalab = function(...) {calls$etalab <<- calls$etalab + 1; etalab},
    get_parca_bdp = function(...) stop("test", call. = FALSE)
  )

  expect_warning(
    res <- get_parca(idu = idu, bdp_geom = TRUE, lieu_dit = FALSE, verbose = FALSE),
    "BDP not available, ETALAB geom only is used."
  )

  expect_s3_class(res, "sf")
  expect_shape(res, nrow = 3)
  expect_all_equal(res[[source_field]], "etalab")

})


