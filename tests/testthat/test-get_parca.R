test_that("get_parca() returns ETALAB parcels", {
  idu <- c("idu1", "idu2", "idu3")

  idu_field <- seq_field("idu")$name
  source_field <- seq_field("source")$name

  etalab <- Rsequoia2:::seq_poly
  etalab[[idu_field]] <- idu
  etalab[[source_field]] <- "etalab"

  calls <- list(etalab = 0, lieu_dit = 0)

  local_mocked_bindings(
    get_parca_etalab = function(...) {
      calls$etalab <<- calls$etalab + 1
      etalab
    },
    get_lieux_dits = function(...) {
      calls$lieu_dit <<- calls$lieu_dit + 1
      stop("get_lieux_dits() should not be called", call. = FALSE)
    }
  )

  res <- get_parca(idu = idu, lieu_dit = FALSE, verbose = FALSE)

  expect_s3_class(res, "sf")
  expect_shape(res, nrow = 3)

  expect_equal(sf::st_crs(res)$epsg, 2154)
  expect_equal(res[[idu_field]], idu)
  expect_all_equal(res[[source_field]], "etalab")

  expect_equal(calls$etalab, 1)
  expect_equal(calls$lieu_dit, 0)
})

test_that("get_parca() joins lieux-dits when lieu_dit = TRUE", {
  idu <- c("idu1", "idu2", "idu3")
  lieu <- c("lieu1", "lieu2", "lieu3")

  idu_field <- seq_field("idu")$name
  locality_field <- seq_field("locality")$name

  etalab <- Rsequoia2:::seq_poly
  etalab[[idu_field]] <- idu

  lieux_dits <- Rsequoia2:::seq_poly
  lieux_dits[[locality_field]] <- lieu

  calls <- list(etalab = 0, lieu_dit = 0)

  local_mocked_bindings(
    get_parca_etalab = function(...) {
      calls$etalab <<- calls$etalab + 1
      etalab
    },
    get_lieux_dits = function(...) {
      calls$lieu_dit <<- calls$lieu_dit + 1
      lieux_dits
    }
  )

  res <- get_parca(idu = idu, lieu_dit = TRUE, verbose = FALSE)

  expect_s3_class(res, "sf")
  expect_shape(res, nrow = 3)

  expect_equal(res[[idu_field]], idu)
  expect_equal(res[[locality_field]], lieu)

  expect_equal(calls$etalab, 1)
  expect_equal(calls$lieu_dit, 1)
})

test_that("get_parca() is verbose when joining lieux-dits", {
  idu <- c("idu1", "idu2", "idu3")
  lieu <- c("lieu1", "lieu2", "lieu3")

  idu_field <- seq_field("idu")$name
  locality_field <- seq_field("locality")$name

  etalab <- Rsequoia2:::seq_poly
  etalab[[idu_field]] <- idu

  lieux_dits <- Rsequoia2:::seq_poly
  lieux_dits[[locality_field]] <- lieu

  local_mocked_bindings(
    get_parca_etalab = function(...) etalab,
    get_lieux_dits = function(...) lieux_dits
  )

  expect_message(
    get_parca(idu = idu, lieu_dit = TRUE, verbose = TRUE),
    "Downloading and joining Lieux dits..."
  )

  expect_message(
    get_parca(idu = idu, lieu_dit = TRUE, verbose = TRUE),
    "Lieux dits joined."
  )
})
