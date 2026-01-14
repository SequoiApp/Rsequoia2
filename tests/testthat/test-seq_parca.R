test_that("seq_parca() works with mocked get_parca()", {

  with_seq_cache({

    local_mocked_bindings(
      get_parca = function(...) p,
      read_matrice = function(...) m,
    )

    parca_path <- seq_parca(dirname = seq_cache, verbose = FALSE, overwrite = TRUE)
    expect_true(file.exists(parca_path))

    parca <- sf::read_sf(parca_path)
    expect_s3_class(parca, "sf")
  })

})

test_that("seq_parca() correct lieu_dit when available ", {

  with_seq_cache({

    lieu_dit <- seq_field("locality")$name
    m[[lieu_dit]] <- NA_character_
    m[[lieu_dit]][1] <- "MATRICE"

    p[[lieu_dit]] <- "PARCA"

    local_mocked_bindings(
      get_parca = function(...) p,
      read_matrice = function(...) m,
    )

    parca_path <- seq_parca(dirname = seq_cache, verbose = F, overwrite = TRUE)
    parca <- sf::read_sf(parca_path)

    expect_all_true(c("MATRICE", "PARCA") %in% parca[[lieu_dit]])
  })

})

test_that("seq_parca() correclty add id from matrice", {

  with_seq_cache({

    id <- seq_field("identifier")$name
    m[[id]] <- "MATRICE_ID"

    local_mocked_bindings(
      get_parca = function(...) p,
      read_matrice = function(...) m,
    )

    parca_path <- seq_parca(dirname = seq_cache, verbose = F, overwrite = TRUE)
    parca <- sf::read_sf(parca_path)

    expect_all_true(parca[[id]] == "MATRICE_ID")
  })

})


