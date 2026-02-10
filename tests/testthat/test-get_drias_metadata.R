test_that("get_drias_metadata works on a local temporary file", {

  txt <- tempfile(fileext = ".txt")
  on.exit(unlink(txt))

  writeLines(
    c(
      "# Date d'extraction : 22/12/2025 - 09h18 loc.",
      "# Producteur : MF-DCSC",
      "# Experience : EUROCORDEX2020",
      "# Modele : RACMO22E_CNRM-CM5",
      "#",
      "# Scenario",
      "# RCP8.5 : Emissions non reduites",
      "# -----------------------------------------------------------------------------",
      "# Horizons",
      "# H1 : Horizon proche",
      "# H2 : Horizon moyen",
      "# H3 : Horizon lointain",
      "# -----------------------------------------------------------------------------",
      "# Type d'indice",
      "# Indices",
      "# NORTAV : Temperature moyenne journaliere (°C)",
      "# NORTR  : Precipitations journalières moyennes (mm)",
      "# -----------------------------------------------------------------------------",
      "# Format des enregistrements",
      "Point;Latitude;Longitude;NORTAV;NORTR;"
    ),
    txt,
    useBytes = TRUE
  )

  meta <- get_drias_metadata(txt)

  expect_type(meta, "list")
  expect_named(
    meta,
    c(
      "date_extraction",
      "producteur",
      "experience",
      "modele",
      "scenario",
      "horizons",
      "indices"
    )
  )

  expect_equal(meta$producteur, "MF-DCSC")
  expect_equal(meta$scenario$code, "RCP8.5")
  expect_equal(nrow(meta$horizons), 3L)
  expect_true(all(meta$horizons$code %in% c("H1", "H2", "H3")))
  expect_true(all(meta$indices$code %in% c("NORTAV", "NORTR")))
})

test_that("txt validation fails early and cleanly", {

  expect_error(
    get_drias_metadata(42),
    class = "drias_invalid_txt"
  )

  expect_error(
    get_drias_metadata(NA_character_),
    class = "drias_invalid_txt"
  )

  expect_error(
    get_drias_metadata(""),
    class = "drias_invalid_txt"
  )

  expect_error(
    get_drias_metadata("does_not_exist.txt"),
    class = "drias_file_not_found"
  )
})
