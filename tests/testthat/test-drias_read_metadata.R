test_that("drias_read_metadata works on a local temporary file", {

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

  meta <- drias_read_metadata(txt)

  expect_type(meta, "list")
  expect_named(meta,
    c("date_extraction", "producteur", "experience", "modele",
      "scenario", "horizon", "indices"
    )
  )

  expect_equal(meta$producteur, "MF-DCSC")
  expect_equal(meta$scenario$code, "RCP8.5")
  expect_equal(nrow(meta$horizon), 3L)
  expect_all_true(meta$horizon$code %in% c("H1", "H2", "H3"))
  expect_all_true(meta$indices$code %in% c("NORTAV", "NORTR"))
})

test_that("txt validation fails early and cleanly", {

  expect_error(drias_read_metadata(42), "must be a character string")
  expect_error(drias_read_metadata(NA_character_), "must be a non-empty character")
  expect_error(drias_read_metadata(""), "must be a non-empty character string")
  expect_error(drias_read_metadata("does_not_exist.txt"),"File not found")

})

test_that("missing sections do not crash", {

  txt <- tempfile(fileext = ".txt")
  on.exit(unlink(txt))

  writeLines(
    c(
      "# Date d'extraction : 22/12/2025",
      "# Producteur : MF-DCSC"
    ),
    txt
  )

  meta <- drias_read_metadata(txt)

  expect_type(meta, "list")
  expect_true(is.data.frame(meta$scenario))
  expect_true(is.data.frame(meta$horizon))
  expect_true(is.data.frame(meta$indices))
})
