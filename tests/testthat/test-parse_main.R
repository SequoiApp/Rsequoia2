test_that("parse_main() works on real life exemple", {
  l0 <- "03 WD 57 BOIS DE LA GARENNE B007 1 671A BS 01 Taillis sous futaies 22 21 15 214,66 C TA 64,40 30"
  expect_equal(
    parse_main(l0),
    data.frame(
      an = "03", prefix = "", section = "WD", numero = "57", lieu_dit = "BOIS DE LA GARENNE",
      rivoli = "B007", fp = "1", s_par = "671A", gr = "BS", classe = "01",
      nature = "Taillis sous futaies", contenance = 22.2115, type = "main"
    )
  )

  l1 <- "03 WD 4 EN TRAVAILLE B094 1 8 52 00"
  expect_equal(
    parse_main(l1),
    data.frame(
      an = "03", prefix = "", section = "WD", numero = "4", lieu_dit = "EN TRAVAILLE",
      rivoli = "B094", fp = "1", s_par = "", gr = "", classe = "",
      nature = "", contenance = 8.52, type = "main"
    )
  )

  l2 <- "03 WD 5 BOIS DE LA GARENNE B007 1 35 13 65"
  expect_equal(
    parse_main(l2),
    data.frame(
      an = "03", prefix = "", section = "WD", numero = "5", lieu_dit = "BOIS DE LA GARENNE",
      rivoli = "B007", fp = "1", s_par = "", gr = "", classe = "",
      nature = "", contenance = 35.1365, type = "main"
    )
  )

  l3 <- "16 A 215 LES LAIRES DE MAIZIERES-SU B033 1 221A BT 03 Taillis simples 09 42 0,13 C TA 0,04 30"
  expect_equal(
    parse_main(l3),
    data.frame(
      an = "16", prefix = "", section = "A", numero = "215", lieu_dit = "LES LAIRES DE MAIZIERES-SU",
      rivoli = "B033", fp = "1", s_par = "221A", gr = "BT", classe = "03",
      nature = "Taillis simples", contenance = 0.0942, type = "main"
    )
  )

  l4 <- "16 D 399 LA COTE DE ROUGENOUX B007 0193 1 221A BT 03 Taillis simples 05 76 0,08 C TA 0,02 30"
  expect_equal(
    parse_main(l4),
    data.frame(
      an = "16", prefix = "", section = "D", numero = "399", lieu_dit = "LA COTE DE ROUGENOUX",
      rivoli = "B007", fp = "1", s_par = "221A", gr = "BT", classe = "03",
      nature = "Taillis simples", contenance = 0.0576, type = "main"
    )
  )

  l5 <- "71 AC 19 LE VILLAGE SUD B058 1 221A S Sols 09 01 0,00"
  expect_equal(
    parse_main(l5),
    data.frame(
      an = "71", prefix = "", section = "AC", numero = "19", lieu_dit = "LE VILLAGE SUD",
      rivoli = "B058", fp = "1", s_par = "221A", gr = "S", classe = "",
      nature = "Sols", contenance = 0.0901, type = "main"
    )
  )

  l6 <- "78 AC 110 LE VILLAGE SUD B058 0014 1 221A T 01 Terres 06 0,08 C TA 0,02 30"
  expect_equal(
    parse_main(l6),
    data.frame(
      an = "78", prefix = "", section = "AC", numero = "110", lieu_dit = "LE VILLAGE SUD",
      rivoli = "B058", fp = "1", s_par = "221A", gr = "T", classe = "01",
      nature = "Terres", contenance = 0.0006, type = "main"
    )
  )

  l7 <- "71 AC 119 35 RUE AUX CHENES 0030 0022 1 221A S Sols 11 42 0,00"
  expect_equal(
    parse_main(l7),
    data.frame(
      an = "71", prefix = "", section = "AC", numero = "119", lieu_dit = "35 RUE AUX CHENES",
      rivoli = "0030", fp = "1", s_par = "221A", gr = "S", classe = "",
      nature = "Sols", contenance = 0.1142, type = "main"
    )
  )

  l8 <- "17 D 217 LA COTELLE B010 1 221A BT 03 Taillis simples 12 61 0,19 C TA 0,06 30"
  expect_equal(
    parse_main(l8),
    data.frame(
      an = "17", prefix = "", section = "D", numero = "217", lieu_dit = "LA COTELLE",
      rivoli = "B010", fp = "1", s_par = "221A", gr = "BT", classe = "03",
      nature = "Taillis simples", contenance = 0.1261, type = "main"
    )
  )

  l9 <- "19 A 864 MONGESSEY B078 1 196A S Sols 02 90 0,00"
  expect_equal(
    parse_main(l9),
    data.frame(
      an = "19", prefix = "", section = "A", numero = "864", lieu_dit = "MONGESSEY",
      rivoli = "B078", fp = "1", s_par = "196A", gr = "S", classe = "",
      nature = "Sols", contenance = 0.029, type = "main"
    )
  )

  l10 <- "04 368 B 656 AMPLEMONT EST B001 1 274B BS 02 Taillis sous futaies 121 58 04 1 510,46 C TA 302,09 20"
  expect_equal(
    parse_main(l10),
    data.frame(
      an = "04", prefix = "368", section = "B", numero = "656", lieu_dit = "AMPLEMONT EST",
      rivoli = "B001", fp = "1", s_par = "274B", gr = "BS", classe = "02",
      nature = "Taillis sous futaies", contenance = 121.5804, type = "main"
    )
  )

  l11 <- "04 368 B 676 FERME DE VILLIERS B125 1 274B E 01 Eaux 6 21 52 259,88 C TA 51,98 20"
  expect_equal(
    parse_main(l11),
    data.frame(
      an = "04", prefix = "368", section = "B", numero = "676", lieu_dit = "FERME DE VILLIERS",
      rivoli = "B125", fp = "1", s_par = "274B", gr = "E", classe = "01",
      nature = "Eaux", contenance = 6.2152, type = "main"
    )
  )

  l12 <-"04 368 B 683 FERME DE VILLIERS B125 1 274B L 01 Landes 12 13 03 67,76 C TA 13,55 20"
  expect_equal(
    parse_main(l12),
    data.frame(
      an = "04", prefix = "368", section = "B", numero = "683", lieu_dit = "FERME DE VILLIERS",
      rivoli = "B125", fp = "1", s_par = "274B", gr = "L", classe = "01",
      nature = "Landes", contenance = 12.1303, type = "main"
    )
  )

  l13 <- "24 B 9 LANDE QUELAINS LA C950 1 105A BT 06 Taillis simples 43 40 0,42 C TA 0,08 20"
  expect_equal(
    parse_main(l13),
    data.frame(
      an = "24", prefix = "", section = "B", numero = "9", lieu_dit = "LANDE QUELAINS LA",
      rivoli = "C950", fp = "1", s_par = "105A", gr = "BT", classe = "06",
      nature = "Taillis simples", contenance = 0.4340, type = "main"
    )
  )

})

test_that("parse_main() always returns a 1-row data.frame with expected columns", {
  res <- parse_main("03 WD 57 BOIS DE LA GARENNE B007 1 35 13 65")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)

  expect_named(
    res,
    c("an","prefix","section","numero","lieu_dit","rivoli",
      "fp","s_par","gr","classe","nature","contenance","type")
  )
})

test_that("parse_main() column types are stable", {
  res <- parse_main("03 WD 5 BOIS DE LA GARENNE B007 1 35 13 65")

  expect_type(res$an, "character")
  expect_type(res$prefix, "character")
  expect_type(res$section, "character")
  expect_type(res$numero, "character")
  expect_type(res$lieu_dit, "character")
  expect_type(res$rivoli, "character")
  expect_type(res$fp, "character")
  expect_type(res$s_par, "character")
  expect_type(res$gr, "character")
  expect_type(res$classe, "character")
  expect_type(res$nature, "character")
  expect_type(res$contenance, "double")
  expect_type(res$type, "character")
})

test_that("parse_main() fills missing optional fields with empty strings", {
  res <- parse_main("03 WD 4 EN TRAVAILLE B094 1 8 52 00")

  expect_identical(res$s_par, "")
  expect_identical(res$gr, "")
  expect_identical(res$classe, "")
  expect_identical(res$nature, "")
})

test_that("parse_main() preserves prefix when present", {
  res <- parse_main("04 368 B 656 AMPLEMONT EST B001 1 274B BS 02 Taillis sous futaies 121 58 04 1 510,46")
  expect_identical(res$prefix, "368")
})

test_that("parse_main() handles apostrophes in nature and lieu_dit", {
  x <- "26 A 1 TERRES D'ESSAI B001 1 221A S Sols d'agrement 04 10 0,00"
  res <- parse_main(x)

  expect_match(res$nature, "d'agrement")
})
