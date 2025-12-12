# Helper: fake matrice used only for tests (not exported)

fake_matrice <- function(
    id = "FOREST", prop = "OWNER",
    insee = "29158", prefix = "000",
    section = "ZR", numero = "0003",
    lieu_dit = "LIEU_DIT", tx_boisee = 0.8
) {

  matrice <- data.frame(
    IDENTIFIANT = id,
    PROPRIETAIRE = prop,
    INSEE = pad_left(insee, 5),
    PREFIXE = pad_left(prefix, 3),
    SECTION = pad_left(section, 2),
    N_PARCA = pad_left(numero, 4),
    LIEU_DIT = lieu_dit,
    TX_BOISEE = as.numeric(tx_boisee),
    stringsAsFactors = FALSE
  ) |> seq_normalize("matrice")

  return(matrice)
}

# Helper: fake parca used only for tests (not exported)
fake_raw_parca <- function(
    insee = "29158", prefix = "000",
    section = "ZR", numero = "0003",
    contenance = 50, lieu_dit = "LIEU_DIT",
    dep = 29,
    geom = sf::st_point(c(-0.0116, 44.6794)) |> sf::st_sfc(crs = 2154)
){

  sf::st_sf(
    IDU        = paste0(insee, prefix, section, numero),
    REG_NOM    = "REG_NOM",
    REG_NUM    = "01",
    DEP_NOM    = "DEP_NOM",
    DEP_NUM    = dep,
    COM_NOM    = "COM_TEST",
    COM_NUM    = insee,
    PREFIXE    = prefix,
    SECTION    = section,
    N_PARCA     = numero,
    LIEU_DIT   = lieu_dit,
    SURF_CA    = contenance,
    geometry   = geom
  ) |> suppressWarnings()
}


# Helper: fake parca used only for tests (not exported)
fake_parca <- function(...){

  fake_raw_parca(...) |>
    transform(
      "IDENTIFIANT" = "FAKE",
      "PROPRIETAIRE" = "OWNER",
      "OCCUP_SOL"  = "BOISEE") |>
    suppressWarnings()
}
