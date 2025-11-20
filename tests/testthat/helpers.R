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
    NUMERO = pad_left(numero, 4),
    LIEU_DIT = lieu_dit,
    TX_BOISEE = as.numeric(tx_boisee),
    stringsAsFactors = FALSE
  )

  return(matrice)
}

# Helper: fake parca used only for tests (not exported)
fake_parca <- function(
    idu = "29158000ZR0003",
    contenance = 50,
    geom = sf::st_point(c(-0.0116, 44.6794)) |> sf::st_sfc(crs = 4326)
){

  sf::st_sf(
    IDU        = idu,
    IDENTIFIANT = "FAKE",
    PROPRIETAIRE = "OWNER",
    REG_NOM    = "REG_NOM",
    REG_NUM    = "01",
    DEP_NOM    = "DEP_NOM",
    DEP_NUM    = "33",
    COM_NOM    = "COM_TEST",
    COM_NUM    = "33103",
    PREFIXE    = "000",
    SECTION    = "AB",
    NUMERO     = "0060",
    LIEU_DIT   = "LIEU_DIT",
    OCCUP_SOL  = "BOISEE",
    CONTENANCE = contenance,
    geometry   = geom
  ) |> suppressWarnings()
}
