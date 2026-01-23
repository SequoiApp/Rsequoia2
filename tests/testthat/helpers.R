# Helper: fake matrice used only for tests (not exported)

fake_matrice <- function(
    id = "FOREST", prop = "OWNER",
    insee = "29158", prefix = "000",
    section = "ZR", numero = "0003",
    lieu_dit = "LIEU_DIT"
) {

  matrice <- data.frame(
    IDENTIFIANT = id,
    PROPRIETAIRE = prop,
    INSEE = pad_left(insee, 5),
    PREFIXE = pad_left(prefix, 3),
    SECTION = pad_left(section, 2),
    NUMERO = pad_left(numero, 4),
    LIEU_DIT = lieu_dit,
    stringsAsFactors = FALSE
  ) |> seq_normalize("matrice")

  return(matrice)
}


