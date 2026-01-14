seq_test_project <- function(with_matrice = TRUE, with_parca = TRUE) {

  seq_dir <- file.path(tempdir(), "seq")
  ext <- system.file("extdata", package = "Rsequoia2")

  if (with_matrice) {
    matrice <- list.files(ext, pattern = "matrice", full.names = TRUE, ignore.case = T)
    file.copy(matrice, seq_dir)
  }

  if (with_parca) {
    parca <- list.files(ext, pattern = "parca", full.names = TRUE, ignore.case = T)

    cfg_path <- system.file("config/seq_path.yaml", package = "Rsequoia2")
    cfg <- yaml::read_yaml(cfg_path)
    ns <- cfg$namespace
    idx <- startsWith("v.seq.parca.poly", names(ns))
    family <- ns[idx][[1]]
    path <- cfg$path[[family]]
    dir.create(file.path(seq_dir, path))
    file.copy(parca, file.path(seq_dir, path))
  }

  return(dir)
}
