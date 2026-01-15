with_seq_cache <- function(code) {
  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache, showWarnings = FALSE)
  assign("seq_cache", seq_cache, envir = parent.frame())

  ext <- system.file("extdata", package = "Rsequoia2")

  m_path <- list.files(ext, pattern = "matrice", full.names = TRUE, ignore.case = T)
  m <- openxlsx2::read_xlsx(m_path)
  assign("m", m, envir = parent.frame())
  file.copy(m_path, seq_cache)

  p_path <- list.files(ext, pattern = "parca", full.names = TRUE, ignore.case = T)
  p <- sf::read_sf(p_path)
  assign("p", p, envir = parent.frame())
  seq_write(x = p, key = "parca", dirname = seq_cache)

  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  force(code)
}
