path <- "C:/Users/PaulCarteron/OneDrive - Racines/personnel/Rsequoia2/inst/extdata"
res <- seq_parca(path, overwrite = TRUE)
file.copy(res, path)

dir <- file.path(path, seq_layer("parca")$path)
unlink(dir, recursive = TRUE, force = TRUE)
