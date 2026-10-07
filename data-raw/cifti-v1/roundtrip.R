args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
root <- normalizePath(args[[1]], mustWork = TRUE)
devtools::load_all()
cases <- jsonlite::read_json(file.path(root, "cases.json"), simplifyVector = TRUE)
for (name in cases) {
  source <- read_cifti(file.path(root, name))
  write_cifti(source, file.path(root, paste0("roundtrip-", name)))
  cat(name, "roundtrip written\n")
}
