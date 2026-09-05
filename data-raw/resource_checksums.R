# Run from the repository root after changing a bundled dataset. R's package
# installer may convert .rda files to a lazy-load database; retain the checksums
# of the source artifacts independently of that installation detail.
stopifnot("sha256sum" %in% getNamespaceExports("tools"))
paths <- c("data/olsen_mtl.rda", "data/fsaverage.rda")
manifest <- data.frame(resource = paths,
                       sha256 = unname(tools::sha256sum(paths)))
utils::write.csv(manifest, "inst/extdata/resource_checksums.csv", row.names = FALSE)
