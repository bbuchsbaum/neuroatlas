#!/usr/bin/env Rscript
# Analytic admission gate for the installed compiled surface engine.
# Usage: Rscript check-surface-engine.R receipt.json
# Exit 1 means the engine is not admissible; preserve that receipt.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, !file.exists(args[[1]]))
stopifnot(requireNamespace("neurotransform", quietly = TRUE))
vertices <- rbind(c(1, 0, 0), c(-1, 0, 0), c(0, 1, 0), c(0, -1, 0),
                  c(0, 0, 1), c(0, 0, -1))
faces <- rbind(c(1, 3, 5), c(3, 2, 5), c(2, 4, 5), c(4, 1, 5),
               c(3, 1, 6), c(2, 3, 6), c(4, 2, 6), c(1, 4, 6))
query <- neurotransform::surface_mesh(matrix(c(1, 1, 1) / sqrt(3), nrow = 1))
orders <- list(original = seq_len(8), reversed = 8:1)
# Ray from the origin meets the positive octant face at (1,1,1)/3.
# Thus equal barycentric weights give z=1/3. Face row order cannot change it.
expected <- 1 / 3
tolerance <- 1e-12
results <- lapply(orders, function(order) {
  mesh <- neurotransform::surface_mesh(vertices, faces[order, ])
  plan <- neurotransform::surface_resampling_plan(query, mesh,
                                                 method = "barycentric")
  list(value = neurotransform::apply_surface_resampling(plan, vertices[, 3]),
       contributing_vertices = plan$cols, weights = plan$vals)
})
values <- vapply(results, `[[`, numeric(1), "value")
passed <- all(is.finite(values)) && all(abs(values - expected) < tolerance)
dll <- getLoadedDLLs()[["neurotransform"]][["path"]]
description <- utils::packageDescription("neurotransform")
receipt <- list(
  schema = "neuroatlas.surface-engine-probe.v1", passed = passed,
  claim = "Spherical barycentric interpolation is local and face-order invariant",
  expected = expected, tolerance = tolerance, results = results,
  version = description$Version, remote_sha = description$RemoteSha,
  dll_sha256 = digest::digest(file = dll, algo = "sha256"),
  probe_sha256 = digest::digest(file = sub("^--file=", "",
    grep("^--file=", commandArgs(), value = TRUE)), algo = "sha256"),
  session = capture.output(sessionInfo()))
jsonlite::write_json(receipt, args[[1]], auto_unbox = TRUE, pretty = TRUE,
                     digits = NA, null = "null")
cat(if (passed) "PASS" else "FAIL", ": expected", expected,
    "observed", paste(values, collapse = ", "), "\n")
quit(status = if (passed) 0L else 1L)
