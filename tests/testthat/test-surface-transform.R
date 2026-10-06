make_toy_geometry <- function(mask = rep(TRUE, 6), target = FALSE) {
  v <- rbind(c(1, 0, 0), c(-1, 0, 0), c(0, 1, 0), c(0, -1, 0),
             c(0, 0, 1), c(0, 0, -1))
  f <- rbind(c(0, 2, 4), c(2, 1, 4), c(1, 3, 4), c(3, 0, 4),
             c(2, 0, 5), c(1, 2, 5), c(3, 1, 5), c(0, 3, 5))
  if (target) {
    a <- c(1, 1, 1) / sqrt(3)
    b <- c(1, -1, 0) / sqrt(2)
    rotation <- rbind(a, b, c(1, 1, -2) / sqrt(6))
    v <- v %*% rotation
  }
  d <- surface_domain("toy", "L", "6v", v, f, mask, "frame", "rev")
  surface_geometry(d, v, f, mask)
}

surface_engine_test <- function() {
  skip_if_not_installed("neurotransform", minimum_version = "0.2.0")
}

test_that("directed surface interpolation binds exact arrays and domains", {
  surface_engine_test()
  src <- make_toy_geometry()
  dst <- make_toy_geometry(target = TRUE)
  op <- get_surface_transform(src, dst, cache_dir = NULL)
  x <- surface_data(diag(6), src$domain)
  y <- apply_surface_transform(x, op)
  expect_equal(unname(y$values[1, ]), c(1, 0, 1, 0, 1, 0) / 3,
               tolerance = 1e-12)
  expect_equal(rowSums(y$values), rep(1, 6), tolerance = 1e-12)
  expect_identical(y$domain, dst$domain)
  expect_false(op$specification$reversible)
  expect_identical(op$specification$qualification, "unqualified")
  expect_true(all(y$coverage$available))
  expect_error(apply_surface_transform(surface_data(1:6, dst$domain), op),
               "exact source domain")
  x$values[1] <- 99
  expect_error(apply_surface_transform(x, op), "modified SurfaceData")
  src$sphere <- src$sphere[6:1, ]
  expect_error(get_surface_transform(src, dst, NULL), "does not match")
  op$plan$vals[1] <- 0.9
  expect_error(apply_surface_transform(surface_data(1:6, dst$domain), op),
               "integrity")
})

test_that("mask coverage and missingness remain distinct from valid zero", {
  surface_engine_test()
  src <- make_toy_geometry(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  dst <- make_toy_geometry(target = TRUE)
  op <- get_surface_transform(src, dst, NULL)
  out <- apply_surface_transform(surface_data(rep(0, 6), src$domain), op)
  expect_identical(out$values[1], 0)
  expect_true(out$coverage$available[1])
  expect_true(any(!out$coverage$available))
  expect_true(all(is.na(out$values[!out$coverage$available])))
  src <- make_toy_geometry()
  op <- get_surface_transform(src, dst, NULL)
  vals <- rep(1, 6)
  vals[1] <- NA_real_
  x <- surface_data(vals, src$domain)
  expect_true(is.na(apply_surface_transform(x, op)$values[1]))
  expect_equal(apply_surface_transform(x, op, "omit")$values[1], 1)
  expect_error(apply_surface_transform(x, op, "error"), "missing|finite|NA")
  # A missing value with exactly zero weight cannot contaminate identity.
  identity <- get_surface_transform(src, src, NULL)
  expect_identical(apply_surface_transform(x, identity)$values[3], 1)
  dst <- make_toy_geometry(c(FALSE, rep(TRUE, 5)), target = TRUE)
  out <- apply_surface_transform(surface_data(rep(0, 6), src$domain),
                                  get_surface_transform(src, dst, NULL))
  expect_true(is.na(out$values[1]))
  expect_identical(as.vector(out$coverage$status)[1], "masked_target")
})

test_that("labels vote categorically and probabilities preserve partial mass", {
  surface_engine_test()
  src <- make_toy_geometry()
  dst <- make_toy_geometry(target = TRUE)
  op <- get_surface_transform(src, dst, NULL)
  table <- data.frame(key = c(-3, 0, 10), name = c("a", "zero", "b"))
  x <- surface_data(c(10, -3, 0, -3, 0, -3), src$domain, "label", table)
  y <- apply_surface_transform(x, op)
  expect_equal(as.numeric(y$values[1]), 0)
  expect_identical(y$label_table, table)
  expect_true(all(y$values %in% table$key))
  largest <- apply_surface_transform(x, op, label_method = "largest")
  expect_true(all(largest$values %in% table$key))
  expect_error(surface_data(rep(0.2, 6), src$domain, "label"), "integer")
  expect_error(surface_data(rep(11, 6), src$domain, "label", table), "covering")
  p <- surface_data(cbind(a = rep(0.1, 6), b = rep(0.2, 6)),
                    src$domain, "probability")
  out <- apply_surface_transform(p, op)
  expect_equal(rowSums(out$values), rep(0.3, 6), tolerance = 1e-12)
  expect_identical(colnames(out$values), c("a", "b"))
  ones <- surface_data(cbind(one = rep(1, 6), zero = rep(0, 6)),
                       src$domain, "probability")
  limits <- apply_surface_transform(ones, op)$values
  expect_equal(limits[, 1], rep(1, 6), tolerance = 1e-12)
  expect_identical(limits[, 2], rep(0, 6))
  expect_true(all(limits >= 0 & limits <= 1))
  expect_error(surface_data(rep(1.1, 6), src$domain, "probability"), "\\[0,1\\]")
})

test_that("operator caches verify ownership, bytes and exact policy identity", {
  surface_engine_test()
  root <- tempfile("surface-cache-")
  on.exit(unlink(root, recursive = TRUE))
  src <- make_toy_geometry()
  dst <- make_toy_geometry(target = TRUE)
  expect_error(get_surface_transform(src, dst, root, offline = TRUE), "offline")
  op <- get_surface_transform(src, dst, root)
  expect_identical(get_surface_transform(src, dst, root, offline = TRUE), op)
  path <- file.path(root, "surface-operators-v1", paste0(op$key, ".rds"))
  writeLines("unowned", file.path(dirname(path), "keep.rds"))
  saveRDS(list(corrupt = TRUE), path)
  expect_error(get_surface_transform(src, dst, root), "file integrity")
  expect_equal(clear_transform_cache(cache_dir = root), 1L)
  expect_true(file.exists(file.path(dirname(path), "keep.rds")))
  op <- get_surface_transform(src, dst, root)
  changed <- make_toy_geometry(c(FALSE, rep(TRUE, 5)))
  op2 <- get_surface_transform(changed, dst, root)
  expect_false(identical(op$key, op2$key))
  receipt <- paste0(path, ".neuroatlas-receipt")
  unlink(receipt)
  expect_error(get_surface_transform(src, dst, root), "Unowned")
  expect_equal(clear_transform_cache(cache_dir = root), 1L)
  expect_true(file.exists(path))
})

make_toy_metric_gifti <- function(path, label = FALSE, hemi = "Left") {
  writeLines(paste0('<GIFTI Version="1.0" NumberOfDataArrays="1">',
    '<MetaData><MD><Name>AnatomicalStructurePrimary</Name><Value>Cortex',hemi,
    '</Value></MD></MetaData>',
    if (label) paste0('<LabelTable><Label Key="0" Red="0" Green="0" ',
      'Blue="0" Alpha="1">zero</Label></LabelTable>') else '<LabelTable/>',
    '<DataArray Intent="',if(label) 'NIFTI_INTENT_LABEL' else 'NIFTI_INTENT_SHAPE',
    '" DataType="NIFTI_TYPE_FLOAT32" ArrayIndexingOrder="RowMajorOrder" ',
    'Dimensionality="1" Dim0="6" Encoding="ASCII" Endian="LittleEndian" ',
    'ExternalFileName="" ExternalFileOffset="0"><MetaData/><Data>',
    '0 0 0 0 0 0</Data></DataArray></GIFTI>'),path)
}

test_that("GIFTI imports retain keys and enforce hemisphere and intent", {
  skip_if_not_installed("gifti")
  domain <- make_toy_geometry()$domain
  path <- tempfile(fileext = ".gii")
  on.exit(unlink(path))
  make_toy_metric_gifti(path, label = TRUE)
  labels <- surface_data(path, domain, "label")
  expect_equal(labels$values, rep(0, 6))
  expect_equal(labels$label_table$key, 0)
  expect_equal(labels$label_table$name, "zero")
  expect_match(labels$file$sha256, "^[0-9a-f]{64}$")
  expect_error(surface_data(path, domain), "cannot be averaged")
  make_toy_metric_gifti(path)
  expect_error(surface_data(path, domain, "label"), "label intent")
  make_toy_metric_gifti(path, hemi = "Right")
  expect_error(surface_data(path, domain), "hemisphere")
})

test_that("cache rejects dangling receipt links before writing", {
  surface_engine_test()
  root <- tempfile("surface-cache-")
  on.exit(unlink(root, recursive = TRUE))
  src <- make_toy_geometry()
  dst <- make_toy_geometry(target = TRUE)
  op <- get_surface_transform(src, dst, root)
  path <- file.path(root, "surface-operators-v1", paste0(op$key, ".rds"))
  receipt <- paste0(path, ".neuroatlas-receipt")
  unlink(c(path, receipt))
  target <- file.path(root, "must-not-create")
  skip_if_not(file.symlink(target, receipt))
  # Sys.readlink() cannot identify Windows junctions. The orphaned-receipt
  # guard must still refuse publication when that platform reports one exists.
  expect_error(get_surface_transform(src, dst, root),
               "symbolic link|Orphaned surface cache receipt")
  expect_false(file.exists(path))
  expect_false(file.exists(target))
  expect_true(basename(receipt) %in% list.files(dirname(receipt)))
})

test_that("probability interpolation accepts unit values despite rounding", {
  surface_engine_test()
  src <- make_toy_geometry()
  q <- c(1, 6, 4)
  q <- q / sqrt(sum(q * q))
  u <- c(1, 0, 0) - q[1] * q
  u <- u / sqrt(sum(u * u))
  z <- c(q[2] * u[3] - q[3] * u[2], q[3] * u[1] - q[1] * u[3],
         q[1] * u[2] - q[2] * u[1])
  v <- src$sphere %*% rbind(q, u, z)
  d <- surface_domain("toy", "L", "6v", v, src$triangles, src$cortex,
                      "frame", "rev")
  dst <- surface_geometry(d, v, src$triangles, src$cortex)
  op <- get_surface_transform(src, dst, NULL)
  out <- apply_surface_transform(surface_data(rep(1, 6), src$domain,
                                              "probability"), op)
  expect_equal(out$values, rep(1, 6), tolerance = 1e-15)
  expect_true(all(out$values <= 1))
  expect_true(all(out$coverage$available))
})
