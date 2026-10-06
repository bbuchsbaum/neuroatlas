#' Bind Registration Geometry to a Surface Domain
#'
#' Checks the ordered sphere, topology and cortical mask against an exact
#' [surface_domain()] descriptor. This does not estimate a registration.
#'
#' @param domain A `SurfaceDomain` descriptor.
#' @param sphere Numeric vertex-by-three matrix, or a GIFTI surface filename.
#' @param triangles Triangle matrix; omit when reading a GIFTI surface.
#' @param cortex Logical cortical inclusion vector, or a binary GIFTI filename.
#' @param index_base Triangle indexing for matrices, `"zero"` or `"one"`.
#'   GIFTI triangles are always zero-based.
#' @return A `SurfaceGeometry` containing the verified domain and arrays.
#' @export
surface_geometry <- function(domain, sphere, triangles = NULL, cortex,
                             index_base = c("zero", "one")) {
  .validate_surface_domain(domain)
  index_base <- match.arg(index_base)
  files <- list()
  if (is.character(sphere)) {
    if (!is.null(triangles)) stop("Do not supply triangles with a GIFTI surface.")
    g <- .surface_read_gifti(sphere, domain)
    files$sphere <- .surface_file_identity(sphere)
    intents <- g$data_info$Intent
    if (sum(intents == "NIFTI_INTENT_POINTSET") != 1L ||
        sum(intents == "NIFTI_INTENT_TRIANGLE") != 1L) {
      stop("GIFTI surface must contain one pointset and one triangle array.")
    }
    sphere <- g$data[[which(intents == "NIFTI_INTENT_POINTSET")]]
    triangles <- g$data[[which(intents == "NIFTI_INTENT_TRIANGLE")]]
    index_base <- "zero"
  }
  if (is.character(cortex)) {
    g <- .surface_read_gifti(cortex, domain)
    files$cortex <- .surface_file_identity(cortex)
    if (length(g$data) != 1L) stop("Cortical mask must contain one GIFTI array.")
    cortex <- as.vector(g$data[[1L]])
    if (anyNA(cortex) || !all(cortex %in% c(0, 1))) {
      stop("Cortical mask must be binary, with 1 denoting included cortex.")
    }
    cortex <- as.logical(cortex)
  }
  # Reuse the constructor's array validation and canonical indexing. Areas
  # remain part of domain identity, but are not used by ordinary interpolation.
  check <- surface_domain(domain$template, domain$hemisphere, domain$density,
    sphere, triangles, cortex, domain$registration, domain$revision,
    index_base = index_base)
  fields <- c("n_vertices", "n_triangles", "coordinates_sha256",
              "topology_sha256", "cortex_sha256")
  if (!identical(check[fields], domain[fields])) {
    stop("Geometry or cortical mask does not match the exact surface domain.")
  }
  if (index_base == "one") triangles <- triangles - 1L
  storage.mode(sphere) <- "double"
  storage.mode(triangles) <- "integer"
  structure(list(domain = domain, sphere = unname(sphere),
    triangles = unname(triangles), cortex = as.logical(cortex), files = files),
    class = c("SurfaceGeometry", "list"))
}

#' Bind Vertex Values to an Exact Surface Domain
#'
#' Explicitly declaring the domain binds the caller's vertex ordering. GIFTI
#' hemisphere metadata, when present, must agree. A GIFTI file alone cannot
#' establish topology or vertex order; the caller must know its domain.
#'
#' @param values Numeric vertex vector or vertex-by-map matrix, or a metric or
#'   label GIFTI filename. `NA` values represent missing data; infinity is rejected.
#' @param domain A `SurfaceDomain` descriptor.
#' @param data_type `"continuous"`, `"label"`, or `"probability"`. Probability
#'   values must lie in [0,1]; channels are never renormalized across maps.
#' @param label_table Optional data frame with unique integer `key` values and
#'   optional names/colors. Required when importing label GIFTI; read from the
#'   file when omitted. Every finite input key must appear in the table.
#' @return `SurfaceData`, preserving values, domain, label table and input identity.
#' @export
surface_data <- function(values, domain,
                         data_type = c("continuous", "label", "probability"),
                         label_table = NULL) {
  .validate_surface_domain(domain)
  data_type <- match.arg(data_type)
  file <- NULL
  if (is.character(values)) {
    g <- .surface_read_gifti(values, domain)
    file <- .surface_file_identity(values)
    intents <- g$data_info$Intent
    if (!length(intents) || any(intents %in%
        c("NIFTI_INTENT_POINTSET", "NIFTI_INTENT_TRIANGLE"))) {
      stop("Expected vertex metrics or labels, not GIFTI geometry.")
    }
    if (data_type == "label") {
      if (!all(intents == "NIFTI_INTENT_LABEL")) {
        stop("Label data require GIFTI label intent.")
      }
      if (is.null(label_table)) {
        if (is.null(g$label) || !"Key" %in% colnames(g$label)) {
          stop("Label GIFTI has no label table.")
        }
        label_table <- data.frame(key = as.numeric(g$label[, "Key"]),
          name = rownames(g$label), stringsAsFactors = FALSE)
        for (field in intersect(c("Red", "Green", "Blue", "Alpha"),
                                colnames(g$label))) {
          label_table[[tolower(field)]] <- as.numeric(g$label[, field])
        }
      }
    } else if (any(intents == "NIFTI_INTENT_LABEL")) {
      stop("GIFTI labels require data_type='label'; keys cannot be averaged.")
    }
    if (!all(vapply(g$data, function(a) nrow(as.matrix(a)) == domain$n_vertices,
                    logical(1)))) stop("GIFTI vertex count does not match domain.")
    values <- do.call(cbind, g$data)
    if (ncol(values) == 1L) values <- as.vector(values)
  }
  assertthat::assert_that(is.numeric(values),
    is.null(dim(values)) || is.matrix(values),
    NROW(values) == domain$n_vertices, NCOL(values) > 0L,
    !any(is.infinite(values)))
  if (data_type == "probability" && any(values < 0 | values > 1, na.rm = TRUE)) {
    stop("Probability values must lie in [0,1].")
  }
  if (data_type == "label") {
    keys <- values[is.finite(values)]
    if (any(keys != trunc(keys) | abs(keys) > .Machine$integer.max)) {
      stop("Labels must be integer keys.")
    }
    if (!is.null(label_table)) {
      if (!is.data.frame(label_table) || !"key" %in% names(label_table) ||
          !is.numeric(label_table$key) || anyNA(label_table$key) ||
          anyDuplicated(label_table$key) ||
          any(!is.finite(label_table$key) |
                label_table$key != trunc(label_table$key) |
                abs(label_table$key) > .Machine$integer.max) ||
          !all(keys %in% label_table$key)) {
        stop("label_table must contain unique integer keys covering the data.")
      }
    }
  } else if (!is.null(label_table)) stop("label_table is only valid for labels.")
  x <- structure(list(values = values, domain = domain, data_type = data_type,
    label_table = label_table, file = file), class = c("SurfaceData", "list"))
  x$id <- .surface_data_id(x)
  x
}

#' Build or Load a Directed Surface Transform
#'
#' Constructs ordinary native closest-point barycentric weights between explicit
#' registered spheres. Source mask exclusion precedes row normalization; target
#' masking follows it. No registration is fitted. Native geometric correctness
#' and exact Workbench parity are distinct qualification claims. Arbitrary
#' supplied geometries are always marked `"unqualified"`.
#'
#' @param from,to Verified `SurfaceGeometry` objects in the same declared sphere
#'   correspondence frame and hemisphere.
#' @param cache_dir Transform cache directory, or `NULL` to disable caching.
#' @param offline Require an existing verified operator cache entry.
#' @return A `SurfaceTransform` with sparse weights, exact domains, engine
#'   fingerprint, integrity digest and directed provenance. Reverse resampling
#'   requires calling this function with swapped geometries; it is not an inverse.
#' @seealso [surface_geometry()], [surface_data()], [apply_surface_transform()]
#' @export
get_surface_transform <- function(from, to,
                                  cache_dir = transform_cache_path(),
                                  offline = FALSE) {
  assertthat::assert_that(is.logical(offline), length(offline) == 1L,
                         !is.na(offline))
  from <- .validate_surface_geometry(from)
  to <- .validate_surface_geometry(to)
  if (!identical(from$domain$hemisphere, to$domain$hemisphere)) {
    stop("Surface transforms require matching hemispheres.")
  }
  if (!identical(from$domain$registration, to$domain$registration)) {
    stop("Surface transforms require the same declared correspondence frame.")
  }
  engine <- .require_surface_engine()
  specification <- list(schema = "neuroatlas.surface-transform.v1",
    from = from$domain, to = to$domain, method = "native_closest_barycentric",
    radius = 100, mask_policy = "source_then_row_normalize_then_target",
    engine = engine, qualification = "unqualified", reversible = FALSE)
  key <- .surface_hash(specification)
  build <- function() {
    # Pass explicit one-based faces to avoid any engine index-base heuristic.
    moving <- neurotransform::surface_mesh(from$sphere, from$triangles + 1L)
    reference <- neurotransform::surface_mesh(to$sphere, to$triangles + 1L)
    plan <- neurotransform::surface_resampling_plan(reference, moving,
      method = "barycentric", spherical = TRUE, radius = 100,
      source_mask = from$cortex, target_mask = to$cortex, outside = "error")
    x <- structure(list(specification = specification, plan = plan, key = key),
                   class = c("SurfaceTransform", "list"))
    x$integrity <- .surface_transform_digest(x)
    .validate_surface_transform(x)
    x
  }
  if (is.null(cache_dir)) {
    if (offline) stop("offline requires an operator cache directory.")
    return(build())
  }
  .surface_cached_operator(key, specification, cache_dir, offline, build)
}

#' Apply a Domain-Bound Surface Transform
#'
#' Continuous maps use row-normalized interpolation. Label keys use categorical
#' voting and are never averaged. Unsupported and target-masked outputs are `NA`;
#' supported zero, including label key zero, remains a valid value. Missingness
#' and source weight mass are returned separately for every map.
#' Probability output roundoff within `1e-12` of [0,1] is clipped to that
#' interval; larger excursions are errors. Input bounds remain strict and
#' probability channels are never renormalized across maps.
#'
#' @param x A `SurfaceData` object bound to the transform's exact source domain.
#' @param transform A verified `SurfaceTransform`.
#' @param na_policy `"propagate"`, `"omit"` with finite-weight renormalization,
#'   or `"error"`. Omission changes the effective operator per map.
#' @param label_method `"aggregate"` votes by total weight per key; ties select
#'   the smallest key. `"largest"` selects the largest-weight source vertex;
#'   ties select the smallest source index. Used only for categorical data.
#' @return `SurfaceData` on the target domain, with `coverage` diagnostics and
#'   provenance including input identity, operator identity and policies.
#' @export
apply_surface_transform <- function(x, transform,
                                    na_policy = c("propagate", "omit", "error"),
                                    label_method = c("aggregate", "largest")) {
  na_policy <- match.arg(na_policy)
  label_method <- match.arg(label_method)
  engine <- .require_surface_engine()
  .validate_surface_transform(transform)
  if (!identical(engine, transform$specification$engine)) {
    stop("Surface operator belongs to a different engine; rebuild it.")
  }
  if (!inherits(x, "SurfaceData") || !identical(x$id, .surface_data_id(x))) {
    stop("Invalid or modified SurfaceData; reconstruct with surface_data().")
  }
  .validate_surface_domain(x$domain)
  if (!identical(x$domain$id, transform$specification$from$id)) {
    stop("Input values do not belong to the transform's exact source domain.")
  }
  result <- neurotransform::apply_surface_resampling(transform$plan, x$values,
    normalize = "element", na_policy = na_policy,
    data_type = if (x$data_type == "label") "label" else "continuous",
    label_method = label_method, label_table = x$label_table, details = TRUE)
  # Channel sums and partial probability mass are deliberately not normalized.
  values <- result$values
  if (x$data_type == "probability") {
    finite <- is.finite(values)
    if (any(values[finite] < -1e-12 | values[finite] > 1 + 1e-12)) {
      stop("Probability interpolation exceeded its numerical bounds.")
    }
    values[finite] <- pmin(1, pmax(0, values[finite]))
  }
  if (is.matrix(values)) colnames(values) <- colnames(x$values)
  output <- surface_data(values, transform$specification$to, x$data_type,
                         x$label_table)
  output$coverage <- result[setdiff(names(result), c("values", "label_table"))]
  output$provenance <- list(input_id = x$id, operator_id = transform$integrity,
    specification = transform$specification, na_policy = na_policy,
    label_method = if (x$data_type == "label") label_method else NULL)
  output
}

.surface_hash <- function(x) digest::digest(x, algo = "sha256", serializeVersion = 2L)

.surface_data_id <- function(x) {
  .surface_hash(list(x$domain$id, x$values, x$data_type, x$label_table, x$file))
}

.surface_file_identity <- function(path) {
  list(path = normalizePath(path, mustWork = TRUE),
       sha256 = digest::digest(file = path, algo = "sha256"))
}

.surface_read_gifti <- function(path, domain) {
  assertthat::assert_that(is.character(path), length(path) == 1L,
                         !is.na(path), file.exists(path))
  if (!requireNamespace("gifti", quietly = TRUE)) {
    stop("GIFTI input requires the optional 'gifti' package.")
  }
  g <- gifti::readgii(path)
  structure <- g$file_meta["AnatomicalStructurePrimary"]
  expected <- if (domain$hemisphere == "L") "CortexLeft" else "CortexRight"
  if (length(structure) && !is.na(structure) && !identical(unname(structure), expected)) {
    stop("GIFTI anatomical structure does not match the declared hemisphere.")
  }
  g
}

.validate_surface_geometry <- function(x) {
  if (!inherits(x, "SurfaceGeometry")) stop("Expected a SurfaceGeometry object.")
  checked <- surface_geometry(x$domain, x$sphere, x$triangles, x$cortex)
  checked$files <- x$files
  checked
}

.surface_transform_digest <- function(x) {
  .surface_hash(list(x$specification, x$key, x$plan))
}

.validate_surface_transform <- function(x) {
  if (!inherits(x, "SurfaceTransform") ||
      !identical(x$integrity, .surface_transform_digest(x)) ||
      !identical(x$key, .surface_hash(x$specification))) {
    stop("Surface operator failed its integrity check.")
  }
  s <- x$specification
  .validate_surface_domain(s$from)
  .validate_surface_domain(s$to)
  p <- x$plan
  if (!inherits(p, "SurfaceResamplingPlan") ||
      !identical(p$n_moving, s$from$n_vertices) ||
      !identical(p$n_reference, s$to$n_vertices) ||
      !identical(p$method, "barycentric") || !isTRUE(p$spherical) ||
      !identical(p$outside, "error") || !is.null(p$area) ||
      length(p$rows) != length(p$cols) || length(p$rows) != length(p$vals) ||
      !length(p$vals) || any(!is.finite(p$vals) | p$vals <= 0) ||
      any(!is.finite(p$rows) | p$rows != trunc(p$rows) |
            p$rows < 1 | p$rows > p$n_reference) ||
      any(!is.finite(p$cols) | p$cols != trunc(p$cols) |
            p$cols < 1 | p$cols > p$n_moving)) {
    stop("Surface operator has invalid sparse weights or geometry.")
  }
  mass <- rowsum(p$vals, p$rows, reorder = FALSE)
  if (nrow(mass) != p$n_reference || any(abs(mass - 1) > 1e-12) ||
      !identical(.surface_array_sha(p$source_mask, TRUE), s$from$cortex_sha256) ||
      !identical(.surface_array_sha(p$target_mask, TRUE), s$to$cortex_sha256)) {
    stop("Surface operator has invalid row mass or cortical masks.")
  }
  invisible(x)
}

.surface_engine_checked <- new.env(parent = emptyenv())

.require_surface_engine <- function() {
  if (!requireNamespace("neurotransform", quietly = TRUE) ||
      utils::packageVersion("neurotransform") < "0.2.0") {
    stop("Surface application requires neurotransform >= 0.2.0; install the pinned revision.")
  }
  required <- c("surface_mesh", "validate_surface_mesh", "surface_resampling_plan",
                "apply_surface_resampling", "apply_surface_adjoint")
  if (!all(required %in% getNamespaceExports("neurotransform"))) {
    stop("Installed neurotransform lacks the required surface contract.")
  }
  dll <- getLoadedDLLs()[["neurotransform"]][["path"]]
  fingerprint <- digest::digest(file = dll, algo = "sha256")
  if (!isTRUE(.surface_engine_checked[[fingerprint]])) {
    v <- rbind(c(1, 0, 0), c(-1, 0, 0), c(0, 1, 0), c(0, -1, 0),
               c(0, 0, 1), c(0, 0, -1))
    f <- rbind(c(1, 3, 5), c(3, 2, 5), c(2, 4, 5), c(4, 1, 5),
               c(3, 1, 6), c(2, 3, 6), c(4, 2, 6), c(1, 4, 6))
    q <- neurotransform::surface_mesh(matrix(c(1, 1, 1) / sqrt(3), 1L))
    for (order in list(1:8, 8:1)) {
      p <- neurotransform::surface_resampling_plan(q,
        neurotransform::surface_mesh(v, f[order, ]))
      got <- neurotransform::apply_surface_resampling(p, diag(6), details = TRUE)
      if (max(abs(got$values - matrix(c(1, 0, 1, 0, 1, 0) / 3, 1L))) > 1e-12 ||
          !all(got$available)) stop("Surface engine failed its geometric admission probe.")
    }
    .surface_engine_checked[[fingerprint]] <- TRUE
  }
  description <- utils::packageDescription("neurotransform")
  list(package = "neurotransform", version = description$Version,
       source_sha = if (is.null(description$RemoteSha)) NA_character_ else description$RemoteSha,
       dll_sha256 = fingerprint,
       r_code_sha256 = .surface_hash(vapply(
         list.files(system.file("R", package = "neurotransform"),
                    full.names = TRUE),
         function(path) digest::digest(file = path, algo = "sha256"),
         character(1), USE.NAMES = FALSE)))
}

.surface_cached_operator <- function(key, specification, cache_dir, offline, build) {
  root <- .transform_cache_root(cache_dir)
  folder <- file.path(root, "surface-operators-v1")
  if (.transform_cache_is_symlink(folder)) stop("Unsafe surface cache symbolic link.")
  path <- file.path(folder, paste0(key, ".rds"))
  if (offline && !file.exists(path)) stop("Surface operator is unavailable in the offline cache.")
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  .with_transform_cache_lock(file.path(root, ".neuroatlas-cache.lock"),
    artifact = NULL, target = NULL, code = function() {
      receipt <- paste0(path, ".neuroatlas-receipt")
      if (.transform_cache_is_symlink(path) ||
          .transform_cache_is_symlink(receipt)) {
        stop("Unsafe surface operator cache symbolic link.")
      }
      if (file.exists(path)) {
        if (.transform_cache_is_symlink(path) || .transform_cache_is_symlink(receipt) ||
            !.transform_cache_owned(path)) stop("Unowned or unsafe surface operator cache entry.")
        stamp <- read.dcf(receipt)
        if (!"sha256" %in% colnames(stamp) ||
            !identical(unname(stamp[1L, "sha256"]),
                       digest::digest(file = path, algo = "sha256"))) {
          stop("Surface operator cache failed its file integrity check.")
        }
        result <- readRDS(path)
        .validate_surface_transform(result)
        if (!identical(result$specification, specification)) {
          stop("Surface operator cache specification mismatch.")
        }
        return(result)
      }
      if (offline) stop("Surface operator is unavailable in the offline cache.")
      if (file.exists(receipt)) stop("Orphaned surface cache receipt; inspect the cache entry.")
      result <- build()
      tmp <- tempfile(".surface-", tmpdir = folder)
      on.exit(unlink(tmp), add = TRUE)
      saveRDS(result, tmp, version = 2)
      sha <- digest::digest(file = tmp, algo = "sha256")
      if (!file.rename(tmp, path)) stop("Could not publish surface operator cache entry.")
      write.dcf(data.frame(owner = "neuroatlas-transform-cache-v1",
        file = basename(path), artifact_id = key, sha256 = sha), receipt)
      result
    })
}
