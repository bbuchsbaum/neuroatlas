#' Read CIFTI-2 Dense Scalar or Label Maps
#'
#' Preserves both cortical and volumetric brain models, their zero-based
#'   indices,
#' map metadata and per-map label tables. Matrix axes are normalized in memory
#' only: brainordinates are rows and maps are columns. The original file layout
#' is retained for writing. CIFTI metadata does not identify a registration
#'   sphere
#' or an exact MNI template; vertex counts alone cannot bind a surface domain.
#'
#' @param file Path to a CIFTI-2 dense scalar or dense label file.
#' @return A `CiftiData` S3 list with `values`, ordered `brain_models`,
#'   `volume`,
#'   `map_names`, `label_tables`, original XML/header/extensions and file
#'   identity.
#'   `available` distinguishes missing or explicitly unassigned samples from
#'   supported zero. Vertex counts do not prove a surface registration identity.
#'   Scalar missing values are retained. Infinite values and label values absent
#'   from their map's label table are rejected. Series/connectivity/parcellated
#'   mappings and CIFTI-1 are not supported.
#' @seealso [write_cifti()], [replace_cifti_values()]
#' @examples
#' # Read an existing file without guessing a surface registration:
#' \dontrun{
#' read_cifti("atlas.dlabel.nii")
#' }
#' @md
#' @export
read_cifti <- function(file) {
  .require_cifti_io()
  assertthat::assert_that(
    is.character(file), length(file) == 1L, !is.na(file),
    file.exists(file), !dir.exists(file)
  )
  before <- .surface_file_identity(file)
  image <- RNifti::readNifti(file)
  header <- RNifti::niftiHeader(image)
  extensions <- RNifti::extensions(image)
  selected <- which(vapply(extensions, function(x) {
    identical(as.integer(attr(x, "code")), 32L)
  }, logical(1)))
  if (length(selected) != 1L) {
    stop("CIFTI requires exactly one XML extension with code 32.")
  }
  xml <- RNifti::extension(image, 32, mode = "character")
  parsed <- .cifti_parse(xml, header)
  if (!header$datatype %in% c(2, 4, 8, 16, 64, 256, 512, 768)) {
    stop("Unsupported CIFTI numeric storage type.")
  }
  values <- matrix(as.double(image), header$dim[6], header$dim[7])
  if (parsed$brain_axis == 1L) values <- t(values)
  parsed$available <- .cifti_available(xml, values)
  .cifti_check_values(values, parsed)
  if (!identical(before, .surface_file_identity(file))) {
    stop("CIFTI file changed while it was being read.")
  }
  result <- c(parsed, list(
    values = values, xml = xml, header = header, extensions = extensions,
    file = before, provenance = list(operation = "read")
  ))
  .cifti_finish(result)
}

#' Replace Values Without Changing CIFTI Brain Models
#'
#' Keeps brain-model rows, map metadata and label tables bound to new values.
#' Changing indices or map counts requires a new explicitly constructed layout.
#' Previously unavailable samples remain unavailable, even when their
#' replacement values are finite or use an existing label key.
#' @param x A `CiftiData` from [read_cifti()].
#' @param values Numeric matrix with the same dimensions as `x$values`.
#' @return A new `CiftiData`, with the input identity recorded in provenance.
#' @seealso [read_cifti()], [write_cifti()]
#' @examples
#' \dontrun{
#' x <- read_cifti("map.dscalar.nii")
#' x <- replace_cifti_values(x, x$values * 2)
#' }
#' @md
#' @export
replace_cifti_values <- function(x, values) {
  .require_cifti_io("xml2")
  .validate_cifti(x)
  assertthat::assert_that(identical(dim(values), dim(x$values)))
  .cifti_check_values(values, x)
  input_id <- x$id
  x$values <- values
  x$available <- x$available & !is.na(values)
  x$xml <- .cifti_availability_xml(x$xml, x$available)
  x$provenance <- list(operation = "replace_values", input_id = input_id)
  .cifti_finish(x)
}

#' Write CIFTI-2 Maps With Preserved Brain Models
#'
#' Writes double-precision values using the original axis layout, XML metadata,
#' brain models, label tables and other NIfTI extensions. Label missing values
#' are rejected; no background label is assigned implicitly.
#' @param x A valid `CiftiData`.
#' @param file Destination ending in `.nii` (uncompressed CIFTI-2).
#' @param overwrite Allow replacement of an existing regular file.
#' @return The normalized destination path, invisibly.
#' @seealso [read_cifti()], [replace_cifti_values()]
#' @examples
#' \dontrun{
#' x <- read_cifti("atlas.dlabel.nii")
#' write_cifti(x, "copy.dlabel.nii")
#' }
#' @md
#' @export
write_cifti <- function(x, file, overwrite = FALSE) {
  .require_cifti_io()
  .validate_cifti(x)
  assertthat::assert_that(
    is.character(file), length(file) == 1L, !is.na(file),
    grepl("\\.nii$", file), dir.exists(dirname(file)),
    is.logical(overwrite), length(overwrite) == 1L, !is.na(overwrite)
  )
  if (dir.exists(file) || (!overwrite && file.exists(file))) {
    stop("Destination exists; choose a new file or explicit overwrite.")
  }
  link <- Sys.readlink(file)
  if (!is.na(link) && nzchar(link)) stop("Destination must not be a symlink.")
  values <- if (x$brain_axis == 1L) t(x$values) else x$values
  image <- RNifti::updateNifti(
    array(values, c(rep(1L, 4), dim(values))),
    template = x$header, datatype = "float64"
  )
  extensions <- x$extensions
  selected <- which(vapply(extensions, function(e) {
    identical(as.integer(attr(e, "code")), 32L)
  }, logical(1)))
  extensions[[selected]] <- structure(charToRaw(x$xml), code = 32L)
  RNifti::extensions(image) <- extensions
  temporary <- tempfile(".cifti-", dirname(file), fileext = ".nii")
  on.exit(unlink(temporary), add = TRUE)
  RNifti::writeNifti(image, temporary, version = 2, datatype = "float64")
  .cifti_keep_matrix_axes(temporary)
  checked <- read_cifti(temporary)
  if (
    !identical(checked$brain_models, x$brain_models) ||
      !identical(checked$volume, x$volume) ||
      !identical(checked$label_tables, x$label_tables) ||
      !identical(checked$map_names, x$map_names) ||
      !isTRUE(all.equal(checked$values, x$values,
        tolerance = 0,
        check.attributes = FALSE
      ))
  ) {
    stop("Written CIFTI does not preserve its values and mappings.")
  }
  if (
    !overwrite && file.exists(file)
  ) {
    stop("Destination appeared while writing.")
  }
  backup <- NULL
  if (file.exists(file)) {
    backup <- tempfile(".cifti-backup-", dirname(file), fileext = ".nii")
    if (
      !file.rename(file, backup)
    ) {
      stop("Could not preserve the existing CIFTI file.")
    }
  }
  if (!file.rename(temporary, file)) {
    if (!is.null(backup) && !file.rename(backup, file)) {
      stop("Could not publish CIFTI; the original is retained at ", backup)
    }
    stop("Could not publish the CIFTI file.")
  }
  if (!is.null(backup)) unlink(backup)
  invisible(normalizePath(file, mustWork = TRUE))
}

.cifti_keep_matrix_axes <- function(path) {
  # RNifti removes trailing singleton dimensions. CIFTI still requires both
  # matrix axes when one map is stored after the brainordinate axis. Restore
  # only NIfTI-2 dim[0], the int64 field at byte offset 16; data stay unchanged.
  connection <- file(path, "r+b")
  on.exit(close(connection))
  header <- readBin(connection, "raw", 24L)
  little <- identical(header[1:4], as.raw(c(28, 2, 0, 0)))
  big <- identical(header[1:4], as.raw(c(0, 0, 2, 28)))
  if (length(header) != 24L || (!little && !big)) {
    stop("Expected a NIfTI-2 header when writing CIFTI.")
  }
  dimensions <- as.raw(c(6, rep(0, 7)))
  if (big) dimensions <- rev(dimensions)
  seek(connection, 16L, origin = "start", rw = "write")
  writeBin(dimensions, connection)
  invisible(path)
}

.require_cifti_io <- function(packages = c("RNifti", "xml2")) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("CIFTI I/O requires the optional '", package, "' package.")
    }
  }
}

.cifti_numbers <- function(text, count, integer = FALSE) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    stop("Invalid CIFTI numeric field.")
  }
  tokens <- strsplit(trimws(text), "[[:space:],]+")[[1]]
  if (integer && any(!grepl("^[+-]?[0-9]+$", tokens))) {
    stop("Invalid CIFTI integer field.")
  }
  values <- suppressWarnings(as.double(tokens))
  if (
    length(values) != count || any(!is.finite(values)) ||
      (integer && any(values != trunc(values)))
  ) {
    stop("Invalid CIFTI numeric field or index count.")
  }
  values
}

.cifti_attr <- function(node, name, lower = 0) {
  text <- xml2::xml_attr(node, name)
  if (is.na(text) || !grepl("^[+-]?[0-9]+$", text)) {
    stop("Invalid CIFTI integer attribute: ", name)
  }
  value <- .cifti_numbers(text, 1L, integer = TRUE)
  if (value < lower || value > .Machine$integer.max) {
    stop("CIFTI attribute outside supported bounds: ", name)
  }
  value
}

.cifti_one <- function(node, path) {
  found <- xml2::xml_find_all(node, path)
  if (length(found) != 1L) stop("Expected one CIFTI element: ", path)
  found[[1]]
}

.cifti_parse <- function(xml, header) {
  if (grepl("<![[:space:]]*(DOCTYPE|ENTITY)", xml, ignore.case = TRUE)) {
    stop("CIFTI XML entity declarations are not supported.")
  }
  doc <- xml2::read_xml(xml, options = "NONET")
  root <- .cifti_one(doc, "/CIFTI")
  if (!xml2::xml_attr(root, "Version") %in% c("2", "2.0")) {
    stop("Only CIFTI-2 is supported.")
  }
  if (
    header$sizeof_hdr != 540 ||
      !header$dim[1] %in% c(5, 6) || any(header$dim[2:5] != 1) ||
      any(header$dim[6:7] < 1)
  ) {
    stop("Expected a two-dimensional CIFTI matrix in NIfTI-2.")
  }
  matrix_node <- .cifti_one(root, "./Matrix")
  axes <- xml2::xml_find_all(matrix_node, "./MatrixIndicesMap")
  dimensions <- xml2::xml_attr(axes, "AppliesToMatrixDimension")
  if (
    length(axes) != 2L || anyNA(dimensions) ||
      !setequal(dimensions, c("0", "1"))
  ) {
    stop("CIFTI requires exactly one mapping for each of two matrix axes.")
  }
  types <- xml2::xml_attr(axes, "IndicesMapToDataType")
  brain <- which(types == "CIFTI_INDEX_TYPE_BRAIN_MODELS")
  maps <- which(types %in% c("CIFTI_INDEX_TYPE_SCALARS", "CIFTI_INDEX_TYPE_LABELS"))
  if (length(brain) != 1L || length(maps) != 1L) {
    stop("Only CIFTI dense scalar and label maps are supported.")
  }
  kind <- if (types[maps] == "CIFTI_INDEX_TYPE_LABELS") "label" else "scalar"
  intent <- if (kind == "label") 3007 else 3006
  if (
    header$intent_code != intent
  ) {
    stop("CIFTI intent and XML mapping disagree.")
  }
  brain_axis <- as.integer(dimensions[brain])
  n_gray <- header$dim[6 + brain_axis]
  n_maps <- header$dim[7 - brain_axis]
  models <- .cifti_models(axes[[brain]], n_gray)
  volume <- .cifti_volume(axes[[brain]])
  for (model in models) {
    if (model$type == "CIFTI_MODEL_TYPE_VOXELS") {
      if (
        is.null(volume) || any(model$indices < 0) ||
          any(sweep(model$indices, 2L, volume$dimensions, `>=`))
      ) {
        stop("CIFTI voxels require volume geometry and in-range indices.")
      }
    }
  }
  named <- xml2::xml_find_all(axes[[maps]], "./NamedMap")
  if (
    length(named) != n_maps
  ) {
    stop("CIFTI map count and matrix dimension disagree.")
  }
  map_names <- vapply(named, function(node) {
    xml2::xml_text(.cifti_one(node, "./MapName"))
  }, character(1))
  tables <- lapply(named, function(node) {
    if (kind == "scalar") {
      return(NULL)
    }
    .cifti_labels(.cifti_one(node, "./LabelTable"))
  })
  list(
    schema = "neuroatlas.cifti-data.v1", brain_axis = brain_axis,
    data_type = kind, brain_models = models, volume = volume,
    map_names = unname(map_names), label_tables = tables
  )
}

.cifti_models <- function(axis, n_gray) {
  nodes <- xml2::xml_find_all(axis, "./BrainModel")
  if (!length(nodes)) stop("CIFTI brain-model axis is empty.")
  models <- lapply(nodes, function(node) {
    type <- xml2::xml_attr(node, "ModelType")
    structure <- xml2::xml_attr(node, "BrainStructure")
    if (
      is.na(type) || !type %in%
        c("CIFTI_MODEL_TYPE_SURFACE", "CIFTI_MODEL_TYPE_VOXELS") ||
        is.na(structure) || !grepl("^CIFTI_STRUCTURE_[A-Z_]+$", structure)
    ) {
      stop("Invalid CIFTI brain-model type or structure.")
    }
    count <- .cifti_attr(node, "IndexCount", 1)
    offset <- .cifti_attr(node, "IndexOffset")
    if (count > n_gray || offset + count > n_gray) {
      stop("CIFTI brain-model range exceeds its matrix axis.")
    }
    surface <- type == "CIFTI_MODEL_TYPE_SURFACE"
    field <- if (surface) "./VertexIndices" else "./VoxelIndicesIJK"
    indices <- .cifti_numbers(
      xml2::xml_text(.cifti_one(node, field)),
      count * if (surface) 1L else 3L,
      integer = TRUE
    )
    vertices <- if (
      surface
    ) {
      .cifti_attr(node, "SurfaceNumberOfVertices", 1)
    } else {
      NULL
    }
    if (surface && any(indices < 0 | indices >= vertices)) {
      stop("CIFTI surface indices are outside the declared vertex count.")
    }
    if (!surface) indices <- matrix(indices, ncol = 3L, byrow = TRUE)
    if (
      anyDuplicated(indices)
    ) {
      stop("Duplicate CIFTI indices in a brain model.")
    }
    list(
      type = type, structure = structure, offset = offset, count = count,
      n_vertices = vertices, indices = indices
    )
  })
  keys <- vapply(models, function(x) paste(x$type, x$structure), character(1))
  if (anyDuplicated(keys)) stop("Duplicate CIFTI brain structures of one type.")
  offsets <- vapply(models, `[[`, numeric(1), "offset")
  counts <- vapply(models, `[[`, numeric(1), "count")
  order <- order(offsets)
  expected <- c(0, head(cumsum(counts[order]), -1L))
  if (
    !identical(unname(offsets[order]), unname(expected)) ||
      sum(counts) != n_gray
  ) {
    stop("CIFTI brain-model ranges must cover the axis without gaps or overlap.")
  }
  models
}

.cifti_volume <- function(axis) {
  nodes <- xml2::xml_find_all(axis, "./Volume")
  if (!length(nodes)) {
    return(NULL)
  }
  if (length(nodes) != 1L) stop("Expected at most one CIFTI volume geometry.")
  dimensions <- .cifti_numbers(xml2::xml_attr(nodes[[1]], "VolumeDimensions"),
    3L,
    integer = TRUE
  )
  if (any(dimensions < 1 | dimensions > .Machine$integer.max)) {
    stop("Invalid CIFTI volume dimensions.")
  }
  node <- .cifti_one(nodes[[1]], "./TransformationMatrixVoxelIndicesIJKtoXYZ")
  affine <- matrix(.cifti_numbers(xml2::xml_text(node), 16L), 4L, byrow = TRUE)
  exponent <- .cifti_attr(node, "MeterExponent", -300)
  if (
    exponent > 300 || !identical(unname(affine[4, ]), c(0, 0, 0, 1)) ||
      det(affine[1:3, 1:3]) == 0
  ) {
    stop("Invalid CIFTI voxel-to-world affine.")
  }
  mm <- affine
  mm[1:3, ] <- affine[1:3, ] * 10^(exponent + 3)
  if (any(!is.finite(mm)) || det(mm[1:3, 1:3]) == 0) {
    stop("CIFTI volume units cannot be represented in millimeters.")
  }
  list(
    dimensions = dimensions, affine = affine, meter_exponent = exponent,
    affine_mm = mm
  )
}

.cifti_labels <- function(node) {
  labels <- xml2::xml_find_all(node, "./Label")
  columns <- lapply(c("Key", "Red", "Green", "Blue", "Alpha"), function(name) {
    vapply(labels, function(label) {
      .cifti_numbers(xml2::xml_attr(label, name), 1L, integer = name == "Key")
    }, numeric(1))
  })
  names(columns) <- c("key", "red", "green", "blue", "alpha")
  table <- as.data.frame(columns)
  table$name <- xml2::xml_text(labels)
  if (
    anyDuplicated(table$key) || any(abs(table$key) > .Machine$integer.max) ||
      any(as.matrix(table[c("red", "green", "blue", "alpha")]) < 0) ||
      any(as.matrix(table[c("red", "green", "blue", "alpha")]) > 1)
  ) {
    stop("Invalid CIFTI label keys or RGBA values.")
  }
  table
}

.cifti_check_values <- function(values, parsed) {
  assertthat::assert_that(
    is.matrix(values), is.numeric(values),
    ncol(values) == length(parsed$map_names),
    nrow(values) == sum(vapply(parsed$brain_models, `[[`, numeric(1), "count")),
    all(is.finite(values) | is.na(values))
  )
  if (parsed$data_type == "label") {
    for (i in seq_len(ncol(values))) {
      if (
        anyNA(values[, i]) ||
          any(!values[, i] %in% parsed$label_tables[[i]]$key)
      ) {
        stop("CIFTI label values must occur in their map's label table; missing labels are not inferred.")
      }
    }
  }
  invisible(values)
}

.cifti_id <- function(x) {
  .surface_hash(unclass(x)[setdiff(names(x), c("id", "provenance"))])
}

.cifti_finish <- function(x) {
  class(x) <- c("CiftiData", "list")
  x$id <- .cifti_id(x)
  x
}

.validate_cifti <- function(x) {
  if (
    !inherits(x, "CiftiData") ||
      !identical(x$schema, "neuroatlas.cifti-data.v1") ||
      !identical(x$id, .cifti_id(x))
  ) {
    stop("Invalid or modified CiftiData; use read_cifti() or replace_cifti_values().")
  }
  .cifti_check_values(x$values, x)
  invisible(x)
}

.cifti_available <- function(xml, values) {
  available <- !is.na(values)
  doc <- xml2::read_xml(xml, options = "NONET")
  maps <- xml2::xml_find_all(doc, "./Matrix/MatrixIndicesMap/NamedMap")
  for (i in seq_along(maps)) {
    node <- xml2::xml_find_all(maps[[i]], paste0(
      './MetaData/MD[Name="neuroatlas.unavailable_brainordinates"]/Value'
    ))
    if (length(node) > 1L) stop("Duplicate CIFTI availability metadata.")
    if (length(node)) {
      text <- trimws(xml2::xml_text(node[[1]]))
      if (!nzchar(text)) next
      count <- length(strsplit(text, "[[:space:]]+")[[1]])
      rows <- .cifti_numbers(text, count, integer = TRUE)
      if (any(rows < 0 | rows >= nrow(values)) || anyDuplicated(rows)) {
        stop("Invalid CIFTI availability indices.")
      }
      available[rows + 1, i] <- FALSE
    }
  }
  available
}

.cifti_availability_xml <- function(xml, available) {
  doc <- xml2::read_xml(xml, options = "NONET")
  maps <- xml2::xml_find_all(doc, "./Matrix/MatrixIndicesMap/NamedMap")
  for (i in seq_along(maps)) {
    previous <- xml2::xml_find_all(maps[[i]], paste0(
      './MetaData/MD[Name="neuroatlas.unavailable_brainordinates"]'
    ))
    xml2::xml_remove(previous)
    rows <- which(!available[, i]) - 1L
    if (length(rows)) {
      metadata <- xml2::xml_find_first(maps[[i]], "./MetaData")
      if (inherits(metadata, "xml_missing")) {
        metadata <- xml2::xml_add_child(maps[[i]], "MetaData")
      }
      node <- xml2::xml_add_child(metadata, "MD")
      xml2::xml_add_child(node, "Name", "neuroatlas.unavailable_brainordinates")
      xml2::xml_add_child(node, "Value", paste(rows, collapse = " "))
    }
  }
  as.character(doc)
}

#' Bind Qualified Cortical Operators to a CIFTI Layout
#'
#' Creates a mixed-layout adapter with explicit hemisphere operators. The file
#' cannot prove a registration identity: supplying an operator declares that its
#' exact source domain matches the CIFTI vertex ordering. Counts are checked but
#' never used to infer that binding. Noncortical models must retain the same
#' indices and volume geometry; their rows may be reordered. Changed voxel grids
#' or support require a separate volumetric operation and are rejected here.
#'
#' @param from Source `CiftiData`, used for its brain-model layout.
#' @param to Target `CiftiData`, used for its brain-model layout only. Source
#'   map
#'   names, metadata and label tables are retained during application.
#' @param cortex Named list of qualified `SurfaceTransform` operators, `L`
#'   and/or
#'   `R`, covering every cortical hemisphere present. Obtain these with
#'   [get_template_transform()] and explicitly verified surface geometries.
#' @param volume_space Caller-declared exact volume template shared by source
#'   and
#'   target, required when voxel models are present. Supply one common
#'   identifier,
#'   or a named `c(from = ..., to = ...)` pair that must agree. This first
#'   adapter
#'   accepts the qualified MNI6/MNI2009c frames only. Affines cannot establish
#'   template identity. Declaring a frame does not establish anatomical
#'   accuracy.
#' @return A `CiftiTransform` binding source/target layouts and cortical
#'   operators.
#' @seealso [apply_cifti_transform()], [read_cifti()]
#' @examples
#' \dontrun{
#' op <- get_cifti_transform(source, reference, list(L = left, R = right),
#'   volume_space = "MNI152NLin6Asym"
#' )
#' }
#' @md
#' @export
get_cifti_transform <- function(from, to, cortex, volume_space = NULL) {
  .validate_cifti(from)
  .validate_cifti(to)
  assertthat::assert_that(is.list(cortex), !is.null(names(cortex)))
  if (
    anyDuplicated(names(cortex))
  ) {
    stop("Duplicate CIFTI hemisphere operators.")
  }
  source_keys <- .cifti_model_keys(from)
  target_keys <- .cifti_model_keys(to)
  if (!setequal(source_keys, target_keys)) {
    stop("CIFTI transforms must preserve all brain structures and model types.")
  }
  if (!identical(from$volume, to$volume)) {
    stop("CIFTI cortical transforms require unchanged volume geometry and units.")
  }
  voxels <- any(vapply(from$brain_models, function(model) {
    model$type == "CIFTI_MODEL_TYPE_VOXELS"
  }, logical(1)))
  if (voxels) {
    if (is.null(volume_space)) {
      stop("CIFTI voxel models require an explicit shared volume_space.")
    }
    assertthat::assert_that(
      is.character(volume_space),
      length(volume_space) %in% c(1L, 2L), !anyNA(volume_space)
    )
    if (
      length(volume_space) == 2L &&
        !identical(names(volume_space), c("from", "to"))
    ) {
      stop("Supply volume_space as a common frame or a named from/to pair.")
    }
    if (
      !all(volume_space %in% c("MNI152NLin6Asym", "MNI152NLin2009cAsym")) ||
        length(unique(volume_space)) != 1L
    ) {
      stop("CIFTI cortical application requires one unchanged exact volume frame.")
    }
  } else if (!is.null(volume_space)) {
    stop("volume_space is only valid when CIFTI voxel models are present.")
  }
  copy_rows <- rep(NA_integer_, nrow(to$values))
  hemispheres <- character()
  for (i in seq_along(to$brain_models)) {
    target <- to$brain_models[[i]]
    source <- from$brain_models[[match(target_keys[i], source_keys)]]
    hemi <- .cifti_hemisphere(target)
    if (!is.null(hemi)) {
      hemispheres <- c(hemispheres, hemi)
      operator <- cortex[[hemi]]
      if (is.null(operator)) stop("Missing CIFTI cortical operator for ", hemi)
      .validate_surface_transform(operator)
      specification <- operator$specification
      if (
        !identical(specification$qualification, "passed") ||
          !.surface_engine_revision_verified(
            "933edddda462593941e167726e8aaa7168ff103a"
          )
      ) {
        stop("CIFTI cortical application requires a qualified pinned operator.")
      }
      if (
        specification$from$hemisphere != hemi ||
          specification$to$hemisphere != hemi ||
          source$n_vertices != specification$from$n_vertices ||
          target$n_vertices != specification$to$n_vertices
      ) {
        stop("CIFTI cortical model does not match its explicit operator binding.")
      }
    } else {
      if (!identical(source$n_vertices, target$n_vertices)) {
        stop("Noncortical surface domains must remain unchanged.")
      }
      source_indices <- .cifti_index_keys(source)
      target_indices <- .cifti_index_keys(target)
      if (!setequal(source_indices, target_indices)) {
        stop("CIFTI transforms must preserve noncortical index support.")
      }
      copy_rows[target$offset + seq_len(target$count)] <-
        source$offset + match(target_indices, source_indices)
    }
  }
  if (!length(hemispheres) || !setequal(hemispheres, names(cortex))) {
    stop("Supply exactly the operators for the CIFTI cortical hemispheres.")
  }
  result <- list(
    schema = "neuroatlas.cifti-transform.v1",
    from_layout = .cifti_layout_id(from), to_layout = .cifti_layout_id(to),
    target = to, cortex = cortex, copy_rows = copy_rows,
    volume_space = unname(volume_space),
    binding = "caller-declared exact cortical operator domains",
    noncortical = "unchanged geometry and support, reordered by structure/index"
  )
  result$id <- .surface_hash(result)
  structure(result, class = c("CiftiTransform", "list"))
}

#' Apply Cortical Resampling While Preserving CIFTI Subcortex
#'
#' Applies the declared qualified surface operator independently to each map.
#' Noncortical values and their availability are copied by structure/index.
#' Source map metadata and label tables are retained. Missing scalar values stay
#' missing. Missing label values require explicit existing label keys, and their
#' unavailable brainordinate indices are retained in per-map XML metadata and
#' `available`; assigning a key does not establish sampled support.
#'
#' @param x Source `CiftiData` with the bound brain-model layout.
#' @param transform A `CiftiTransform` from [get_cifti_transform()].
#' @param na_policy Missing-value policy for cortical interpolation, passed to
#'   [apply_surface_transform()].
#' @param label_method Categorical interpolation policy, passed to
#'   [apply_surface_transform()].
#' @param missing_labels `NULL` rejects unsupported label samples. Otherwise,
#'   supply one existing label key per map, or a single key shared by every map.
#' @return A `CiftiData` on the target layout with original map metadata,
#'   per-brainordinate availability and cortical execution provenance.
#' @seealso [write_cifti()], [apply_template_transform()]
#' @examples
#' \dontrun{
#' mapped <- apply_cifti_transform(source, op, missing_labels = 0)
#' write_cifti(mapped, "mapped.dlabel.nii")
#' }
#' @md
#' @export
apply_cifti_transform <- function(
  x, transform,
  na_policy = c("propagate", "omit", "error"),
  label_method = c("aggregate", "largest"), missing_labels = NULL
) {
  .require_cifti_io("xml2")
  .validate_cifti(x)
  na_policy <- match.arg(na_policy)
  label_method <- match.arg(label_method)
  if (
    !inherits(transform, "CiftiTransform") ||
      !identical(transform$schema, "neuroatlas.cifti-transform.v1") ||
      !identical(
        transform$id,
        .surface_hash(unclass(transform)[setdiff(names(transform), "id")])
      )
  ) {
    stop("Invalid or modified CiftiTransform.")
  }
  if (!identical(.cifti_layout_id(x), transform$from_layout)) {
    stop("CIFTI source layout does not match its transform binding.")
  }
  if (x$data_type != "label" && !is.null(missing_labels)) {
    stop("missing_labels is only valid for CIFTI label maps.")
  }
  if (!is.null(missing_labels)) {
    assertthat::assert_that(
      is.numeric(missing_labels),
      length(missing_labels) %in% c(1L, ncol(x$values)),
      all(is.finite(missing_labels))
    )
    missing_labels <- rep(missing_labels, length.out = ncol(x$values))
    for (j in seq_len(ncol(x$values))) {
      if (!missing_labels[j] %in% x$label_tables[[j]]$key) {
        stop("Each missing label key must occur in that map's label table.")
      }
    }
  }
  target <- transform$target
  values <- matrix(NA_real_, nrow(target$values), ncol(x$values))
  available <- matrix(FALSE, nrow(values), ncol(values))
  copied <- which(!is.na(transform$copy_rows))
  source_rows <- transform$copy_rows[copied]
  values[copied, ] <- x$values[source_rows, , drop = FALSE]
  available[copied, ] <- x$available[source_rows, , drop = FALSE]
  source_keys <- .cifti_model_keys(x)
  target_keys <- .cifti_model_keys(target)
  coverage <- list()
  for (i in seq_along(target$brain_models)) {
    model <- target$brain_models[[i]]
    hemi <- .cifti_hemisphere(model)
    if (is.null(hemi)) next
    source <- x$brain_models[[match(target_keys[i], source_keys)]]
    source_rows <- source$offset + seq_len(source$count)
    target_rows <- model$offset + seq_len(model$count)
    operator <- transform$cortex[[hemi]]
    for (j in seq_len(ncol(values))) {
      field <- rep(NA_real_, source$n_vertices)
      field[source$indices + 1] <- x$values[source_rows, j]
      field[source$indices[!x$available[source_rows, j]] + 1] <- NA_real_
      data <- surface_data(
        field, operator$specification$from,
        if (x$data_type == "label") "label" else "continuous",
        if (x$data_type == "label") x$label_tables[[j]] else NULL
      )
      result <- apply_surface_transform(data, operator, na_policy, label_method)
      sampled <- result$values[model$indices + 1]
      available[target_rows, j] <- !is.na(sampled)
      if (x$data_type == "label" && anyNA(sampled)) {
        if (is.null(missing_labels)) {
          stop("Unsupported CIFTI label samples require explicit missing_labels.")
        }
        sampled[is.na(sampled)] <- missing_labels[j]
      }
      values[target_rows, j] <- sampled
      coverage[[paste(hemi, j, sep = ".")]] <- result$coverage
    }
  }
  doc <- xml2::read_xml(x$xml, options = "NONET")
  old <- xml2::xml_find_first(
    doc,
    './Matrix/MatrixIndicesMap[@IndicesMapToDataType="CIFTI_INDEX_TYPE_BRAIN_MODELS"]'
  )
  target_doc <- xml2::read_xml(target$xml, options = "NONET")
  replacement <- xml2::xml_find_first(
    target_doc,
    './Matrix/MatrixIndicesMap[@IndicesMapToDataType="CIFTI_INDEX_TYPE_BRAIN_MODELS"]'
  )
  xml2::xml_set_attr(replacement, "AppliesToMatrixDimension", x$brain_axis)
  xml2::xml_replace(old, replacement)
  xml <- .cifti_availability_xml(as.character(doc), available)
  header <- x$header
  header$dim[6 + x$brain_axis] <- nrow(values)
  parsed <- .cifti_parse(xml, header)
  parsed$available <- available
  .cifti_check_values(values, parsed)
  .cifti_finish(c(parsed, list(
    values = values, xml = xml, header = header, extensions = x$extensions,
    file = NULL, provenance = list(
      operation = "cortical_resample",
      input_id = x$id, input_file = x$file, transform_id = transform$id,
      binding = transform$binding, coverage = coverage,
      na_policy = na_policy, label_method = label_method,
      missing_labels = missing_labels
    )
  )))
}

.cifti_layout_id <- function(x) {
  .surface_hash(list(x$brain_models, x$volume))
}

.cifti_model_keys <- function(x) {
  vapply(x$brain_models, function(model) {
    paste(model$type, model$structure)
  }, character(1))
}

.cifti_hemisphere <- function(model) {
  if (model$type != "CIFTI_MODEL_TYPE_SURFACE") {
    return(NULL)
  }
  if (model$structure == "CIFTI_STRUCTURE_CORTEX_LEFT") {
    return("L")
  }
  if (model$structure == "CIFTI_STRUCTURE_CORTEX_RIGHT") {
    return("R")
  }
  NULL
}

.cifti_index_keys <- function(model) {
  if (is.matrix(model$indices)) {
    apply(model$indices, 1L, paste, collapse = ",")
  } else {
    as.character(model$indices)
  }
}
