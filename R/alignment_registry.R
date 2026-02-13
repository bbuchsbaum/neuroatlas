#' Atlas Alignment Lookup
#'
#' Returns a structured compatibility/alignment summary between two atlas
#' representations based on atlas provenance metadata.
#'
#' For same family/model/representation comparisons across templates, this
#' function consults the space transform registry when available.
#'
#' @param x Source atlas object.
#' @param y Target atlas object.
#'
#' @return A list of class `"atlas_alignment"` with fields:
#'   `from`, `to`, `compatible`, `relation`, `method`, `confidence`,
#'   `status`, `requires_transform`, and `notes`.
#'
#' @examples
#' ref <- new_atlas_ref(
#'   family = "schaefer",
#'   model = "Schaefer2018",
#'   representation = "volume",
#'   template_space = "MNI152NLin6Asym",
#'   coord_space = "MNI152",
#'   confidence = "high"
#' )
#' a <- structure(list(atlas_ref = ref), class = c("schaefer", "atlas"))
#' b <- structure(list(atlas_ref = ref), class = c("schaefer", "atlas"))
#' atlas_alignment(a, b)$relation
#' @export
atlas_alignment <- function(x, y) {
  from_ref <- atlas_ref(x)
  to_ref <- atlas_ref(y)

  if (identical(from_ref$family, to_ref$family) &&
      identical(from_ref$model, to_ref$model) &&
      identical(from_ref$representation, to_ref$representation) &&
      identical(from_ref$template_space, to_ref$template_space)) {
    return(structure(
      list(
        from = from_ref,
        to = to_ref,
        compatible = TRUE,
        relation = "identical",
        method = "identity",
        confidence = "exact",
        status = "available",
        requires_transform = FALSE,
        notes = "Same family/model/representation/template."
      ),
      class = c("atlas_alignment", "list")
    ))
  }

  if (identical(from_ref$family, to_ref$family) &&
      identical(from_ref$model, to_ref$model) &&
      identical(from_ref$representation, to_ref$representation) &&
      !identical(from_ref$template_space, to_ref$template_space)) {
    route <- NULL
    if (!is.na(from_ref$template_space) && !is.na(to_ref$template_space)) {
      route <- .find_direct_space_route(
        .space_transform_registry(),
        .normalize_space_id(from_ref$template_space),
        .normalize_space_id(to_ref$template_space)
      )
    }

    if (is.null(route)) {
      route_method <- "resample"
      route_conf <- "approximate"
      route_status <- "planned"
      route_notes <- "Same model/representation across different templates; no direct route registered."
    } else {
      route_method <- route$transform_type[[1]]
      route_conf <- route$confidence[[1]]
      route_status <- route$status[[1]]
      route_notes <- route$notes[[1]]
    }

    return(structure(
      list(
        from = from_ref,
        to = to_ref,
        compatible = TRUE,
        relation = "same_representation_different_template",
        method = route_method,
        confidence = route_conf,
        status = route_status,
        requires_transform = TRUE,
        notes = route_notes
      ),
      class = c("atlas_alignment", "list")
    ))
  }

  reg <- .alignment_registry_table()
  hit <- reg[
    reg$family == from_ref$family &
      reg$model == from_ref$model &
      reg$from_representation == from_ref$representation &
      reg$to_representation == to_ref$representation,
    ,
    drop = FALSE
  ]

  if (nrow(hit) > 0L) {
    row <- hit[1, , drop = FALSE]
    return(structure(
      list(
        from = from_ref,
        to = to_ref,
        compatible = TRUE,
        relation = row$relation[[1]],
        method = row$method[[1]],
        confidence = row$confidence[[1]],
        status = row$status[[1]],
        requires_transform = !identical(row$method[[1]], "identity"),
        notes = row$notes[[1]]
      ),
      class = c("atlas_alignment", "list")
    ))
  }

  structure(
    list(
      from = from_ref,
      to = to_ref,
      compatible = FALSE,
      relation = "unknown",
      method = "none",
      confidence = "uncertain",
      status = "planned",
      requires_transform = NA,
      notes = "No registered alignment route."
    ),
    class = c("atlas_alignment", "list")
  )
}


#' Atlas Transform Manifest
#'
#' Returns currently known cross-representation alignment routes and their
#' implementation status.
#'
#' @param scope Manifest scope. `"alignment"` returns family/model alignment
#'   routes. `"space"` returns template/space transform routes.
#'
#' @return
#' A data frame manifest.
#' For `scope = "alignment"` this is a tibble of atlas-family representation
#' routes.
#' For `scope = "space"` this is the space transform registry returned by
#' [space_transform_manifest()].
#'
#' @examples
#' # Atlas-family alignment routes
#' atlas_transform_manifest("alignment")
#'
#' # Space-to-space routes
#' atlas_transform_manifest("space")
#' @export
atlas_transform_manifest <- function(scope = c("alignment", "space")) {
  scope <- match.arg(scope)
  if (identical(scope, "alignment")) {
    return(.alignment_registry_table())
  }
  space_transform_manifest()
}


#' Print Method for Atlas Alignment Results
#'
#' @param x An `atlas_alignment` object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @examples
#' ref <- new_atlas_ref(
#'   family = "schaefer",
#'   model = "Schaefer2018",
#'   representation = "volume",
#'   template_space = "MNI152NLin6Asym",
#'   coord_space = "MNI152",
#'   confidence = "high"
#' )
#' a <- structure(list(atlas_ref = ref), class = c("schaefer", "atlas"))
#' print(atlas_alignment(a, a))
#' @export
print.atlas_alignment <- function(x, ...) {
  cat("<atlas_alignment>\n")
  cat("  compatible:", x$compatible, "\n")
  cat("  relation:", x$relation, "\n")
  cat("  method:", x$method, "\n")
  cat("  confidence:", x$confidence, "\n")
  cat("  status:", x$status, "\n")
  cat("  requires_transform:", x$requires_transform, "\n")
  invisible(x)
}


#' @keywords internal
#' @noRd
.alignment_registry_table <- function() {
  tibble::tibble(
    family = c(
      "schaefer", "schaefer",
      "glasser", "glasser",
      "subcortical", "subcortical"
    ),
    model = c(
      "Schaefer2018", "Schaefer2018",
      "HCP-MMP1.0", "HCP-MMP1.0",
      "CIT168", "CIT168"
    ),
    from_representation = c(
      "volume", "surface",
      "volume", "surface",
      "volume", "volume"
    ),
    to_representation = c(
      "surface", "volume",
      "surface", "volume",
      "volume", "volume"
    ),
    relation = c(
      "same_model_projected",
      "same_model_projected",
      "same_model_projected",
      "same_model_projected",
      "same_model_different_template",
      "same_model_different_template"
    ),
    method = c(
      "volume_to_surface_projection",
      "surface_to_volume_projection",
      "volume_to_surface_projection",
      "surface_to_volume_projection",
      "resample",
      "resample"
    ),
    confidence = c(
      "approximate",
      "approximate",
      "uncertain",
      "uncertain",
      "approximate",
      "approximate"
    ),
    status = c(
      "planned",
      "planned",
      "planned",
      "planned",
      "available",
      "available"
    ),
    notes = c(
      "Schaefer cross-representation path; transform backend not yet implemented.",
      "Schaefer cross-representation path; transform backend not yet implemented.",
      "Glasser cross-representation path; provenance mismatch risk.",
      "Glasser cross-representation path; provenance mismatch risk.",
      "TemplateFlow subcortical atlases can be resampled across spaces.",
      "TemplateFlow subcortical atlases can be resampled across spaces."
    )
  )
}
