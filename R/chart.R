
#' Plot Glasser Atlas Values
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated. Use
#' \code{plot_brain(glasser_surf(), vals = ...)} instead for interactive
#' cortical surface visualisation.
#'
#' @param vals A data frame containing values to plot, must include columns:
#'   \itemize{
#'     \item label: character, matching ggseg Glasser atlas labels
#'     \item value: numeric, values to visualize for each region
#'   }
#'   If NULL (default), all regions will be assigned a value of 1
#' @param value_col Character string specifying the name of the column in vals
#'   containing the values to plot. Defaults to "value"
#' @param position Character string specifying layout type.
#'
#' @return An error; this function has been removed.
#'
#' @examples
#' \dontrun{
#' # Deprecated — use plot_brain(glasser_surf(), vals = ...) instead
#' }
#'
#' @export
plot_glasser <- function(vals = NULL, value_col = "value", position = "dispersed") {
  lifecycle::deprecate_stop(
    "0.2.0", "plot_glasser()",
    "plot_brain()",
    details = "Use plot_brain(glasser_surf(), vals = ...) instead."
  )
}
