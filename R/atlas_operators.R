#' Filter Atlas with Infix Operator
#'
#' A concise infix operator for filtering atlas regions by metadata attributes.
#' Equivalent to calling \code{\link{filter_atlas}} but allows a more fluent
#' pipe-friendly syntax.
#'
#' @param atlas An atlas object.
#' @param expr An unquoted filter expression using column names from
#'   \code{\link{roi_metadata}}. Multiple conditions can be combined with
#'   \code{&} and \code{|}.
#'
#' @return A new atlas object containing only the matching ROIs.
#'
#' @examples
#' \dontrun{
#' atlas <- get_schaefer_atlas(parcels = "200", networks = "7")
#'
#' # Filter to left-hemisphere Default network
#' sub <- atlas %where% (network == "Default" & hemi == "left")
#'
#' # Filter by label pattern
#' vis <- atlas %where% grepl("Vis", label)
#' }
#'
#' @seealso \code{\link{filter_atlas}} for the underlying function,
#'   \code{\link{roi_metadata}} for available filter columns
#'
#' @importFrom rlang enquo
#' @export
`%where%` <- function(atlas, expr) {
  if (!inherits(atlas, "atlas")) {
    stop("Left-hand side of %where% must be an atlas object")
  }
  expr_q <- rlang::enquo(expr)
  filter_atlas(atlas, .dots = list(expr_q))
}
