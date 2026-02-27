# Shared export for the cluster.explorer companion package
#
# Provides a clean public interface to the internal polygon rendering
# pipeline so that cluster.explorer can build surface pick-lookup tables
# without accessing neuroatlas internals.
#
#' @include plot_brain.R
NULL

#' Build brain polygon render data for a surface atlas
#'
#' Projects surface atlas geometry into 2D polygon panels for each
#' hemisphere and view. Results are memoised so repeated calls with
#' identical arguments return cached data.
#'
#' @param surfatlas A surfatlas object (e.g. from \code{\link{schaefer_surf}()}).
#' @param views Character vector of views to render
#'   (e.g. \code{c("lateral", "medial")}).
#' @param surface Character: surface type (\code{"inflated"}, \code{"pial"},
#'   \code{"white"}).
#' @param projection_smooth Integer: number of Laplacian smoothing iterations
#'   on projected coordinates (default \code{0L}).
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{polygons}}{Tibble of 2D projected polygon vertices with
#'       columns \code{x, y, poly_id, parcel_id, label, hemi, view, panel}.}
#'     \item{\code{boundaries}}{Tibble of boundary edges with columns
#'       \code{x, y, xend, yend, panel}.}
#'   }
#' @export
build_brain_polygon_data <- function(surfatlas, views, surface,
                                     projection_smooth = 0L) {
  .build_merged_polygon_data_memo(surfatlas, views, surface, projection_smooth)
}
