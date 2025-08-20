
#' Plot Glasser Atlas Values
#'
#' @description
#' Creates an interactive visualization of values mapped onto the Glasser atlas
#' using echarts4r. The visualization shows both hemispheres in lateral and medial
#' views, arranged in a 2x2 grid.
#'
#' @param vals A data frame containing values to plot, must include columns:
#'   \itemize{
#'     \item label: character, matching ggseg Glasser atlas labels (e.g., "lh_L_V1")
#'     \item value: numeric, values to visualize for each region
#'   }
#'   If NULL (default), all regions will be assigned a value of 1
#' @param value_col Character string specifying the name of the column in vals containing
#'   the values to plot. Defaults to "value"
#' @param position Character string specifying layout type. Currently only "dispersed"
#'   is supported (stacked layout is planned for future versions)
#'
#' @return An echarts4r visualization object containing the 2x2 grid of brain views
#'
#' @examples
#' \dontrun{
#' # Basic visualization with uniform coloring
#' plot_glasser()
#'
#' # Create sample data (requires ggsegGlasser package)
#' if (requireNamespace("ggsegGlasser", quietly = TRUE)) {
#'   glasser_atlas <- get("glasser", envir = asNamespace("ggsegGlasser"))
#'   vals <- data.frame(
#'     label = glasser_atlas$data$label,
#'     value = rnorm(nrow(glasser_atlas$data))
#'   )
#'   # Plot the data
#'   plot_glasser(vals)
#' }
#'
#' # Using a different column name
#' vals$intensity <- vals$value
#' plot_glasser(vals, value_col = "intensity")
#' }
#'
#' @importFrom dplyr filter inner_join group_by summarize left_join rename mutate .data
#' @importFrom tidyr pivot_longer
#' @importFrom sf st_as_sf st_multipolygon st_sfc st_sf st_bbox st_geometry
#' @import ggseg
#' @export
plot_glasser <- function(vals=NULL, value_col = "value", position = "dispersed") {
  # Check and load required packages
  required_packages <- c("geojsonio", "echarts4r", "ggsegGlasser")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    if ("ggsegGlasser" %in% missing_packages) {
      stop("Package 'ggsegGlasser' is required for this function but is not installed.\n",
           "To install it, run:\n",
           "  remotes::install_github('ggseg/ggsegGlasser')\n",
           "or use the ggseg r-universe:\n",
           "  options(repos = c(ggseg = 'https://ggseg.r-universe.dev',\n",
           "                    CRAN = 'https://cloud.r-project.org'))\n",
           "  install.packages('ggsegGlasser')",
           call. = FALSE)
    } else {
      stop("The following packages are required but not installed: ", 
           paste(missing_packages, collapse = ", "))
    }
  }
  # No library() calls needed - will use :: notation

  # Get the ggseg Glasser atlas data
  glasser_atlas <- get("glasser", envir = asNamespace("ggsegGlasser"))
  ggseg_data <- glasser_atlas$data

  # Create default values if vals is NULL
  if (is.null(vals)) {
    vals <- data.frame(
      label = ggseg_data$label,
      value = rep(1, nrow(ggseg_data))
    )
    value_col <- "value"
  }

  if (!all(c("label", value_col) %in% colnames(vals))) {
    stop(sprintf("'vals' must contain columns 'label' and '%s'", value_col))
  }

  if (!position %in% c("dispersed", "stacked")) {
    warning("'position' must be either 'dispersed' or 'stacked'. Using 'dispersed'.")
    position <- "dispersed"
  }

  # Check if regions in vals match atlas regions
  missing_regions <- setdiff(vals$label, ggseg_data$label)
  if (length(missing_regions) > 0) {
    warning("Some regions in 'vals' are not present in the Glasser atlas")
  }

  # Join values with atlas data
  plot_data <- ggseg_data %>%
    left_join(vals, by = "label") %>%
    dplyr::rename(value = !!value_col)

  # Create the four views
  g1 <- geojsonio::geojson_json(dplyr::filter(plot_data, .data$hemi == "left" & .data$side == "lateral"))
  g2 <- geojsonio::geojson_json(dplyr::filter(plot_data, .data$hemi == "right" & .data$side == "lateral"))
  g3 <- geojsonio::geojson_json(dplyr::filter(plot_data, .data$hemi == "left" & .data$side == "medial"))
  g4 <- geojsonio::geojson_json(dplyr::filter(plot_data, .data$hemi == "right" & .data$side == "medial"))

  # Create the four charts
  chart1 <- get_chart(g1, plot_data, "value", "lat", TRUE)
  chart2 <- get_chart(g2, plot_data, "value", "med", FALSE)
  chart3 <- get_chart(g3, plot_data, "value", "lat2", FALSE)
  chart4 <- get_chart(g4, plot_data, "value", "med2", FALSE) %>%
    echarts4r::e_connect_group("4charts")

  # Arrange charts in 2x2 grid
  echarts4r::e_arrange(chart1, chart2, chart3, chart4,
                       rows = 2, cols = 2, width = "lg")
}

#' Helper Function to Create Individual Brain Charts
#' @keywords internal
#' @noRd
get_chart <- function(geo, data, var, name, show = TRUE) {
  data %>%
    echarts4r::e_charts(.data$label) %>%
    echarts4r::e_map_register(name, geo) %>%
    echarts4r::e_map_(var, map = name, nameProperty = "label") %>%
    echarts4r::e_visual_map_(var,
                             show = show,
                             padding = 0,
                             orient = "horizontal",
                             left = "center",
                             bottom = 0
    ) %>%
    echarts4r::e_theme("dark") %>%
    echarts4r::e_group("4charts")
}

