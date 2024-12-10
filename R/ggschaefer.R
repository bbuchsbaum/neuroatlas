#' Get ggseg-Compatible Schaefer Atlas
#'
#' @description
#' Retrieves the appropriate Schaefer atlas data structure compatible with ggseg
#' visualization based on the atlas name parameters.
#'
#' @param atlas An atlas object containing Schaefer parcellation information.
#'   The name should follow the format "schaeferN_M" where:
#'   \itemize{
#'     \item N is the number of networks (7 or 17)
#'     \item M is the number of parcels (100 to 1000 in steps of 100)
#'   }
#'
#' @return A ggseg brain atlas object for visualization
#'
#' @details
#' The function extracts the network count and parcel count from the atlas name
#' and returns the corresponding ggseg-compatible atlas object. Supports Schaefer
#' atlases with 7 or 17 networks and 100-1000 parcels (in steps of 100).
#'
#' @importFrom stringr str_extract_all
#' @import ggsegSchaefer
#' @export
get_ggseg_atlas <- function(atlas) {
  # Extract numbers from the atlas name
  matches <- stringr::str_extract_all(atlas$name, "\\d+")
  atlas_num <- unlist(matches)
  
  # Check for valid inputs
  if ((atlas_num[1] %in% seq(100, 1000, 100)) && (atlas_num[2] %in% c(7, 17))) {
    atlas_string <- paste0("schaefer", atlas_num[2], "_", atlas_num[1])
    return(get(atlas_string))
  } else {
    stop("Invalid atlas name. Must be Schaefer atlas with 7 or 17 networks and 100-1000 parcels.")
  }
}

#' Map Values to Schaefer Atlas Format
#'
#' @description
#' Converts values associated with atlas regions into a format suitable for
#' visualization with the Schaefer atlas in ggseg.
#'
#' @param atlas An atlas object containing Schaefer parcellation information
#' @param vals Numeric vector of values to map to atlas regions
#' @param thresh Numeric vector of length 2 specifying (min, max) thresholds.
#'   Values outside this range will be set to NA. Default: c(0,0)
#' @param pos Logical. If TRUE, uses raw values; if FALSE, uses absolute values
#'   for thresholding. Default: FALSE
#'
#' @return A ggseg brain atlas object with mapped values
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate left_join filter
#' @importFrom tibble as_tibble
#' @importFrom ggseg as_brain_atlas
#' @export
map_to_schaefer <- function(atlas, vals, thresh=c(0,0), pos=FALSE) {
  assert_that(length(vals) == length(atlas$orig_labels),
              msg="Length of vals must match number of atlas regions")
  
  fun <- if (pos) identity else abs
  
  ggatl <- get_ggseg_atlas(atlas)
  
  ret <- tibble(
    statistic = vals,
    label = atlas$orig_labels,
    hemi = atlas$hemi
  )
  
  rboth <- ggatl %>%
    as_tibble() %>%
    mutate(label = substr(label, 15, nchar(label))) %>%
    left_join(ret) %>%
    filter(!is.na(statistic)) %>%
    mutate(statistic = ifelse(
      fun(statistic) <= thresh[1] | fun(statistic) > thresh[2],
      statistic, NA
    )) %>%
    ggseg::as_brain_atlas()
}

#' Create Interactive Schaefer Atlas Visualization
#'
#' @description
#' Creates an interactive visualization of the Schaefer atlas using ggseg,
#' with optional value mapping and customizable appearance.
#'
#' @param atlas An atlas object containing Schaefer parcellation information
#' @param vals Numeric vector of values to visualize on the atlas
#' @param thresh Numeric vector of length 2 specifying (min, max) thresholds.
#'   Values outside this range will be set to NA. Default: c(0,0)
#' @param pos Logical. If TRUE, uses raw values; if FALSE, uses absolute values
#'   for thresholding. Default: FALSE
#' @param palette Character string specifying the color palette to use.
#'   Default: "Spectral"
#' @param interactive Logical. If TRUE, creates an interactive plot with tooltips.
#'   Default: TRUE
#' @param lim Numeric vector of length 2 specifying the range for color mapping.
#'   Default: range(vals)
#'
#' @return A ggplot2 object (if interactive=FALSE) or a ggiraph object
#'   (if interactive=TRUE) showing the visualization
#'
#' @examples
#' \dontrun{
#' # Load Schaefer atlas
#' atlas <- get_schaefer_atlas("7networks", 100)
#'
#' # Create random values for visualization
#' vals <- rnorm(100)
#'
#' # Create interactive plot
#' ggseg_schaefer(atlas, vals)
#'
#' # Create static plot with custom thresholds
#' ggseg_schaefer(atlas, vals, thresh=c(-1, 1), interactive=FALSE)
#' }
#'
#' @importFrom ggplot2 aes scale_fill_distiller
#' @importFrom ggseg ggseg
#' @importFrom ggiraph girafe opts_tooltip opts_hover opts_selection
#' @importFrom scales squish
#' @export
ggseg_schaefer <- function(atlas, vals, thresh=c(0,0), pos=FALSE,
                          palette="Spectral", interactive=TRUE, lim=range(vals)) {
  gatl <- map_to_schaefer(atlas, vals, thresh, pos)
  
  ggobj <- ggseg::ggseg(
    atlas = gatl,
    position = "stacked",
    colour = "gray",
    interactive = FALSE,
    guide = TRUE,
    mapping = aes(fill = statistic, tooltip = region, data_id = label)
  ) +
    scale_fill_distiller(
      palette = palette,
      limits = lim,
      direction = -1,
      oob = scales::squish
    )
  
  if (interactive) {
    ggiraph::girafe(
      ggobj = ggobj,
      width_svg = 8,
      height_svg = 6,
      options = list(
        opts_tooltip(
          opacity = .7,
          css = "font-family: Arial, Helvetica, sans-serif;"
        ),
        opts_hover(css = "fill:yellow;"),
        opts_selection(
          css = "fill:red;",
          type = "single",
          only_shiny = FALSE
        )
      )
    )
  } else {
    ggobj
  }
}
