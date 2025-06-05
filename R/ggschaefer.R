# Global variables to avoid R CMD check NOTEs
utils::globalVariables(c("hemi", "orig_label", "hemi.x", "hemi.y"))

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
    # Use :: operator to access the object from ggsegSchaefer
    return(eval(parse(text = paste0("ggsegSchaefer::", atlas_string))))
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
#' @return A tibble with mapped values suitable for ggseg visualization
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate left_join filter .data select coalesce
#' @importFrom tibble as_tibble
#' @importFrom ggseg as_brain_atlas
#' @export
map_to_schaefer <- function(atlas, vals, thresh=c(0,0), pos=FALSE) {
  assert_that(length(vals) == length(atlas$orig_labels),
              msg="Length of vals must match number of atlas regions")
  
  fun <- if (pos) identity else abs
  
  ggatl <- get_ggseg_atlas(atlas)
  
  # Create a mapping between our labels and ggseg labels
  # Our labels are like "LH_Vis_1", ggseg labels are like "lh_7Networks_LH_Vis_1"
  networks_str <- if (grepl("7networks", atlas$name, ignore.case = TRUE)) "7Networks_" else "17Networks_"
  
  ret <- tibble(
    statistic = vals,
    orig_label = atlas$orig_labels,
    hemi = atlas$hemi
  ) %>%
    mutate(
      # Convert our labels to match ggseg format
      ggseg_label = paste0(
        ifelse(hemi == "left", "lh_", "rh_"),
        networks_str,
        orig_label
      )
    )
  
  # Join with ggseg atlas data
  rboth <- ggatl$data %>%
    left_join(ret, by = c("label" = "ggseg_label")) %>%
    mutate(
      # Preserve original hemi column from ggseg data
      hemi = coalesce(.data$hemi.x, .data$hemi.y),
      statistic = ifelse(
        is.na(.data$statistic) | 
        fun(.data$statistic) <= thresh[1] | 
        fun(.data$statistic) > thresh[2],
        NA_real_, 
        .data$statistic
      )
    ) %>%
    select(-orig_label, -hemi.x, -hemi.y)  # Remove temporary columns
  
  # Return the tibble instead of the brain atlas object
  return(rboth)
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
  mapped_data <- map_to_schaefer(atlas, vals, thresh, pos)
  
  # Get the ggseg atlas and update its data
  gatl <- get_ggseg_atlas(atlas)
  gatl$data <- mapped_data
  
  ggobj <- ggseg::ggseg(
    atlas = gatl,
    position = "stacked",
    colour = "gray",
    interactive = FALSE,
    guide = TRUE,
    mapping = aes(fill = .data$statistic, tooltip = .data$region, data_id = .data$label)
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
