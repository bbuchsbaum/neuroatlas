# Global variables to avoid R CMD check NOTEs
utils::globalVariables(c("hemi", "orig_label", "hemi.x", "hemi.y", "x", "y", "group", "fill", "region", "label"))

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
#' @export
get_ggseg_atlas <- function(atlas) {
  # Check if ggsegSchaefer is available
  if (!requireNamespace("ggsegSchaefer", quietly = TRUE)) {
    stop("Package 'ggsegSchaefer' is required for this function but is not installed.\n",
         "To install it, run:\n",
         "  remotes::install_github('LCBC-UiO/ggsegSchaefer')\n",
         "or use the ggseg r-universe:\n",
         "  options(repos = c(ggseg = 'https://ggseg.r-universe.dev',\n",
         "                    CRAN = 'https://cloud.r-project.org'))\n",
         "  install.packages('ggsegSchaefer')",
         call. = FALSE)
  }
  
  # Extract numbers from the atlas name (e.g., "Schaefer-100-7networks")
  matches <- stringr::str_extract_all(atlas$name, "\\d+")
  atlas_num <- unlist(matches)
  
  # Atlas name format is "Schaefer-{parcels}-{networks}networks"
  # So atlas_num[1] is parcels, atlas_num[2] is networks
  parcels <- as.numeric(atlas_num[1])
  networks <- as.numeric(atlas_num[2])
  
  # Check for valid inputs
  if ((parcels %in% seq(100, 1000, 100)) && (networks %in% c(7, 17))) {
    atlas_string <- paste0("schaefer", networks, "_", parcels)
    # Use :: operator to access the object from ggsegSchaefer
    atlas_obj <- tryCatch({
      # Try multiple ways to access the object
      if (exists(atlas_string, envir = asNamespace("ggsegSchaefer"))) {
        get(atlas_string, envir = asNamespace("ggsegSchaefer"))
      } else if (exists(atlas_string, envir = as.environment("package:ggsegSchaefer"))) {
        get(atlas_string, envir = as.environment("package:ggsegSchaefer"))
      } else {
        # Try using :: operator directly
        eval(parse(text = paste0("ggsegSchaefer::", atlas_string)))
      }
    }, error = function(e) {
      stop("Failed to load atlas '", atlas_string, "' from ggsegSchaefer: ", e$message,
           call. = FALSE)
    })
    return(atlas_obj)
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
#' @importFrom ggplot2 aes scale_fill_distiller ggplot theme_void coord_fixed
#' @importFrom ggseg ggseg
#' @importFrom ggiraph girafe opts_tooltip opts_hover opts_selection geom_polygon_interactive
#' @importFrom scales squish
#' @export
ggseg_schaefer <- function(atlas, vals, thresh=c(0,0), pos=FALSE,
                          palette="Spectral", interactive=TRUE, lim=range(vals)) {
  mapped_data <- map_to_schaefer(atlas, vals, thresh, pos)
  
  # Get the ggseg atlas and update its data
  gatl <- get_ggseg_atlas(atlas)
  gatl$data <- mapped_data
  
  # Check if we're using the forked version with interactive support
  has_interactive_support <- tryCatch({
    # Try to call ggseg with interactive parameter
    args <- list(
      atlas = gatl,
      position = "stacked",
      colour = "gray",
      interactive = interactive,
      guide = TRUE,
      mapping = aes(fill = .data$statistic)
    )
    
    # Add interactive aesthetics if using interactive mode
    if (interactive) {
      args$mapping <- aes(fill = .data$statistic, tooltip = .data$region, data_id = .data$label)
    }
    
    ggobj <- do.call(ggseg::ggseg, args)
    TRUE
  }, error = function(e) {
    # Fallback for CRAN version without interactive parameter
    FALSE
  })
  
  if (!has_interactive_support) {
    # Use CRAN version of ggseg and handle interactivity ourselves
    base_plot <- ggseg::ggseg(
      atlas = gatl,
      position = "stacked",
      colour = "gray",
      guide = TRUE,
      mapping = aes(fill = .data$statistic)
    )
    
    if (interactive) {
      # Create interactive version manually
      # We need to modify the ggplot object to use ggiraph geometries
      ggobj <- .make_ggseg_interactive(base_plot, gatl$data)
    } else {
      ggobj <- base_plot
    }
  } else {
    # Using forked version, ggobj is already created above
    ggobj <- ggobj
  }
  
  # Add color scale
  ggobj <- ggobj +
    scale_fill_distiller(
      palette = palette,
      limits = lim,
      direction = -1,
      oob = scales::squish
    )
  
  # Return the plot (already interactive if forked version was used)
  if (interactive && !has_interactive_support) {
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

#' Convert ggseg plot to interactive
#' @keywords internal
#' @noRd
#' @importFrom ggplot2 ggplot_build ggplot_gtable aes
#' @importFrom ggiraph geom_polygon_interactive
.make_ggseg_interactive <- function(plot, data) {
  # This is a simplified approach - recreate the plot with interactive geoms
  # Extract the plot data
  plot_data <- ggplot_build(plot)$data[[1]]
  
  # Merge with original data to get labels
  if ("group" %in% names(plot_data) && "group" %in% names(data)) {
    plot_data <- merge(plot_data, data, by = "group", all.x = TRUE)
  }
  
  # Create new interactive plot
  ggplot(plot_data, aes(x = .data$x, y = .data$y, group = .data$group)) +
    geom_polygon_interactive(
      aes(fill = .data$fill, tooltip = .data$region, data_id = .data$label),
      colour = "gray"
    ) +
    theme_void() +
    coord_fixed()
}
