

#' @keywords internal
#' @noRd
#' @import ggsegSchaefer
get_ggseg_atlas <- function(atlas) {
  # Extract numbers from the atlas name
  matches <- stringr::str_extract_all(atlas$name, "\\d+")
  atlas_num <- unlist(matches)
  
  # Check for valid inputs
  if ((atlas_num[1] %in% seq(100, 1000, 100)) && (atlas_num[2] %in% c(7, 17))) {
    atlas_string <- paste0("schaefer", atlas_num[2], "_", atlas_num[1])
    return(get(atlas_string))
  } else {
    stop("Invalid atlas name.")
  }
}


#' @keywords internal
#' @noRd
map_to_schaeffer <- function(atlas, vals, thresh=c(0,0), pos=FALSE) {
  assert_that(length(vals) == length(atlas$orig_labels))
  fun <- if (pos) {
    identity
  } else {
    abs
  }
  
  ggatl <- get_ggseg_atlas(atlas)
  
  
  #mind <- match(atlas$orig_labels, substr(ggsegSchaefer::schaefer17_400$data$label, 15, nchar(ggsegSchaefer::schaefer17_400$data$label)))
  ret <- tibble(statistic=vals, label=atlas$orig_labels, hemi=atlas$hemi)
  
  rboth <- ggatl %>%
    as_tibble() %>%
    mutate(label = substr(label, 15, nchar(label))) %>%
    left_join(ret) %>% filter(!is.na(statistic)) %>%
    mutate(statistic=ifelse(fun(statistic) <= thresh[1] | fun(statistic) > thresh[2], statistic, NA)) %>%
    ggseg::as_brain_atlas()
}


#' @export
ggseg_schaefer <- function(atlas, vals, thresh=c(0,0), pos=FALSE, palette="Spectral", interactive=TRUE, lim=range(vals)) {
  gatl <- map_to_schaeffer(atlas, vals, thresh, pos)
  ggobj <- ggseg::ggseg(atlas=gatl, position="stacked", colour="gray", interactive=FALSE, guide=TRUE,
                        mapping=aes(fill=statistic, tooltip=region,data_id=label)) + scale_fill_distiller(palette=palette, 
                                                                                                          #breaks=seq(limits[1], limits[2], length.out=10),
                                                                                                          limits=lim,
                                                                                                          direction=-1,
                                                                                                          oob=scales::squish)
  
  #na.value="grey"
  if (interactive) {                                                                                                                                                              
    ggiraph::girafe(ggobj=ggobj, width_svg=8, height_svg=6, options=list(
      opts_tooltip(opacity = .7,
                   css="font-family: Arial, Helvetica, sans-serif;"),
      opts_hover(css = "fill:yellow;"),
      opts_selection(css="fill:red;", type = "single", only_shiny = FALSE)))
  } else {
    ggobj
  }
  
}
