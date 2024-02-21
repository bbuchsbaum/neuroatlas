#' @import ggseg
#' @import ggsegGlasser
#' @import geojsonsf
#' 
plot_glasser <- function(x, vals, position="dispersed") {
  # stack <- match.arg(position,
  #                    c("stacked", "dispersed"),
  #                    several.ok = FALSE
  # )
  # 
  # if(stack == "stacked"){
  #   atlas <- as_ggseg_atlas(glasser)
  #   atlas <- tidyr::unnest(atlas, ggseg)
  #   atlas <- ggseg:::stack_brain(atlas)
  #   atlas  %>%
  #     group_by(.id) %>% summarise(geometry = st_combine(geometry)) %>%
  #     st_cast("POLYGON") -> res_sfdata
  # }
  
  mset <- atlas %>% group_by(.id) %>% summarize(poly={
    lat <- split(.lat, .subid)
    lon <- split(.long, .subid)
    list(st_multipolygon(list(lapply(1:length(lat), function(i) {
      cbind(lon[[i]], lat[[i]])
    }))))
  }) 
  
  fgeom <- st_sfc(mset$poly)
  fstack <- st_sf(data.frame(hemi=glasser$data$hemi, region=glasser$data$region, 
                   side=glasser$data$side, label=glasser$data$label), 
        geometry = fgeom) 
                
  
  ## unravel, then recreate multipolygons
  get_geo <- function(hem, sid, pt) {
    g <- dplyr::filter(glasser$data, hemi==hem & side == sid & !is.na(region))
    bbox <- sf::st_bbox(g)
    if (hem == "right") {
      stg <- sf::st_geometry(g)
      #stg <- stg + c(-bbox$xmin, bbox$ymax)
      stg <- stg + c(pt[0], pt[1])
      g <- sf::st_sf(stg)
      #g <- g + c(-bbox$xmin, bbox$ymax)
    }
    
    g
    #geo <- geojsonio::geojson_json(g)
  }
  
  get_chart <- function(geo, data, var, name, show=TRUE) {
    data %>%
      echarts4r::e_charts(label) %>%
      echarts4r::e_map_register(name, geo) %>%
      echarts4r::e_map_(var, map = name, nameProperty="label") %>%
      echarts4r::e_visual_map_(var, 
                               #categories=vals$region,
                               scale=function(x) x,
                               #type="piecewise",
                               #calculable=FALSE,
                               inRange=list(color=data$color.x[1:180]),
                               show=show, padding=0) %>%
      e_theme("blue") %>%
      echarts4r::e_group("4charts")
  }
  
  gz <- geojsonio::geojson_json(fstack)
  get_chart(gz, vals, "joe", "lat", TRUE) 
  
  g1 <- geojsonio::geojson_json(dplyr::filter(glasser$data, hemi=="left" & side == "lateral"))
  g2 <- geojsonio::geojson_json(dplyr::filter(glasser$data, hemi=="right" & side == "lateral"))
  g3 <- geojsonio::geojson_json(dplyr::filter(glasser$data, hemi=="left" & side == "medial"))
  g4 <- geojsonio::geojson_json(dplyr::filter(glasser$data, hemi=="right" & side == "medial"))
 
  palette <- data.frame(color=glasser$palette, region=names(glasser$palette))
  vals <- dplyr::inner_join(vals, palette, by="region")
  chart1 <- get_chart(g1, vals, "joe", "lat", TRUE) 
  chart2 <- get_chart(g2, vals, "joe", "med", FALSE)  
  chart3 <- get_chart(g3, vals, "joe", "lat2", FALSE) 
  chart4 <- get_chart(g4, vals, "joe", "med2", FALSE)   %>% 
    echarts4r::e_connect_group("4charts")
  echarts4r::e_arrange(chart1,chart2, chart3, chart4, rows = 2, cols = 2, width="lg")
}


# stack_brain <- function (atlas){
#   if(unique(atlas$type) == "cortical"){
#     stack <- dplyr::group_by(atlas, hemi, side)
#     stack <- calc_stack(stack)
#     
#     atlas = mutate(atlas,
#                    .lat = ifelse(hemi %in% "right",
#                                  .lat + (stack$.lat_max[1]), .lat),
#                    .long = ifelse(hemi %in% "right" & side %in% "lateral",
#                                   .long - stack$.long_min[3], .long),
#                    .long = ifelse(hemi %in% "right" & side %in%  "medial",
#                                   .long + (stack$.long_min[2] - stack$.long_min[4]),
#                                   .long))
#     
#   }else if(unique(atlas$type) == "subcortical"){
#     stack <- group_by(atlas, side)
#     stack <- calc_stack(stack)
#     stack <- arrange(stack, .long_min)
#     
#     for(k in 1:nrow(stack)){
#       atlas <-  mutate(atlas,
#                        .lat = ifelse(side %in% stack$side[k],
#                                      .lat + mean(stack$.lat_max)*k, .lat),
#                        .long = ifelse(side %in% stack$side[k],
#                                       .long - stack$.long_mean[k],
#                                       .long))
#     }
#   }else{
#     cat("Atlas '.type' not set, stacking not possible.")
#   }
#   
#   return(atlas)
# }
# 
# calc_stack <- function(stack){
#   stack <- summarise_at(
#     stack,
#     vars(.long,
#          .lat),
#     list(min = min, max = max, sd = sd, mean = mean)
#   )
#   
#   stack <- mutate(stack, sd = .lat_sd + .long_sd)
#   
#   stack$.lat_max[1] <- ifelse(
#     stack$.lat_max[1]/4.5 < stack$.lat_sd[1],
#     stack$.lat_max[1] + stack$.lat_sd[1],
#     stack$.lat_max[1]
#   )
#   stack
# }