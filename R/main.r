################################################################################
#                 3D maps with sf and ggplot2 in R
#                 Milos Popovic
#                 2022/05/01
################################################################################

setwd("C:/Users/milos/Downloads/forest_height_brazil")

windowsFonts(georg = windowsFont('Georgia'))

install.packages('terra', repos='https://rspatial.r-universe.dev')
# libraries we need
libs <- c("rayshader", "tidyverse", "sf", 
  "classInt", "giscoR", "terra", "exactextractr")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# define longlat CRS
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

# 1. GET BRAZIL SF DATA
#---------
get_brazil_sf <- function(brazil, brazil_hex, brazil_sf) {

  brazil <- giscoR::gisco_get_countries(
    year = "2016",
    epsg = "4326",
    resolution = "10",
    country = "Brazil")  %>% 
    st_transform(3575)

  # Make grid of circa 30,000 m2
  brazil_hex <- st_make_grid(brazil, 
      cellsize = (3 * sqrt(3) * 107^2)/2, 
      what = "polygons", 
      square = F) %>%
    st_intersection(brazil) %>%
    st_sf() %>%
    mutate(id = row_number()) %>% filter(
    st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% 
    st_cast("MULTIPOLYGON") #transform to multipolygons

  brazil_sf <- st_transform(brazil_hex, crs = crsLONGLAT) #transform back to longlat

  return(brazil_sf)
}

# 2. LOAD FOREST RASTER
#---------

get_forests <- function(url, forests) {

  url <- c("/vsicurl/https://glad.geog.umd.edu/Potapov/Forest_height_2019/Forest_height_2019_SAM.tif")
  forests <- rast(url)

  return(forests)
}

# 3. AGGREGATE RASTER VALUES
#---------

get_brazil_forest_agg <- function(brazil_sf, forests, brazil_forest, brazil_ext) {

  forests <- get_forests()
  brazil_sf <- get_brazil_sf()

  brazil_forest <-  forests
  brazil_ext <- st_bbox(brazil_sf) %>% as.vector()
  ext(brazil_forest) <- brazil_ext
  br_forest <- terra::aggregate(brazil_forest, fact=5)
  
  return(br_forest)
}

# 4. ZONAL STATISTICS
#---------

get_brazil_zonal_stats <- function(br_forest, brazil_sf, brazil_df, br_df) {

  br_forest <- get_brazil_forest_agg()
  brazil_sf <- get_brazil_sf()

  brazil_df  <- exact_extract(br_forest, brazil_sf, "mean")
  br_df <- as.data.frame(brazil_df)
  br_df$id <- 1:max(nrow(br_df))

  return(br_df)
}

# 5. MERGE NEW DF WITH HEX OBJECT
#---------

get_brazil_final <- function(f, brazil_sf, br_df) {

  br_df <- get_brazil_zonal_stats()
  brazil_sf <- get_brazil_sf()

  f <- left_join(br_df, brazil_sf, by="id") %>% # join with transformed sf object
      st_as_sf() %>%
      filter(!st_is_empty(.)) #remove null features

  names(f)[1] <- "val"
  f$val[is.na(f$val)] <- 0

  return(f)
}

f <- get_brazil_final()

# 6. GENERATE BREAKS AND COLORS
#---------

get_breaks <- function(vmin, vmax, brk, breaks, all_breaks) {

  vmin <- min(f$val, na.rm=T)
  vmax <- max(f$val, na.rm=T)

  brk <- round(classIntervals(f$val, 
              n = 6, 
              style = 'fisher')$brks, 1) %>%
        head(-1) %>%
        tail(-1) %>%
        append(vmax)

  breaks <- c(vmin, brk)
  all_breaks <- list(vmin, vmax, breaks)
  return(all_breaks)
}

get_colors <- function(cols, newcol, ncols, cols2) {

  cols = rev(c("#1b3104", "#386c2e", 
          "#498c44", "#5bad5d", "#8dc97f", "#c4e4a7"))
  newcol <- colorRampPalette(cols)
  ncols <- 6
  cols2 <- newcol(ncols)

  return(cols2)
}
 
# 7. MAP
#---------

get_forest_map <- function(breaks, vmin, vmax, cols2, p) {

  vmin <- get_breaks()[[1]]
  vmax <- get_breaks()[[2]]
  breaks <- get_breaks()[[3]]
  cols2 <- get_colors()

  p <- ggplot(f) +
    geom_sf(aes(fill = val), color = NA, size=0) +
    scale_fill_gradientn(name="In meters",
      colours=cols2,
      breaks=breaks,
      labels=round(breaks, 1),
      limits=c(vmin, vmax))+   
    guides(fill=guide_legend(
      direction = "horizontal",
      keyheight = unit(2.5, units = "mm"),
      keywidth = unit(2.55, units = "mm"),
      title.position = 'top',
      title.hjust = .5,
      label.hjust = .5,
      nrow = 7,
      byrow = T,
      reverse = F,
     label.position = "left")) +
  coord_sf(crs = crsLONGLAT)+
  theme_minimal() +
  theme(text = element_text(family = "georg", color = "#22211d"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = c(0.8, .1),
    legend.text = element_text(size=6, color="white"),
    legend.title = element_text(size=8, color="white"),
    panel.grid.major = element_line(color = "grey60", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=18, color="#498c44", hjust=1, vjust=-5),
    plot.caption = element_text(size=6, color="white", hjust=.15, vjust=20),
    plot.subtitle = element_text(size=12, color="#498c44", hjust=1, vjust=-5),
    plot.margin = unit(c(t=0, r=0, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
    plot.background = element_rect(fill = "grey60", color = NA), 
    panel.background = element_rect(fill = "grey60", color = NA), 
    legend.background = element_rect(fill = "grey60", color = NA),
    panel.border = element_blank())+
  labs(x = "", 
    y = NULL, 
    title = "Average forest cover height in Brazil", 
    subtitle = expression(paste("per 30,000", m^{2}, "of land area")), 
    caption = "©2022 Milos Popovic (https://milospopovic.net)\n Data: https://glad.umd.edu/dataset")

  return(p)
}

p <- get_forest_map()

plot_gg(p,
  multicore = T,
  width=5,
  height=5,
  scale=150,
  shadow_intensity = .75,
  sunangle = 360,
  offset_edges=T,
  windowsize=c(1400,866),
  zoom = .4, 
  phi = 30, 
  theta = -30)

render_snapshot("brazil_forest_height_2019.png", clear=T)
