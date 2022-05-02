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
    legend.text = element_text(size=8, color="white"),
    legend.title = element_text(size=10, color="white"),
    panel.grid.major = element_line(color = "grey60", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=18, color="#498c44", hjust=1),
    plot.caption = element_text(size=6, color="white", hjust=.15, vjust=20),
    plot.subtitle = element_text(size=12, color="#498c44", hjust=1),
    plot.margin = unit(c(t=0, r=0, b=0, l=0),"lines"),
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
