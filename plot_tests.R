library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

#install.packages(c("cowplot", "googleway", "ggrepel", "ggspatial", "libwgeom")
                 
amy <- dplyr::filter(storms, name == "Amy")
amy

                 
world <- ne_countries(scale = "medium", returnclass = "sf") 

ggplot(data = world) +
  geom_sf() +
  theme_bw()
                 
ggplot(data = world) +
    geom_sf() +
    coord_sf(xlim = c(-80, -50), ylim = c(25, 45), expand = FALSE) +
    geom_point(data = amy, aes(long, lat, color = wind), size = 4) + 
      scale_color_gradient(low = "#fee0d2", high = "#de2d26") + 
      theme_bw()


amy$time_incs <- 1:nrow(amy)

rownames(amy)
1:nrow(amy)

# slider to zoom in and out
# plotting the time by color
# sort by name and year rather than just name
# color the map
# color the background