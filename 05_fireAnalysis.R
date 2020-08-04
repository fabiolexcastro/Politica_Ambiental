

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, ggpubr, ggspatial)

g <- gc(reset = TRUE)
options(scipen = 999)
rm(list = ls())

# Load data ---------------------------------------------------------------
fldr <- list.files('../shp/fire', full.names = T)
fles <- list.files(fldr, full.names = TRUE, pattern = '.shp$')
shps <- map(.x = fles, .f = st_read)
mpos <- st_read('../shp/base/mpios_geo_ok.shp')
mntr <- mpos %>% filter(NOM_MUNICI ==  'MONTERREY')
vrds <- st_read('../shp/base/veredas_ok.shp')

plot(st_geometry(mntr))

# Knowing the shapefile area
st_crs(4326)
mntr <- st_transform(x = mntr, crs = st_crs(3116))
area <- st_area(mntr) %>% as.numeric
area <- area / 10000
shps <- map(.x = shps, .f = function(k) k %>% st_transform(crs = st_crs(3116)))
vrds <- st_transform(vrds, crs = st_crs(3116))
mpos <- st_transform(mpos, crs = st_crs(3116))
mntr <- st_transform(mntr, crs = st_crs(3116))

# To cut the three points dataset for only Monterrey ----------------------
shps <- map(.x = shps, .f = function(k) k %>% st_intersection(., mntr))
nmes <- c('VIIRS J1', 'MODIS', 'VIIRS')

# To make the map ---------------------------------------------------------
g1 <- ggplot() +
  geom_sf(data = mpos, fill = NA, size = 0.9, col = 'grey') +
  geom_sf(data = vrds, fill = NA) +
  geom_sf(data = mntr, fill = NA, size = 1.1) +
  geom_sf(data = shps[[2]], col = '#3B0B0B', size = 1.1) +
  ggtitle(label = 'Puntos de calor registrados por sensor MODIS',
          subtitle = paste0('Cantidad: ', nrow(shps[[2]]))) +
  theme_bw() +
  labs(caption = 'Fechas: 2000-11-01 a 2019-12-31') +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = 'bold')) +
  coord_sf(xlim = extent(mntr)[1:2], 
           ylim = extent(mntr)[3:4],
           crs = st_crs(3116)) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.6, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", 
                   width_hint = 0.5) 

g2 <- ggplot() +
  geom_sf(data = mpos, fill = NA, size = 0.9, col = 'grey') +
  geom_sf(data = vrds, fill = NA) +
  geom_sf(data = mntr, fill = NA, size = 1.1) +
  geom_sf(data = shps[[3]], col = '#DF3A01', size = 1.1) +
  ggtitle(label = 'Puntos de calor registrados por sensor VIIRS',
          subtitle = paste0('Cantidad: ', nrow(shps[[3]]))) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = 'bold')) +
  labs(caption = 'Fechas: 2000-11-01 a 2019-12-31') +
  coord_sf(xlim = extent(mntr)[1:2], 
           ylim = extent(mntr)[3:4],
           crs = st_crs(3116)) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.6, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", 
                   width_hint = 0.5) 

gg <- ggarrange(g1, g2, nrow = 1, ncol = 2)
ggsave(plot = gg, filename = '../png/maps/nasa_firms.png', units = 'in', width = 14, height = 9, dpi = 300)





