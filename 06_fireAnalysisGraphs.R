
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, lubridate,
               RColorBrewer, ggspatial)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999,
        stringsAsFactors = FALSE)

# Load data ---------------------------------------------------------------

# Base shapes
mpos <- st_read('../shp/base/mpios_geo_ok.shp')
mntr <- mpos %>% filter(NOM_MUNICI ==  'MONTERREY')
vrds <- st_read('../shp/base/veredas_ok.shp')
vrds <- vrds %>% dplyr::select(DPTOMPIO, CODIGO_VER, NOMBRE_VER, AREA_HA)

# Fire shapes
mods <- st_read('../shp/fire/mpio/fire_modis.shp') %>% 
  dplyr::select(-TYPE, -ID_ESPACIA)
viir <- st_read('../shp/fire/mpio/fire_viirs.shp') %>% 
  dplyr::select(-TYPE, -ID_ESPACIA) %>% 
  mutate(DAYNIGHT = NA) %>% 
  dplyr::select(LATITUDE, LONGITUDE, BRIGHTNESS = BRIGHT_TI4, SCAN, TRACK, ACQ_DATE,
                ACQ_TIME, SATELLITE, INSTRUMENT, CONFIDENCE, VERSION, BRIGHT_T31 = BRIGHT_TI5,
                FRP, DAYNIGHT, OBJECTID, NOM_MUNICI, COD_DEPTO, NOMBRE_DPT, geometry)

# Join the two datasets ---------------------------------------------------
fire <- rbind(mods, viir)
fire <- st_transform(x = fire, crs = st_crs(4326))

# Counting the fire points per each town ----------------------------------
fire <- st_intersection(x = fire, y = vrds)
fire_cnt <- fire %>% 
  as.data.frame %>% 
  dplyr::select(-geometry) %>% 
  group_by(NOMBRE_VER) %>% 
  dplyr::summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count))
vrds <- inner_join(vrds, fire_cnt, by = 'NOMBRE_VER')
plot(dplyr::select(vrds, count))
ext <- extent(vrds)

# Making the map ----------------------------------------------------------
gg_fire <- ggplot() +
  geom_sf(data = mpos, fill = NA) +
  geom_sf(data = vrds, aes(fill = count)) +
  # geom_sf_label(data = vrds, aes(label = NOMBRE_VER)) +
  scale_fill_gradientn(colours = brewer.pal(name = 'YlOrRd', n = 9)) +
  coord_sf(xlim = ext[1:2], ylim = ext[3:4], crs = st_crs(4326),
           label_axes = waiver()) +
  theme_bw() +
  ggtitle(label = 'Conteo de puntos de calor en Monterrey Casanare') +
  labs(fill = 'Conteo puntos\nde calor',
       caption = 'Adaptado de NASA FIRMS, 2020') +
  theme(legend.position = 'bottom',
        legend.key.width = unit(3.5, 'line'),
        plot.title = element_text(size = 14, hjust = 0.5, face = 'bold')) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.6, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", 
                   width_hint = 0.5) 
ggsave(plot = gg_fire, 
       filename = '../png/maps/puntos_calor_vereda.png', 
       units = 'in', width = 9, height = 12, dpi = 300)  

vrds <- vrds %>% rename(count_fire = count)

# To write the final shapes -----------------------------------------------
st_write(obj = vrds, 
         dsn = '../shp/fire/mpio', 
         layer = 'fire_modis_veredas', 
         d %>% r = 'ESRI Shapefile')



