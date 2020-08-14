
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, sf, tidyverse)

# Load data ---------------------------------------------------------------
confl <- st_read('../shp/conflicto_uso_suelo/conflicto_uso_veredas.shp')
fires <- read_csv('../tbl/hotspots/hotspots_removeOTL.csv')
fires <- st_as_sf(x = fires, coords = c('LONGITUDE', 'LATITUDE'))
vrdas <- st_read('../shp/base/veredas_ok.shp')

# Projecting shapes to wgs 1984 -------------------------------------------
confl <- st_transform(x = confl, crs = st_crs(4326))
st_crs(fires) <- st_crs(4326)

# Intersect data ----------------------------------------------------------
intrs <- st_intersection(x = fires, y = confl)

# Summarize ---------------------------------------------------------------
smm <- intrs %>% 
  as.data.frame %>% 
  dplyr::select(-geometry) %>% 
  group_by(Conflicto) %>% 
  dplyr::summarise(count = n()) %>% 
  ungroup()

# To make the maps --------------------------------------------------------
plot(dplyr::select(intrs, Conflicto))

gg_map <- ggplot() +
  geom_sf(data = intrs, aes(fill = Conflicto, color = Conflicto)) +
  geom_sf(data = vrdas, fill = NA) +
  ggtitle(label = 'Tipo de conflicto de uso del suelo para\nlos puntos de calor') +
  theme(legend.position = 'bottom', 
        plot.title = element_text(size = 16, face = 'bold', hjust = 0.5))
ggsave(plot = gg_map,
       filename = '../png/maps/conflicto_uso_hotspot.png',
       units = 'in', width = 7, height = 11, dpi = 300)

st_write(obj = intrs, dsn = '../shp/conflicto_uso_suelo',
         layer = 'conflicto_uso_suelo_hotspots', 
         driver = 'ESRI Shapefile')

dir.create('../tbl/conflicto')
write.csv(smm, '../tbl/conflicto/conflicto_uso_suelo_incendios.csv', row.names = FALSE)
