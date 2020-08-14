

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)

# Load data ---------------------------------------------------------------

# Hotspots
hot <- read_csv('../tbl/hotspots/hotspots_removeOTL.csv')
hot <- st_as_sf(hot, coords = c('LONGITUDE', 'LATITUDE'))

# Buffer 365 meters
st_crs(hot) <- st_crs(4326)
hot <- st_transform(x = hot, crs = st_crs(3116))
hot <- st_buffer(x = hot, dist = 365)
hot <- st_transform(x = hot, crs = st_crs(4326))

# Ecosistemas
eco <- st_read('../shp/ecosistemas/ecosistemas_veredas.shp')
eco <- st_transform(eco, crs = st_crs(4326))

# Overlay between ecosistemas and hotspots --------------------------------
hot_eco <- st_intersection(x = hot, y = eco)

smm <- hot_eco %>% 
  as.data.frame() %>% 
  dplyr::select(ECOS_SINTE) %>% 
  filter(ECOS_SINTE != 'Rio') %>% 
  group_by(ECOS_SINTE) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>% 
  mutate(porc = count / sum(count) * 100)

smm <- smm %>% mutate(ECOS_SINTE = factor(ECOS_SINTE, levels = ECOS_SINTE))

# To make the graph -------------------------------------------------------
gg <- ggplot(data = smm, aes(x = ECOS_SINTE, y = porc)) +
  geom_col() +
  labs(x = '', 
       y = 'Porcentaje (%)') + 
  ggtitle(label = 'Pérdidas de ecosistemas asociados posiblemente a puntos de calor') +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5))
ggsave(plot = gg, 
       filename = '../png/graphs/ecosistemas_hotspots.png', 
       units = 'in', width = 12, height = 9, dpi = 300)




