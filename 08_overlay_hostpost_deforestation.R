
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, lubridate,
               RColorBrewer, ggspatial, outliers)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999,
        stringsAsFactors = FALSE)

# Load data ---------------------------------------------------------------
pnts <- read_csv('../tbl/hotspots/hotspots_removeOTL.csv')
vrd <- shapefile('../shp/base/veredas_ok.shp')
lss <- raster('../tif/forest/hansen/Hansen_GFC-2019-v1.7_lossyear_10N_080W.tif')

# Cutting loss year by Monterrey ------------------------------------------
lss <- raster::crop(lss, vrd)
lss <- raster::mask(lss, vrd)

# Table to points ---------------------------------------------------------
coordinates(pnts) <- ~ LONGITUDE + LATITUDE
crs(pnts) <- crs(lss)
pnts <- pnts %>% spTransform(x = ., CRSobj = '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
bffr <- raster::buffer(x = pnts, width = 365)
bffr <- spTransform(x = bffr, CRSobj = crs(lss))
bffr <- SpatialPolygonsDataFrame(bffr, data.frame(id = 1))
bffr@data$gid <- 1:nrow(bffr@data)

# Raster to points --------------------------------------------------------
lss[which(lss[] == 0)] <- NA
lss <- rasterToPoints(lss, spatial = FALSE)
lss <- as_tibble(lss)
names(lss) <- c('lon', 'lat', 'year')
lss <- lss %>% mutate(id = 1:nrow(.)) %>% dplyr::select(id, lon, lat, year)

# Intersect between hotspots and deforestation zones ----------------------
intr <- raster::extract(bffr, lss[,2:3])
intr <- as.data.frame(intr) %>% as_tibble(intr)
intr <- intr %>% dplyr::select(gid)
lss <- cbind(lss, intr) %>% as_tibble()
lss_hot <- drop_na(lss)

# Calculating hectareas
3119 * 900 / 10000 # 280 has de deforestacion podrian estar asociadas a incendios

# To make the map ---------------------------------------------------------
gg_map <- ggplot() +
  geom_sf(data = st_as_sf(vrd), fill = NA) +
  geom_point(data = lss_hot, aes(x = lon, y = lat), color = 'red') +
  labs(x = 'Longitud',
       y = 'Latitud',
       caption = 'Adaptado de Hansen et al 2019 y NASA - FIRMS') +
  ggtitle(label = 'Zonas de deforestación posiblemente\nasociadas a puntos de calor') +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'))

ggsave(plot = gg_map,
       filename = '../png/maps/bosque_hotspots.png', 
       units = 'in', width = 12, height = 9, dpi = 300)

# Nota
# Revisar la cantidad de incendios asociados a deforestacion por vereda, sacar el count

smm_fire <- raster::extract(vrd, lss_hot[,2:3]) %>% 
  pull(NOMBRE_VER) %>% 
  table() %>% 
  as.data.frame() %>% 
  setNames(c('Vereda', 'Frecuencia')) %>% 
  mutate(Vereda = iconv(Vereda, from = 'UTF-8', to = 'latin1')) %>% 
  arrange(desc(Frecuencia)) %>% 
  mutate(Has = Frecuencia * 900 / 10000,
         Has = round(Has, 1),
         Porcentaje = Has / sum(Has) * 100,
         Porcentaje = round(Porcentaje, 1))

write.csv(smm_fire, '../tbl/hotspots/count_incendios_deforestation.csv', row.names = FALSE)  

top_smm_fire <- smm_fire %>% 
  top_n(n = 10, wt = Porcentaje) %>% 
  mutate(Vereda = factor(Vereda, levels = pull(., 1)))

gg_top <- ggplot(data = top_smm_fire, aes(x = Vereda, y = Porcentaje)) + 
  geom_col() +
  labs(x = 'Vereda', y = 'Porcentaje (%)') +
  scale_y_continuous(limits = c(0, 100)) +
  ggtitle(label = 'Participación de cantidad de puntos de calor que\nestiman deforestación en las veredas de Monterrey') +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5))
ggsave(plot = gg_top, 
       filename = '../png/graphs/top10_puntoscalor_deforestation.png', 
       units = 'in', width = 12, height = 9, dpi = 300)
