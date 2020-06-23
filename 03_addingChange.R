
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, terra, sf, tidyverse, ggspatial)

rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
thr <- readRDS('../rds/threshold_forest_hansen.rds')
f00 <- raster('../tif/forest/hansen/Hansen_treecover_monterrey.tif')
mps <- shapefile('../shp/base/mpios_geo_ok.shp')
mtr <- mps[mps@data$NOM_MUNICI %in% 'MONTERREY',]
vrd <- shapefile('../shp/base/veredas_ok.shp')

# Area municipio
prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
mtr_prj <- spTransform(x = mtr, CRSobj = prj)
area(mtr_prj) / 10000

de1 <- raster::getData(name = 'SRTM', lon = as.numeric(coordinates(mtr)[,1]), lat = as.numeric(coordinates(mtr)[,2]))
de2 <- raster::getData(name = 'SRTM', lon = -72.921, lat = 5.159)
de1 <- raster::crop(de1, mtr) %>% raster::mask(., mtr)
de2 <- raster::crop(de2, mtr) %>% raster::mask(., mtr)
dem <- mosaic(de1, de2, fun = 'mean')
writeRaster(dem, '../tif/dem/dem_monterrey.tif', overwrite = T)

# Reclassify forest cover
f00[which(f00[] < thr)]  <- 2
f00[which(f00[] >= thr)] <- 1

f00 <- raster::crop(f00, vrd)
f00 <- raster::mask(f00, vrd)

# Extract by mask, loss year -----------------------------------------------
lss <- raster('../tif/forest/hansen/Hansen_GFC-2019-v1.7_lossyear_10N_080W.tif')
lss <- raster::crop(lss, vrd)
lss <- raster::mask(lss, vrd)

gan <- raster('../tif/forest/hansen/Hansen_GFC-2019-v1.7_gain_10N_080W.tif')
gan <- raster::crop(gan, vrd)
gan <- raster::mask(gan, vrd)

# Adding change -----------------------------------------------------------
f00 <- raster::resample(f00, lss, method = 'bilinear')
f19 <- f00

lss[which(lss[] > 0)] <- 2
lss[which(lss[] == 0)] <- 1

f19[which(lss[] == 2)] <- 3
f19[which(gan[] == 1)] <- 1

# To make the map (before and after) --------------------------------------
frs <- stack(f00, f19)
names(frs) <- c('tree2000', 'tree2019')
vls <- rasterToPoints(frs)
vls <- as_tibble(vls)
vls <- vls %>% gather(var, value, -x, -y)
vls <- vls %>% mutate(value = round(value, 0))
vls <- vls %>% mutate(value = factor(value, levels = c('1', '2', '3')))
vls <- vls %>% mutate(var = if_else(var == 'tree2000', 'Cobertura boscosa 2000', 'Cobertura boscosa 2019'))

gg <- ggplot(vls) +
  geom_tile(aes(x = x, y = y, fill = value)) +
  facet_wrap(~var) +
  scale_fill_manual(values = c('#0B610B', '#DDDEA4', '#FF0000'),
                    labels = c('Bosque', 'No bosque', 'Pérdida'),
                    name = '') +
  ggtitle(label = 'Cambio de cobertura de boscosa') +
  geom_sf(data = st_as_sf(vrd), fill = NA) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold')) +
  labs(x = 'Longitud', 
       y = 'Latitud', 
       caption = 'Adaptado de Hansen et al., 2014') +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.6, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", 
                   width_hint = 0.5) 
ggsave(plot = gg, filename = '../png/maps/bosque_nobosque_2000_2019.png',
       units = 'in', width = 12, height = 9, dpi = 300)

vls %>% mutate(value = as.numeric(as.character(value))) %>% filter(var == 'tree2000') %>% filter(value == 2) %>% pull() %>% sum(na.rm = T)
vls %>% mutate(value = as.numeric(as.character(value))) %>% filter(var == 'tree2019') %>% filter(value == 2) %>% pull() %>% sum(na.rm = T)

