
# Load data ---------------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, terra, sf, tidyverse, ggspatial)

rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
tcv <- raster('../tif/forest/hansen/Hansen_GFC-2019-v1.7_treecover2000_10N_080W.tif')
mps <- shapefile('../shp/base/mpios_geo_ok.shp')
frs <- raster('../tif/forest/ideam/Bosque_No_Bosque_2000/Geotiff/SBQ_SMBYC_BQNBQ_V5_2000.tif')
vrd <- st_read('../shp/base/veredas.shp')

# Subsetting treecover
mtr <- mps[mps@data$NOM_MUNICI %in% 'MONTERREY',]
tcv <- raster::crop(x = tcv, y = mtr)
tcv <- raster::mask(x = tcv, mask = mtr)

# Subsetting forest / no forest
frs <- raster::crop(frs, mtr)
frs <- raster::mask(frs, mtr)

# Knowing the threshold --------------------------------------------------
frs <- raster::resample(frs, tcv, method = 'bilinear')
stk <- stack(frs, tcv)
names(stk) <- c('ideam', 'hansen')

# Raster to table
tbl <- rasterToPoints(stk) 
tbl <- as_tibble(tbl)

tbl_frs <- filter(tbl, ideam == 1)
avg_frs <- data.frame(type = 'forest', value = pull(tbl_frs, hansen))
thr_frs <- mean(avg_frs, na.rm = TRUE)

# To make the histogram
gg_hist <- ggplot(data = avg_frs, aes(x = value)) +
  geom_histogram(bins = 100) +
  ggtitle(label = 'Histograma para los valores de porcentaje de bosque\nHansen et al., 2013') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 7, face = "bold"),
        axis.title.y = element_text(size = 7, face = "bold")) +
  labs(x = 'Bosque (%)',
       y = 'Frecuencia')
ggsave(plot = gg_hist,
       filename = '../png/graphs/histograma_bosque_hansen.png',
       units = 'cm',
       width = 12,
       height = 8, 
       dpi = 300)

# To save the threshold value
saveRDS(thr_frs, file = '../rds/threshold_forest_hansen.rds')

# To make the forest map --------------------------------------------------
tbl_frs <- mutate(tbl, hansen_bin = if_else(hansen < thr_frs, 0, 1))
rst_frs <- rasterFromXYZ(tbl_frs[,c(1, 2, 5)])
tbl_frs <- rasterToPoints(rst_frs)
tbl_frs <- as_tibble(tbl_frs)
mtr <- st_as_sf(mtr) 

gg_map <- ggplot() +
  geom_tile(data = tbl_frs, aes(x = x, y = y, fill = factor(hansen_bin))) +
  scale_fill_manual(values = c('#DDDEA4', '#0B610B'),
                    labels = c('No bosque', 'Bosque'),
                    name = '') +
  geom_sf(data = mtr, fill = NA) +
  geom_sf(data = vrd, fill = NA) +
  # scale_x_continuous(breaks = c(72, 24)) +
  coord_sf() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom') +
  labs(x = 'Longitud',
       y = 'Latitud')

ggsave(plot = gg_map,
       filename = '../png/maps/bosque_nobosque_hns_2000.png',
       units = 'in', 
       width = 9,
       height = 7,
       dpi = 300)
  

