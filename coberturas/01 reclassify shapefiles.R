

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, ggspatial, tidyverse, glue, gtools, fs, fasterize, RColorBrewer, colorspace)

# Load data ---------------------------------------------------------------
fles <- list.files('../shp', full.names = T, pattern = '.shp$')
shpf <- map(fles, st_read)
tbls <- map(shpf, as_tibble)
tbls <- map(tbls, function(x) dplyr::select(x, -geometry))

tb00 <- tbls[[1]]
tb00 <- tb00 %>% dplyr::select(NIVEL3) %>% mutate(year = 2000)
tb00 <- tb00 %>% setNames(c('cobertura', 'year'))
tb00 <- tb00 %>% mutate(cobertura = str_sub(cobertura, 8, nchar(cobertura)))

tb05 <- tbls[[2]]
tb05 <- tb05 %>% dplyr::select(NIVEL3) %>% mutate(year = 2005)
tb05 <- tb05 %>% setNames(c('cobertura', 'year'))
tb05 <- tb05 %>% mutate(cobertura = str_sub(cobertura, 8, nchar(cobertura)))

tb10 <- tbls[[3]]
tb10 <- tb10 %>% dplyr::select(LEYENDA3N) %>% mutate(year = 2010)
tb10 <- tb10 %>% setNames(c('cobertura', 'year'))
tb10 <- tb10 %>% mutate(cobertura = str_sub(cobertura, 8, nchar(cobertura)))

tb18 <- tbls[[4]]
tb18 <- tb18 %>% dplyr::select(cobert) %>% mutate(year = 2018)
tb18 <- tb18 %>% setNames(c('cobertura', 'year'))

dir_create('../tbl/shp_tbl')
Map('write.csv', tbls, glue('../tbl/shp_tbl/{basename(fles)}.csv'), row.names = F)

tbal <- list(tb00, tb05, tb10, tb18) %>% bind_rows()
tbal <- tbal %>% distinct(cobertura, year)
write.csv(tbal, '../tbl/shp_tbl/cov_allv.csv', row.names = FALSE)

# Read reclassify ---------------------------------------------------------
rclf <- read_csv('../tbl/shp_tbl/cov_allv_v2.csv')
rclf <- rclf %>% distinct(rcl) %>% mutate(gid = 1:12) %>% inner_join(., rclf, by = 'rcl')

# Tidy each shapefile -----------------------------------------------------
sh00 <- shpf[[1]] %>% 
  dplyr::select(cobertura = NIVEL3, geometry) %>% 
  mutate(cobertura = str_sub(cobertura, 8, nchar(cobertura))) %>% 
  inner_join(., rclf, by = c('cobertura' = 'cobertura')) %>% 
  mutate(year = 2000)

sh05 <- shpf[[2]] %>% 
  dplyr::select(cobertura = NIVEL3, geometry) %>% 
  mutate(cobertura = str_sub(cobertura, 8, nchar(cobertura))) %>% 
  inner_join(., rclf, by = c('cobertura' = 'cobertura')) %>% 
  mutate(year = 2005)

sh10 <- shpf[[3]] %>% 
  dplyr::select(cobertura = LEYENDA3N, geometry) %>% 
  mutate(cobertura = str_sub(cobertura, 8, nchar(cobertura))) %>% 
  inner_join(., rclf, by = c('cobertura' = 'cobertura')) %>% 
  mutate(year = 2010)

sh18 <- shpf[[4]] %>% 
  dplyr::select(cobertura = cobert, geometry) %>%  
  inner_join(., rclf, by = c('cobertura' = 'cobertura')) %>% 
  mutate(year = 2018)

# Fasterize ---------------------------------------------------------------
mask <- raster('G:/Proyectos/politica_ambiental/tif/cover_mpio/rcl/cover_rcl_2000_v2.tif')
limt <- shapefile('G:/Proyectos/politica_ambiental/shp/base/mpio_monterrey.shp')
limt <- spTransform(x = limt, CRSobj = crs(mask))
mask <- raster::crop(mask, limt) %>% raster::mask(., limt)
mask <- mask * 0 + 1
mask <- projectRaster(mask, crs = '+proj=longlat +datum=WGS84 +no_defs')

sh00 <- st_transform(x = sh00, crs = st_crs(4326))
sh05 <- st_transform(x = sh05, crs = st_crs(4326))
sh10 <- st_transform(x = sh10, crs = st_crs(4326))
sh18 <- st_transform(x = sh18, crs = st_crs(4326))

rs00 <- fasterize(sf = sh00, raster = mask, field = 'gid')
rs05 <- fasterize(sf = sh05, raster = mask, field = 'gid')
rs10 <- fasterize(sf = sh10, raster = mask, field = 'gid')
rs18 <- fasterize(sf = sh18, raster = mask, field = 'gid')

# Write these rasters
writeRaster(rs00, '../tif/cover/rs00.tif')
writeRaster(rs05, '../tif/cover/rs05.tif')
writeRaster(rs10, '../tif/cover/rs10.tif')
writeRaster(rs18, '../tif/cover/rs18.tif')

par(mfrow = c(2, 2))
plot(rs00)
plot(rs05)
plot(rs10)
plot(rs18)

# Rasters to table --------------------------------------------------------
tble <- raster::stack(rs00, rs05, rs10, rs18) %>% 
  setNames(paste0('y_', c('00', '05', '10', '18'))) %>% 
  rasterToPoints(., spatial = FALSE) %>% 
  as_tibble() %>% 
  gather(year, value, -x, -y) %>% 
  mutate(year = gsub('y_', '', year),
         year = paste0(20, year))
tble <- tble %>% 
  mutate(year = factor(year, levels = c(2000, 2005, 2010, 2018)))
tble <- tble %>% 
  mutate(value = factor(value, levels = 1:12))


# To make the maps --------------------------------------------------------
lbls <- rclf %>% distinct(gid, rcl)

gmap <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = value)) +
  scale_fill_manual(values = brewer.pal(n = 12, name = 'Paired'),
                    labels = lbls$rcl) +
  facet_wrap(.~year) +
  coord_sf(crs = st_crs(3116)) + 
  labs(x = 'Longitude', y = 'Latitude', fill = 'Tipo de \nCobertura') +
  theme_void() + 
  theme(legend.position = 'bottom', 
        strip.text = element_text(size = 14, face = 'bold')) +
  guides(fill = guide_legend(ncol = 2)) +
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering()) 

dir_create('../png/maps')
ggsave(plot = gmap, filename = '../png/maps/cov_rcl_all.jpg', 
       units = 'in', width = 8, height = 10, dpi = 300)

