
# 01 Read KMZ -------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, mapsf, tidyverse, rlang, hrbrthemes)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
file <- '../kml/Cover_2019_forest.kml'
cvrt <- read_sf(file)

# To see the frequency table ----------------------------------------------
lbls <- table(cvrt$Name) %>% 
  as.data.frame() %>% 
  setNames(c('Name', 'Frequency')) %>% 
  mutate(Name = as.character(Name)) 

clss <- c('Bosque de galeria', 'Bosque de galeria', 'Bosque denso',
          'Bosque fragmentado', 'Cultivos', 'Cultivo de palma africana', 'Cultivo de palma africana',
          'Cultivo permanente', 'Herbazal', 'Tejido urbano', 'Mosaico cultivos, pastos, espacios naturales',
          'Mosaico cultivos, pastos, espacios naturales', 'Palma africana', 'Pasto enmalezado', 'Pastos limpios',
          'Pasto enmalezado', 'Pastos limpios', 'Piscicola', 'Tierra desnuda', rep('Vegetacion secundaria',  4))

lbls <- lbls %>% mutate(Name_2 = clss)
cvrt <- inner_join(cvrt, lbls, by = 'Name')
cvrt <- cvrt %>% dplyr::select(Name, Frequency, geometry)
unique(cvrt$geometry)

cvrt %>% pull(Name) %>% unique()

postn <- st_geometry_type(cvrt) %>% as.character()
mplgn <- which(postn == 'MULTIPOLYGON')
mpnts <- which(postn == 'POINT')

# Polygons
plgns <- cvrt[mplgn,]
plgns <- st_zm(plgns)
plgns <- inner_join(plgns, lbls, by = c('Name' = 'Name'))
plgns <- plgns %>% dplyr::select(Name = Name_2, geometry)
unique(plgns$Name)

# Points
point <- cvrt[mpnts,]
point <- st_zm(point)
point <- inner_join(point, lbls, by = c('Name' = 'Name'))
point <- point %>% dplyr::select(Name = Name_2, geometry)
unique(point$Name)

dir.create('../shp/classes', recursive = TRUE)
st_write(obj = plgns, dsn = '../shp/classes', layer = 'plgns_v2', driver = 'ESRI Shapefile', overwrite = TRUE, append = TRUE)
st_write(obj = point, dsn = '../shp/classes', layer = 'point_v2', driver = 'ESRI Shapefile', overwrite = TRUE, append = TRUE)

# Unique labels -----------------------------------------------------------
lbls <- distinct(lbls, Name_2) %>% 
  rename(Name = Name_2) %>% 
  mutate(gid = 1:nrow(.))

# Rasterize ---------------------------------------------------------------
maskr <- stack('../raster/composite_2019_vrd.tif')
maskr <- maskr[[1]]
maskr <- maskr * 0
plgns <- plgns %>% inner_join(., lbls, by = 'Name')
plgns <- as(plgns, 'Spatial')
plgns_rstr <- raster::rasterize(plgns, maskr, field = 'gid')
plgns_pnts <- rasterToPoints(plgns_rstr, spatial = TRUE)
plgns_pnts <- plgns_pnts %>% 
  st_as_sf() %>% 
  inner_join(., lbls, by = c('layer' = 'gid')) %>% 
  as(., 'Spatial')
plgns_pnts <- plgns_pnts %>% st_as_sf %>% dplyr::select(-layer) %>% as(., 'Spatial')

# Point Join with the table
point <- point %>% inner_join(., lbls, by = 'Name')
point <- point %>% dplyr::select(-gid)
point <- point %>% as(., 'Spatial')

# Checking the names
names(plgns_pnts)
names(point)

# Join the two shapefiles into only one
all <- rbind(plgns_pnts, point)
all <- cbind(as.data.frame(coordinates(all)), all$Name)
all <- as_tibble(all)
all <- all %>% setNames(c('lon', 'lat', 'name'))

# Piscicolas --------------------------------------------------------------
pscls <- st_read('../shp/points/point_2019.shp')
mf_map(pscls, pch = 16, col = 'red')
pscls <- st_coordinates(pscls) %>% 
  as.data.frame() %>% 
  setNames(c('lon', 'lat')) %>% 
  mutate(name = 'Piscicola')

# Add piscicola to the global database ------------------------------------
all <- rbind(all, pscls)

# Some nice graphs to see our data ----------------------------------------
gg_bar <- table(all$name) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  mutate(Var1 = factor(Var1, levels = Var1)) %>% 
  ggplot(data = ., aes(x = Var1, y = Freq, fill = Var1)) +
  geom_col() +
  geom_text(aes(label = Freq, vjust = 0), fontface = 'bold') +
  theme_ipsum() +
  theme(axis.text.x = element_text(size = 9, angle = 45, vjust = 0.6, face = 'bold'), 
        legend.position = 'top',
        axis.text.y = element_text(size = 9, face = 'bold'),
        legend.text = element_text(size = 7)) +
  scale_y_log10() +
  labs(fill = '', x = '', y = 'Frecuencia (n)')
ggsave(plot = gg_bar, filename = '../png/graphs/geom_bar_frequency.png', units = 'in', 
       width = 9, height = 6.5, dpi = 300)

# Save the final table
dir.create('../tbl')
write.csv(all, file = '../tbl/training_points.csv', row.names = FALSE)




