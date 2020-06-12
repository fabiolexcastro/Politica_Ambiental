
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, terra, sf, tidyverse, ggspatial)

rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
tabFunc <- function(indx, extracted, region, regname) {
  dat <- as.data.frame(table(extracted[[indx]]))
  dat$name <- region[[regname]][[indx]]
  return(dat)
}
count_area_forest <- function(x, y){
  
  # Proof
  # x <- frs
  # y <- 'Cobertura Boscosa'
  
  print(y)
  x <- raster::crop(x, vrd)
  x <- raster::mask(x, vrd)
  
  pix <- res(x)[1] * res(x)[2]
  
  # To count the area (hectareas)
  ext <- raster::extract(x, vrd, method = 'simple')
  tabs <- lapply(seq(ext), tabFunc, ext, vrd, 'NOMBRE_VER')
  df <- do.call(rbind, tabs)#unimos todas las tablas en una sola
  colnames(df) <- c('value', 'freq', 'name')
  df <- as_tibble(df)
  df <- df %>% mutate(mts = freq * pix)
  df <- df %>% mutate(has = mts / 10000)
  df <- df %>% mutate(type = if_else(value == '1', 'Bosque', 'No Bosque'))
  df <- df %>% mutate(name = str_to_sentence(name))
  df <- df %>% group_by(name) %>% mutate(prc = has / sum(has) * 100)
  df <- df %>% mutate(type = factor(type, levels = c('Bosque', 'No Bosque')))
  
  # To make the graph
  gg <- ggplot(data = df, aes(x = type, y = prc, group = type, fill = type)) +
    geom_col() +
    scale_fill_manual(values = c('#0B610B', '#F2F5A9'),
                      name = '') +
    facet_wrap(~ name) +
    theme(legend.position = 'top') +
    labs(x = '',
         y = 'Porcentaje (%)') 
  
  ggsave(plot = gg, filename = paste0('../png/graphs/', y, '.png'), units = 'in',
         width = 9, height = 11, dpi = 300)
  write.csv(df, paste0('../tbl/forest/', y, '.csv'), row.names = FALSE)
  
  
}

# Load data ---------------------------------------------------------------
thr <- readRDS('../rds/threshold_forest_hansen.rds')
frs <- raster('../tif/forest/hansen/Hansen_treecover_monterrey.tif')
mps <- shapefile('../shp/base/mpios_geo_ok.shp')
mtr <- mps[mps@data$NOM_MUNICI %in% 'MONTERREY',]
vrd <- shapefile('../shp/base/veredas_ok.shp')

prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

# Projecting the veredas file 
vrd <- spTransform(vrd, prj)
frs <- projectRaster(frs, crs = prj)

# Reclassify forest cover
frs[which(frs[] < thr)]  <- 2
frs[which(frs[] >= thr)] <- 1

# Extract by mask, loss year -----------------------------------------------
lss <- raster('../tif/forest/hansen/Hansen_GFC-2019-v1.7_lossyear_10N_080W.tif')
lss <- raster::crop(lss, spTransform(vrd, crs(lss)))
lss <- raster::mask(lss, spTransform(vrd, crs(lss)))
lss <- projectRaster(lss, crs = prj)

gan <- raster('../tif/forest/hansen/Hansen_GFC-2019-v1.7_gain_10N_080W.tif')
gan <- raster::crop(gan, spTransform(vrd, crs(gan)))
gan <- raster::mask(gan, spTransform(vrd, crs(gan)))
gan <- projectRaster(gan, crs = prj)

# Making the barplot for the treecover (year = 2000)
lss <- projectRaster(lss, crs = prj)
gan <- projectRaster(gan, crs = prj)

# Count area forest -------------------------------------------------------
count_area_forest(x = frs, y = 'Cobertura boscosa')

# Count loss forest area =--------------------------------------------------
count_yearly <- function(x, y){
  
  x <- lss
  y <- 'Perdida bosque'
  
  pnt <- rasterToPoints(x)
  pnt <- as.data.frame(pnt)
  pnt <- as_tibble(pnt)
  pnt <- setNames(pnt, c('x', 'y', 'value'))
  
  pix <- res(x)[1] * res(x)[2]
  
  vls <- raster::extract(vrd, pnt[,1:2])
  vls <- vls[,'NOMBRE_VER']
  pnt <- pnt %>% mutate(vereda = vls)
  pnt <- drop_na(pnt)
  pnt <- pnt %>% mutate(value = round(value))
  pnt <- pnt %>% filter(value > 0)
  pnt <- pnt %>% mutate(year = 2000 + value)
  pnt <- pnt %>% dplyr::select(-value)
  lvl <- as.character(2001:2019)
  pnt <- pnt %>% mutate(year = factor(year, levels = lvl))
  pnt <- pnt %>% group_by(vereda, year) %>% dplyr::summarise(count = n()) %>% ungroup()
  pnt <- pnt %>% mutate(mts = count * pix)
  pnt <- pnt %>% mutate(has = mts / 10000)
  
  # To make the graph
  gg <- ggplot(data = pnt, aes(x = year, y = has, group = 1)) +
    geom_line(size = 1.1) +
    facet_wrap(~ vereda) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = '',
         y = 'Porcentaje (%)') 
  
  ggsave(plot = gg, filename = paste0('../png/graphs/', y, '.png'), units = 'in',
         width = 11, height = 13, dpi = 300)
  write.csv(pnt, '../tbl/forest/Perdida_bosque_anual.csv', row.names = FALSE)

  
  
  
 } 





