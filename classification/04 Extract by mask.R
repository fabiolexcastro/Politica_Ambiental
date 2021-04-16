
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools)

rm(list = ls())

# Function to extract by mask ---------------------------------------------
extract_mask <- function(fle, vra){
  
  # fle <- fls[20]
  # vra <- vrd %>% filter(NOMBRE_VER == 'BARBASCO')
  
  nme <- as.character(vra$NOMBRE_VER)
  rst <- raster::stack(fle)
  rst <- raster::crop(rst, as(vra, 'Spatial'))
  rst <- raster::mask(rst, as(vra, 'Spatial'))
  print('Done!')
  writeRaster(x = rst, filename = paste0('../raster/process/', nme, '.tif'), overwrite = TRUE)
  return(rst)
  
}

# Load data ---------------------------------------------------------------
vrd <- st_read('E:/politica_ambiental/shp/base/veredas_ok.shp')
zur <- st_read('../shp/veredas/ZONA_URBANA.shp')

veredas <- vrd %>% pull(NOMBRE_VER) %>% unique %>% sort()
rsl <- list.files('../raster/process')

nrow(vrd)

# Rasters 
fls <- list.files('../raster/raw', full.names = TRUE, pattern = '.tif$')
fls <- grep('cmp_', fls, value = TRUE)

nms
basename(rsl)

# To apply the function ---------------------------------------------------
nms <- vrd$NOMBRE_VER %>% sort()

for(i in 1:length(fls)-1){
  extract_mask(fle = fls[i], vra = vrd %>% filter(NOMBRE_VER == nms[i]))
}

extract_mask(fle = fls[22], vra = zur)


