
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse)

g <- gc(reset = TRUE)
options(scipen = 999)

# Load data
root <- '../tif/casanare_v3'
fles <- list.files(root, full.names = TRUE, pattern = '.tif$')
tmin <- grep('tmin', fles, value = TRUE)
tmax <- grep('tmax', fles, value = TRUE)
prec <- grep('chirps', fles, value = TRUE)
mntr <- shapefile('../shp/base/veredas_mas_monterrey.shp')

# Extract by mask 
my_crop <- function(x){ print(x); x %>% raster %>% crop(., mntr) %>% raster::mask(., mntr)}
dir.create('../tif/monterrey_v1_prec')

# Tmin
tmin_crop <- map(tmin, my_crop)
Map('writeRaster', x = tmin_crop, filename = paste0('../tif/monterrey_v1/', names(stack(tmin_crop)), '.tif'), overwrite = TRUE)

# Tmax
tmax_crop <- map(tmax, my_crop)
Map('writeRaster', x = tmax_crop, filename = paste0('../tif/monterrey_v1/', names(stack(tmax_crop)), '.tif'), overwrite = TRUE)

# Prec
prec_crop <- map(prec, my_crop)
Map('writeRaster', x = prec_crop, filename = paste0('../tif/monterrey_v1/', names(stack(prec_crop)), '.tif'), overwrite = TRUE)



