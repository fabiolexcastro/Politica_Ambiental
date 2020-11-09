
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
mnt <- st_read('../shp/base/veredas_mas_monterrey.shp')

# To get the coordinates
crd <- as(mnt, 'Spatial') %>% coordinates() %>% as.data.frame %>% slice(1) %>% setNames(c('lon', 'lat'))

# Download current climate ------------------------------------------------
tmax <- raster::getData(name = 'worldclim', var = 'tmax', res = 0.5, lon = crd[,1], lat = crd[,2])
tmax <- raster::crop(tmax, mnt) %>% raster::mask(., mnt)

tavg <- raster::getData(name = 'worldclim', var = 'tmean', res = 0.5, lon = crd[,1], lat = crd[,2])
tavg <- raster::crop(tmax, mnt) %>% raster::mask(., mnt)

tmin <- raster::getData(name = 'worldclim', var = 'tmin', res = 0.5, lon = crd[,1], lat = crd[,2])
tmin <- raster::crop(tmin, mnt) %>% raster::mask(., mnt)

prec <- raster::getData(name = 'worldclim', var = 'prec', res = 0.5, lon = crd[,1], lat = crd[,2])
prec <- raster::crop(tmin, mnt) %>% raster::mask(., mnt)

# Write the final raster --------------------------------------------------
Map('writeRaster', x = unstack(tmax), filename = paste0('../tif/climate/current/tmax_', 1:12, '.tif'), overwrite = TRUE)
Map('writeRaster', x = unstack(tavg), filename = paste0('../tif/climate/current/tavg_', 1:12, '.tif'), overwrite = TRUE)
Map('writeRaster', x = unstack(tmin), filename = paste0('../tif/climate/current/tmin_', 1:12, '.tif'), overwrite = TRUE)
Map('writeRaster', x = unstack(prec), filename = paste0('../tif/climate/current/prec_', 1:12, '.tif'), overwrite = TRUE)

# Digital Elevation Model 
dem <- raster::getData(name = 'SRTM', lon = crd[,1], lat = crd[,2])
dem <- raster::crop(dem, mnt) %>% raster::mask(., mnt)
plot(mnt, add = TRUE)

de2 <- raster::getData(name = 'SRTM', lon = crd[,1], lat = 5.2)
de2 <- raster::crop(de2, mnt) %>% raster::mask(., mnt)

# Mosaicking
de3 <- raster::mosaic(x = dem, y = de2, fun = 'mean')
writeRaster(x = de3, filename = '../tif/dem/dem_monterrey_ha.tif', overwrite = TRUE)
