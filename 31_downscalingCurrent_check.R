

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, sf, RSAGA, gtools)

g <- gc(reset = TRUE)
rm(list = ls())

# Function to use ---------------------------------------------------------
downscaling <- function(fle){
  
  env <- rsaga.env(path = 'C:/SAGA')
  
  print('Processing...!')
  # fle <- temp[1]
  
  root.dem <- '../tif/dem/dem_monterrey_ha_bff.tif'
  root.tmp <- '../tif/climate/current/worldclim_v2/'
  
  rsl <- rsaga.geoprocessor(lib = 'statistics_regression',
                            module = 'GWR for Grid Downscaling',
                            param = list(PREDICTORS = paste0(root.dem),
                                         REGRESSION = paste0(root.tmp, 'ha_', basename(fle)),
                                         DEPENDENT = fle),
                            intern = TRUE,
                            display.command = TRUE,
                            env = env)
  
  print('Done!')
  return(rsl)
  
}

# Load data ---------------------------------------------------------------
fls <- list.files('E:/data/WORLDCLIM/Version20', full.names = TRUE, pattern = '.tif$')
fls <- grep('tav', fls, value = TRUE)
lim <- shapefile('../shp/base/veredas_mas_monterrey.shp')
stk <- stack(fls)
dem <- raster('../tif/dem/dem_casanare_ha.tif')

# Project the limit shapefile ---------------------------------------------
prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
lim.prj <- spTransform(x = lim, CRSobj = prj)
lim.prj <- raster::buffer(x = lim.prj, width = 3000)
lim.bff <- spTransform(x = lim.prj, CRSobj = crs(lim))

# DEM Cutting -------------------------------------------------------------
dem <- raster::crop(dem, lim.bff) 
dem <- raster::mask(dem, lim.bff)

writeRaster(dem, '../tif/dem/dem_monterrey_ha_bff.tif')

# Extract by mask ---------------------------------------------------------
stk.cut <- raster::crop(stk, lim.bff)
stk.cut <- raster::mask(stk.cut, lim.bff)
stk.cut <- unstack(stk.cut)

# Write the files ---------------------------------------------------------
Map('writeRaster', x = stk.cut, filename = paste0('../tif/climate/current/bff_tavg_', 1:12, '.tif'), overwrite = TRUE)

# Downscaling -------------------------------------------------------------
fls_tav <- list.files('../tif/climate/current', full.names = TRUE, pattern = '.tif$')
fls_tav <- grep('bff_tav', fls_tav, value = TRUE)
fls_tav <- mixedsort(fls_tav)

# Testing the function ----------------------------------------------------
map(.x = fls_tav, .f = downscaling)

fls <- list.files('../tif/climate/current/worldclim_v2', full.names = TRUE, pattern = '.tif')
fls <- grep('bff', fls, value = TRUE)
fls <- mixedsort(fls)

stk <- raster::stack(fls)
stk <- raster::crop(stk, lim) 
stk <- raster::mask(stk, lim)
stk <- unstack(stk)

Map('writeRaster', x = stk, filename = paste0('../tif/climate/current/ha_tavg_', 1:12, '.tif'), overwrite = TRUE)
