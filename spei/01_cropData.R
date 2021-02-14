
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, foreach, parallel, doSNOW)
 
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
my_crop <- function(fle){
  
  print(fle)
  rst <- raster(fle)
  rst <- raster::crop(rst, dptos)
  rst <- raster::mask(rst, dptos)
  nme <- basename(fle)
  writeRaster(x = rst, filename = paste0('../tif/casanare/', nme), overwrite = TRUE)
  print('Done!')
  
}

# Load data ---------------------------------------------------------------

# CHIRTS ------------------------------------------------------------------
root <- '//catalogue/BaseLineData_cluster04/GLOBAL/Climate/CHIRTS'
fles <- list.files(root, full.names = TRUE, pattern = '.tif$')
dptos <- shapefile('../shp/base/MGN_DPTO_POLITICO.shp')
dptos <- dptos[dptos@data$DPTO_CNMBR == 'CASANARE',]

# Apply the function ------------------------------------------------------
map(.x = fles, .f = my_crop)



foreach(i = 1:length(fles), .verbose = TRUE) %dopar% {
  
  require(pacman)
  pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)
  
  my_crop(fle = fles[i])
  
}

stopCluster(cl)






