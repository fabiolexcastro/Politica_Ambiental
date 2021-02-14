

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
  writeRaster(x = rst, filename = paste0('../tif/casanare_v2/chirps/', nme), overwrite = TRUE)
  print('Done!')
  
}

# Load data ---------------------------------------------------------------

# CHIRPS ------------------------------------------------------------------
root <- '//dapadfs/data_cluster_4/observed/gridded_products/chirps/daily'
fles <- list.files(root, full.names = TRUE, pattern = '.tif$')
year <- 1983:2016
fles <- grep(paste(year, collapse = '|'), fles, value = TRUE)

dptos <- shapefile('../shp/base/MGN_DPTO_POLITICO.shp')
dptos <- dptos[dptos@data$DPTO_CNMBR == 'CASANARE',]


# Apply the function ------------------------------------------------------
cl <- makeCluster(20)
registerDoSNOW(cl)

foreach(i = 1:length(fles), .verbose = TRUE) %dopar% {
  
  require(pacman)
  pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)
  
  my_crop(fle = fles[i])
  
}

stopCluster(cl)





