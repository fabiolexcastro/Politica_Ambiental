

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, furrr, future, stringr, sf, tidyverse, gtools)

rm(list = ls())

# Functions to use --------------------------------------------------------
get_mosaic <- function(nmr){
  
  # Proof
  # nmr <- 1
  
  print(nmr)
  
  # To read the band as a raster
  rst <- map(.x = 1:length(fles), .f = function(k){
    rst <- raster::stack(fles[k])
    rst <- rst[[nmr]]
  })
  
  # To make the mosaic
  names(rst) <- NULL
  rst$fun <- mean
  msc <- do.call(mosaic, rst)
  writeRaster(msc, filename = paste0('../raster/mosaic/mnt_', nms_bnd[nmr], '.tif'))
  print('Done...!')
  
}

# Load data ---------------------------------------------------------------
fles <- list.files('../raster/process', full.names = TRUE, pattern = '.tif$')
vrds <- st_read('E:/politica_ambiental/shp/base/veredas_ok.shp')

names(stack(fles[[1]]))

# Cargar nombres de las bandas --------------------------------------------
nms_bnd <- names(stack('../raster/raw/cmp_barbasco.tif'))

# To apply the mosaic -----------------------------------------------------

# Manually
map(.x = 1:length(nms_bnd), .f = get_mosaic)

# Parallel
plan(cluster, workers = 6, gc = TRUE)
rslt <- furrr::future_map(.x = 1:length(nms_bnd), .f = get_mosaic)
future:::ClusterRegistry('stop')
gc(reset = TRUE)




