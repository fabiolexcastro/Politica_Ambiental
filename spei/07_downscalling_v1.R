
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, sf, RSAGA, gtools)

rm(list = ls())

# Function to use ---------------------------------------------------------
downscaling <- function(fle){
  
  print('Processing...!')
  # fle <- tmax[1]
  
  root.dem <- '../tif/dem/dem_casanare_ha.tif'
  root.tmp <- '../tif/casanare_v3/'
  
  rsl <- rsaga.geoprocessor(lib = 'statistics_regression',
                            module = 'GWR for Grid Downscaling',
                            param = list(PREDICTORS = paste0(root.dem),
                                         REGRESSION = paste0(root.tmp, basename(fle)),
                                         DEPENDENT = fle),
                            intern = TRUE,
                            display.command = TRUE,
                            env = env)
  
  print('Done!')
  return(rsl)
  
}

# Load data ---------------------------------------------------------------

# Environment SAGA
env <- rsaga.env(path = '//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_monterrey/saga/saga-7.9.0_x64')

# List files temperature
vars <- list.files('../tif/casanare_v2/chirts/monthly', full.names = TRUE, pattern = '.tif$') %>% 
  mixedsort() 
tmin <- grep('tmin', vars, value = TRUE)
tmax <- grep('tmax', vars, value = TRUE)

# Making the downscaling --------------------------------------------------
for(i in 2:length(tmax)){
  
  print(tmax[i])
  downscaling(fle = tmax[i])
  print('Done!')
  
}



