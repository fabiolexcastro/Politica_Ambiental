
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, sf, RSAGA, gtools)

# Function to use ---------------------------------------------------------
downscaling <- function(fle){
  
  print('Processing...!')
  # fle <- temp[1]
  
  root.dem <- '../tif/dem/dem_monterrey_ha.tif'
  root.tmp <- '../tif/climate/current/'
  
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

# Environment SAGA
env <- rsaga.env(path = 'C:/SAGA')

# List files temperature
vars <- list.files('../tif/climate/current', full.names = TRUE, pattern = '.tif$') %>% 
  mixedsort() 
temp <- grep('tm', vars, value = TRUE)
prec <- grep('pr', vars, value = TRUE)
tavg <- grep('ta', vars, value = TRUE)

# Making the downscaling --------------------------------------------------
for(i in 1:length(tavg)){
  
  print(temp[i])
  downscaling(fle = tavg[i])
  print('Done!')
  
}

