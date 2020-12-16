
# Load libraries ---------------------------------------------------
require(pacman) 
pacman::p_load(raster, rgdal, raster, sf, fst, gtools, stringr, tidyverse, RColorBrewer, pastecs, psych)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Read the functions ------------------------------------------------------
source('35_functions.R')

# Load data --------------------------------------------------------
crn <- list.files('../tif/climate/current/worldclim_v2/stack', full.names = TRUE, pattern = '.tif$') 
crn <- mixedsort(crn)
vrs <- c('prec', 'tmean', 'tmax', 'tmin')  
lim <- shapefile('../shp/base/veredas_mas_monterrey.shp')

crn <- stack(crn)

# RCP 4.5 -----------------------------------------------------------------
ftr <- list.files('../workspace/climate/raster/ensemble/rcp_4.5/2030s', full.names = TRUE)
ftr <- mixedsort(ftr)
ftr <- stack(ftr)

make_stack <- function(cr, ft){
  
  cr <- crn
  ft <- ftr
  
  names(cr) <- paste0('crn_', names(cr))
  names(ft) <- paste0('ftr_', names(ft))
  
  ms.cr <- cr[[1]] * 0
  ms.ft <- ft[[1]] * 0
  
  print('Stacking current - future')
  ft <- raster::crop(ft, ms.cr) %>% raster::mask(., ms.cr)
  st <- addLayer(cr, ft)
  
  print('Raster to table')
  tb <- rasterToPoints(st, spatial = FALSE) %>% as_tibble()
  
  print('Tidy to table')
  tb <- tb %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -gid, -x, -y) 
  tb <- tb %>% mutate(period = str_sub(var, 1, 3))
  tb <- tb %>% mutate(variable = str_sub(var, 5, 8))
  tb <- tb %>% mutate(month = parse_number(var))
  tb <- tb %>% dplyr::select(gid, x, y, period, variable, month, value)
  tb <- tb %>% spread(period, value)
  tb <- tb %>% mutate(ftr = ifelse(variable %in% c('tmax', 'tmea', 'tmin'), ftr / 10, ftr))
  tb <- tb %>% mutate(difference = ftr - crn)
  tb <- tb %>% mutate(porcentaje = (difference / crn) * 100)
  
}




