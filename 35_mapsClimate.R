
# Load libraries ---------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, raster, gtools, stringr)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
read_raster <- function(x){
  
  x <- grep(x, crn, value = TRUE)
  x <- mixedsort(x)
  x <- stack(x) 
  
  return(x)
  
}
get_table_current <- function(st, vr){
  
  # st <- crn.ppt
  # vr <- 'prec'
  
  tbl <- rasterToPoints(st)
  tbl <- as.data.frame(tbl)
  tbl <- as_tibble(tbl)
  names(tbl) <- c('x', 'y', paste0(vr, '_', 1:12))
  tbl <- tbl %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -x, -y, -gid)
  mnt <- str_split(string = pull(tbl, var), pattern = '_')
  mth <- sapply(1:length(mnt), function(k) mnt[[k]][2])
  tbl <- tbl %>% mutate(month = as.numeric(mth))
  tbl <- tbl %>% mutate(variable = vr)
  tbl <- tbl %>% dplyr::select(gid, x, y, variable, month, value)
  
  print('Done!')
  return(tbl)
  
}
get_table_future <- function(vr){
  
  # vr <- 'prec'
  
  rst <- grep(vr, fls, value = TRUE)
  rst <- mixedsort(rst)
  rst <- stack(rst)
  tbl <- rasterToPoints(rst)
  tbl <- as.data.frame(tbl)
  tbl <- as_tibble(tbl)  
  tbl <- tbl %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -x, -y, -gid)
  mnt <- str_split(string = pull(tbl, var), pattern = '_')
  mth <- sapply(1:length(mnt), function(k) mnt[[k]][2])
  tbl <- tbl %>% mutate(month = as.numeric(mth))
  tbl <- tbl %>% mutate(variable = vr)
  tbl <- tbl %>% dplyr::select(gid, x, y, variable, month, value)
  print('Done!')
  return(tbl)
  
}
calc_ensemble <- function(pth){
  
  pth <- paste0('../tif/climate/future/rcp_4.5/', yrs[1])
  vrs <- c('prec', 'tmean', 'tmax', 'tmin') 
  gcm <- list.files(pth, full.names = T)
  ftr <- map(.x = 1:length(gcm), .f = function(k) list.files(gcm[k], full.names = TRUE, pattern = '.tif') %>% grep('/ha_', ., value = TRUE) %>% mixedsort())
  
  vrs <- c(paste0('prec_', 1:12), paste0('tmean_', 1:12), paste0('tmin_', 1:12), paste0('tmax_', 1:12))
  vrs <- grep(paste0(c('prec', 'tmean'), collapse = '|'), vrs, value = TRUE)
  vrs <- paste0(vrs, '.tif')
  
  rst <- map(.x = 1:length(vrs), .f = function(k){
    
    rst <- lapply(1:length(ftr), function(j){
      
      print(j)
      print(vrs[k])
      fle <- grep(vrs[k], ftr[[j]], value = TRUE)
      print(fle)
      lyr <- raster(fle)
      return(lyr)
      
    })
    
    rst <- stack(rst)
    avg <- mean(rst)
    print(paste0('Done ', vrs[k], ftr[[j]]))
    return(avg)
    
  })
  
  rst <- stack(rst)
  names(rst) <- gsub('.tif', '', vrs)
  print('Done!')
  
  out <- paste0('E:/politica_ambiental/workspace/climate/raster/ensemble/rcp_4.5/', yrs[1])
  Map('writeRaster', x = unstack(rst), filename = paste0(out, '/', names(rst), '.tif'), overwrite = TRUE)
  
}

# Load data --------------------------------------------------------
crn <- list.files('../tif/climate/current', full.names = TRUE, pattern = '.tif$') 
crn <- grep('ha', crn, value = TRUE)  
vrs <- c('prec', 'tavg', 'tmax', 'tmin')  

# Current -----------------------------------------------------------------
crn.ppt <- read_raster(x = vrs[1])   
crn.tav <- read_raster(x = vrs[2])
crn.tmx <- read_raster(x = vrs[3])
crn.tmn <- read_raster(x = vrs[4]) 

# Future ------------------------------------------------------------------
yrs <- list.files('../tif/climate/future/rcp_4.5')

# To make the maps --------------------------------------------------------
fls <- list.files(paste0('E:/politica_ambiental/workspace/climate/raster/ensemble/rcp_4.5/', yrs[1]), full.names = TRUE)

# Precipitation
crn.ppt.tbl <- get_table_current(st = crn.ppt, vr = 'prec')
crn.ppt.tbl <- crn.ppt.tbl %>% mutate(period = 'current')
ftr.ppt.tbl <- get_table_future(vr = 'prec')
ftr.ppt.tbl <- ftr.ppt.tbl %>% mutate(period = 'future')
ppt.tbl <- rbind(crn.ppt.tbl, ftr.ppt.tbl)
ppt.tbl <- ppt.tbl %>% spread(period, value)

# Temperature mean
ftr.tav.tbl <- get_table_future(vr = 'tmean')


