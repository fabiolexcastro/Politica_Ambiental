
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, sf, tidyverse, ccafs, gtools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Function to download ----------------------------------------------------
download_climate <- function(vr, pr, sc){
  
  # Proof
  vr <- 'prec'
  pr <- '2050s'
  sc <- 'RCP 4.5'
  
  print('Filtering the variable and the period')
  vr_nm <- vrs %>% filter(variable == vr) %>% pull(1)
  pr_nm <- prd %>% filter(period == pr) %>% pull(1)
  sn_nm <- scn %>% filter(scenario == sc) %>% pull(1)
  
  looking <- cc_search(
    file_set = 12,
    scenario = sn_nm,
    period = pr_nm,
    resolution = 1,
    variable = vr_nm,
    extent = 'region'
  )
  
  looking <- grep('b2_asc.zip', looking, value = TRUE)
  gcms <- str_sub(looking, start = 93, end = nchar(looking) - 10)
  gcms <- str_split(gcms, pattern = '/')
  gcms <- sapply(1:length(gcms), function(k) gcms[[k]][1])
  
  download <- lapply(1:length(looking), function(k){
    
    print(paste0('Start with ', looking[k]))
    rstr <- cc_data_fetch(key = looking[k])
    rstr <- cc_data_read(rstr)
    rstr <- raster::crop(rstr, shp) %>% raster::mask(., shp)
    
    dir_output <- paste0('../tif/climate/future/rcp_', parse_number(sc), '/', pr, '/', gcms[k])
    ifelse(!dir.exists(dir_output), dir.create(dir_output, recursive = TRUE), 'Directory exist')
    Map('writeRaster', x = unstack(rstr), filename = paste0(dir_output, '/', names(rstr), '.tif'), overwrite = TRUE)
    
    rm(rstr, dir_output)
    
    cache <- list.files(rappdirs::user_cache_dir('ccafs'), full.names = TRUE)
    map(.x = 1:length(cache), .f = function(k) file.remove(cache[k]))
    cc_cache_delete_all(force = TRUE)
    print(paste0('Done ', looking[k]))
    
  })
  
}


# Load data ---------------------------------------------------------------
shp <- shapefile('../shp/base/veredas_mas_monterrey.shp')

# Labels
prd <- data.frame(
  value = c(4:7),
  period = c('2030s', '2040s', '2050s', '2060s')
)

vrs <- data.frame(
  value = 2:5,
  variable = c('prec', 'tmax', 'tmean', 'tmin')
)

scn <- data.frame(
  value = c(8, 10), 
  scenario = c('RCP 4.5', 'RCP 8.5')
)

# To apply the function ---------------------------------------------------

# Precipitation
lapply(1:2, function(i){
  
  lapply(1:4, function(j){
    
    print(' --------------------------------------- ')
    print(' --------------------------------------- ')
    print(' --------------- To start -------------- ')
    print(pull(prd, 2)[j])
    print(pull(scn, 2)[k])
    print(' --------------------------------------- ')
    print(' --------------------------------------- ')
    
    download_climate(vr = 'prec', pr = pull(prd, 2)[j], sc = pull(scn, 2)[i])
    
    print(' ------------- Done -------------- ')
    
  })
  
})

# Temperature mean
lapply(1:2, function(k){
  
  lapply(1:4, function(j){
    
    print(' --------------------------------------- ')
    print(' --------------------------------------- ')
    print(' --------------- To start -------------- ')
    print(pull(prd, 2)[j])
    print(pull(scn, 2)[k])
    print(' --------------------------------------- ')
    print(' --------------------------------------- ')
    
    download_climate(vr = 'tmean', pr = pull(prd, 2)[j], sc = pull(scn, 2)[k])
    
  })
  
})

# Temperature minimum
lapply(1:2, function(k){
  
  lapply(1:4, function(j){
    
    print(' --------------------------------------- ')
    print(' --------------------------------------- ')
    print(' --------------- To start -------------- ')
    print(pull(prd, 2)[j])
    print(pull(scn, 2)[k])
    print(' --------------------------------------- ')
    print(' --------------------------------------- ')
    
    download_climate(vr = 'tmin', pr = pull(prd, 2)[j], sc = pull(scn, 2)[k])
    
  })
  
})

# Temperature maximum
lapply(1:2, function(k){
  
  lapply(1:4, function(j){
    
    print(' --------------------------------------- ')
    print(' --------------------------------------- ')
    print(' --------------- To start -------------- ')
    print(pull(prd, 2)[j])
    print(pull(scn, 2)[k])
    print(' --------------------------------------- ')
    print(' --------------------------------------- ')
    
    download_climate(vr = 'tmax', pr = pull(prd, 2)[j], sc = pull(scn, 2)[k])
    
  })
  
})

