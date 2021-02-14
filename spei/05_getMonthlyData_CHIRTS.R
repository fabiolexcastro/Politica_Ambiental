

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, foreach, parallel, doSNOW)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
fls <- list.files('../tif/casanare_v2/chirts', full.names = TRUE, pattern = '.tif$')
yrs <- 1983:2016

# Function to use ---------------------------------------------------------
aggregated_data_month <- function(yr){
  
  # yr <- 1983
  
  fl <- grep(yr, fls, value = TRUE)
  fl <- grep('Tmin', fl, value = TRUE)
  df <- data.frame(year = yr, file = basename(fl), path = fl)
  df <- as_tibble(df)  
  df <- df %>% mutate(month = as.numeric(str_sub(file, 11, 12)))
  
  rs <- map(1:12, function(k){
    
    print(k)
    st <- df %>% 
      filter(month == k) %>% 
      pull(path) %>% 
      stack()
    rs <- mean(st)
    print('Done')
    return(rs)
    
  })
  
  Map('writeRaster', x = rs, filename = paste0('../tif/casanare_v2/chirts/monthly/chirts_tmin_', yr, '_', 1:12, '.tif'), overwrite = TRUE)
  print('Done!')
  
}

# Apply the function ------------------------------------------------------
cl <- makeCluster(17)
makeCluster(cl)

foreach(i = 1:length(yrs), .verbose = TRUE) %dopar% {
  
  require(pacman)
  pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)
  
  aggregated_data_month(yr = yrs[i])
  
}

stopCluster(cl)



