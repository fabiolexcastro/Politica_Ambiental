
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse, SPEI, gtools, foreach, doSNOW, parallel, imputeTS)

g <- gc(reset = TRUE)
rm(list = ls())

# Functions to use
stack_year <- function(yr){
  
  # yr <- 1983
  print(yr)
  tn <- grep('tmin', fles, value = TRUE) %>% grep(yr, ., value = TRUE) %>% mixedsort() %>% stack()
  tx <- grep('tmax', fles, value = TRUE) %>% grep(yr, ., value = TRUE) %>% mixedsort() %>% stack()
  pr <- grep('chirps', fles, value = TRUE) %>% grep(yr, ., value = TRUE) %>% mixedsort() %>% stack()
  
  print('Stacking')
  st <- stack(tn, tx, pr)
  names(st) <- c(paste0('tmin_', 1:12), paste0('tmax_', 1:12), paste0('prec_', 1:12))
  
  print('Table to points')
  tb <- rasterToPoints(st)
  tb <- as_tibble(tb)
  tb <- tb %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -gid, -x, -y)
  tb <- tb %>% mutate(month = parse_number(var))
  tb$var <- unlist(str_extract_all(tb$var, "[a-z]+"))
  tb <- tb %>% spread(var, value)
  tb <- tb %>% mutate(year = yr)
  tb <- tb %>% dplyr::select(gid, x, y, year, month, prec, tmax, tmin)
  
  cl <- makeCluster(12)
  registerDoSNOW(cl)
  
  e2 <- foreach(i = 1:length(unique(tb$gid)), .verbose = TRUE) %dopar% {
    
    library(pacman)
    pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse, SPEI, gtools, foreach, doSNOW, parallel)
    
    rs <- tb %>% filter(gid == i) 
    et <- hargreaves(Tmin = pull(rs, tmin), Tmax = pull(rs, tmax), Pre = pull(rs, prec), lat = unique(rs$y))
    rs <- rs %>% mutate(etp = as.numeric(et))
    print('Done')
    return(rs)
    
  }
  
  stopCluster(cl)
  
  e2 <- bind_rows(e2)
  saveRDS(e2, file = paste0('../rds/etp_', yr, '.rds'))
  print('Done!')
  
}

# Load data
root <- '../tif/monterrey_v1'
fles <- list.files(root, full.names = TRUE, pattern = '.tif$')

# Apply function
years <- 1983:2016

map(years, stack_year)

# Read the results
tbls <- list.files('../rds', full.names = TRUE, pattern = '.rds$')
tbls <- map(tbls, readRDS)
tbls <- bind_rows(tbls)
tbls <- tbls %>% mutate(baln = prec - etp)

# To calculate the SPEI
cl <- makeCluster(20)
registerDoSNOW(cl)

gids <- unique(tbls$gid)

rsltdo <- foreach(i = 1:length(gids), .verbose = TRUE) %dopar% {
  
  require(pacman)
  pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse, SPEI, gtools, foreach, doSNOW, parallel, imputeTS)
  
  tbl <- tbls %>% 
    filter(gid == i)
  bln <- tbl %>% 
    pull(baln) %>% 
    na_interpolation(x = ., option = 'linear')
  
  bln_ts <- ts(bln, start = 1983, end = 2017, frequency = 12)
  rslt <- spei(bln_ts, 1)
  rslt <- rslt$fitted %>% as.numeric
  rslt <- rslt[1:nrow(tbl)]
  
  tbl <- tbl %>% mutate(spei = rslt)
  return(tbl)
  
}

stopCluster(cl)

rsltdo2 <- bind_rows(rsltdo)
saveRDS(rsltdo2, file = '../rds/spei_all.rds')



