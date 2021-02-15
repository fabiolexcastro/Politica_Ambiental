
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools, SPEI, RColorBrewer)
require(hrbrthemes)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use
reclassify_spei <- function(yr, mn){
  
  # yr <- 1983
  # mn <- 1
  
  print(yr)
  print(mn)
  
  tb <- spei %>% 
    filter(year == yr & month == mn) %>% 
    dplyr::select(x, y, spei)
  
  rs <- rasterFromXYZ(tb)
  mt <- matrix(c(-0.5, 5, 1, -1, -0.5, 2, -1.5, -1, 3, -2, -1.5, 4, -5, -2, 5), byrow = T, ncol = 3)
  rc <- reclassify(x = rs, rcl = mt)
  
  print('Done')
  return(rc)
  
}

# Load data
spei <- readRDS('../rds/climate/spei_all.rds')
year <- 1983:2016
mont <- 1:12
cmbn <- expand.grid(year, mont) %>% arrange(Var1, Var2)

# To apply the function
rclf <- lapply(1:nrow(cmbn), function(k) reclassify_spei(yr = cmbn[k,1], mn = cmbn[k,2]))

# Add the name to the matrix
cmbn <- cmbn %>% mutate(name = paste0('y_', Var1, '_', Var2))

rclf <- stack(rclf)
names(rclf) <- pull(cmbn, name)

mtrx <- rasterToPoints(rclf, spatial = FALSE)
mtrx <- as_tibble(mtrx)
mtrx <- mtrx %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -gid, -x, -y)

# Labels
mt <- matrix(c(-0.5, 5, 1, -1, -0.5, 2, -1.5, -1, 3, -2, -1.5, 4, -5, -2, 5), byrow = T, ncol = 3)
mt <- as.data.frame(mt) %>% mutate(class = c('No sequia', 'Sequia ligera', 'Sequia moderada', 'Sequia severa', 'Sequia extrema'))
mt <- mt[,c(3, 4)]

# Join the two tables
mtrx <- inner_join(mtrx, mt, by = c('value' = 'V3'))
saveRDS(object = mtrx, file = '../rds/climate/spei_reclassify.rds')

mtrx %>% filter(value == 5) %>% pull(var) %>% unique()

