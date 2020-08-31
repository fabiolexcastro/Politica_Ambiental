

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
create_forest <- function(yr){
  
  # yr <- 2005
  
  # Getting the number of the year
  yr <- str_sub(yr, start = 3, end = 4)
  yr <- as.numeric(yr)
  
  # Filtering in the table
  tb <- tbl %>% mutate(loss_2 = ifelse(loss > yr, 0, 1))
  tb <- tb %>% mutate(treecover_2 = ifelse(loss == 1, NA, treecover))
  
  # Table to raster
  rs <- rasterFromXYZ(tb[,c(1, 2, 6)])
  writeRaster(rs, filename = paste0('../tif/forest/hansen/forest_', yr, '.tif'), overwrite = TRUE)
  print('Done!')
  return(rs)
  
}


# Load data ---------------------------------------------------------------

lim <- shapefile('../shp/base/veredas_mas_monterrey.shp')
thr <- readRDS('../rds/threshold_forest_hansen.rds')

# Cover layers
fls <- list.files('../shp/cobertura/monterrey/all', full.names = T, pattern = '.shp$')
cov <- lapply(1:3, function(k) shapefile(fls[[k]]))

# Forest layers
frs <- raster('../tif/forest/hansen/Hansen_GFC-2019-v1.7_treecover2000_10N_080W.tif')
frs <- frs %>% raster::crop(., lim) %>% raster::mask(., lim)
lss <- raster('../tif/forest/hansen/Hansen_GFC-2019-v1.7_lossyear_10N_080W.tif')
lss <- lss %>% raster::crop(., lim) %>% raster::mask(., lim)

writeRaster(lss, '../tif/forest/hansen/loss.tif')

# Getting the mask
msk <- frs
msk[is.na(msk)] <- 0
msk <- msk * 0 + 1
msk <- raster::crop(msk, lim) %>% raster::mask(., lim)

# Getting only the forest 
frs[which(frs[] < thr)] <- NA
frs[which(frs[] >= thr)] <- 1
writeRaster(frs, '../tif/forest/hansen/forest_2000.tif')

# Stacking forest and loss 
stk <- stack(frs, lss)
names(stk) <- c('treecover', 'loss')
tbl <- rasterToPoints(stk) %>% as_tibble()


# Creating forest ---------------------------------------------------------

# 2005
frs_05 <- create_forest(yr = 2005)
# 2010
frs_10 <- create_forest(yr = 2010)

dim(rasterToPoints(frs_05))
dim(rasterToPoints(frs_10))
