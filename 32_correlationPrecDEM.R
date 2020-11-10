
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools)

g <- gc(reset = TRUE)
options(scipen = 999)
rm(list = ls())

# Function to estimate the correlation table
my_correlation <- function(v1, v2){
  
  rs <- vls %>% dplyr::select(v1, v2) %>% as.matrix() %>% cor() %>% as.numeric()
  rs <- rs[2]
  rs <- data.frame(var1 = v1, var2 = v2, correlation = rs)
  return(rs)
  
}

# Load data
fls <- list.files('../tif/climate/current', full.names = TRUE, pattern = '.tif$')
fls <- grep('prec', fls, value = TRUE)
fls <- mixedsort(fls)

dem <- raster('../tif/dem/dem_monterrey_ha.tif')
prc <- stack(fls)

# Project the raster files 
dem.prj <- projectRaster(dem, crs = '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
trn.prj <- raster::terrain(dem, opt = c('slope', 'aspect', 'TPI', 'TRI', 'roughness'), unit = 'degrees')
trn.geo <- projectRaster(trn.prj, crs = crs(dem))

# Write the derivates DEM 
Map('writeRaster',  x = unstack(trn.geo), filename = paste0('../tif/dem/', names(trn.geo), '.tif'), overwrite = TRUE)

# Take a sample from the DEM 
dem.tbl <- rasterToPoints(prc, spatial = FALSE)
dem.tbl <- as_tibble(dem.tbl)
dem.sub <- sample_n(tbl = dem.tbl, size = round(nrow(dem.tbl) * 0.5, 0), repLACE = FALSE)
write.csv(dem.sub, '../tbl/dem/ppt_sample.csv', row.names = FALSE)

# Extract the values from the derivates DEM raster
vls <- raster::extract(trn.geo, dem.sub[,1:2])
vls <- as.data.frame(vls)
vls <- cbind(dem.sub, vls)
vls.dem <- raster::extract(dem, dem.sub[,1:2])
vls <- cbind(vls, vls.dem)
vls <- drop_na(vls)
vls <- as_tibble(vls)

write.csv(vls, '../tbl/dem/dem_prec.csv', row.names = FALSE)

# Which are the possible combination of variables
nms <- names(vls)[3:20]
nms.ppt <- nms[1:12]
nms.dem <- nms[13:18]

nms <- expand.grid(nms.dem, nms.ppt)
rsl <- list()

for(i in 1:nrow(nms)){
  
  print(i)
  rsl[[i]] <- my_correlation(v1 = nms[i,1], v2 = nms[i, 2])
  
}

rsl <- bind_rows(rsl)
rsl <- as_tibble(rsl)
rsl <- map(.x = 1:12, .f = function(k){
  rsl %>% filter(var2 == paste0('prec_', k)) %>% mutate(correlation = abs(correlation)) %>% top_n(x = ., n = 1, wt = correlation)
})
rsl <- bind_rows(rsl)
write.csv(rsl, '../tbl/dem/dem_correlation.csv', row.names = FALSE)
