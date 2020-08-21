
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, landscapemetrics)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
landscapemetrics
# Load data ---------------------------------------------------------------
frs <- raster('../tif/forest//hansen/Hansen_GFC-2019-v1.7_treecover2000_10N_080W.tif')
lss <- raster('../tif/forest/hansen/Hansen_GFC-2019-v1.7_lossyear_10N_080W.tif')
lim <- shapefile('../shp/base/veredas_mas_monterrey.shp')
thr <- readRDS('../rds/threshold_forest_hansen.rds')

# Getting the forest raster (year = 2010) ---------------------------------

# Cutting the data
lss <- lss %>% raster::crop(., lim) %>% raster::mask(., lim)
frs <- frs %>% raster::crop(., lim) %>% raster::mask(., lim)

# Getting the mask
msk <- frs
msk[is.na(msk)] <- 0
msk <- msk * 0 + 1
msk <- raster::crop(msk, lim) %>% raster::mask(., lim)

# Getting only the forest 
frs[which(frs[] < thr)] <- NA
frs[which(frs[] >= thr)] <- 1

# Removing the loss pixels to forest raster
stk <- stack(frs, lss)
tbl <- rasterToPoints(stk) %>% as_tibble()
tbl <- tbl %>% setNames(c('lon', 'lat', 'treecover', 'loss'))
tbl <- tbl %>% mutate(loss_2 = ifelse(loss > 10, 0, 1))
tbl <- tbl %>% mutate(treecover_2 = ifelse(loss == 1, NA, treecover))

rsl <- rasterFromXYZ(tbl[,c(1, 2, 6)])

par(mfrow = c(1,2))
plot(frs, main = 2000)
plot(rsl, main = 2010)
par(mfrow = c(1,1))

writeRaster(rsl, '../tif/forest/hansen/forest_noforest_2010.tif')


# Getting the landscape metrics -------------------------------------------
prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
crs(rsl) <- crs(lim)
rsl <- projectRaster(rsl, crs = prj)
rsl <- rsl * 0 + 1

# Cover layer
cov <- shapefile('../shp/cobertura/cobertura_monterrey.shp')

# Rasterize cover layer




# Perimeter
perim <- lsm_p_perim(rsl)
areas <- lsp_p_area(rsl)



