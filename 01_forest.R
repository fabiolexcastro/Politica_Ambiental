
# Load data ---------------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, terra, sf, tidyverse)

rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
tcv <- raster('../tif/forest/hansen/Hansen_GFC-2019-v1.7_treecover2000_10N_080W.tif')
mps <- shapefile('../shp/base/mpios_geo_ok.shp')
frs <- raster('../tif/forest/ideam/Bosque_No_Bosque_2000/Geotiff/SBQ_SMBYC_BQNBQ_V5_2000.tif')

# Subsetting treecover
mtr <- mps[mps@data$NOM_MUNICI %in% 'MONTERREY',]
tcv <- raster::crop(x = tcv, y = mtr)
tcv <- raster::mask(x = tcv, mask = mtr)

# Subsetting forest / no forest
frs <- raster::crop(frs, mtr)
frs <- raster::mask(frs, mtr)

# Knowing the threshold --------------------------------------------------
frs <- raster::resample(frs, tcv, method = 'bilinear')
stk <- stack(frs, tcv)
names(stk) <- c('ideam', 'hansen')

# Raster to table
tbl <- rasterToPoints(stk) 
tbl <- as_tibble(tbl)


