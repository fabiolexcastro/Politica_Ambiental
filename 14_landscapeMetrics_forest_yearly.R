
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
  tb <- tb %>% mutate(treecover_2 = ifelse(loss_2 == 1, NA, treecover))
  
  # Table to raster
  rs <- rasterFromXYZ(tb[,c(1, 2, 6)])
  writeRaster(rs, filename = paste0('../tif/forest/hansen/forest_', yr, '.tif'), overwrite = TRUE)
  print('Done!')
  return(rs)
  
}
filter_cover <- function(shp){
  
  # shp <- cov_00
  
  shp <- shp %>% 
    st_as_sf %>% 
    st_transform(x = ., crs = st_crs(3116)) %>% 
    mutate(area_mt = st_area(.) %>% as.numeric,
           area_ha = area_mt / 10000)
  
  tbl <- shp %>% 
    as.data.frame %>% 
    dplyr::select(-geometry) %>% 
    as_tibble() %>% 
    group_by(NIVEL3) %>% 
    summarise(area_ha = sum(area_ha)) %>% 
    ungroup() %>% 
    arrange(desc(area_ha)) %>% 
    mutate(porcentaje = area_ha / sum(area_ha) * 100,
           porcentaje_sum = cumsum(porcentaje))
  
  print('Done!')
  return(tbl)
  
}

# Load data ---------------------------------------------------------------
lim <- shapefile('../shp/base/veredas_mas_monterrey.shp')
thr <- readRDS('../rds/threshold_forest_hansen.rds')

# Forest processing -------------------------------------------------------
# Cover layers
fls <- list.files('../shp/cobertura/monterrey/all', full.names = T, pattern = '.shp$')
cov <- lapply(1:3, function(k) shapefile(fls[[k]]))

# Forest layers
frs <- raster('../tif/forest/hansen/Hansen_GFC-2019-v1.7_treecover2000_10N_080W.tif')
frs <- frs %>% raster::crop(., lim) %>% raster::mask(., lim)
lss <- raster('../tif/forest/hansen/Hansen_GFC-2019-v1.7_lossyear_10N_080W.tif')
lss <- lss %>% raster::crop(., lim) %>% raster::mask(., lim)
writeRaster(lss, '../tif/forest/hansen/loss.tif')

lss_tbl <- table(lss[]) %>% as.data.frame() %>% mutate(Var1 = as.numeric(as.character(Var1)) + 2000)
lss_tbl %>% filter(Var1 > 2000 & Var1 <= 2005) %>% pull(Freq) %>% sum()
lss_tbl %>% filter(Var1 > 2000 & Var1 <= 2010) %>% pull(Freq) %>% sum()

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

# Write the raster files
writeRaster(frs_05, '../tif/forest/hansen/forest_05.tif', overwrite = TRUE) 
writeRaster(frs_10, '../tif/forest/hansen/forest_10.tif', overwrite = TRUE)

frs_05_shp <- rasterToPolygons(frs_05)
frs_10_shp <- rasterToPolygons(frs_10)
frs_05_shp <- aggregate(frs_05_shp, 'treecover_2')
frs_10_shp <- aggregate(frs_10_shp, 'treecover_2')
frs_00_shp <- frs
frs_00_shp <- rasterToPolygons(frs_00_shp)
colnames(frs_00_shp@data) <- 'treecover_2'
frs_00_shp <- aggregate(frs_05_shp, 'treecover_2')

# Write the forest shapefiles (2000, 2005, 2010)
shapefile(frs_00_shp, '../shp/cover_forest/forest_2000.shp')
shapefile(frs_05_shp, '../shp/cover_forest/forest_2005.shp')
shapefile(frs_10_shp, '../shp/cover_forest/forest_2010.shp')

crs(frs_00_shp) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
crs(frs_05_shp) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
crs(frs_10_shp) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

# Cover layers ------------------------------------------------------------
cov_00 <- shapefile('../shp/cobertura/monterrey/all/coberturas_2000.shp') %>% spTransform(., CRSobj = crs(frs_10_shp))
cov_05 <- shapefile('../shp/cobertura/monterrey/all/coberturas_2005.shp') %>% spTransform(., CRSobj = crs(frs_10_shp))
cov_10 <- shapefile('../shp/cobertura/monterrey/all/coberturas_2010.shp') %>% spTransform(., CRSobj = crs(frs_10_shp))

# Cutting cover vs forest
cov_00 <- raster::crop(cov_00, frs_00_shp) 
cov_05 <- raster::crop(cov_05, frs_05_shp) 
cov_10 <- raster::crop(cov_10, frs_10_shp) 

dir.create('../shp/cobertura/monterrey/forest')
shapefile(cov_00, '../shp/cobertura/monterrey/forest/coberturas_2000.shp')
shapefile(cov_05, '../shp/cobertura/monterrey/forest/coberturas_2005.shp')
shapefile(cov_10, '../shp/cobertura/monterrey/forest/coberturas_2010.shp')

# Less cover representation filter -----------------------------------------
cov_00_tbl <- filter_cover(shp = cov_00)
cov_05_tbl <- filter_cover(shp = cov_05)
cov_10_tbl <- filter_cover(shp = cov_10)

cov_00_tbl <- cov_00_tbl %>% rename(ha_00 = area_ha, p_00 = porcentaje, p_00_sum = porcentaje_sum)
cov_05_tbl <- cov_05_tbl %>% rename(ha_05 = area_ha, p_05 = porcentaje, p_05_sum = porcentaje_sum)
cov_10_tbl <- cov_10_tbl %>% rename(ha_10 = area_ha, p_10 = porcentaje, p_10_sum = porcentaje_sum)
cov_10_tbl <- cov_10_tbl %>% rename(NIVEL3 = LEYENDA3N)

cov_tbl <- full_join(cov_00_tbl, cov_05_tbl, by = 'NIVEL3') %>% 
  full_join(., cov_10_tbl, by = 'NIVEL3')

write.csv(cov_tbl, '../tbl/cobertura/cobertura_area_00_05_10.csv', row.names = FALSE)
