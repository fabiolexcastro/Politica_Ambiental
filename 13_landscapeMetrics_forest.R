
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, landscapemetrics)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

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

writeRaster(rsl, '../tif/forest/hansen/forest_noforest_2010.tif', overwrite = TRUE)

# Getting the landscape metrics -------------------------------------------
prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
crs(rsl) <- crs(lim)
rsl <- projectRaster(rsl, crs = prj)
rsl <- rsl * 0 + 1
frs <- rsl

lim_prj <- spTransform(x = lim, CRSobj = prj)

# Cover layer
cov <- shapefile('../shp/cobertura/cobertura_monterrey.shp')

# Rasterize cover layer
cov_tbl <- data.frame(
  gid = 1:length(unique(cov$LEYENDA3N)),
  type_1 = str_sub(unique(cov$LEYENDA3N), start = 7, end = nchar(unique(cov$LEYENDA3N))),
  type_2 = unique(cov$LEYENDA3N)
                   )
cov <- inner_join(st_as_sf(cov), cov_tbl, by = c('LEYENDA3N' = 'type_2'))
cov <- as(cov, 'Spatial')
crs(cov) <- crs(lim)
cov <- spTransform(x = cov, CRSobj = prj)

compareCRS(x = cov, y = rsl)

cov_lyr <- rasterize(x = cov, y = frs, field = 'gid')
write.csv(cov_tbl, '../tbl/cobertura/leyenda_cobertura.csv', row.names = FALSE)
writeRaster(cov_lyr, '../tif/cover/coverlayer_2010.tif', overwrite = TRUE)

rsl <- cov_lyr * frs
writeRaster(rsl, '../tif/cover/coverlayer_forest_2010.tif', overwrite = TRUE)
res <- res(rsl)[1] * res(rsl)[1]

rsl_has <- data.frame(table(rsl[])) %>% 
  mutate(Var1 = as.numeric(Var1)) %>% 
  inner_join(., cov_tbl[,1:2], by = c('Var1' = 'gid')) %>% 
  setNames(c('gid', 'Frequency', 'Cobertura')) %>% 
  mutate(meters = Frequency * res,
         hectares = round(meters / 10000, 0)) %>% 
  mutate(porc = hectares / sum(hectares) * 100,
         porc = round(porc, 1)) %>% 
  dplyr::select(gid, Cobertura, hectares, porc) %>% 
  arrange(desc(porc))

write.csv(rsl_has, '../tbl/cobertura/leyenda_cobertura_filter.csv', row.names = FALSE)

rsl_has <- read_csv('../tbl/cobertura/leyenda_cobertura_filter.csv')

# Perimeter
perim <- lsm_p_perim(rsl)
areas <- lsm_p_area(rsl)

# Patches and boundaries
bld_blck <- get_patches(rsl)
bld_bnds <- get_boundaries(rsl)[[1]]

rsl_has %>% filter(gid %in% c(4, 12, 17)) # Plantacion forestal y tejido urbano continuo

# Calculate landscape metrics
abb <- lsm_abbreviations_names
lsm <- calculate_lsm(rsl, level = 'patch')
lsm %>% distinct(metric) %>% inner_join(., abb, by = 'metric')
unique(lsm$metric)

pd <- calculate_lsm(rsl, metric = 'pd')

# Metrica: CA (Area total) Unidades en ha ---------------------------------
ca <- lsm %>% 
  filter(metric == 'area') %>% 
  group_by(class) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(., cov_tbl[,c(1, 2)], by = c('class' = 'gid')) %>% 
  mutate(type_1 = str_sub(type_1, 2, nchar(type_1))) %>% 
  dplyr::select(class, cobertura = type_1, has = value) %>% 
  arrange(desc(has)) %>% 
  mutate(porc = has / sum(has) * 100)

# Numero de parches -------------------------------------------------------
nc <- lsm %>% 
  filter(metric == 'ncore') %>%
  group_by(class) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>%
  inner_join(., cov_tbl[,c(1, 2)], by = c('class' = 'gid')) %>% 
  mutate(type_1 = str_sub(type_1, 2, nchar(type_1))) %>% 
  dplyr::select(class, cobertura = type_1, has = value) %>% 
  arrange(desc(has)) %>% 
  mutate(porc = has / sum(has) * 100)

# Densidad de parches -----------------------------------------------------
pd <- lsm_c_pd(rsl) %>% 
  inner_join(., cov_tbl[,c(1, 2)], by = c('class' = 'gid')) %>% 
  mutate(type_1 = str_sub(type_1, 2, nchar(type_1))) %>% 
  arrange(desc(value)) 

# Indice de parche mas largo ----------------------------------------------
lpi <- lsm_c_lpi(rsl) %>% 
  inner_join(., cov_tbl[,c(1, 2)], by = c('class' = 'gid')) %>% 
  mutate(type_1 = str_sub(type_1, 2, nchar(type_1))) %>% 
  arrange(desc(value))

# Indice de agregacion ----------------------------------------------------
ai <- lsm_c_ai(rsl) %>% 
  inner_join(., cov_tbl[,c(1, 2)], by = c('class' = 'gid')) %>% 
  mutate(type_1 = str_sub(type_1, 2, nchar(type_1))) %>% 
  arrange(desc(value))



# Categories --------------------------------------------------------------
cat_01 <- bld_blck$`1`
cat_01 <- rasterToPolygons(cat_01)
shapefile(cat_01, '../shp/cover_forest/2010/category_01.shp')

# Enn landscape metrics
lsm_enn <- rsl %>% lsm_p_enn(., directions = 8)



lsm_enn %>% filter(class == 1)




