

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, furrr, future, stringr, sf, tidyverse, gtools)

rm(list = ls())


# Load data ---------------------------------------------------------------

fls <- list.files('../raster/mosaic', full.names = TRUE, pattern = '.tif$')
stk <- stack(fls)

msk <- raster('../raster/mosaic/mnt_blue_max.tif')
msk <- msk * 0 + 1

# Points 
fls <- list.files('../shp/training/points', full.names = TRUE, pattern = '.shp$')
pnt <- map(fls, shapefile)
pnt <- do.call(rbind, pnt)
pnt
plot(pnt)

# Polygons
fls <- list.files('../shp/training/polygon', full.names = TRUE, pattern = '.shp$')
plg <- map(fls, shapefile)
plg
plg <- do.call(rbind, plg)

# Generate points inside polygons
lbl <- as.data.frame(plg) %>% dplyr::select(-Id) %>% pull(clase) %>% unique()
lbl <- data.frame(categoria = lbl, gid = c(1, 2, 1, 3))
lb2 <- lbl %>% slice(c(1, 2, 4))
plg <- plg %>% st_as_sf %>% inner_join(., lbl, by = c('clase' = 'categoria')) %>% dplyr::select(-Id) %>% as(., 'Spatial')

# Get sample
plg.pnt <- map(.x = 1:nrow(plg), .f = function(k){
  
  plg %>% 
    st_as_sf %>% 
    slice(k) %>% 
    as(., 'Spatial') %>% 
    raster::rasterize(.,  msk, 'gid') %>% 
    rasterToPoints() %>% 
    as.data.frame %>% 
    sample_n(tbl = ., size = 10, replace = FALSE)
  
})

plg.pnt <- map(.x = 1:length(plg.pnt), .f = function(k){plg.pnt[[k]] %>% mutate(gid = plg$gid[k])})
plg.pnt <- bind_rows(plg.pnt) 
plg.pnt <- as_tibble(plg.pnt)
plg.pnt <- inner_join(plg.pnt, lbl, by = c('gid' = 'gid'))
plg.pnt <- inner_join(plg.pnt, lb2, by = c('gid' = 'gid'))
plg.pnt <- plg.pnt %>% dplyr::select(gid, x, y, categoria = categoria.y)

# Join poligons and points ------------------------------------------------
plg
pnt

pnt <- coordinates(pnt) %>% as.data.frame() %>% as_tibble %>% mutate(clase = pnt@data$clase) %>% setNames(c('x', 'y', 'clase'))
plg.pnt <- plg.pnt %>% dplyr::select(x, y, clase = categoria)
all <- rbind(pnt, plg.pnt)

write.csv(all, '../tbl/training/points_sample.csv', row.names = FALSE)

# To extract the values ---------------------------------------------------
vls <- raster::extract(stk, all[,1:2])
vls <- cbind(all, vls)
vls <- as_tibble(vls)


library("mlr3")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3filters")
library("mlr3misc")
library("paradox")
library("mlr3tuning")



