
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, landscapemetrics)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)


# Load data ---------------------------------------------------------------
fls <- list.files('../tif/cover', full.names = T, pattern = '.tif$')
fls <- fls[1:3]
stk <- stack(fls)
lbl <- read_csv('../tbl/cobertura/label_coberturas_rcl.csv')

stk


stk[which(stk[] == 1)] <- NA

Map('writeRaster', x = unstack(stk), filename = paste0('../tif/cover/cov_frs_', c('00', '05', '10'), '_rcl_2.tif'))

coount <- rasterToPoints(stk) %>% 
  as_tibble() %>% 
  setNames(c('x', 'y', 'y00', 'y05', 'y10'))

coount %>% 
  group_by(var) %>% 
  summarise(count = n()) %>% 
  ungroup()
