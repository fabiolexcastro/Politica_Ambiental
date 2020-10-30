
# Load libraries -------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, readxl)

g <- gc(reset = TRUE)
rm(list = ls())

prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

# Functions to use --------------------------------------------------------
source('./20_functions.R')

# Load data ---------------------------------------------------------------
fls <- list.files('../tif/cover_mpio', full.names = TRUE, pattern = '.tif$')
yrs <- c(2000, 2005, 2010)
stk <- stack(fls)
lbl <- read_csv('../tbl/covers_years/cover_years_summary_area_rcl_v2.xlsx')
lbl[is.na(lbl)] <- 0
lbl <- lbl %>% dplyr::select(gid, reclasify)
lbl <- lbl %>% distinct(gid, reclasify)
lbl <- lbl %>% mutate(reclasify = iconv(reclasify, to = 'latin1'))

# Load pca table -----------------------------------------------------------
pca <- read_csv('../tbl/pca/contrib_metricas_all.csv')
pca <- pca %>% arrange(sort(dim_1_2000))
mtc <- c('c_ai', 'p_area', 'c_np', 'c_lpi', 'c_pland', 'c_cpland', 'lsm_c_dcad_function')

# Reclassify again ---------------------------------------------------------

# Reclassify 2000
stk[[1]][which(stk[[1]][] == 9)] <- 3

# Reclassify 2005
stk[[2]]

# Reclassify 2010
stk[[3]][which(stk[[3]][] == 2)] <- 1

# Write the new raster reclassify
Map('writeRaster', x = unstack(stk), filename = paste0('../tif/cover_mpio/cover_rcl_', yrs, '_v2.tif'))

# To calculate the metrics ------------------------------------------------

# Aggregation index
agg_idx_tbl <- map2(.x = unstack(stk), .y = yrs, .f = agg_idx)
agg_idx_tbl <- agg_idx_tbl %>% 
  bind_rows() %>% 
  mutate(year = factor(year, levels = c('2000', '2005', '2010')))
write.csv(agg_idx_tbl, '../tbl/cobertura_metrics/agg_idx_tbl.csv', row.names = FALSE)

# Area function
area_tbl <- map2(.x = unstack(stk), .y = yrs, .f = area_function)
area_tbl <- area_tbl %>% 
  bind_rows() %>% 
  mutate(year = factor(year, levels = c('2000', '2005', '2010')))
write.csv(area_tbl, '../tbl/cobertura_metrics/area_tbl.csv', row.names = FALSE)

# Number of patch
np_tbl <- map2(.x = unstack(stk), .y = yrs, .f = np_function)
np_tbl <- np_tbl %>% 
  bind_rows() %>% 
  mutate(year = factor(year, levels = c('2000', '2005', '2010')))
write.csv(np_tbl, '../tbl/cobertura_metrics/np_tbl.csv', row.names = FALSE)

# Largets patch index
lrg_tbl <- map2(.x = unstack(stk), .y = yrs, .f = lrg_pth)
lrg_tbl <- lrg_tbl %>% 
  bind_rows() %>% 
  mutate(year = factor(year, levels = c('2000', '2005', '2010')))
write.csv(lrg_tbl, '../tbl/cobertura_metrics/lrg_tbl.csv', row.names = FALSE)

# Pland index
pland_tbl <- map2(.x = unstack(stk), .y = yrs, .f = pland_function)
pland_tbl <- pland_tbl %>% 
  bind_rows() %>% 
  mutate(year = factor(year, levels = c('2000', '2005', '2010')))
write.csv(pland_tbl, '../tbl/cobertura_metrics/pland_tbl.csv', row.names = FALSE)

# Cpland index
cpland_tbl <- map2(.x = unstack(stk), .y = yrs, .f = cpland_function)
cpland_tbl <- cpland_tbl %>% 
  bind_rows() %>% 
  mutate(year = factor(year, levels = c('2000', '2005', '2010')))
write.csv(cpland_tbl, '../tbl/cobertura_metrics/cpland_tbl.csv', row.names = FALSE)

# Density patch area
dnst_tbl <- map2(.x = unstack(stk), .y = yrs, .f = lsm_c_dcad_function)
dnst_tbl <- dnst_tbl %>% 
  bind_rows() %>% 
  mutate(year = factor(year, levels = c('2000', '2005', '2010')))
write.csv(dnst_tbl, '../tbl/cobertura_metrics/dnst_tbl.csv', row.names = FALSE)

