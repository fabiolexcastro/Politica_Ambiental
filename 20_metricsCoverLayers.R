
# Load libraries -------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, readxl)

g <- gc(reset = TRUE)
rm(list = ls())

prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

# Load data ---------------------------------------------------------------
fls <- list.files('../tif/cover_mpio', full.names = TRUE, pattern = '.tif$')
yrs <- c(2000, 2005, 2010)
stk <- stack(fls)
lbl <- read_csv('../tbl/covers_years/cover_years_summary_area_rcl_v2.xlsx')
lbl[is.na(lbl)] <- 0

# Load pca table
pca <- read_csv('../tbl/pca/contrib_metricas_all.csv')
pca <- pca %>% arrange(sort(dim_1))

mtc <- c('c_ai', 'p_area', 'c_np', 'c_area_cv', 'c_lpi', 'c_pland')