

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse, zoo)

g <- gc(reset = TRUE)
options(scipen = 999)
rm(list = ls())

# Load data ---------------------------------------------------------------
tbl <- readRDS(file = '../rds/climate/spei_all_v2.rds')
vrd <- shapefile('../shp/base/veredas_ok.shp')

# Processing the table ----------------------------------------------------
make_graph <- function(nme){
  
  nme <- 'spei'
  
  print(nme)
  tb <- tbl %>% dplyr::select(gid, x, y, year, month, nme)
  tb <- tb %>% group_by(year, month) %>% dplyr::summarise(spei = mean(spei)) %>% ungroup()
  tb <- tb %>% mutate(month = ifelse(month < 10, paste0('0', month), month))
  tb <- tb %>% mutate(fecha = paste0(year, '/', month, '/01'))
  t2 <- tb %>% mutate(fecha = as.Date(fecha))
  t2 <- t2 %>% mutate(fecha = as.yearmon(fecha))
  
  print('To make the graph')
  gg <- ggplot(data = t2, aes(x = fecha, y = spei)) +
    geom_col() +
    scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
    labs(x = 'Fecha', y = 'SPEI')
  
  
  
}




















