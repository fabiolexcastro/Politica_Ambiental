
b
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, fasterize, landscapemetrics)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

# Load data ---------------------------------------------------------------
fls <- list.files('../tif/cover', full.names = T, pattern = '.tif$')
fls <- fls[1:3]
stk <- stack(fls)
lbl <- read_csv('../tbl/cobertura/label_coberturas_rcl.csv')
lbl <- lbl %>% mutate(name = str_sub(name, start = 8, end = nchar(name)))

# Projecting raster
stk <- projectRaster(stk, crs = prj, method = 'ngb')
names(stk) <- c('cov_00', 'cov_05', 'cov_10')

# Calculating the metrics -------------------------------------------------

# Porcentaje de ocupacion de cada una de las categorias 

pland_function <- function(rst, year){

  rsl <- rst %>% 
    rasterToPoints %>% 
    as_tibble() %>% 
    setNames(c('x', 'y', 'cov')) %>% 
    group_by(cov) %>% 
    dplyr::summarise(count = n()) %>% 
    ungroup() %>% 
    mutate(porc = count / sum(count) * 100) %>% 
    inner_join(., lbl, by = c('cov' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
  
}

cpland <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = pland_function)
cpland <- bind_rows(cpland)

ggplot(data = cpland, aes(x = name, y = porc, fill = year, group = year)) +
  geom_col(position = 'dodge') 

