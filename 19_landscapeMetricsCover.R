
# Load libraries -------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, readxl)

g <- gc(reset = TRUE)
rm(list = ls())

# Load libraries ----------------------------------------------------------
fls <- list.files('../shp/cobertura/monterrey/all', full.names = TRUE, pattern = '.shp$')
shp <- map(.x = fls, .f = st_read)
yrs <- c(2000, 2005, 2010)

# Make the summary of each cover for every year ---------------------------

# Changing the colnames
shp[[1]] <- shp[[1]] %>% rename(LEYENDA = LEYENDA)
shp[[2]] <- shp[[2]] %>% rename(LEYENDA = LEYENDA)
shp[[3]] <- shp[[3]] %>% rename(LEYENDA = LEYENDA3N)

# Tidy the attribute table of each shapefile
lbl <- map(.x = shp, .f = function(x) x %>% pull(LEYENDA) %>% unique())
lbl <- map(.x = 1:3, .f = function(x) data.frame(year = yrs[x], label = lbl[x]))
lbl <- map(.x = 1:3, .f = function(x) lbl[[x]] %>% setNames(c('year', 'label')) %>% as_tibble)

# To calculate the area
shp <- map(.x = 1:3, .f = function(x) shp[[x]] %>% st_transform(., crs = st_crs(3116)))
shp <- map(.x = 1:3, .f = function(x) shp[[x]] %>% mutate(mts = as.numeric(st_area(shp[[x]])), has = mts / 10000))  
shp <- map(.x = 1:3, .f = function(x) shp[[x]] %>% dplyr::select(LEYENDA, mts, has))

# To make the summary (finally)
smm <- map(.x = 1:3, .f = function(x) shp[[x]] %>% as.data.frame %>% group_by(LEYENDA) %>% dplyr::summarise(has_sum = sum(has, na.rm = TRUE)) %>% ungroup())
smm <- map(.x = 1:3, .f = function(x) smm[[x]] %>% mutate(year = yrs[x]))
smm <- bind_rows(smm)
smm <- smm %>% spread(year, has_sum)
smm <- smm %>% setNames(c('leyenda', 'year_2000', 'year_2005', 'year_2010'))

out <- '../tbl/covers_years'
dir.create(out)
write.csv(smm, paste0(out, '/cover_years_summary_area.csv'), row.names = FALSE)

# Read the cover layer
lbl <- read_excel('../tbl/covers_years/cover_years_summary_area_rcl.xlsx')
lbl_uni <- lbl %>% distinct(reclasify) %>% mutate(gid = 1:nrow(.))
lbl <- inner_join(lbl, lbl_uni, by = 'reclasify')

shp_rcl <- map(.x = 1:3, .f = function(x){
  
  shp[[x]] %>% 
    inner_join(., lbl, by = c('LEYENDA' = 'leyenda')) %>% 
    as(., 'Spatial') %>% 
    aggregate(., 'gid')
  
})

