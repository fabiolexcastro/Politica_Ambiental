
# Load libraries   -----
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools, RColorBrewer, hrbrthemes)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data --------------------------
tbl <- readRDS(file = '../rds/spei_all_v2.rds')
yrs <- unique(tbl$year)
vrd <- st_read('../shp/base/veredas_ok.shp')

# Functions to use ------------------
make_map <- function(yr){
  
  print(yr)
  
  # yr <- 1983
  df <- tbl %>% 
    filter(year == yr) %>% 
    dplyr::select(x, y, year, month, spei_3) 
  
  print(summary(df$spei_3))
  
  df <- df %>% 
    inner_join(., data.frame(month = 1:12, month_abb = month.abb), by = 'month') %>% 
    mutate(month_abb = factor(month_abb, levels = month.abb),
           spei_3 = ifelse(spei_3 >= -1, NA, spei_3)) 
  
  if(nrow(drop_na(df)) == 0){
    
    pp <- print('No hacer ni papa')
    
  } else {
    
    gg <- ggplot() +
      geom_tile(data = df, aes(x = x, y = y, fill = spei_3)) +
      facet_wrap(.~month_abb) +
      scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 8, name = 'YlOrRd'), na.value = 'white') +
      geom_sf(data = vrd, fill = NA) +
      coord_sf(expand = FALSE) +
      ggtitle(label = yr) +
      theme(legend.position = 'bottom',
            legend.key.width = unit(5, 'line')) +
      scale_x_continuous(breaks = seq(-72.9, -72.7, by = 0.1))
    
    ggsave(plot = gg, filename = paste0('../png/maps/spei/years/', yr, '.png'), units = 'in', width = 10, height = 10, dpi = 300)
    pp <- print('Done!')
    
  }

  return(pp)
  
}

yrs <- 1983:2016
ppp <- map(.x = yrs, .f = make_map)
yrs[which(unlist(ppp) == 'No hacer ni papa')]


