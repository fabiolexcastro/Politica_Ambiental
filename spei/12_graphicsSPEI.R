
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse, SPEI, gtools, foreach, doSNOW, parallel, imputeTS, zoo)
library(hrbrthemes)
library(ggpubr)

g <- gc(reset = TRUE)
rm(list = ls())

# Functions to use --------------------------------------------------------
make_graph <- function(nme){
  
  # nme <- 'spei_2'
  
  print(nme)
  tb <- tbl %>% dplyr::select(gid, x, y, year, month, nme)
  tb <- tb %>% setNames(c('gid', 'x', 'y', 'year', 'month', 'value'))
  tb <- tb %>% group_by(year, month) %>% dplyr:::summarise(value = mean(value)) %>% ungroup()
  tb <- tb %>% mutate(month = ifelse(month < 10, paste0('0', month), month))
  tb <- tb %>% mutate(fecha = paste0(year, '/', month, '/01'))
  tb <- tb %>% mutate(fecha = as.Date(fecha))
  tb <- tb %>% mutate(pos = value >= 0)
  tb <- tb %>% filter(month %in% c('06', '07', '08'))
  
  # tb$value_sum <- cumsum(coalesce(tb$value, 0)) + tb$value*0
  
  print('To make the graph')
  gg <- ggplot(data = tb, aes(x = fecha, y = value, fill = pos)) +
    geom_col() +
    # geom_line(aes(x = fecha, y = value_sum), col = 'green') +
    scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
    scale_fill_manual(values = c('red', 'blue')) +
    labs(x = 'Fecha', y = 'SPEI') +
    theme_ipsum() +
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 90, face = 'bold', size = 12),
          axis.text.y = element_text(face = 'bold', size = 12)) +
    geom_hline(aes(yintercept = -1), col = 'black', size = 1.3)
  
  ggsave(plot = gg, filename = paste0('../png/spei/', nme, '.png'), units = 'in', width = 14, height = 8, dpi = 300)
  
  print('Done!')
  return(gg)
  
}


# Load data ---------------------------------------------------------------
tbl <- readRDS(file = '../rds/spei_all_v2.rds')

# To make the graphs ------------------------------------------------------
names(tbl)
nms <- paste0('spei_', c(1, 2, 3, 4))

ggs <- map(.x = nms, .f = make_graph)

gg_all <- ggarrange()

