

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse, SPEI, gtools, ggspatial, RColorBrewer, foreach, doSNOW, parallel, imputeTS, zoo)
library(hrbrthemes)
library(ggpubr)

g <- gc(reset = TRUE)
rm(list = ls())

# Functions to use --------------------------------------------------------
make_table <- function(x){
  
  y <- x %>% 
    dplyr::select(gid, x, y, year, month, spei_1) %>% 
    group_by(gid, x, y, month) %>% 
    dplyr::summarise(spei_1 = mean(spei_1, na.rm = TRUE)) %>% 
    ungroup()
  
}

make_map <- function(mnt){
  
  # mnt <- 1
  
  df1 <- tb1 %>% filter(month == mnt) %>% rename(prd_1 = spei_1)
  df2 <- tb2 %>% filter(month == mnt) %>% rename(prd_2 = spei_1)
  dfm <- inner_join(df1, df2)
  dfm <- dfm %>% gather(var, value, -gid, -x, -y, -month)
  dfm <- dfm %>% mutate(var = ifelse(var == 'prd_1', 'Periodo 1 (1983 - 1999)', 'Periodo 2 (2000 - 2016)'))
  
  mtx <- lbl[,c(1, 2, 4)]
  rs1 <- dfm %>% dplyr::select(x, y, value) %>% rasterFromXYZ() %>% reclassify(x = ., rcl = mtx)
  
  gg <- ggplot(data = dfm, aes(x = x, y= y, fill = value)) +
    geom_tile() +
    facet_wrap(.~var) +
    coord_equal() +
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 8, name = 'RdBu')) +
    theme_ipsum_pub() +
    labs(x = 'Longitude', y = 'Latitud', fill = month.abb[mnt]) +
    theme(legend.position = 'bottom',
          legend.key.width = unit(5, 'line'),
          legend.text = element_text(size = 12, face = 'bold'),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10)) +
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering)
  
  ggsave(plot = gg, filename = paste0('../png/maps/spei/two_periods_', mnt, '.png'), units = 'in', width = 12, height = 9)
  
}

# Load data ---------------------------------------------------------------
tbl <- readRDS(file = '../rds/spei_all_v2.rds')
yrs <- unique(tbl$year)
prd <- list(c(1983:1999), c(2000:2016))

# Separating the years ----------------------------------------------------
tb1 <- tbl %>% filter(year %in% prd[[1]])
tb2 <- tbl %>% filter(year %in% prd[[2]])

# Table to raster ---------------------------------------------------------
tb1 <- make_table(x = tb1)
tb2 <- make_table(x = tb2)

# Testing a graph ---------------------------------------------------------
lbl <- data.frame(
  min = c(-0.5,  -1.0, -1.5, -2.0, -Inf),
  max = c(Inf, -0.5, -1.0, -1.5, -2),
  cls = c('Sin sequía', 'Sequía ligera', 'Sequía moderada', 'Sequía severa', 'Sequía extrema'),
  cl2 = 1:5
)

# Apply the function to make all the maps ---------------------------------
map(.x = 1:12, .f = make_map)









