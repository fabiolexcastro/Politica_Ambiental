

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, hrbrthemes)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
shp <- st_read('../shp/erosion/erosion_monterrey_dissolve.shp')
tbl <- as.data.frame(shp)
tbl <- tbl[,c(2, 5)]
tbl <- tbl %>% mutate(class = c('Afloramientos rocosos', 'Cuerpos de agua', 'Muy ligero', 'Zona urbana'))
tbl <- tbl %>% mutate(class = factor(class, levels = c('Afloramientos rocosos', 'Cuerpos de agua', 'Muy ligero', 'Zona urbana')))

# To make the graph -------------------------------------------------------
gg <- ggplot(data = tbl, aes(x = class, y = round(has, 0), fill = class)) +
  geom_col() +
  scale_fill_manual(values = c('#828282', '#73b2ff', '#a3ff73', '#a87000')) +
  theme_ipsum() +
  labs(x = '', y = 'Hectáreas (ha)', fill = '') +
  theme(legend.position = 'none')

ggsave(plot = gg, filename = '../png/graphs/erosion/erosion_monterrey.png', units = 'in', width = 9, height = 6, dpi = 300)

