 
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, lubridate,
               RColorBrewer, ggspatial, outliers)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999,
        stringsAsFactors = FALSE)

# Load data ---------------------------------------------------------------

# Base
mpos <- st_read('../shp/base/mpios_geo_ok.shp')
mntr <- mpos %>% filter(NOM_MUNICI ==  'MONTERREY')
vrds <- st_read('../shp/base/veredas_ok.shp')
vrds <- vrds %>% dplyr::select(DPTOMPIO, CODIGO_VER, NOMBRE_VER, AREA_HA)

# Fire
fire <- st_read('../shp/fire/mpio/fire_modis_veredas.shp') 
pnts <- st_read('../shp/fire/mpio/fire_points.shp')%>% 
  mutate(CONFIDENCE = as.numeric(CONFIDENCE))

# Confiability graph ------------------------------------------------------

tble <- pnts %>% 
  as.data.frame %>% 
  dplyr::select(-geometry) %>% 
  as_tibble() %>% 
  dplyr::select(LONGITUDE, LATITUDE, ACQ_DATE, ACQ_TIME, CONFIDENCE) %>% 
  mutate(CONFIDENCE = as.numeric(CONFIDENCE)) %>% 
  mutate(CONFIDENCE = ifelse(is.na(CONFIDENCE), mean(tble$CONFIDENCE, na.rm = TRUE), CONFIDENCE))

# To remove Outliers
nrow(tble)
nrow(drop_na(tble))
norm <- scores(tble[,c('CONFIDENCE')]) 
norm_na <- norm
norm_na[abs(norm_na) > 3.5] <- NA
names(norm_na) <- 'normal'
normpoints <- cbind(tble, normal = norm_na) %>% 
  as_data_frame()
tble <- normpoints
tble <- tble[!is.na(tble$normal),]

gghist <- ggplot(data = tble, aes(x = CONFIDENCE)) +
  geom_histogram(bins = 20) +
  labs(x = 'Confiabilidad (%)', 
       y = 'Frecuencia',
       caption = 'Adaptado de NASA - FIRMS') +
  ggtitle(label = 'Histograma de confiabilidad para los puntos de calor') +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'))
ggsave(plot = gghist, filename = '../png/graphs/Histograma confiabilidad.png', units = 'in',
       width = 12, height = 9, dpi = 300)

# To make the season graph ------------------------------------------------

