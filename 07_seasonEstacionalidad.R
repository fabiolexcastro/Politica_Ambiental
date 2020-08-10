 
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

# Tidy the table
tble <- pnts %>% 
  as.data.frame %>% 
  dplyr::select(-geometry) %>% 
  as_tibble() %>% 
  dplyr::select(LONGITUDE, LATITUDE, BRIGHTNESS, ACQ_DATE, ACQ_TIME, CONFIDENCE) %>% 
  mutate(CONFIDENCE = as.numeric(CONFIDENCE)) %>% 
  mutate(CONFIDENCE = ifelse(is.na(CONFIDENCE), mean(CONFIDENCE, na.rm = TRUE), CONFIDENCE))

# To remove the outliers --------------------------------------------------
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

write.csv(tble, '../tbl/hotspots/hotspots_removeOTL.csv', row.names = FALSE)

# Time line graph ------------------------------------------------------
tble <- tble %>% 
  mutate(month = month(ACQ_DATE),
         year = year(ACQ_DATE),
         day = day(ACQ_DATE))
smmr <- tble %>% 
  group_by(month) %>% 
  dplyr::summarise(count = n()) %>% 
  ungroup() %>% 
  inner_join(., data.frame(month = 1:12, month_abb = month.abb), by = 'month') %>% 
  dplyr::select(month_abb, count) %>% 
  mutate(month_abb = factor(month_abb, levels = month.abb))
lbl <- data.frame(month_abb = month.abb,
                  month = c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic'))
smmr <- inner_join(smmr, lbl, by = 'month_abb')
smmr <- smmr %>% dplyr::select(month, count) %>% mutate(month = factor(month, levels = c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')))

gg_count <- ggplot(data = smmr, aes(x = month, y = count)) +
  geom_col() +
  labs(x = '',
       y = 'Conteo puntos de calor',
       caption ='Adaptado de NASA - FIRMS') +
  ggtitle(label = 'Cantidad de puntos de calor - mes (2001 a 2019)\nMonterrey - Casanare') +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5))
ggsave(plot = gg_count, 
       filename = '../png/graphs/Puntos de calor por mes.png', 
       units = 'in', width = 12, height = 9, dpi = 300)

write.csv(smmr, '../tbl/hotspots/count_months_summarise.csv', row.names = FALSE)

summary(tble$BRIGHTNESS)
sd(tble$BRIGHTNESS)
