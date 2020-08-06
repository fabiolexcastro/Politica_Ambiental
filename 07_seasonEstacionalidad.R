

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, lubridate,
               RColorBrewer, ggspatial)

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

# To make the season graph ------------------------------------------------
tble <- fire %>% 
  as.data.frame %>% 
  dplyr::select(-geometry) %>% 
  dplyr::select(LONGITUDE, LATITUDE, ACQ_DATE, ACQ_TIME, CONFIDENCE) %>% 
  as_tibble() %>% 
  mutate(CONFIDENCE = parse_number(CONFIDENCE))
  
hist(tble$CONFIDENCE)

