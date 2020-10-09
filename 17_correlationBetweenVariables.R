
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, fasterize, landscapemetrics)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
tbl <- list.files('../tbl/metrics', full.names = TRUE, pattern = '.csv$')
nms <- basename(tbl) %>% gsub('.csv', '', .)

# Read tables -------------------------------------------------------------
tbl <- map(.x= tbl, .f = read_csv)
tbl <- map(.x = 1:length(tbl), .f = function(k) tbl[[k]] %>% mutate(metrica = nms[k]))
nrw <- map(.x = tbl, .f = nrow) %>% unlist()

# Summarizing the largest tables ------------------------------------------
tbl_sml <- tbl[which(nrw <= 21)]
tbl_sml <- map(.x = 1:length(tbl_sml), .f = function(k){
  
  tbl_sml[[k]] %>% 
    dplyr::select(metrica, class, year, name)
  
})

tbl_big <- tbl[which(nrw > 21)]
tbl_big <- map(.x = 1:length(tbl_big), .f = function(k){
  
  tbl_big[[k]] %>% 
    group_by(metrica, class, year, name) %>% 
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>% 
    ungroup() %>% 
    dplyr::select(metrica, class, year, name)
  
})

tbl_sml <- bind_rows(tbl_sml)
tbl_big <- bind_rows(tbl_big)
tbl <- rbind(tbl_sml, tbl_big)


