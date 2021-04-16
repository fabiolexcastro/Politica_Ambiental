
# 02 Generate statitics to the composite-----------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, mapsf, tidyverse, rlang, hrbrthemes)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
cmpst <- stack('../raster/composite_2019_vrd.tif')
cmpst <- cmpst[[2:7]]