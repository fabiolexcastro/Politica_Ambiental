
# Load libraries ---------------------------------------------------
require(pacman) 
pacman::p_load(raster, rgdal, raster, sf, fst, gtools, stringr, tidyverse, RColorBrewer, pastecs, psych)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Read the functions ------------------------------------------------------
source('35_functions.R')

# Load data --------------------------------------------------------
crn <- list.files('../tif/climate/current/worldclim_v2/stack', full.names = TRUE, pattern = '.tif$') 
crn <- mixedsort(crn)
vrs <- c('prec', 'tmean', 'tmax', 'tmin')  
lim <- shapefile('../shp/base/veredas_mas_monterrey.shp')
vrd <- shapefile('../shp/base/veredas_ok.shp')

crn <- stack(crn)

# RCP 4.5 2030s  ---------------------------------------------------------
ftr <- list.files('../workspace/climate/raster/ensemble/rcp_4.5/2030s', full.names = TRUE)
rcp <- str_split(string = ftr, pattern = '/')
rcp <- sapply(1:length(rcp), function(k) rcp[[k]][6])
yea <- str_split(string = ftr, pattern = '/')
yea <- sapply(1:length(yea), function(k) yea[[k]][7])
yea <- unique(yea)
ftr <- mixedsort(ftr)
ftr <- stack(ftr)

make_stack(cr = crn, ft = ftr)

# RCP 4.5 2050s -----------------------------------------------------------
ftr <- list.files('../workspace/climate/raster/ensemble/rcp_4.5/2050s', full.names = TRUE)
ftr <- mixedsort(ftr)
rcp <- str_split(string = ftr, pattern = '/')
rcp <- sapply(1:length(rcp), function(k) rcp[[k]][6])
rcp <- unique(rcp)
yea <- str_split(string = ftr, pattern = '/')
yea <- sapply(1:length(yea), function(k) yea[[k]][7])
yea <- unique(yea)
ftr <- stack(ftr)

make_stack(cr = crn, ft = ftr)

# RCP 4.5 2080s -----------------------------------------------------------
ftr <- list.files('../workspace/climate/raster/ensemble/rcp_4.5/2080s', full.names = TRUE)
ftr <- mixedsort(ftr)
rcp <- str_split(string = ftr, pattern = '/')
rcp <- sapply(1:length(rcp), function(k) rcp[[k]][6])
rcp <- unique(rcp)
yea <- str_split(string = ftr, pattern = '/')
yea <- sapply(1:length(yea), function(k) yea[[k]][7])
yea <- unique(yea)
ftr <- stack(ftr)

make_stack(cr = crn, ft = ftr)

# RCP 8.5 2030s  ---------------------------------------------------------
ftr <- list.files('../workspace/climate/raster/ensemble/rcp_8.5/2030s', full.names = TRUE)
rcp <- str_split(string = ftr, pattern = '/')
rcp <- sapply(1:length(rcp), function(k) rcp[[k]][6])
yea <- str_split(string = ftr, pattern = '/')
yea <- sapply(1:length(yea), function(k) yea[[k]][7])
yea <- unique(yea)
ftr <- mixedsort(ftr)
ftr <- stack(ftr)

make_stack(cr = crn, ft = ftr)

# RCP 8.5 2050s -----------------------------------------------------------
ftr <- list.files('../workspace/climate/raster/ensemble/rcp_8.5/2050s', full.names = TRUE)
ftr <- mixedsort(ftr)
rcp <- str_split(string = ftr, pattern = '/')
rcp <- sapply(1:length(rcp), function(k) rcp[[k]][6])
rcp <- unique(rcp)
yea <- str_split(string = ftr, pattern = '/')
yea <- sapply(1:length(yea), function(k) yea[[k]][7])
yea <- unique(yea)
ftr <- stack(ftr)

make_stack(cr = crn, ft = ftr)

# RCP 8.5 2080s -----------------------------------------------------------
ftr <- list.files('../workspace/climate/raster/ensemble/rcp_8.5/2080s', full.names = TRUE)
ftr <- mixedsort(ftr)
rcp <- str_split(string = ftr, pattern = '/')
rcp <- sapply(1:length(rcp), function(k) rcp[[k]][6])
rcp <- unique(rcp)
yea <- str_split(string = ftr, pattern = '/')
yea <- sapply(1:length(yea), function(k) yea[[k]][7])
yea <- unique(yea)
ftr <- stack(ftr)

make_stack(cr = crn, ft = ftr)

