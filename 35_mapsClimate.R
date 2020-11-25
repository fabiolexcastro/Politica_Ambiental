
# Load libraries ---------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, raster, sf, fst, gtools, stringr, tidyverse, RColorBrewer, pastecs, psych)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
read_raster <- function(x){
  
  x <- grep(x, crn, value = TRUE)
  x <- mixedsort(x)
  x <- stack(x) 
  
  return(x)
  
}
get_table_current <- function(st, vr){
  
  # st <- crn.ppt
  # vr <- 'prec'
  
  tbl <- rasterToPoints(st)
  tbl <- as.data.frame(tbl)
  tbl <- as_tibble(tbl)
  names(tbl) <- c('x', 'y', paste0(vr, '_', 1:12))
  tbl <- tbl %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -x, -y, -gid)
  mnt <- str_split(string = pull(tbl, var), pattern = '_')
  mth <- sapply(1:length(mnt), function(k) mnt[[k]][2])
  tbl <- tbl %>% mutate(month = as.numeric(mth))
  tbl <- tbl %>% mutate(variable = vr)
  tbl <- tbl %>% dplyr::select(gid, x, y, variable, month, value)
  
  print('Done!')
  return(tbl)
  
}
get_table_future <- function(vr){
  
  # vr <- 'prec'
  
  rst <- grep(vr, fls, value = TRUE)
  rst <- mixedsort(rst)
  rst <- stack(rst)
  tbl <- rasterToPoints(rst)
  tbl <- as.data.frame(tbl)
  tbl <- as_tibble(tbl)  
  tbl <- tbl %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -x, -y, -gid)
  mnt <- str_split(string = pull(tbl, var), pattern = '_')
  mth <- sapply(1:length(mnt), function(k) mnt[[k]][2])
  tbl <- tbl %>% mutate(month = as.numeric(mth))
  tbl <- tbl %>% mutate(variable = vr)
  tbl <- tbl %>% dplyr::select(gid, x, y, variable, month, value)
  print('Done!')
  return(tbl)
  
}
calc_ensemble <- function(pth){
  
  pth <- paste0('../tif/climate/future/rcp_4.5/', yrs[1])
  vrs <- c('prec', 'tmean', 'tmax', 'tmin') 
  gcm <- list.files(pth, full.names = T)
  ftr <- map(.x = 1:length(gcm), .f = function(k) list.files(gcm[k], full.names = TRUE, pattern = '.tif') %>% grep('/ha_', ., value = TRUE) %>% mixedsort())
  
  vrs <- c(paste0('prec_', 1:12), paste0('tmean_', 1:12), paste0('tmin_', 1:12), paste0('tmax_', 1:12))
  vrs <- grep(paste0(c('tmax', 'tmin'), collapse = '|'), vrs, value = TRUE)
  vrs <- paste0(vrs, '.tif')
  
  rst <- map(.x = 1:length(vrs), .f = function(k){
    
    rst <- lapply(1:length(ftr), function(j){
      
      print(j)
      print(vrs[k])
      fle <- grep(vrs[k], ftr[[j]], value = TRUE)
      fle <- grep('/ha_t', fle, value = TRUE)
      print(fle)
      lyr <- raster(fle)
      return(lyr)
      
    })
    
    rst <- stack(rst)
    avg <- mean(rst)
    print(paste0('Done ', vrs[k], ftr[[j]]))
    return(avg)
    
  })
  
  rst <- stack(rst)
  names(rst) <- gsub('.tif', '', vrs)
  print('Done!')
  
  out <- paste0('E:/politica_ambiental/workspace/climate/raster/ensemble/rcp_4.5/', yrs[1])
  Map('writeRaster', x = unstack(rst), filename = paste0(out, '/', names(rst), '.tif'), overwrite = TRUE)
  
}
mapping <- function(columna){
  
  gg <- ggplot(tb) +
    geom_tile(aes_string(x = 'x', y = 'y', fill = columna)) +
    facet_wrap(~ month_abb) +
    scale_fill_gradientn(colours = clr, 
                         na.value = 'white') + #, limits = c(stt[1], stt[2])
    geom_polygon(data = vrd, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
    coord_equal() +
    theme_bw() +
    theme(legend.position = 'bottom',
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key.width = unit(5, 'line')) +
    guides(shape = guide_legend(override.aes = list(size = 10))) +
    labs(x = 'Longitud', y = 'Latitud', fill = gsub('_', ' ', columna))
  
}
make.map <- function(var){
  
  # var <- 'prec'
  
  tb <- tbl %>% dplyr::select(gid, x, y, month_abb, contains(match = var))
  tb <- tb %>% setNames(c('gid', 'x', 'y', 'month_abb', 'Linea_base', 'Futuro'))
  vr.ab <- str_sub(var, 1, 2)
  
  vls <- c(pull(tb, 5), pull(tb, 6))
  stt <- summary(vls) %>% as.numeric()
  stt <- stt[c(1,6)]
  
  summary(tb$Linea_base)
  summary(tb$Futuro)
  
  if(vr.ab == 'tm'){
    
    clr <- RColorBrewer::brewer.pal(n = 8, name = 'YlOrRd')
    
  } else {
    
    clr <- RColorBrewer::brewer.pal(n = 8, name = 'Blues')
    
  }
  
  map.crn <- mapping(columna = 'Linea_base')
  map.ftr <- mapping(columna = 'Futuro')
  ggsave(plot = map.crn, filename = paste0('../png/maps/climate/', 'rcp45_2030s_', var, '_current.png'), units = 'in', width = 12, height = 9, dpi = 300)
  ggsave(plot = gg, filename = paste0('../png/maps/climate/', 'rcp45_2030s_', var, '_future.png'), units = 'in', width = 12, height = 9, dpi = 300)
  
  print('Done!')
  
}

# Load data --------------------------------------------------------
crn <- list.files('../tif/climate/current', full.names = TRUE, pattern = '.tif$') 
crn <- grep('ha', crn, value = TRUE)  
vrs <- c('prec', 'tavg', 'tmax', 'tmin')  

# Current -----------------------------------------------------------------
crn.ppt <- read_raster(x = vrs[1])   
crn.tav <- read_raster(x = vrs[2])
crn.tmx <- read_raster(x = vrs[3])
crn.tmn <- read_raster(x = vrs[4]) 

# Future ------------------------------------------------------------------
yrs <- list.files('../tif/climate/future/rcp_4.5')

# To make the maps --------------------------------------------------------
fls <- list.files(paste0('E:/politica_ambiental/workspace/climate/raster/ensemble/rcp_4.5/', yrs[1]), full.names = TRUE)

# Precipitation
crn.ppt.tbl <- get_table_current(st = crn.ppt, vr = 'prec') %>% mutate(period = 'current')
ftr.ppt.tbl <- get_table_future(vr = 'prec') %>% mutate(period = 'future')
ppt.tbl <- rbind(crn.ppt.tbl, ftr.ppt.tbl)
ppt.tbl <- ppt.tbl %>% spread(period, value)
names(ppt.tbl) <- c('gid', 'x', 'y', 'variable', 'month', 'current_prec', 'future_prec')

# Temperature mean
crn.tav.tbl <- get_table_current(st = crn.tav, vr = 'tmean') %>% mutate(period = 'current')
ftr.tav.tbl <- get_table_future(vr = 'tmean') %>% mutate(period = 'future')
tav.tbl <- rbind(crn.tav.tbl, ftr.tav.tbl)
tav.tbl <- tav.tbl %>% spread(period, value)
names(tav.tbl) <- c('gid', 'x', 'y', 'variable', 'month', 'current_tmean', 'future_tmean')

# Temperature min
crn.tmn.tbl <- get_table_current(st = crn.tmn, vr = 'tmin') %>% mutate(period = 'current')
ftr.tmn.tbl <- get_table_future(vr = 'tmin') %>% mutate(period = 'future')
tmn.tbl <- rbind(crn.tmn.tbl, ftr.tmn.tbl)
tmn.tbl <- tmn.tbl %>% spread(period, value)
names(tmn.tbl) <- c('gid', 'x', 'y', 'variable', 'month', 'current_tmin', 'future_tmin')

# Temperature max
crn.tmx.tbl <- get_table_current(st = crn.tmx, vr = 'tmax') %>% mutate(period = 'current')
ftr.tmx.tbl <- get_table_future(vr = 'tmax') %>% mutate(period = 'future')
tmx.tbl <- rbind(crn.tmx.tbl, ftr.tmx.tbl)
tmx.tbl <- tmx.tbl %>% spread(period, value)
names(tmx.tbl) <- c('gid', 'x', 'y', 'variable', 'month', 'current_tmax', 'future_tmax')

# Join all the tables into only one ---------------------------------------
lst.tbl <- list(ppt.tbl, tav.tbl, tmn.tbl, tmx.tbl)
lst.tbl <- map(.x = lst.tbl, .f = function(x) x %>% dplyr::select(-variable))
tbl.all <- lst.tbl %>% purrr::reduce(inner_join, by = c("gid", "x", "y", "month"))

saveRDS(object = tbl.all, file = '../workspace/climate/table/tbl_rcp45_2030s.rds')
write_fst(x = tbl.all, path = '../workspace/climate/table/tbl_rcp45_2030s.fst')

# Remove objects
rm(list = ls())

# Read the table ----------------------------------------------------------
vrd <- shapefile('../shp/base/veredas_ok.shp')
tbl <- read_fst('../workspace/climate/table/tbl_rcp45_2030s.fst')
tbl <- as_tibble(tbl)
names(tbl) <- c('gid', 'x', 'y', 'month', 'current_prec', 'future_prec', 'current_tmean', 'future_tmean', 'current_tmin', 'future_tmin', 'current_tmax', 'future_tmax')

# Labels / months
mnt <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dic')
lbl <- data.frame(month = 1:12, month_abb = mnt)
tbl <- inner_join(tbl, lbl, by = 'month')
tbl <- tbl %>% mutate(month_abb = factor(month_abb, levels = mnt))
tbl <- tbl %>% mutate_if(.predicate = contains('tm'), .funs = . / 10)

tbl <- as_tibble(cbind(tbl %>% dplyr::select(-contains('tm')), tbl %>% dplyr::select(contains('tm')) %>% mutate_all(.funs = function(x){x / 10})))

# Apply the function to make the maps -------------------------------------
vrs <- c('prec', 'tmean', 'tmax', 'tmin')
map(.x = vrs, .f = make.map)
