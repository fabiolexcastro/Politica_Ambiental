

# Load libraries ---------------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools, SPEI)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
get_table <- function(var){
  
  # var <- 'prec'
  
  print(paste0('To start ', var))
  rst <- grep(var, crn, value = TRUE)
  rst <- stack(rst)
  tbl <- rasterToPoints(rst, spatial = FALSE)
  tbl <- as_tibble(tbl)
  tbl <- mutate(tbl, gid = 1:nrow(tbl))
  tbl <- gather(tbl, var, value, -gid, -x, -y)
  tbl <- mutate(tbl, variable = str_sub(var, 1, 4), month = parse_number(var))
  tbl <- dplyr::select(tbl, -var, -variable)
  names(tbl) <- c('x', 'y', 'gid', var, 'month')
  print('Done')
  return(tbl)
  
}

# Load data ---------------------------------------------------------------
crn <- list.files('../tif/climate/current/worldclim_v2/stack', full.names = TRUE, pattern = '.tif$') 
crn <- mixedsort(crn)

# Get the tables ----------------------------------------------------------
pre <- get_table(var = 'prec')
tmx <- get_table(var = 'tmax')
tmn <- get_table(var = 'tmin')

tbl <- list(pre, tmx, tmn) %>% purrr::reduce(.x = ., .f = inner_join)
tbl <- tbl %>% dplyr::select(gid, x, y, month, prec, tmax, tmin)
gds <- unique(tbl$gid)

hrg <- map(.x = 1:length(gds), .f = function(k){
  
  print(k)
  sub <- tbl %>% filter(gid == k)
  hrg <- hargreaves(Tmin = pull(sub, tmin), Tmax = pull(sub, tmax), lat = unique(sub$y), Pre = pull(sub, prec))
  hrg <- as.numeric(hrg)
  sub <- sub %>% mutate(etp = hrg)
  names(sub) <- c('gid', 'x', 'y', 'month', 'prec', 'tmax', 'tmin', 'etp')
  sub <- sub %>% mutate(bal = prec - etp)
  sub <- as.data.frame(sub)
  return(sub)
  
})

hrg <- bind_rows(hrg)
dir.create('../rds/etp')
saveRDS(object = hrg, file = '../rds/etp/hargreaves_balance.rds')

# Table to raster ---------------------------------------------------------
map(1:12, function(k){
  
  hrg %>% 
    filter(month == k) %>% 
    dplyr::select(x, y, bal) %>% 
    rasterFromXYZ() %>% 
    writeRaster(x = ., filename = paste0('../tif/climate/current/worldclim_v2/bal_', k, '.tif'), overwrite = TRUE)
  
})

fls <- list.files('../tif/climate/current/worldclim_v2', full.names = T, pattern = 'bal')
fls <- mixedsort(fls)
stk <- stack(fls)
vls <- rasterToPoints(stk, spatial = FALSE)
vls <- as_tibble(vls)
vls <- vls %>% 
  mutate(gid = 1:nrow(.)) %>% 
  gather(var, value, -gid, -x, -y) %>% 
  mutate(month = parse_number(var))

lbl <- data.frame(month = 1:12, mes = c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic'))
vls <- inner_join(vls, lbl, by = 'month')
vls <- vls %>% mutate(mes = factor(mes, levels = c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')))
vrd <- shapefile('../shp/base/veredas_ok.shp') 

# To make the map ---------------------------------------------------------
clr <- RColorBrewer::brewer.pal(n = 8, name = 'BrBG')

gg <- ggplot(vls) +
  geom_tile(aes_string(x = 'x', y = 'y', fill = 'value')) +
  facet_wrap(~ mes) +
  scale_fill_gradientn(colours = clr, 
                       na.value = 'white') +
  geom_polygon(data = vrd, aes(x=long, y = lat, group = group), color = 'grey', fill = 'NA') +
  coord_equal() +
  theme_bw() +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.width = unit(5, 'line')) +
  guides(shape = guide_legend(override.aes = list(size = 10))) +
  labs(x = 'Longitud', y = 'Latitud', fill = 'Balance (mm)')

ggsave(plot = gg, filename = '../png/maps/climate/spei/bal_map.png', units = 'in', width = 11, height = 10, dpi = 300)

