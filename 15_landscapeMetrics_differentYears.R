
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, fasterize, landscapemetrics)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

# Functions to use --------------------------------------------------------
create_graph <- function(tbl, nme, axs_y){
  gg <- ggplot(data = tbl, aes(x = as.character(class), y = value, fill = year, group = year)) +
    geom_col(position = 'dodge') +
    scale_fill_manual(values = c('#01DF3A', '#0B610B', '#5F4C0B')) +
    labs(x = '',
         y = axs_y, 
         fill = '') +
    theme(legend.position = 'top', 
          axis.text.x = element_text(angle = 0, vjust = 0.5, size = 10),
          axis.text.y = element_text(size = 10)) 
  ggsave(plot = gg, filename = paste0('../png/graphs/landscapemetrics/', nme, '.png'), units = 'in', width = 12, height = 9, dpi = 300)  
  return(gg)
}
create_boxpl <- function(tbl, nme, axs_y){
  gg <- ggplot(data = tbl, aes(x = as.character(class), group = class, y = value, fill = class, group = year)) +
    geom_boxplot(position = 'dodge') +
    facet_wrap(.~ year, nrow = 3, ncol = 1) +
    labs(x = '',
         y = axs_y, 
         fill = '') +
    theme(legend.position = 'top', 
          axis.text.x = element_text(angle = 0, vjust = 0.5, size = 10),
          axis.text.y = element_text(size = 10)) 
  ggsave(plot = gg, filename = paste0('../png/graphs/landscapemetrics/', nme, '.png'), units = 'in', width = 12, height = 9, dpi = 300)  
  return(gg)
}
pland_function <- function(rst, year){
  
  # rst <- stk[[1]]
  # year <- 2000
  
  rsl <- rst %>% 
    lsm_c_pland() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
  
}
cpland_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_cpland() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
np_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_np() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
te_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_te() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
mean_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_area_mn() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
cv_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_area_cv() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
area_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_p_area() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
path_radios_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_p_gyrate() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
shape_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_p_shape() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
frctal_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_p_frac() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
para_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_p_para() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
contig_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_p_contig() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
perim_frg_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_pafrac() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
euc_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_p_enn() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}



# Load data ---------------------------------------------------------------
fls <- list.files('../tif/cover', full.names = T, pattern = '.tif$')
fls <- fls[1:3]
stk <- stack(fls)
lbl <- read_csv('../tbl/cobertura/label_coberturas_rcl.csv')
lbl <- lbl %>% mutate(name = str_sub(name, start = 8, end = nchar(name)))

# Projecting raster
stk <- projectRaster(stk, crs = prj, method = 'ngb')
names(stk) <- c('cov_00', 'cov_05', 'cov_10')

# Calculating the metrics -------------------------------------------------

# Porcentaje de ocupacion de cada una de las categorias 
pland <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = pland_function) %>% bind_rows()
gg_pland <- create_graph(tbl = pland, nme = 'pland')

# Core percentage land average --------------------------------------------
cpland <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = cpland_function) %>% bind_rows()
gg_cpland <- create_graph(tbl = cpland, nme = 'cpland')

# Number of parches NP ----------------------------------------------------
np <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = np_function)
np <- bind_rows(np)
gg_np <- create_graph(tbl = np, nme = 'np', axs_y = 'Número de parches')

# Total class edge area ---------------------------------------------------
te <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = te_function)
te <- bind_rows(te)
gg_te <- create_graph(tbl = te, nme = 'te', axs_y = 'm^2')

# Mean of patch area -------------------------------------------------------
avg_patch <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = mean_function)
avg_patch <- bind_rows(avg_patch)
gg_te <- create_graph(tbl = avg_patch, nme = 'avg_patch', axs_y = 'm^2')

# CV  ---------------------------------------------------------------------
cv_patch <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = cv_function)
cv_patch <- bind_rows(cv_patch)
gg_cv <- create_graph(tbl = cv_patch, nme = 'cv_patch', axs_y = 'ha')

# Area patch --------------------------------------------------------------
area_patch <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = area_function)
area_patch <- bind_rows(area_patch)
gg_area <- create_graph(tbl = area_patch, nme = 'area_patch', axs_y = 'ha')

# Coefficient of variation radius of gyration (Area and edge metri --------
radius <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = path_radios_function)
radius <- bind_rows(radius)
gg_rdus <- create_graph(tbl = radius, nme = 'gyrate', axs_y = 'CV')

# Shape index -------------------------------------------------------------
shpe <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = shape_function)
shpe <- bind_rows(shpe)

# Fractal dimension index -------------------------------------------------
frctl <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = frctal_function)
frctl <- bind_rows(frctl)
gg_frctl <- create_graph(tbl = frctl, nme = 'fractal', axs_y = 'none')

gg <- ggplot(data = frctl, aes(x = as.character(class), y = value, fill = as.factor(class))) +
  geom_boxplot() +
  facet_wrap(.~ year, nrow = 3)
  scale_fill_manual(values = c('#01DF3A', '#0B610B', '#5F4C0B')) +
  labs(x = '',
       y = axs_y, 
       fill = '') +
  theme(legend.position = 'top', 
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10)) 
ggsave(plot = gg, filename = paste0('../png/graphs/landscapemetrics/', 'fractal', '.png'), units = 'in', width = 12, height = 9, dpi = 300)  

# Perimeter-Area ratio ----------------------------------------------------
paraf <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = para_function)
paraf <- bind_rows(paraf)
gg_paraf <- create_graph(tbl = paraf, nme = 'Perimeter - Area ratio', axs_y = 'none')

# Contiguity index --------------------------------------------------------
cntgi <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = contig_function)
cntgi <- bind_rows(cntgi)
gg_cntgi <- create_boxpl(tbl = cntgi, nme = 'Contiguity index', axs_y = 'none')

# Perimeter area fragtal dimenction ---------------------------------------
perim_frg <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = perim_frg_function)
perim_frg <- bind_rows(perim_frg)
gg_perim_frg <- create_graph(tbl = perim_frg, nme = 'Perimeter-Area Fractal dimension', axs_y = 'none')

# Euclidean nearest neighbor distance -------------------------------------
euc_enn <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = euc_function)
euc_enn <- bind_rows(euc_enn)
euc_enn <- euc_enn %>% mutate(value = value / 1000)
gg_euc_enn <- create_graph(tbl = euc_enn, nme = 'Euclidean nearest patch', axs_y = 'Km')

euc_enn %>% 
  group_by(year, class) %>% 
  dplyr::summarise(min = min(value), max = max(value), avg = mean(value)) %>%
  ungroup() %>% 
  filter(class != 1) %>%
  ggplot(data = .) +
  geom_point(aes(x = class, y = min), col = 'black') +
  geom_line(aes(x = class, y = avg), col = 'green') +
  geom_point(aes(x = class, y = max), col = 'red') +
  facet_wrap(.~year, nrow = 3)

# 4 index y terminamos



# Show patches ------------------------------------------------------------
plot(stk)
vrd <- shapefile('../shp/base/vereda_test.shp')
vrd <- spTransform(vrd, CRSobj = crs(stk))
lsc <- raster::crop(stk[[1]], vrd) %>% raster::mask(., vrd)
bnd <- get_boundaries(lsc)[[1]]

writeRaster(lsc, './landscape.tif')
writeRaster(bnd, './boundaries.tif')


plot(lsc)
plot(get_boundaries(lsc)[[1]], col = 'red', add = TRUE)



pth_00 <- show_patches(stk[[1]], class = 'global', labels = FALSE)
crs_00 <- show_cores(stk[[1]])
lsm_sp <- spatialize_lsm(landscape = stk[[1]])
plot()
show_patches(stk[[1]])


spatialize_lsm(landscape = stk[[1]], what = 'lsm_c_te')

get_patches(stk[[1]])[[1]] %>% plot()