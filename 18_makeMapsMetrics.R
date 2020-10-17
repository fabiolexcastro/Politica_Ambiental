
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, missMDA, ggspatial, ggpubr, factoextra, FactoMineR, rgdal, rgeos, stringr, sf, tidyverse, fasterize, landscapemetrics, outliers)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

# Load libraries ----------------------------------------------------------
fls <- list.files('../tif/cover', full.names = T, pattern = '.tif$')
fls <- grep('_2.tif', fls, value = TRUE)
stk <- stack(fls)
lbl <- read_csv('../tbl/cobertura/label_coberturas_rcl.csv')
lbl <- lbl %>% mutate(name = str_sub(name, start = 8, end = nchar(name)))
lbl <- lbl %>% filter(gid != 1)

vrd <- st_read('../shp/base/veredas_ok.shp')
vrd <- vrd %>% st_transform(x = ., crs = st_crs(3116))

# Projecting raster -------------------------------------------------------
stk <- projectRaster(stk, crs = prj, method = 'ngb')
names(stk) <- c('cov_00', 'cov_05', 'cov_10')

# Class map ---------------------------------------------------------------
tbl <- rasterToPoints(stk, spatial = FALSE)
tbl <- as_tibble(tbl)
tbl <- inner_join(tbl, lbl, by = c('cov_00' = 'gid')) %>% 
  rename(cov_00_lbl = name) %>% 
  inner_join(., lbl, by = c('cov_05' = 'gid')) %>% 
  rename(cov_05_lbl = name) %>% 
  inner_join(., lbl, by = c('cov_10' = 'gid')) %>%
  rename(cov_10_lbl = name)
tbl <- tbl %>% 
  dplyr::select(-cov_00, -cov_05, -cov_10) %>% 
  gather(var, category, -x, -y)
tbl <- tbl %>% 
  mutate(category = factor(category, levels = lbl$name))

lbls <- c('cov_00_lbl' = 'Año 2000', 'cov_05_lbl' = 'Año 2005', 'cov_10_lbl' = 'Año 2010')
gg_class <- ggplot(tbl) +
  geom_tile(aes(x = x, y = y, fill = category)) +
  facet_wrap(~var, labeller = labeller(var = lbls)) +
  scale_fill_manual(values = c('#DF01D7', '#0B3B0B', '#5FB404', '#3A2F0B', '#088A85', '#F7FE2E')) +
  geom_sf(data = vrd, fill = NA) +
  theme_bw() +
  theme(legend.position = 'top',
        axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
  labs(fill = '', x = '', y = '') +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),
                         style = north_arrow_fancy_orienteering) 

ggsave(plot = gg_class, filename = '../png/maps/class_cover.png', 
       units = 'in', width = 13, height = 7.5, dpi = 300)

# Show patches ------------------------------------------------------------
show_patches(stk[[1]])
?show_patches

tst <- get_boundaries(stk[[1]])[[1]]
tst <- get_patches(stk[[1]], class = 4)[[1]]
tst <- get_nearestneighbour(tst, return_id = TRUE)


