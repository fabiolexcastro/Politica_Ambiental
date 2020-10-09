
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, fasterize, landscapemetrics, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
source('./15_functions.R')

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
gg_pland <- create_graph(tbl = pland, nme = 'pland', axs_y = 'Porcentaje')
write.csv(pland, '../tbl/metrics/pland.csv', row.names = FALSE)

# Core percentage land average --------------------------------------------
cpland <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = cpland_function) %>% bind_rows()
gg_cpland <- create_graph(tbl = cpland, nme = 'cpland', axs_y = 'Porcentaje')
write.csv(cpland, '../tbl/metrics/cpland.csv', row.names = FALSE)

# Number of parches NP ----------------------------------------------------
np <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = np_function)
np <- bind_rows(np)
gg_np <- create_graph(tbl = np, nme = 'np', axs_y = 'Número de parches')
write.csv(np, '../tbl/metrics/number_patch.csv', row.names = FALSE)

# Total class edge area ---------------------------------------------------
te <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = te_function)
te <- bind_rows(te)
gg_te <- create_graph(tbl = te, nme = 'te', axs_y = 'm^2')
write.csv(te, '../tbl/metrics/total_class_edge.csv', row.names = FALSE)

# Mean of patch area -------------------------------------------------------
avg_patch <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = mean_function)
avg_patch <- bind_rows(avg_patch)
gg_pthch <- create_graph(tbl = avg_patch, nme = 'avg_patch', axs_y = 'm^2')
write.csv(avg_patch, '../tbl/metrics/mean_patch_area.csv', row.names = FALSE)

# CV  ---------------------------------------------------------------------
cv_patch <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = cv_function)
cv_patch <- bind_rows(cv_patch)
gg_cv <- create_graph(tbl = cv_patch, nme = 'cv_patch', axs_y = 'ha')
write.csv(cv_patch, '../tbl/metrics/cv_patch_area.csv', row.names = FALSE)

# Area patch --------------------------------------------------------------
area_patch <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = area_function)
area_patch <- bind_rows(area_patch)
area_patch <- area_patch %>% filter(class != 1)
area_patch %>% pull(value) %>% range()
gg_area <- create_boxpl(tbl = area_patch, nme = 'area_patch', axs_y = 'ha', lowest = 0, uppest = 1.5, outliers = NA)
write.csv(area_patch, '../tbl/metrics/area_patch.csv', row.names = FALSE)

# Coefficient of variation radius of gyration (Area and edge metric) --------
radius <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = path_radios_function)
radius <- bind_rows(radius)
gg_rdus <- create_boxpl(tbl = radius, nme = 'gyrate', axs_y = 'CV', outliers = NA, lowest = 0, uppest = 300)
write.csv(radius, '../tbl/metrics/gyration.csv', row.names = FALSE)

# Shape index -------------------------------------------------------------
shpe <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = shape_function)
shpe <- bind_rows(shpe)
gg_shpe <- create_boxpl(tbl = shpe, nme = 'shape_index', axs_y = '', outliers = NA, lowest = 0, uppest = 3)
write.csv(shpe, '../tbl/metrics/shape_index.csv', row.names = FALSE)

# Fractal dimension index -------------------------------------------------
frctl <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = frctal_function)
frctl <- bind_rows(frctl)
range(frctl$value)
gg_frctl <- create_boxpl(tbl = frctl, nme = 'fractal', axs_y = '', outliers = NA, lowest = 0.9, uppest = 1.5)
write.csv(frctl, '../tbl/metrics/fractal_index.csv', row.names = FALSE)

# Perimeter-Area ratio ----------------------------------------------------
paraf <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = para_function)
paraf <- bind_rows(paraf)
range(paraf)
gg_paraf <- create_boxpl(tbl = paraf, nme = 'Perimeter - Area ratio', axs_y = '', outliers = NA, lowest = 0, uppest = 0.2)
write.csv(paraf, '../tbl/metrics/paraf.csv', row.names = FALSE)

# Contiguity index --------------------------------------------------------
cntgi <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = contig_function)
cntgi <- bind_rows(cntgi)
gg_cntgi <- create_boxpl(tbl = cntgi, nme = 'Contiguity index', axs_y = '', outliers = NA, lowest = 0, uppest = 1)
write.csv(paraf, '../tbl/metrics/contiguity_function.csv', row.names = FALSE)

# Perimeter area fragtal dimenction ---------------------------------------
perim_frg <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = perim_frg_function)
perim_frg <- bind_rows(perim_frg)
gg_perim_frg <- create_graph(tbl = perim_frg, nme = 'Perimeter-Area Fractal dimension', axs_y = '')
write.csv(perim_frg, '../tbl/metrics/perimeter_area_fractal_dimension.csv', row.names = FALSE)

perim_frg <- perim_frg %>% filter(class != 1)

gg <- ggplot(data = perim_frg, aes(x = as.numeric(year), y = value, color = as.character(class), group = as.character(class))) +
  geom_line(size = 2, linemitre = 100) +
  scale_x_continuous(limits = c(2000, 2010), breaks = seq(2000, 2010, 5)) +
  scale_colour_manual(values = brewer.pal(n = 6, name = 'Paired')) +
  theme_bw() + 
  theme(legend.position = 'top', #c(0.1, 0.85)
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) +
  labs(x = '', y = '') +
  guides(colour = guide_legend(nrow = 1, ncol = 6))

ggsave(plot = gg, filename = '../png/graphs/landscapemetrics/Perimeter-Area Fractal dimension.png', units = 'in', width = 12, height = 9, dpi = 300)

# Euclidean nearest neighbor distance -------------------------------------
euc_enn <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = euc_function)
euc_enn <- bind_rows(euc_enn)
euc_enn <- euc_enn %>% mutate(value = value / 1000)
gg_euc_enn <- create_boxpl(tbl = euc_enn, nme = 'Euclidean nearest patch', axs_y = 'Km', lowest = 0, uppest = 0.5, outliers = NA)
write.csv(euc_enn, '../tbl/metrics/euclidean_nearest_neighbor.csv', row.names = FALSE)

# Clumpiness index --------------------------------------------------------
clm <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = clumpy_function)
clm <- bind_rows(clm)
gg_clm <- create_graph(tbl = clm, nme = 'Clumpy index', axs_y = 'index')
write.csv(clm, '../tbl/metrics/clumpiness_index.csv',  row.names = FALSE)

gg <- ggplot(data = clm, aes(x = as.numeric(year), y = value, color = as.character(class), group = as.character(class))) +
  geom_line(size = 2, linemitre = 100) +
  scale_x_continuous(limits = c(2000, 2010), breaks = seq(2000, 2010, 5)) +
  scale_colour_manual(values = brewer.pal(n = 7, name = 'Paired')) +
  theme_bw() + 
  theme(legend.position = 'top', #c(0.1, 0.85)
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) +
  labs(x = '', y = '') +
  guides(colour = guide_legend(nrow = 1, ncol = 7))

ggsave(plot = gg, filename = '../png/graphs/landscapemetrics/Clumpiness index.png', units = 'in', width = 12, height = 9, dpi = 300)

# Percentage of like adjacencies ------------------------------------------
padj <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = p_adj)
padj <- bind_rows(padj)
write.csv(padj, '../tbl/metrics/percentage of like adjacencies.png', row.names = FALSE)

gg <- ggplot(data = clm, aes(x = as.numeric(year), y = value, color = as.character(class), group = as.character(class))) +
  geom_line(size = 2, linemitre = 100) +
  scale_x_continuous(limits = c(2000, 2010), breaks = seq(2000, 2010, 5)) +
  scale_colour_manual(values = brewer.pal(n = 7, name = 'Paired')) +
  theme_bw() + 
  theme(legend.position = 'top', #c(0.1, 0.85)
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) +
  labs(x = '', y = '') +
  guides(colour = guide_legend(nrow = 1, ncol = 7))

ggsave(plot = gg, filename = '../png/graphs/landscapemetrics/Percentaje of like adjacencies.png', units = 'in', width = 12, height = 9, dpi = 300)

# Aggregation index -------------------------------------------------------
agg <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = agg_idx)
agg <- bind_rows(agg)
gg_agg <- create_graph(tbl = agg, nme = 'Aggregation index', axs_y = 'Index')
write.csv(agg, '../tbl/metrics/aggregation index.csv', row.names = FALSE)

gg <- ggplot(data = agg, aes(x = as.numeric(year), y = value, color = as.character(class), group = as.character(class))) +
  geom_line(size = 2, linemitre = 100) +
  scale_x_continuous(limits = c(2000, 2010), breaks = seq(2000, 2010, 5)) +
  # scale_y_continuous(limits = c(0, 100)) +
  scale_colour_manual(values = brewer.pal(n = 7, name = 'Paired')) +
  theme_bw() + 
  theme(legend.position = 'top', #c(0.1, 0.85)
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) +
  labs(x = '', y = '') +
  guides(colour = guide_legend(nrow = 1, ncol = 7))

ggsave(plot = gg, filename = '../png/graphs/landscapemetrics/Aggregation index.png', units = 'in', width = 12, height = 9, dpi = 300)

# Largest patch index -----------------------------------------------------
lrg <- map2(.x = unstack(stk), .y = c('2000', '2005', '2010'), .f = lrg_pth)
lrg <- bind_rows(lrg)
gg_lrg <- create_graph(tbl = lrg, nme = 'Largest patch index', axs_y = 'Porcentaje')
ggsave(plot = gg_lrg, filename = '../png/graphs/landscapemetrics/Largest patch index.png', units = 'in', width = 12, height = 9, dpi = 300)

write.csv(lrg, '../tbl/metrics/largest patch index.csv', row.names = FALSE)

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
