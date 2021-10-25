
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, fs, gridExtra, ggpubr, rgeos, stringr, sf, tidyverse, gridExtra, fasterize, landscapemetrics, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
source('02 functions landscape.R')

# Load data ---------------------------------------------------------------
fls <- dir_ls('../tif/cover')
stk <- stack(fls)
lbl <- read_csv('../tbl/shp_tbl/cov_allv_v2.csv')
lbl <- lbl %>% distinct(rcl) %>% mutate(gid = 1:12) 
lbl 

# Projecting rasters ------------------------------------------------------
stk <- projectRaster(stk, crs = prj, method = 'ngb')
names(stk) <- c('cov_00', 'cov_05', 'cov_10', 'cov_18')
yrs <- c('2000', '2005', '2010', '2018')

# Calculating the metrics -------------------------------------------------

# Porcentaje de ocupacion de cada una de las categorias -------------------
pland <- map2(.x = unstack(stk), .y = yrs, .f = pland_function) %>% bind_rows() %>% mutate(class = factor(class, levels = 1:12))
gg_pland <- create_graph(tbl = pland, nme = 'pland', axs_y = 'Porcentaje')
write.csv(pland, '../tbl/metrics/pland.csv', row.names = F)

# Core percentage land average --------------------------------------------
cpland <- map2(.x = unstack(stk), .y = yrs, .f = cpland_function) %>% bind_rows() %>% mutate(class = factor(class, levels = 1:12))
gg_cpland <- create_graph(tbl = cpland, nme = 'cpland', axs_y = 'Porcentaje')
write.csv(cpland, '../tbl/metrics/cpland.csv', row.names = F)

# Total class edge area ---------------------------------------------------
te <- map2(.x = unstack(stk), .y = yrs, .f = te_function) %>% bind_rows() %>% mutate(class = factor(class, levels = 1:12))
gg_te <- create_graph(tbl = te, nme = 'te', axs_y = 'm^2')
write.csv(te, '../tbl/metrics/te.csv', row.names = F)

# Mean of path area -------------------------------------------------------
avg_patch <- map2(.x = unstack(stk), .y = yrs, .f = mean_function)
avg_patch <- bind_rows(avg_patch)
# gg_pthch <- create_graph(tbl = avg_patch, nme = 'avg_patch', axs_y = 'm^2')
write.csv(avg_patch, '../tbl/metrics/avg_patch.csv', row.names = F)

# CV ----------------------------------------------------------------------
cv_patch <- map2(.x = unstack(stk), .y = yrs, .f = cv_function) %>% bind_rows() %>% mutate(class = factor(class, levels = 1:12))
# gg_pthch <- create_graph(tbl = cv_patch, nme = 'cv_patch', axs_y = 'CV')
write.csv(cv_patch, '../tbl/metrics/cv_patch.csv', row.names = FALSE)

# Area patch --------------------------------------------------------------
area_patch <- map2(.x = unstack(stk), .y = yrs, .f = area_function) %>% bind_rows() %>% mutate(class = factor(class, levels = 1:12))
area_patch %>% pull(value) %>% range()
create_boxpl(tbl = area_patch, nme = 'area_patch', axs_y = 'ha', lowest = 0, uppest = 360, outliers = NA)
write.csv(area_patch, '../tbl/metrics/area_patch.csv', row.names = FALSE)

# Coefficient of variation radius of gyration (Area and edge metric--------
radius <- map2(.x = unstack(stk), .y = yrs, .f = path_radios_function) %>% bind_rows() %>% mutate(class = factor(class, levels = 1:12))
radius %>% pull(value) %>% range()
create_boxpl(tbl = radius, nme = 'gyrate', axs_y = 'meters', outliers = NA, lowest = 0, uppest = 2000)
write.csv(radius, '../tbl/metrics/radius.csv', row.names = FALSE)

# Shape index -------------------------------------------------------------
shpe <- map2(.x = unstack(stk), .y = yrs, .f = shape_function) %>% bind_rows %>% mutate(class = factor(class, levels = 1:12))
create_boxpl(tbl = shpe, nme = 'shape_index', axs_y = '', outliers = NA, lowest = 0, uppest = 8)
write.csv(shpe, '../tbl/metrics/shpe.csv', row.names = FALSE)
  
# Fractal dimension index -------------------------------------------------
frctl <- map2(.x = unstack(stk), .y = yrs, .f = frctal_function) %>% bind_rows() %>% mutate(class = factor(class, levels = 1:12))
# create_boxpl(tbl = frctl, nme = 'fractal', axs_y = '', outliers = NA, lowest = 1, uppest = 1.28)
write.csv(frctl, '../tbl/metrics/frctl.csv', row.names = FALSE)

# Perimeter Area - Ratio --------------------------------------------------
paraf <- map2(.x = unstack(stk), .y = yrs, .f = para_function)
paraf <- bind_rows(paraf)
paraf <- mutate(paraf, class = factor(class, levels = 1:12))
range(paraf$value)
# create_boxpl(tbl = paraf, nme = 'Perimeter - Area ratio', axs_y = '', outliers = NA, lowest = 0, uppest = 0.15)
write.csv(paraf, '../tbl/metrics/paraf.csv', row.names = FALSE)

# Contiguity index --------------------------------------------------------
cntgi <- map2(.x = unstack(stk), .y = yrs, .f = contig_function)
cntgi <- bind_rows(cntgi)
cntgi <- mutate(cntgi, class = factor(class, levels = 1:12))
create_boxpl(tbl = cntgi, nme = 'Contiguity index', axs_y =  '', outliers = NA, lowest = 0, uppest = 1)

write.csv(cntgi, '../tbl/metrics/cntgi.csv', row.names = FALSE)

# Perimeter area fragtal dimenction ---------------------------------------
perim_frg <- map2(.x = unstack(stk), .y = yrs, .f = perim_frg_function)
perim_frg <- bind_rows(perim_frg)
perim_frg <- mutate(perim_frg, class = factor(class, levels = 1:12))
create_graph(tbl = perim_frg, nme = 'perim_frg', axs_y = '')

write.csv(perim_frg, '../tbl/metrics/perim_frg.csv', row.names = FALSE)

# Euclidean nearest neighboor distance -------------------------------------
euc_enn <- map2(.x = unstack(stk), .y = yrs, .f = euc_function) %>% bind_rows %>% mutate(class = factor(class, levels = 1:12))
euc_enn <- euc_enn %>% mutate(value = value / 1000)
create_boxpl(tbl = euc_enn, nme = 'Euclidean neartes patch', axs_y = 'Km', lowest = 0, uppest = 21, outliers = NA)

write.csv(euc_enn, '../tbl/metrics/euc_enn.csv', row.names = FALSE)

# Clumpiness index --------------------------------------------------------
clm <- map2(.x = unstack(stk), .y = yrs, .f = clumpy_function) %>% bind_rows() %>% mutate(class = factor(class, levels = 1:12))
clm
range(clm$value)
create_graph(tbl = clm, nme = 'Clumpy index', axs_y = 'index')
write.csv(clm, '../tbl/metrics/clm.csv', row.names = FALSE)

# Percentage of like adjacencies ------------------------------------------
padj <- map2(.x = unstack(stk), .y = yrs, .f = p_adj) %>% bind_rows %>% mutate(class = factor(class, levels = 1:12))
padj <- bind_rows(padj)
padj <- mutate(padj, year = as.numeric(year))

gpad <- ggplot(data = padj, aes(x = year, y = value, color = class)) +
  geom_line(size = 2, linemitre = 100) +
  scale_colour_manual(values = brewer.pal(n = 12, name = 'Paired'), 
                      labels = lbl$rcl) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(x = '', y = 'Porcentaje', color = '')

ggsave(plot = gpad, filename = '../png/graphs/landscapemetrics/percentaje of like adjacencies.png', 
       units = 'in', width = 12, height = 9, dpi = 300)

write.csv(padj, '../tbl/metrics/padj.csv', row.names = FALSE)

# Aggregation index -------------------------------------------------------
agg <- map2(.x = unstack(stk), .y = yrs, .f = agg_idx) %>% bind_rows %>% mutate(class = factor(class, levels = 1:12))
create_graph(tbl = agg, nme = 'Aggregation index', axs_y = 'Porcentaje')
write.csv(agg, '../tbl/metrics/agg.csv', row.names = FALSE)

# Largest patch index -----------------------------------------------------
lrg <- map2(.x = unstack(stk), .y = yrs, .f = lrg_pth) %>% bind_rows %>% mutate(class = factor(class, levels = 1:12))
create_graph(tbl = lrg, nme = 'Largest patch index', axs_y = 'Porcentaje')

write.csv(lrg, '../tbl/metrics/lrg.csv', row.names = FALSE)

# Line
lrg
lrg <- mutate(lrg, class = as.numeric(as.character(class)))
lrg <- lrg %>% filter(!class %in% 9) 
lb2 <- lbl %>% filter(!gid %in% 9)

glrg <- ggplot(data = lrg, aes(x = year, y = value, color = as.character(class), group = class)) + 
  geom_line(size = 2) + 
  scale_colour_manual(values = brewer.pal(n = 11, name = 'Paired'), labels = pull(lb2, rcl)) +
  theme_bw() + 
  theme(legend.position = 'bottom')  +
  labs(x = '', y = '', color = '')

ggsave(plot = glrg, filename = '../png/graphs/landscapemetrics/Largest patch index_v2 linea.png', 
       units = 'in', width = 10, height = 9, dpi = 300)

write.csv(lrg, '../tbl/metrics/lrg.csv', row.names = FALSE)

# Disjunct core area density ----------------------------------------------
lsm_dcad <- map2(.x = unstack(stk), .y = yrs, .f = lsm_c_dcad_function) %>% bind_rows %>% mutate(class = factor(class, levels = 1:12))
create_graph(tbl = lsm_dcad, nme = 'Disjunct core area density', axs_y = '# Parches / 100 ha')

write.csv(lsm_dcad, '../tbl/metrics/lsm_dcad.csv', row.names = FALSE)

