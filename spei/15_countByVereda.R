
# Load libraries -------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools, RColorBrewer, hrbrthemes)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use -----------------------------------
make_count <- function(yr){
  
  # yr <- 1984
  
  tb <- tbl %>%
    filter(year == yr)
  
  rs <- map(.x = 1:12, .f = function(k){
    
    print(paste0('To start ', k))
    d <- tb %>% filter(month == k)
    d <- st_as_sf(x = d, coords = c('x', 'y'))
    st_crs(d) <- st_crs(4326)
    
    print('Intersection')
    z <- st_intersection(x = d, y = vrd)
    d <- z %>% as_tibble() %>% dplyr::select(-geometry)
    d <- d %>% dplyr::select(NOMBRE_VER, gid, year, month, spei_3)
    
    c <- findInterval(d$spei_3, lbls$V1, all.inside = TRUE)
    d <- d %>% mutate(class = c)
    g <- d %>% group_by(NOMBRE_VER, class) %>% summarise(count = n()) %>% ungroup()
    g <- g %>% mutate(month = k, year = yr)
    
    print('Done!')
    return(g)
    
  })
  
  rs <- bind_rows(rs)
  return(rs)
  
}

# Load data ------------------------------------------
tbl <- readRDS(file = '../rds/spei_all_v2.rds')
yrs <- unique(tbl$year)
vrd <- st_read('../shp/base/veredas_ok.shp')
summary(tbl$spei_3)
lbls <- as.data.frame(matrix(c(-3, -2, 1, -2, -1.5, 2, -1.5, -1.25, 3, -1.25, -1.0, 4, -1, 1, 5, 1, 1.25, 6, 1.25, 1.5, 7, 1.5, 1.75, 8, 1.75, 2, 9, 2, 2.5, 10), ncol = 3, byrow = T))

# Apply the function ------------------------------------------------------
rsl <- map(.x = yrs, .f = make_count)
rsl <- bind_rows(rsl)
dir.create('../rds/conteo_veredas')
saveRDS(rsl , file = '../rds/conteo_veredas/resultado_count_veredas_spei.rds')

# Functions to use --------------------------------------------------------
make_graph <- function(vereda){
  
  # vereda <- rsl$NOMBRE_VER[1]
  
  rs <- rsl %>% filter(NOMBRE_VER == vereda)
  
  rs <- rs %>% 
    group_by(class, year, month) %>% 
    mutate(porc = count / sum(count) * 100) %>% 
    ungroup()
  
  lb <- rs %>% distinct(lim_inf, lim_sup, lims) %>% arrange(lim_inf)
  
  unique(rs$lims)
  rs <- rs %>% 
    mutate(lims = factor(lims, levels = lb$lims))
  
  cl <- brewer.pal(n = 8, name = 'RdBu')
  cl <- c(cl[1:4], 'grey', cl[5:8])
  
  rs <- inner_join(rs, data.frame(month = 1:12, month_abb = month.abb), by = 'month')
  rs <- rs %>% mutate(month_abb = factor(month_abb, levels = month.abb))
  
  gg <- ggplot(data = rs, aes(x = month_abb, y = porc, fill = lims)) +
    geom_col() +
    facet_wrap(.~year) +
    scale_fill_manual(values = cl) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    theme_ipsum() +
    theme(legend.position = 'top',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(x = 'Meses', y = 'Porcentaje (%)', fill = 'Clase') +
    guides(fill = guide_legend(nrow = 1)) 
  
  ggsave(plot = gg, filename = paste0('../png/graphs/veredas/', vereda, '.png'), units = 'in', width = 18, height = 12, dpi =)
  
  return(rs)
  
}

# Read RDS ----------------------------------------------------------------
rsl <- readRDS('../rds/conteo_veredas/resultado_count_veredas_spei.rds')
rsl <- drop_na(rsl)
rsl <- inner_join(rsl, lbls, by = c('class' = 'V3'))
rsl <- rsl %>% mutate(lims = paste0(V1, ' - ', V2))
rsl <- rsl %>% dplyr::select(NOMBRE_VER, lim_inf = V1, lim_sup = V2, lims, class, year, month, count)

length(unique(rsl$NOMBRE_VER))

vrd <- unique(rsl$NOMBRE_VER)
rsltd <- map(.x = vrd, .f = make_graph)

rsltd <- bind_rows(rsltd)
unique(rsltd$porc)






