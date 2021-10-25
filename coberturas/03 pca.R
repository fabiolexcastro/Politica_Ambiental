
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, missMDA, ggpubr, factoextra,
               hrbrthemes, gridExtra,
               FactoMineR, fasterize, glue, landscapemetrics, outliers, fs, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
make_pca_class <- function(yr, lgnd){
  
  # yr <- 2000
  # lgnd <- 'none'
  
  cat('Start ', yr, '\n')
  tbl <- dplyr::filter(tbl.cls, year == yr)
  tbl <- dplyr::select(tbl, rcl, value, metrica)
  tbl <- tbl %>% spread(metrica, value)
  
  # Make the PCA Analysis
  rsp <- PCA(tbl[,3:ncol(tbl)])
  
  # Graph components contribution
  gcn <- fviz_eig(rsp, addlabels = TRUE, hjust = -0.3) +
    theme_ipsum_es() +
    labs(x = 'Componentes', 
         y = 'Contribución de explicación de\n las varianzas') + 
    ggtitle(label = glue('Contribución de las componentes - Año {yr}')) + 
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 11, face = 'bold'),
          axis.title.x = element_text(size = 11, face = 'bold'), 
          plot.title = element_text(size = 14, face = 'bold', hjust = 0.5))
  
  # Contribution of each variable
  cnt <- round(cbind(rsp$var$contrib[,1:2]), 2) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    setNames(c('metrica', paste0('dim_', 1:2))) %>% 
    as_tibble() %>% 
    arrange(desc(dim_1))
  
  gvr <- fviz_pca_var(rsp,
                      col.var = "contrib", # Color by contributions to the PC
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE) + # Avoid text overlapping
    labs(color = 'Contribución',
         x = glue('Componente 1 ({round(rsp$eig[1,2], 1)}%)'),
         y = glue('Componente 2 ({round(rsp$eig[2,2], 1)}%)')) +
    ggtitle(label = glue('Variable PCA - Año: {yr}')) +
    theme(legend.position = lgnd, #"bottom"
          legend.key.width = unit(3, 'line'), 
          plot.title = element_text(size = 15, face = 'bold', hjust = 0.5)) 
  
  cat('Done\n')
  return(list(gcn, gvr))
  
}

make_pca_patch <- function(yr){
  
  # yr <- 2000
  
  cat('Start ', yr, '\n')
  
  tbl <- filter(tbl.prc, year == yr)
  tbl <- dplyr::select(tbl, rcl, value, metrica)
  tbl <- tbl %>% group_by(rcl, metrica) %>% summarise(value = mean(value, na.rm = TRUE)) %>% ungroup()
  tbl <- spread(tbl, metrica, value)
  
  # Make the PCA Analysis
  rsp <- PCA(tbl[,3:ncol(tbl)])
  
  # Graph components contribution
  gcn <- fviz_eig(rsp, addlabels = TRUE, hjust = -0.3) +
    theme_ipsum_es() +
    labs(x = 'Componentes', 
         y = 'Contribución de explicación de\n las varianzas') + 
    ggtitle(label = glue('Contribución de las componentes - Año {yr}')) + 
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 11, face = 'bold'),
          axis.title.x = element_text(size = 11, face = 'bold'), 
          plot.title = element_text(size = 14, face = 'bold', hjust = 0.5))
  
  # Contribution of each variable
  cnt <- round(cbind(rsp$var$contrib[,1:2]), 2) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    setNames(c('metrica', paste0('dim_', 1:2))) %>% 
    as_tibble() %>% 
    arrange(desc(dim_1))
  
  # Make the PCA graph
  gvr <- fviz_pca_var(rsp,
                      col.var = "contrib", # Color by contributions to the PC
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE) + # Avoid text overlapping
    labs(color = 'Contribución',
         x = glue('Componente 1 ({round(rsp$eig[1,2], 1)}%)'),
         y = glue('Componente 2 ({round(rsp$eig[2,2], 1)}%)')) +
    ggtitle(label = glue('Variable PCA - Año: {yr}')) +
    theme(legend.position = 'bottom', #""
          legend.key.width = unit(3, 'line'), 
          plot.title = element_text(size = 15, face = 'bold', hjust = 0.5)) 
  
  cat('Done\n')
  return(list(gcn, gvr))
  
}

# Load data ---------------------------------------------------------------
pth <- '../tbl/metrics'
fls <- dir_ls(pth)
tbl <- map(.x = fls, .f = read_csv)
nms <- basename(fls) %>% gsub('.csv', '', .)
tbl <- map(.x = 1:length(tbl), .f = function(k) 
  tbl[[k]] %>% mutate(metrica = nms[k]))

lbl <- data.frame(name = nms)
lbl <- read.csv('../tbl/metrics.csv')

# -------------------------------------------------------------------------
# Grupos de tablas --------------------------------------------------------
# -------------------------------------------------------------------------

tbl <- bind_rows(tbl)
tbl <- inner_join(tbl, lbl, by = c('metrica' = 'name'))

# --------------------------------------------------------------------------
# Clase --------------------------------------------------------------------
# --------------------------------------------------------------------------
cls <- filter(lbl, type == 'class')
tbl.cls <- filter(tbl, type == 'class')

smm.cls <- tbl.cls %>% 
  group_by(type, metrica, function.) %>% 
  summarise(count = n()) %>% 
  ungroup()

# Normalization
nrm.cls <- tbl.cls %>% 
  dplyr::select(-id) %>% 
  drop_na() %>% 
  group_by(year, metrica) %>% 
  summarise(score = scores(value, 'z')) %>% 
  ungroup() 

# PCA Analysis -------------------------------------------------------------
ggs_pca <- map2(.x = c(2000, 2005, 2010, 2018), .y = rep('bottom', 4), .f = make_pca_class)
ggs_pca_cnt <- lapply(ggs_pca, `[[`, 2) 

fg1 <- annotate_figure(ggs_pca_cnt[[1]])
fg2 <- annotate_figure(ggs_pca_cnt[[2]])
fg3 <- annotate_figure(ggs_pca_cnt[[3]])
fg4 <- annotate_figure(ggs_pca_cnt[[4]])

ggs_pca_all <- grid.arrange(ggs_pca_cnt[[1]], ggs_pca_cnt[[2]], 
                            ggs_pca_cnt[[3]], ggs_pca_cnt[[4]], common.legend = TRUE, legend = 'bottom')

lbl %>% filter(type == 'class')

all <- grid.arrange(
  fg1, fg2, fg3, fg4, 
  bottom = text_grob(
    'agg = Indice de agregación, avg_patch = Promedio del área del parche, clm = Indice de aglomeración, cpland = % de área del núcleo,
    cv_patch = CV del parche, lrg = Índice del parche más largo, lsm_dcad = Densidad del área central disyunta,
    padj = % de adyacencias similares, perim_frg = Dimensión fractal del área perimétral, pland = Porcentaje de la clase del paisaje, te = Borde total', size = 11))

ggsave(plot = all, 
       filename = '../png/graphs/pca/pca_grp_class.jpg', units = 'in', 
       width = 12, height = 10, dpi = 300)


# --------------------------------------------------------------------------
# Parche --------------------------------------------------------------------
# --------------------------------------------------------------------------
prc <- filter(lbl, type == 'patch')
tbl.prc <- filter(tbl, type == 'patch')

smm.prc <- tbl.prc %>% 
  group_by(type, metrica, function.) %>% 
  summarise(count = n()) %>% 
  ungroup()

# Normalization 
nrm.cls <- tbl.prc %>% 
  dplyr::select(-id) %>% 
  drop_na() %>% 
  group_by(year, metrica) %>% 
  summarise(score = scores(value, 'z')) %>% 
  ungroup()

# PCA Analysis --------------------------------------------------------------

ggs_pca <- map(.x = c(2000, 2005, 2010, 2018), .f = make_pca_patch)
ggs_pca_cnt <- lapply(ggs_pca, `[[`, 2) 

fg1 <- annotate_figure(ggs_pca_cnt[[1]])
fg2 <- annotate_figure(ggs_pca_cnt[[2]])
fg3 <- annotate_figure(ggs_pca_cnt[[3]])
fg4 <- annotate_figure(ggs_pca_cnt[[4]])

all <- grid.arrange(
  fg1, fg2, fg3, fg4, 
  bottom = text_grob(
    'area_patch = Área del parche, cntgi = Índice de contigüidad, euc_enn = Distancia euclideana al parche más cercano,
    frctl = Índice de dimensión fractal, paraf = Relación perimetro - área, radius = Relación de giro,
    shpe = Índice de forma', size = 11))

ggsave(plot = all, 
       filename = '../png/graphs/pca/pca_grp_patch.jpg', units = 'in', 
       width = 12, height = 10, dpi = 300)
