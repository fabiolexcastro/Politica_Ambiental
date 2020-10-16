
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, missMDA, ggpubr, factoextra, FactoMineR, rgdal, rgeos, stringr, sf, tidyverse, fasterize, landscapemetrics, outliers)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
tbl <- list.files('../tbl/metrics', full.names = TRUE, pattern = '.csv$')
tbl <- tbl[-grep('metrics_all_normalization', tbl, value = FALSE)]
nms <- basename(tbl) %>% gsub('.csv', '', .)
lbl <- read_csv('../tbl/labels_metrics.csv')

# Read tables -------------------------------------------------------------
tbl <- map(.x= tbl, .f = read_csv)
tbl <- map(.x = 1:length(tbl), .f = function(k) tbl[[k]] %>% mutate(metrica = nms[k]))
nrw <- map(.x = tbl, .f = nrow) %>% unlist()

# Summarizing the largest tables ------------------------------------------
tbl_sml <- tbl[which(nrw <= 21)]
tbl_sml <- map(.x = 1:length(tbl_sml), .f = function(k){
  
  tbl_sml[[k]] %>% 
    dplyr::select(metrica, class, year, name, value)

})

tbl_big <- tbl[which(nrw > 21)]
tbl_big <- map(.x = 1:length(tbl_big), .f = function(k){
  
  tbl_big[[k]] %>% 
    group_by(metrica, class, year, name) %>% 
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>% 
    ungroup() %>% 
    dplyr::select(metrica, class, year, name, value)
  
})

tbl_sml <- bind_rows(tbl_sml)
tbl_big <- bind_rows(tbl_big)
tbl <- rbind(tbl_sml, tbl_big)
tbl <- drop_na(tbl) # It's ok

saveRDS(object = tbl, file = '../rds/metrics_means.rds')

# Normalization -----------------------------------------------------------
vle <- tbl %>% 
  pull(value) %>% 
  round(., 1) %>% 
  scores(., 'z') 
tbl <- tbl %>% mutate(value_norm = vle)

mean(vle) %>% round(., 1)
sd(vle)
unique(tbl$metrica) %>% length()

tbl <- tbl %>% 
  mutate(metrica = str_replace(string = metrica, pattern = ' ', replacement = '_')) %>% 
  dplyr::select(-value)
tbl <- inner_join(tbl, lbl, by = 'metrica')
tbl <- tbl %>% dplyr::select(abbreviation, class, year, name, value_norm)
tbl <- tbl %>% spread(abbreviation, value_norm)
tbl <- tbl %>% mutate_if(is.numeric, round, 3)

write.csv(tbl, '../tbl/metrics/metrics_all_normalization.csv', row.names = FALSE)

# PCA Analysis ------------------------------------------------------------

# 2000 -------------------------------------------
tbl.2000 <- tbl %>% filter(year == 2000)
res.pca.2000 <- PCA(tbl.2000[,4:20])
plot(res.pca.2000)
plot.PCA(res.pca.2000, choix = 'in', invisible = 'ind.sup')

png(filename = '../png/graphs/pca/pca_analysis.png', units = 'in', width = 12, height = 9, res = 300)
plot.PCA(res.pca.2000, choix = 'var', invisible = 'quanti.sup')
dev.off()

res.hcpc <- HCPC(res.pca.2000)

# Contribucion de las dimensiones a la explicacion de la varianza total de las variables
gg.scr.00 <- fviz_eig(res.pca.2000, addlabels = TRUE, hjust = -0.3) + 
  theme_bw() +
  labs(x = 'Componentes', 
       y = 'Contribución de explicación de\n las varianzas') +
  ggtitle(label = 'Contribución de las componentes - Año 2000') +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'), 
        plot.title = element_text(size =12, face = 'bold', hjust = 0.5))
ggsave(plot = gg.scr.00, filename = '../png/graphs/pca/Scree Plot 2000.png',
       units = 'in', width = 9, height = 6, dpi = 300)

# Identificar cual es la contribucion de cada variable (metrica) a la varianza / variabilidad de todo mi pull de datos
contrib.00 <- round(cbind(res.pca.2000$var$contrib[,1:2]), 2) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('metrica', paste0('dim_', 1:2))) %>% 
  as_tibble() %>% 
  arrange(desc(dim_1))

gg.cnt.00 <- fviz_pca_var(res.pca.2000,
                          col.var = "contrib", # Color by contributions to the PC
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE) + # Avoid text overlapping
  labs(color = 'Contribución',
       x = 'Componente 1 (63.6%)',
       y = 'Componente 2 (21.6%)') +
  theme(legend.position = 'none',
        legend.key.width = unit(3, 'line')) +
  ggtitle(label = '')
ggsave(plot = gg.cnt.00, filename = '../png/graphs/pca/pca_contrib_2000.png',
       units = 'in', width = 9, height = 6, dpi = 300)

write.csv(contrib.00, '../tbl/pca/contrib_metricas_2000.csv', row.names = FALSE)

# 2005 -------------------------------------------
tbl.2005 <- tbl %>% filter(year == 2005)
res.pca.2005 <- PCA(tbl.2005[,4:20])

# Contribucion de las dimensiones a la explicacion de la varianza total de las variables
gg.scr.05 <- fviz_eig(res.pca.2005, addlabels = TRUE, hjust = -0.3) + 
  theme_bw() +
  labs(x = 'Componentes', 
       y = 'Contribución de explicación de\n las varianzas') +
  ggtitle(label = 'Contribución de las componentes - Año 2005') +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'), 
        plot.title = element_text(size =12, face = 'bold', hjust = 0.5))
ggsave(plot = gg.scr.05, filename = '../png/graphs/pca/Scree Plot 2005.png',
       units = 'in', width = 9, height = 6, dpi = 300)

# Identificar cual es la contribucion de cada variable (metrica) a la varianza / variabilidad de todo mi pull de datos
contrib.05 <- round(cbind(res.pca.2005$var$contrib[,1:2]), 2) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('metrica', paste0('dim_', 1:2))) %>% 
  as_tibble() %>% 
  arrange(desc(dim_1))

gg.cnt.05 <- fviz_pca_var(res.pca.2005,
                          col.var = "contrib", # Color by contributions to the PC
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE) + # Avoid text overlapping
  labs(color = 'Contribución',
       x = 'Componente 1 (69.3%)',
       y = 'Componente 2 (20.2%)') +
  theme(legend.position = 'none',
        legend.key.width = unit(3, 'line')) +
  ggtitle(label = '')
ggsave(plot = gg.cnt.05, filename = '../png/graphs/pca/pca_contrib_2005.png',
       units = 'in', width = 9, height = 6, dpi = 300)

write.csv(contrib.05, '../tbl/pca/contrib_metricas_2005.csv', row.names = FALSE)

# 2010 -------------------------------------------
tbl.2010 <- tbl %>% filter(year == 2010)
res.pca.2010 <- PCA(tbl.2010[,4:20])

# Contribucion de las dimensiones a la explicacion de la varianza total de las variables
gg.scr.10 <- fviz_eig(res.pca.2010, addlabels = TRUE, hjust = -0.3) + 
  theme_bw() +
  labs(x = 'Componentes', 
       y = 'Contribución de explicación de\n las varianzas') +
  ggtitle(label = 'Contribución de las componentes - Año 2010') +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'), 
        plot.title = element_text(size =12, face = 'bold', hjust = 0.5))
ggsave(plot = gg.scr.10, filename = '../png/graphs/pca/Scree Plot 2010.png',
       units = 'in', width = 9, height = 6, dpi = 300)

contrib.10 <- round(cbind(res.pca.2010$var$contrib[,1:2]), 2) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('metrica', paste0('dim_', 1:2))) %>% 
  as_tibble() %>% 
  arrange(desc(dim_1))

gg.cnt.10 <- fviz_pca_var(res.pca.2010,
                          col.var = "contrib", # Color by contributions to the PC
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE) + # Avoid text overlapping
  labs(color = 'Contribución',
       x = 'Componente 1 (69%)',
       y = 'Componente 2 (20.4%)') +
  theme(legend.position = 'bottom',
        legend.key.width = unit(3, 'line')) +
  ggtitle(label = '')
ggsave(plot = gg.cnt.10, filename = '../png/graphs/pca/pca_contrib_2010.png',
       units = 'in', width = 9, height = 6, dpi = 300)

write.csv(contrib.10, '../tbl/pca/contrib_metricas_2005.csv', row.names = FALSE)

# Join the pca graphics --------------------------------------------------
gg.all <- ggarrange(gg.cnt.00, gg.cnt.05, gg.cnt.10, ncol = 1, nrow = 3, labels = c('PCA 2000', 'PCA 2005', 'PCA 2010'))
ggsave(plot = gg.all, filename = '../png/graphs/pca/pca_contrib_years.png',
       units = 'in', width = 5, height = 14, dpi = 300)

# Tidy the table contributino --------------------------------------------
contrib.00 <- contrib.00 %>% setNames(c('metrica', 'dim_1_2000', 'dim_2_2000'))
contrib.05 <- contrib.05 %>% setNames(c('metrica', 'dim_1_2005', 'dim_2_2005'))
contrib.10 <- contrib.10 %>% setNames(c('metrica', 'dim_1_2010', 'dim_2_2010'))

contrib <- list(contrib.00, contrib.05, contrib.10) %>% 
  purrr::reduce(inner_join, by = 'metrica') 
  
write.csv(contrib, '../tbl/pca/contrib_metricas_all.csv', row.names = FALSE)

contrib <- contrib %>% 
  gather(var, value, -metrica) %>% 
  mutate(year = str_sub(var, 7, 10),
         dimension = str_sub(var, 1, 5) %>% str_replace(., '_', ' ')) %>% 
  dplyr::select(metrica, dimension, year, value)


# Scree plot - all in only one -----------------------------------------------
gg_contrib <- ggplot(data = aes(x = ))


# End ---------------------------------------------------------------------