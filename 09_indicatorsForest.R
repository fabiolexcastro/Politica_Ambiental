
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, terra, trend, sf, tidyverse, ggspatial)

rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------

tbl <- read_csv('../tbl/forest/Perdida_bosque_anual.csv')
frs <- read_csv('../tbl/forest/Cobertura Boscosa.csv')

# Summarize forest --------------------------------------------------------
smm_frs <- frs %>% 
  group_by(type) %>% 
  summarise(has = sum(has)) %>% 
  ungroup()

print(paste0('Se tiene un total de ', round(smm_frs[1,2], 1), ' hectareas de bosque'))

# Summarize deforestation -------------------------------------------------
tbl_smm <- tbl %>% 
  group_by(year) %>% 
  dplyr::summarise(has = sum(has)) %>% 
  ungroup()

# Nice graph --------------------------------------------------------------
gg <- ggplot(data = tbl, aes(x = year, y = has)) +
  geom_line() +
  labs(x = '',
       y = 'Hectáreas', 
       caption = 'Adaptado de Hansen et al., 2019') +
  ggtitle(label = 'Cantidad de área deforestada por año en Monterrey') +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5))
ggsave(plot = gg, 
       filename = '../png/graphs/DeforestationYearly.png', 
       units = 'in', width = 12, height = 9, dpi = 300)


# All ---------------------------------------------------------------------

tot_frs <- as.numeric(round(smm_frs[1,2], 1))

tbl_smm <- tbl_smm %>% 
  mutate(deforest_cum = cumsum(tbl_smm$has),
         forest_cum = tot_frs - deforest_cum,
         rate = has / forest_cum * 100)

write.csv(tbl_smm, '../tbl/forest/average_deforestation_byyear.csv', row.names = FALSE)


print(paste0('El average rate of deforestation es de ', round(mean(pull(tbl_smm, 5)), 2), ' %'))

# El % de area respecto al municipio

tbl_smm <- tbl_smm %>% 
  mutate(total_porc_forest = forest_cum / 78095 * 100)
write.csv(tbl_smm, '../tbl/forest/average_deforestation_byyear.csv', row.names = FALSE)


