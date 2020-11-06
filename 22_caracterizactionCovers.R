
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, readxl, xlsx, magrittr, ggrepel, rgdal, rgeos, stringr, sf, tidyverse, terra)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
lbl <- read_excel('../tbl/cobertura/labels_classes_type.xlsx')[,c(2, 5)]
tbl <- read_csv('../tbl/covers_years/cover_years_summary_area_rcl_v2.csv')

# Tidy the table ----------------------------------------------------------
tbl <- tbl %>% 
  dplyr::select(gid_2, reclassify_2, year_2000, year_2005, year_2010) %>% 
  inner_join(., lbl, by = 'gid_2') %>% 
  group_by(reclassify_2, type_2) %>% 
  summarise(year_2000 = sum(year_2000, na.rm = TRUE),
            year_2005 = sum(year_2005, na.rm = TRUE),
            year_2010 = sum(year_2010, na.rm = TRUE)) %>% 
  ungroup()

# To make the summarise --------------------------------------------------
smm <- tbl %>% 
  gather(year, value, -reclassify_2, -type_2) %>% 
  group_by(type_2, year) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
  ungroup()

smm <- smm %>% 
  arrange(desc(value)) %>% 
  group_by(year) %>% 
  dplyr::mutate(porc = value / sum(value) * 100) %>% 
  dplyr::mutate(porc = round(porc, 1)) %>% 
  mutate(type_2 = factor(type_2, levels = c('Transición de naturales a intervenidas', 'Ecosistemas transformados', 'Ecosistemas naturales', 'Zonas artificializadas')))

# A test graphic
gg <- ggplot(data = smm, aes(x = year, y = porc, color = type_2, fill = type_2)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_manual(name = '', values = c('#D4B300', '#CD8F0E', '#C0691B', '#B3401F')) +
  scale_colour_manual(name = '', values =  c('#D4B300', '#CD8F0E', '#C0691B', '#B3401F')) +
  theme_bw() +
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold')) +
  scale_x_discrete(labels = c('2000', '2005', '2010')) +
  labs(x = '', y = '%') +
  geom_label_repel(aes(label = porc), color = 'white', size = 3.5, point.padding = NA, show.legend = FALSE)
  # geom_label(aes(label = porc), color = 'white', size = 3.5)
  # geom_text(aes(label = porc), color = 'white')

ggsave(plot = gg, filename = '../png/graphs/covers/transition_covers.png', units = 'in', width = 9, height = 6, dpi = 300)


