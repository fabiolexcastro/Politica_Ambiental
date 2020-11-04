

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, readxl, xlsx, magrittr, rgdal, rgeos, stringr, sf, tidyverse, terra)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
fls <- list.files('../tif/cover_mpio', full.names = TRUE, pattern = '.tif$')
fls <- grep('_v2.tif', fls, value = TRUE)
stk <- map(.x = fls, .f = raster)
stk <- stack(stk)
lbl <- read_csv('../tbl/covers_years/cover_years_summary_area_rcl_v2.csv')
lbl <- lbl %>% dplyr::select(gid_2, reclassify_2) %>% distinct()
lbl <- lbl %>% mutate(type = c('antropico', 'antropico', 'antropico', 'natural', 'antropico - natural', 'natural', 'antropico - natural', 'natural'))

# Raster to table ---------------------------------------------------------
tbl <- rasterToPoints(stk, spatial = FALSE)
tbl <- as_tibble(tbl)
names(tbl) <- c('x', 'y', 'cover_00', 'cover_05', 'cover_10')

# Comparing the periods ---------------------------------------------------
tbl %<>% 
  mutate(cmp_00_05 = ifelse(cover_00 == cover_05, 'No change', 'Change'),
         cmp_05_10 = ifelse(cover_05 == cover_10, 'No change', 'Change')) 

tbl <- inner_join(tbl, lbl, by = c('cover_00' = 'gid_2')) %>% 
  rename(lbl_00 = reclassify_2) %>% 
  inner_join(., lbl, by = c('cover_05' = 'gid_2')) %>% 
  rename(lbl_05 = reclassify_2) %>% 
  inner_join(., lbl, by = c('cover_10' = 'gid_2')) %>% 
  rename(lbl_10 = reclassify_2) %>% 
  dplyr::select(x, y, lbl_00, lbl_05, lbl_10, cmp_00_05, cmp_05_10)

tbl <- tbl %>% 
  mutate(cov_00_05 = paste0(lbl_00, ' - ', lbl_05),
         cov_05_10 = paste0(lbl_05, ' - ', lbl_10)) 

saveRDS(object = tbl, file = '../rds/cobertura_differences_years.rds')

tbl <- readRDS('../rds/cobertura_differences_years.rds')

# Subsetting the filter (only change)
chn <- tbl %>% 
  filter(cmp_00_05 == 'Change' | cmp_05_10 == 'Change')

# 2000 vs 2005 -----------------------------------------------------------
chn_00_05 <- chn %>% 
  dplyr::select(x, y, cov_00_05) %>% 
  group_by(cov_00_05) %>%
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>% 
  setNames(c('Coberturas', 'Conteo_pixeles')) %>% 
  mutate(year = '2000 - 2005')

chn_05_10 <- chn %>% 
  dplyr::select(x, y, cov_05_10) %>% 
  group_by(cov_05_10) %>%
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>% 
  setNames(c('Coberturas', 'Conteo_pixeles')) %>% 
  mutate(year = '2005 - 2010')

changes <- rbind(chn_00_05, chn_05_10)
changes <- changes %>% dplyr::select(year, Coberturas, Conteo_pixeles)
write.xlsx(changes, file = "../tbl/cobertura/changes_covers_years.xlsx",
           sheetName = "sheet1", append = FALSE)
write.xlsx(lbl, file = '../tbl/cobertura/labels_classes_type.xlsx')
