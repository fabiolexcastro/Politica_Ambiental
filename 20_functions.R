
# Functions to use -------------------------------------
library(landscapemetrics)
library(tidyverse)

agg_idx <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_ai() %>% 
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

np_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_np() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}

lrg_pth <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_lpi() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}

pland_function <- function(rst, year){

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

lsm_c_dcad_function <- function(rst, year){
  
  rsl <- rst %>% 
    lsm_c_dcad() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
  
}



create_graph <- function(tbl, nme, axs_y){
  gg <- ggplot(data = tbl, aes(x = as.character(class), y = value, fill = year, group = year)) +
    geom_col(position = 'dodge') +
    scale_fill_manual(values = c('#ff7f7e', '#7fcfc4', '#f8d67f')) +
    theme_bw() +
    labs(x = '',
         y = axs_y, 
         fill = '') +
    theme(legend.position = 'top', # c(0.9, 0.8)
          axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          legend.text = element_text(size = 12)) 
  ggsave(plot = gg, filename = paste0('../png/graphs/landscapemetrics_covers/', nme, '.png'), units = 'in', width = 9, height = 6, dpi = 300)  
  return(gg)
}

create_boxpl <- function(tbl, nme, axs_y, lowest, uppest, outliers){
  # tbl <- area_patch
  # nme <- 'area_patch'
  # axs_y <- 'ha'
  gg <- ggplot(data = tbl, aes(x = as.character(class), y = value, color = year)) +
    geom_boxplot(outlier.shape = outliers, size = 1.1) +
    scale_y_continuous(limits = c(lowest, uppest)) +
    scale_color_manual(values = c('#ff7f7e', '#7fcfc4', '#f8d67f')) +
    labs(x = '',
         y = axs_y, 
         fill = '') +
    theme_bw() + 
    theme(legend.position = c(0.1, 0.85), #'top', 
          axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          legend.text = element_text(size = 12),
          legend.title = element_blank()) 
  ggsave(plot = gg, filename = paste0('../png/graphs/landscapemetrics_covers/', nme, '.png'), units = 'in', width = 12, height = 9, dpi = 300)  
  return(gg)
}