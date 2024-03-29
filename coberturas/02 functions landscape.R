
create_graph <- function(tbl, nme, axs_y){
  
  tbl = cpland; nme = 'cpland'; axs_y = 'Porcentaje'
  tbl = pland; nme = 'pland'; axs_y = 'Porcentaje'
  lb <- lbl %>% setNames(c('Clase', '#'))
  lbgg <- ggtexttable(lb, rows = NULL, 
                      theme = ttheme(base_size = 6, 'lBlack', 
                                     colnames.style = colnames_style(face = 'bold', size = 8, fill = 'white')))

  # tbl <- tbl %>% mutate(value = value / 10000)
  # ymx <- max(tbl$value)
  
  gg <- ggplot(data = tbl, aes(x = class, y = value, fill = year, group = year)) +
    geom_col(position = 'dodge') +
    scale_fill_manual(values = c('#ff7f7e', '#7fcfc4', '#f8d67f', '#896E6E')) +
    theme_bw() +
    labs(x = '',
         y = axs_y, 
         fill = '') +
    theme(legend.position = 'top', # c(0.9, 0.8)
          axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          legend.text = element_text(size = 12)) +
    annotation_custom(ggplotGrob(lbgg), xmin = 10, xmax = 12, ymin = 30, ymax = 50) #+
    # coord_cartesian(ylim = c(0,1))
  ggsave(plot = gg, filename = paste0('../png/graphs/landscapemetrics/', nme, '_v2.png'), units = 'in', width = 13, height = 8, dpi = 300)  
  return(gg)
  
}

create_boxpl <- function(tbl, nme, axs_y, lowest, uppest, outliers){
  # tbl <- area_patch
  # nme <- 'area_patch'
  # axs_y <- 'ha'
  
  lb <- lbl %>% setNames(c('Clase', '#'))
  lbgg <- ggtexttable(lb, rows = NULL, 
                      theme = ttheme(base_size = 6, 'lBlack', 
                                     colnames.style = colnames_style(face = 'bold', size = 8, fill = 'white')))
  
  
  
  gg <- ggplot(data = tbl, aes(x = class, y = value, color = year)) +
    geom_boxplot(outlier.shape = outliers, size = 1.1) +
    scale_y_continuous(limits = c(lowest, uppest)) +
    scale_color_manual(values = c('#ff7f7e', '#7fcfc4', '#f8d67f', '#896E6E')) +
    labs(x = '',
         y = axs_y, 
         fill = '') +
    theme_bw() + 
    theme(legend.position = c(0.1, 0.8), #'top', # 0.1, 0.85 
          axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          legend.text = element_text(size = 12),
          legend.title = element_blank()) +
    annotation_custom(ggplotGrob(lbgg), xmin = 8, xmax = 11, ymin = 0.7, ymax = 1) 
  ggsave(plot = gg, filename = paste0('../png/graphs/landscapemetrics/', nme, '_v2.png'), units = 'in', width = 12, height = 9, dpi = 300)  
  return(gg)
}
 pland_function <- function(rst, year){
  
  # rst <- stk[[1]]
  # year <- 2000
  
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
np_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_np() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
te_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_te() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
mean_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_area_mn() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
cv_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_area_cv() %>% 
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
path_radios_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_p_gyrate() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
shape_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_p_shape() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
frctal_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_p_frac() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
para_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_p_para() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
contig_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_p_contig() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
perim_frg_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_pafrac() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
euc_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_p_enn() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
clumpy_function <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_clumpy() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
p_adj <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_pladj() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
}
agg_idx <- function(rst, year){
  rsl <- rst %>% 
    lsm_c_ai() %>% 
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
lsm_c_dcad_function <- function(rst, year){
  
  rsl <- rst %>% 
    lsm_c_dcad() %>% 
    inner_join(., lbl, by = c('class' = 'gid')) %>% 
    mutate(year = year)
  print('Done!')
  return(rsl)
  
}


