
make_map <- function(vr){
  
  # vr <- 'tmax'
  
  vl <- tb %>% filter(variable == vr) 
  
  if(vr == 'prec'){
    
    clr <- RColorBrewer::brewer.pal(n = 8, name = 'BrBG')
    
    gg <- ggplot(vl) +
      geom_tile(aes_string(x = 'x', y = 'y', fill = 'difference')) +
      facet_wrap(~ month_abb) +
      scale_fill_gradientn(colours = clr, 
                           na.value = 'white') +
      geom_polygon(data = vrd, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
      coord_equal() +
      theme_bw() +
      theme(legend.position = 'bottom',
            plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key.width = unit(5, 'line')) +
      guides(shape = guide_legend(override.aes = list(size = 10))) +
      labs(x = 'Longitud', y = 'Latitud', fill = 'Diferencia (mm)')
    
    ggsave(plot = gg, filename = paste0('../png/maps/climate/', rcp, '_', yea, '_', vr, '_', 'diferencia', '.png'), units = 'in', width = 11, height = 11, dpi = 300)
    
    clr <- RColorBrewer::brewer.pal(n = 8, name = 'BrBG')
    
    gg <- ggplot(vl) +
      geom_tile(aes_string(x = 'x', y = 'y', fill = 'crn')) +
      facet_wrap(~ month_abb) +
      scale_fill_gradientn(colours = clr, 
                           na.value = 'white') +
      geom_polygon(data = vrd, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
      coord_equal() +
      theme_bw() +
      theme(legend.position = 'bottom',
            plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key.width = unit(5, 'line')) +
      guides(shape = guide_legend(override.aes = list(size = 10))) +
      labs(x = 'Longitud', y = 'Latitud', fill = 'Precipitación (mm)')
    
    ggsave(plot = gg, filename = paste0('../png/maps/climate/', vr, '_', 'baseline', '.png'), units = 'in', width = 11, height = 11, dpi = 300)
    
  } else {
    
    clr <- RColorBrewer::brewer.pal(n = 8, name = 'YlOrRd')
    
    gg <- ggplot(vl) +
      geom_tile(aes_string(x = 'x', y = 'y', fill = 'difference')) +
      facet_wrap(~ month_abb) +
      scale_fill_gradientn(colours = clr, 
                           na.value = 'white') +
      geom_polygon(data = vrd, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
      coord_equal() +
      theme_bw() +
      theme(legend.position = 'bottom',
            plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key.width = unit(5, 'line')) +
      guides(shape = guide_legend(override.aes = list(size = 10))) +
      labs(x = 'Longitud', y = 'Latitud', fill = 'Diferencia (°C)')
    
    ggsave(plot = gg, filename = paste0('../png/maps/climate/', rcp, '_', yea, '_', vr, '_', 'diferencia', '.png'), units = 'in', width = 11, height = 11, dpi = 300)
    
    clr <- RColorBrewer::brewer.pal(n = 8, name = 'YlOrRd')
    
    gg <- ggplot(vl) +
      geom_tile(aes_string(x = 'x', y = 'y', fill = 'crn')) +
      facet_wrap(~ month_abb) +
      scale_fill_gradientn(colours = clr, 
                           na.value = 'white') +
      geom_polygon(data = vrd, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
      coord_equal() +
      theme_bw() +
      theme(legend.position = 'bottom',
            plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key.width = unit(5, 'line')) +
      guides(shape = guide_legend(override.aes = list(size = 10))) +
      labs(x = 'Longitud', y = 'Latitud', fill = '°C')
    
    ggsave(plot = gg, filename = paste0('../png/maps/climate/', vr, '_', 'baseline', '.png'), units = 'in', width = 11, height = 11, dpi = 300)
    
    
  }
  
  print('Done!')
  
}

make_stack <- function(cr, ft){
  
  # cr <- crn
  # ft <- ftr
  
  names(cr) <- paste0('crn_', names(cr))
  names(ft) <- paste0('ftr_', names(ft))
  
  ms.cr <- cr[[1]] * 0
  ms.ft <- ft[[1]] * 0
  
  print('Stacking current - future')
  ft <- raster::crop(ft, ms.cr) %>% raster::mask(., ms.cr)
  st <- addLayer(cr, ft)
  
  print('Raster to table')
  tb <- rasterToPoints(st, spatial = FALSE) %>% as_tibble()
  
  print('Tidy to table')
  tb <- tb %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -gid, -x, -y) 
  tb <- tb %>% mutate(period = str_sub(var, 1, 3))
  tb <- tb %>% mutate(variable = str_sub(var, 5, 8))
  tb <- tb %>% mutate(month = parse_number(var))
  tb <- tb %>% dplyr::select(gid, x, y, period, variable, month, value)
  tb <- tb %>% spread(period, value)
  tb <- tb %>% mutate(ftr = ifelse(variable %in% c('tmax', 'tmea', 'tmin'), ftr / 10, ftr))
  tb <- tb %>% mutate(difference = ftr - crn)
  tb <- tb %>% mutate(porcentaje = (difference / crn) * 100)
  
  print('To save the file')
  saveRDS(object = tb, file = paste0('../rds/climate/', rcp, '_', yea, '.rds'))
  
  print('To make the map')
  vrs <- unique(tb$variable)
  
  print('To make the labels')
  lbl <- data.frame(month = 1:12, month_abb = c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dic'))
  tb <- inner_join(tb, lbl, by = 'month')
  tb <- tb %>% mutate(month_abb = factor(month_abb, levels = c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dic')))
  
  map(vrs, make_map)
  
  print('Done Done Done')
  
}