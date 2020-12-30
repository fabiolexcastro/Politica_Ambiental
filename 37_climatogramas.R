
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)

rm(list = ls())
options(scipen = 999)

# Load data
fls <- list.files("E:/politica_ambiental/rds/climate", full.names = TRUE, pattern = '.rds$')

# Function to use 
get_climatograma <- function(fle){
  
  # fle <- fls[6]
  tbl <- readRDS(fle)
  smm <- tbl %>% 
    group_by(variable, month_abb) %>% 
    summarise(crn = mean(crn),
              ftr = mean(ftr)) %>% 
    ungroup() %>% 
    gather(period, value, -variable, -month_abb)
  
  gg <- ggplot(data = smm %>% filter(variable == 'prec'), aes(x = month_abb)) +
    geom_bar(aes(y = value, group = period, fill = period), stat = 'identity', position = 'dodge') +
    scale_fill_manual(name = 'Precipitación', 
                      values = c('#58acfa', '#0431b4'),
                      labels = c('ftr' = 'Futuro', 'crn' = 'Línea base')) +
    ylab("Precipitación (mm)")
  
  rlc <- mean(smm %>% filter(variable == 'prec') %>% pull(value) / smm %>% filter(variable == 'tmax') %>% pull(value))
  rlc <- rlc * 2
  
  gg <- gg +
    geom_line(data = smm %>% filter(variable == 'tmax'), aes(y = value * rlc, colour = period, group = period, linetype = 'E'), size = 1.2) +
    geom_line(data = smm %>% filter(variable == 'tmin'), aes(y = value * rlc, colour = period, group = period, linetype = 'D'), size = 1.2) +
    scale_y_continuous(sec.axis = sec_axis(~./rlc, name = 'Temperatura ºC')) +
    scale_color_manual(name = 'Temperatura',
                       values = c('ftr' = 'black', 'crn' = 'grey'),
                       labels = c('ftr' = 'Futuro', 'crn' = 'Línea base')) +
    scale_linetype_manual(name = '',
                          values = c('D' = 2, 'E' = 1),
                          labels = c('D' = 'Min', 'E' = 'Máx')) +
  theme_bw() +
  theme(legend.position = 'bottom',
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank()) +
    guides(linetype = guide_legend(nrow = 2, keywidth = 3, order = 4, title.position = 'top', size = 15),
           color = guide_legend(nrow = 2, keywidth = 3, order = 3, title.position = 'top', size = 15),
           fill = guide_legend(order = 1, title.position = 'top', size = 15),
           size = guide_legend(order = 2, nrow = 2, title.position = 'top', size = 15)) +
    xlab('')
        
  # ggsave(plot = gg, filename = '../png/graphs/climograma/climograma_rcp_85_2080s.png', units = 'in', width = 11, height = 9, dpi = 300)
  return(smm)
  
}

tbls <- map(.x = fls, .f = get_climatograma)
type <- c('rcp_45_2030s', 'rcp_45_2050s', 'rcp_45_2080s', 'rcp_85_2030s', 'rcp_85_2050s', 'rcp_85_2080s')
tbls <- map(.x = 1:length(tbls), .f = function(k) tbls[[k]] %>% setNames(c('variable', 'mes', 'periodo', type[k])))
tbls <- tbls %>% purrr::reduce(.x = ., .f = inner_join)
tbls <- tbls %>% filter(variable != 'tmea')

crn <- tbls %>% filter(periodo == 'crn')
crn <- crn %>% dplyr::select(variable, mes, periodo, current = rcp_45_2030s)
ftr <- tbls %>% filter(periodo != 'crn')
ftr <- ftr %>% mutate(current = pull(crn, current))
ftr <- ftr %>% dplyr::select(variable, mes, current, everything())
write.csv(ftr, '../tbl/climate/climograma.csv', row.names = FALSE)

