
# Load libraries -----------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, gtools, RSAGA, tidyverse, stringr, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Function to use ---------------------------------------------------------
downscaling_future <- function(rc, pr, vr){
  
  # Proof
  rc <- rcp[1]
  pr <- prd[1]
  vr <- 'prec'
  
  print(paste0('To start ', rc, ' ', pr, ' '))
  
  path <- glue('{root}{rc}/{pr}')
  gcms <- list.files(path, full.names = TRUE)
  
  lapply(2:length(gcms), function(k){
    
    print(gcms[k])
    fles <- list.files(gcms[k], full.names = TRUE, pattern = '.tif')
    fles <- mixedsort(fles)
    fles <- grep(vr, fles, value = TRUE)
    
    lapply(1:length(fles), function(j){
      
      print('To process')
      root.dem <- '../tif/dem/dem_monterrey_ha.tif'
      
      out <- paste0(fles[j])
      nch <- nchar(basename(fles[j]))
      out <- str_sub(string = out, start = 1, end = nchar(out) - nch)
      out <- glue('{out}ha_{basename(fles[j])}')
      
      rsl <- rsaga.geoprocessor(lib = 'statistics_regression',
                                module = 'GWR for Grid Downscaling',
                                param = list(PREDICTORS = root.dem,
                                             REGRESSION = out,
                                             DEPENDENT = fles[j]),
                                intern = TRUE,
                                display.command = TRUE,
                                env = env)
      
      print('Done!')
      
    })
    
  })
  
}


# Load data ---------------------------------------------------------------
zne <- shapefile('../shp/base/veredas_mas_monterrey.shp')
zne@data$gid <- 1

lim <- aggregate(zne, 'gid')

# Environment SAGA
env <- rsaga.env(path = 'C:/SAGA')

# Options
rcp <- c('rcp_4.5', 'rcp_8.5')
prd <- c('2030s', '2040', '2050s', '2060s')
vrs <- c('prec', 'tmax', 'tmean', 'tmin')

# Root path
root <- 'E:/politica_ambiental/tif/climate/future/'

# To apply the function ---------------------------------------------------

lapply(1:length(rcp), function(r){
  
  print('------------------------------ To start a new ------------------------------')
  print(rcp[r])
  
  lapply(1:length(prd), function(p){
    
    print(prd[p])
    
    lapply(1:length(vrs), function(v){
      
      print(vrs[v])
      
      downscaling_future(rc = rcp[r], pr = prd[p], vr = vrs[v])
      
      print('-------------------------------------- Done -------------------------------------')
      
    })
    
  })
  
})

downscaling_future(rc, pr, vr)



