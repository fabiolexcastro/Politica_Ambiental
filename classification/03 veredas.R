

# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools)

# Load veredas
shp <- st_read('E:/politica_ambiental/shp/base/veredas_ok.shp')

# Vereda
st_crs(shp)
rsl <- list()

# Escritura de cada shapefile
for(i in 1:nrow(shp)){
  
  rsl[[i]] <- shp %>% slice(i)
  
  st_write(obj = rsl[[i]], dsn = '../shp/veredas',
           layer = gsub(' ', '_', rsl[[i]]$NOMBRE_VER), driver = 'ESRI Shapefile')
  
}

# Checking the results
fls <- list.files('../shp/veredas', full.names = TRUE, pattern = '.shp$')

out <- paste0(sort(shp$NOMBRE_VER), '.shp')
diff(basename(fls), out)

out == basename(fls)

