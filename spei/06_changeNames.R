
# To change the names
library(stringr)

root <- '../tif/casanare_v2/chirts/monthly'
fles <- list.files(root, full.names = TRUE, pattern = '.tif$')
fles <- fles[-grep('tmin', fles, value = FALSE)]
nmes <- basename(fles)
nmes <- str_sub(nmes, start = 8, end = nchar(nmes))

for(i in 1:length(fles)){
  
  print(nmes[i])
  file.rename(from = fles[i], to = paste0(root, '/', 'chirts_', 'tmax_', nmes[i]))
  
}