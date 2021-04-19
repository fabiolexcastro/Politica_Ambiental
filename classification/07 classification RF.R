
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, dismo, caret, mlbench, rgdal, rgeos, randomForest, furrr, future, stringr, sf, tidyverse, gtools)
pacman::p_load(mlr3, mlr3learners, mlr3pipelines, mlr3filters, mlr3misc, paradox, mlr3tuning)

rm(list = ls())
source('FunctionsRFclustering.R')

# Functions to use --------------------------------------------------------
rf.clust <- function(occ, nforest, ntrees, nVars, nclasses){
  # occ = back_swd; nforest = 50; ntrees = 500; nVars = 8; nclasses = 2
  datRF_presences <- occ[,3:ncol(occ)] %>% as.data.frame()
  print(nrow(datRF_presences))
  attach(datRF_presences)
  no.forests <- nforest
  no.trees <- ntrees
  distRF_presences <- RFdist(datRF_presences, mtry1 = nVars, no.trees, no.forests, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
  no.presencesclasses <- nclasses
  labelRF <- pamNew(distRF_presences$cl1, no.presencesclasses)
  print(table(labelRF))
  clusterdata <- hclust(as.dist(distRF_presences$cl1), method = 'ward.D2')
  return(list(labelRF, clusterdata))
}

# Load data ---------------------------------------------------------------
tbl <- read_csv('../tbl/training/points_sample_vrbles.csv')
lbl <- tbl %>% distinct(gid, clase)
vrb <- colnames(tbl)[5:length(colnames(tbl))]

fls <- list.files('../raster/mosaic', full.names = TRUE, pattern = '.tif$')
fls <- grep(paste0(vrb, collapse = '|'), fls, value = TRUE)
stk <- stack(fls)

plot(stk[[1]])

clusteredpresdata <- tbl

# Background processing ---------------------------------------------------
SPspecies <- SpatialPoints(tbl[,1:2])
back_raster <- stk[[1]] * 0 + 1
speciescell <- raster::extract(msk, SPspecies, cellnumber = TRUE)
back_raster[speciescell[,1]] <- NA
samplesize <- round(min(summary(as.factor(clusteredpresdata$gid))) / 2, 0) 
NumberOfClusters <- max(clusteredpresdata$gid) 
ratio <- NumberOfClusters/1
numberofpresences <- nrow(clusteredpresdata) 
crs(back_raster) <- crs(msk)
back <- randomPoints(back_raster, 1*numberofpresences) %>% as_tibble()
coordinates(back) <- ~ x + y
back_swd  <- raster::extract(stk, back) %>% 
  cbind(coordinates(back), .)

dir.create('../tbl/rf/run_1', recursive = TRUE)
write.csv(back_swd, '../tbl/points/run_1/back_swd.csv', row.names = FALSE)

# Cluster analysis to pseudoabsences
bckclust <- rf.clust(occ = back_swd, nforest = 50, ntrees = 500, nVars = 8, nclasses = 2)
datRF <- as.data.frame(back_swd[,3:ncol(back_swd)])
attach(datRF)
no.forests <- 50#raw = 25
no.trees <- 500
distRF <- RFdist(datRF, mtry1 = 8, no.trees, no.forests, addcl1 = T, addcl2 = F, imp =T, oob.prox1 = T)# mtry1 = 4 raw  # es la cantidad de variables a utilizar en cada no
no.absenceclasses <- 2
labelRF <- pamNew(distRF$cl1, no.absenceclasses)
detach(datRF)
classdata <- cbind(pb = as.factor(labelRF), back_swd[,3:ncol(back_swd)])


presvalue_swd <- clusteredpresdata[,5:ncol(clusteredpresdata)] %>%
  cbind(pb = (clusteredpresdata$gid + no.absenceclasses), .) %>%
  as_tibble() %>% 
  na.omit() %>%
  as.data.frame() %>%
  as_tibble() 
# presvalue_swd <- dplyr::select(presvalue_swd, pb)
presvalue_swd <- mutate(presvalue_swd, pb = as.factor(pb))
classdata_2 <- cbind(pb = as.data.frame(classdata)$pb, classdata[,2:ncol(classdata)]) # Background

presvalue_swd %>% filter(pb == 3)

dim(classdata_2); dim(presvalue_swd)
# presvalue_swd <- presvalue_swd %>% dplyr::select(-cluster)

allclasses_swd <- rbind(classdata_2, presvalue_swd[,1:ncol(classdata_2)])
unique(allclasses_swd$pb)
as_tibble(allclasses_swd)
write.csv(allclasses_swd, '../tbl/rf/run_1/all_classes_swd.csv', row.names = FALSE)

# To make the random forest analysis --------------------------------------
vrs <- names(stk)
model1 <- as.formula(paste('factor(pb) ~', paste(paste(vrs), collapse = '+', sep =' ')))
rflist <- vector('list', 50) 
auc <- vector('list', 50)

library(pROC)

for(repe in 1:50){ # 50 bosques
  
  print(repe)
  pressample <- list()
  
  for (i in 1:(NumberOfClusters+no.absenceclasses)){
    
    print('Start for')
    
    if(any(i==c(1:no.absenceclasses))) { 
      
      print('Any')
      rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), 
                     size = samplesize*NumberOfClusters/2/no.absenceclasses)
    } else {
      
      rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), size = samplesize)
      
    }
    pressample[[i]] <- allclasses_swd[rows,] 
  }
  
  species <- na.omit(do.call(rbind, pressample)) 
  head(species)
  Samplesplit <- sample(rownames(species)) 
  
  envtrain <- species[Samplesplit[1:(0.8*nrow(species))],] 
  envtest <- species[Samplesplit[(0.8*nrow(species)):nrow(species)],] 
  
  rfmodel <- randomForest(model1, data = envtrain, ntree = 500, na.action = na.omit, nodesize = 2) 
  
  # dir.create('../rf/output/run1/models', recursive = TRUE)
  save(rfmodel, file = paste('../rf/output/run1/models/', NumberOfClusters, 'Prob_' , 'rep_' ,repe, '.rdata' ,sep=''))
  rflist[[repe]] <- rfmodel
  
  # AUC 
  predicted <- as.numeric(predict(rfmodel, envtest))
  observed <- as.vector(envtest[,'pb'])
  auc[[repe]] <- auc(observed, predicted) 
  rm(rfmodel)
  
  cat(auc[[repe]] ,'\n')
  
}

auc <- unlist(auc)
mean(auc)
rff <- do.call(randomForest::combine, rflist)
importance <- as.data.frame(rff$importance)

run <- 'run_1'
save(rflist, file = paste('../rData/', run, '/rflist_', NumberOfClusters, '.rdata', sep = ''))
save(importance, file = paste0('../rData/', run, '/importanceRF.rData'))
save(auc, file = paste0('../rData/', run, '/aucRF_dist.rData'))
save(rff, file = paste0('../rData/', run, '/rff_dist.rData'))

# Predict modell
lyr <- stk
climatevalues  <- data.frame(getValues(lyr))
# NumberOfClusters <- 15

rasterProbs <- predict(rff, climatevalues, type = 'prob') # proximity = T
rasterProbs_na <- na.omit(rasterProbs)
sum_rasterProbs_na <- apply(rasterProbs_na, 1, sum)

rasterRF <- rowSums(rasterProbs[,c(3:(NumberOfClusters+2))])
uncertainty <- apply(rasterProbs, 1, max)  

rasterRFprob <- lyr[[1]]
values(rasterRFprob) <- rasterRF 

rasterRFuncertainty <- lyr[[1]]
values(rasterRFuncertainty) <- uncertainty 

rasterRF <- max.col(rasterProbs, 'first')
rasterRFclass <- lyr[[1]]
values(rasterRFclass) <- rasterRF

writeRaster(rasterRFclass, paste0('../_rf/_output/_run1/_results/_raw/RF_5Clust_current.asc'), format = 'ascii', overwrite = T)
writeRaster(rasterRFprob, paste0('../_rf/_output/_run1/_results/_raw/RF_5Prob_current.asc'), format = 'ascii', overwrite = T)
writeRaster(rasterRFuncertainty, paste0('../_rf/_output/_run1/_results/_raw/RF_5Unc_current.asc'), format = 'ascii', overwrite = T)






