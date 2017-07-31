

full <- read.csv("full_comp_dens_df.csv")

pls.full <- full[full$period %in% "PLS",]

newdf <- pls.full[,2:43]
newdf$randcell <- NA
newdf$bimodal <- NA
newdf$PC1 <- pls.full$PC1
newdf$PC1bins <- pls.full$PC1bins
brays <- matrix(nrow = length(pls.full$cell), ncol = 50)

# start with the first grid cell, then find a random grid cell in the same envt:
for (i in 1:length(pls.full$cell)){ 
  
  gridcell <- pls.full[i,]
  grid1 <- pls.full[i,]$cell
  
  bin <- pls.full[pls.full$cell %in% grid1,]$PC1bins
  # find a random grid cell within the same envt
  subset<- pls.full[pls.full$PC1bins %in% bin & ! pls.full$cell %in% grid1, ]
  randcell <- subset[sample(x = nrow(subset) , size = 50, replace = TRUE),]
  
  diffs <- randcell
  difmean <- randcell[1,]
  # calculate the diffs between the average grid cell
  
  randmeans<- colMeans(randcell[,7:43], na.rm =TRUE)
  difmean[ 7:43] <- randmeans - gridcell[,7:43]
  
  
  # calculate the absolute difference of all the diffs
  for(j in 1:nrow(randcell)){
    brays[i,j] <- rowSums(data.frame(lapply((randcell[j,7:43] - gridcell[,7:43]), abs)), na.rm=TRUE)/rowSums(data.frame(randcell[j,7:43] + gridcell[,7:43]), na.rm=TRUE)
  }
  
  # find the mean difference
  newdf[i, 6:42] <- data.frame( lapply(difmean[7:43], abs) )
  #newdf[i,]$randcell <- randcell$cell
  newdf[i,]$bimodal <- as.factor(gridcell$biboth)
  cat(i)
  
}

write.csv(brays, "brays_full.csv")
write.csv(newdf, "newdf.csv")
