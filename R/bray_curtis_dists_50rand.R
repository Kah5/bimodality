# script for calculating bray curtis dist between each grid cell in the bimodal space and 50 rand samples 
# run on crc:
library(ggplot2)
library(vegan)
library(cluster)
library(reshape2)



#full <- read.csv("outputs/cluster/full_comp_dens_df.csv")
full <- read.csv("outputs/cluster/full_comp_dens_df.csv")
#full <- read.csv("data/full_comp_dens_df.csv")
pls.full <- full[full$period %in% "PLS",]
pls.full <- pls.full[!duplicated(pls.full$cell),]
fia.full <- full[full$period %in% "FIA",]
fia.full <- fia.full[!duplicated(fia.full$cell),]

newdf <- pls.full[,2:44]
newdf$PC1 <- pls.full$PC1
newdf$PC1bins <- pls.full$PC1bins
newdf <- newdf[complete.cases(newdf[,7:44]) & ! is.na(newdf$cell), ]
#newdf$randcell <- NA
newdf$bray_dist <- NA

#newdf <- newdf[newdf$PC1 >= -2 & newdf$PC1 <= 1.0,]
brays <- matrix(nrow = length(newdf$cell), ncol = 50)
test.dist <- vegdist(newdf[complete.cases(newdf[,7:43]),7:43], method = "bray")
dist.matrix <- as.matrix(test.dist)
row.names(dist.matrix) <- newdf[complete.cases(newdf[,7:43]),]$cell
saveRDS(dist.matrix , "outputs/pls_bray_curtis_dist_matrix.rds")

# start with the first grid cell, then find a random grid cell in the same envt:
for (i in 1:length(newdf$cell)){ 
#for(i in 1:100){
  gridcell <- newdf[i,]
  grid1 <- newdf[i,]$cell
  
  #bin <- newdf[newdf$cell %in% grid1,]$PC1bins
  # find a random grid cell within the same envt
  bin <- range(newdf[newdf$cell %in% grid1,]$PC1 - 0.15, newdf[newdf$cell %in% grid1,]$PC1 + 0.15) 
  
  # find a random grid cell within the same envt
  subset <- newdf[newdf$PC1 >= bin[1] & newdf$PC1 <= bin[2] & !newdf$cell %in% grid1, ]
  #subset<- newdf[ ! newdf$cell %in% grid1, ]
  randcell <- subset[sample(x = nrow(subset) , size = 50, replace = TRUE),]
  
  diffs <- randcell
  difmean <- randcell[1,]
  cells <- as.character(randcell$cell)
  
  length(dist.matrix[ cells,i])
  
  brays[i,1:50] <- as.numeric(dist.matrix[cells, i])[0:50]
  # calculate the diffs between the average grid cell
  
  #randmeans<- colMeans(randcell[,7:43], na.rm =TRUE)
  #difmean[ 7:43] <- randmeans - gridcell[,7:43]
  
  
  # calculate the absolute difference of all the diffs
  #for(j in 1:nrow(randcell)){
   # brays[i,j] <- rowSums(data.frame(lapply((randcell[j,7:43] - gridcell[,7:43]), abs)), na.rm=TRUE)/rowSums(data.frame(randcell[j,7:43] + gridcell[,7:43]), na.rm=TRUE)
  #}
  # function ot calculate the absolute difference of all the difs
  #getdiff <- function (x){ rowSums(data.frame(lapply((x -  gridcell[,7:43]), abs)), na.rm=TRUE)/rowSums(data.frame(x +  gridcell[,7:43]), na.rm=TRUE)}
  
  #brays[i,] <- apply(X = randcell[,7:43],1, getdiff)
  # find the mean difference
  #newdf[i, 6:42] <- data.frame( lapply(difmean[7:43], abs) )
  #newdf[i,]$randcell <- randcell$cell
  #newdf[i,]$bimodal <- as.factor(gridcell$biboth)
  
}

brays <- data.frame(brays)
brays$cell <- newdf$cell
brays$x <- newdf$x
brays$y <- newdf$y

write.csv(newdf, "data/outputs/newdf_bray_curtis50.csv")
write.csv(brays , "data/outputs/brays_bray_curtis50.csv")


head(brays)

#-------------------------- do the same for FIA
fia.full <- full[full$period %in% "FIA",]
fia.full <- fia.full[!duplicated(fia.full$cell),]

newdff <- fia.full[,2:44]
newdff$PC1 <- fia.full$PC1
newdff$PC1bins <- fia.full$PC1bins
newdff <- newdff[complete.cases(newdff[,7:44]) & ! is.na(newdff$cell), ]
#newdf$randcell <- NA
newdf$bray_dist <- NA

#newdf <- newdf[newdf$PC1 >= -2 & newdf$PC1 <= 1.0,]
brays.f <- matrix(nrow = length(newdff$cell), ncol = 50)
test.dist <- vegdist(newdff[complete.cases(newdff[,7:43]),7:43], method = "bray")
dist.matrix.f <- as.matrix(test.dist)
row.names(dist.matrix.f) <- newdff[complete.cases(newdff[,7:43]),]$cell
saveRDS(dist.matrix.f , "outputs/fia_bray_curtis_dist_matrix.rds")

# start with the first grid cell, then find a random grid cell in the same envt:
for (i in 1:length(newdff$cell)){ 
  
  gridcell <- newdff[i,]
  grid1 <- newdff[i,]$cell
  
  #bin <- newdf[newdf$cell %in% grid1,]$PC1bins
  # find a random grid cell within the same envt
  bin <- range(newdff[newdff$cell %in% grid1,]$PC1 - 0.15, newdff[newdff$cell %in% grid1,]$PC1 + 0.15) 
  
  # find a random grid cell within the same envt
  subset <- newdff[newdff$PC1 >= bin[1] & newdff$PC1 <= bin[2] & !newdff$cell %in% grid1, ]
  #subset<- newdf[ ! newdf$cell %in% grid1, ]
  randcell <- subset[sample(x = nrow(subset) , size = 50, replace = TRUE),]
  

  diffs <- randcell
  difmean <- randcell[1,]
  cells <- as.character(randcell$cell)
  
  length(dist.matrix.f[ cells,i])
  
  brays.f[i,1:50] <- as.numeric(dist.matrix.f[cells, i])[0:50]
  
  # calculate the absolute difference of all the diffs
  #for(j in 1:nrow(randcell)){
  # brays[i,j] <- rowSums(data.frame(lapply((randcell[j,7:43] - gridcell[,7:43]), abs)), na.rm=TRUE)/rowSums(data.frame(randcell[j,7:43] + gridcell[,7:43]), na.rm=TRUE)
  #}
  # function ot calculate the absolute difference of all the difs
  #getdiff <- function (x){ rowSums(data.frame(lapply((x -  gridcell[,7:43]), abs)), na.rm=TRUE)/rowSums(data.frame(x +  gridcell[,7:43]), na.rm=TRUE)}
  
#  brays[i,] <- apply(X = randcell[,7:43],1, getdiff)
  # find the mean difference
 # newdf[i, 6:42] <- data.frame( lapply(difmean[7:43], abs) )
  #newdf[i,]$randcell <- randcell$cell
  #newdf[i,]$bimodal <- as.factor(gridcell$biboth)
  
}

brays.f <- data.frame(brays.f)
brays.f$cell <- newdff$cell
brays.f$x <- newdff$x
brays.f$y <- newdff$y

write.csv(newdff, "data/outputs/newdf_fia.csv")
write.csv(brays.f , "data/outputs/brays_bimodal_fia.csv")

#newdf <- read.csv("outputs/newdf.csv")
#brays <- read.csv("outputs/brays_bimodal.csv")


clust_plot6 <- read.csv('data/density_pls_with_clusters.csv')
#clust_plot6 <- read.csv('outputs/cluster/density_pls_with_clusters.csv')

brays.2 <- merge(brays, clust_plot6[,c("x", "y", "cell", "speciescluster")])
brays.m <- reshape2::melt(brays.2, id.vars = c("cell", "x", "y","X", "speciescluster"))

png(height = 6, width = 12,units = "in",res=300,"data/outputs/bray_curtis_dist_rand_50_hists_spec_fia.png")
ggplot(brays.m, aes(value, fill = speciescluster)) +geom_histogram()+
  scale_fill_manual(values = c('#beaed4','#ffff99','#386cb0', '#f0027f','#fdc086','#7fc97f'))+facet_wrap(~speciescluster)
dev.off()

png(height = 6, width = 6,units = "in",res=300,"data/outputs/bray_curtis_dist_rand_50_hists_fia.png")
ggplot(brays.m, aes(value)) +geom_histogram()
dev.off()