# script for calculating bray curtis dist between each grid cell in the bimodal space and 50 rand samples 
# run on crc:
library(ggplot2)
library(vegan)
library(cluster)
library(reshape2)
library(reshape)


full <- read.csv("outputs/cluster/full_comp_dens_df.csv")
#full <- read.csv("data/full_comp_dens_df.csv")
pls.full <- full[full$period %in% "PLS",]
fia.full <- full[full$period %in% "FIA",]

newdf <- pls.full[,2:44]

newdf$randcell <- NA
newdf$bimodal <- NA
newdf$PC1 <- pls.full$PC1
newdf$PC1bins <- pls.full$PC1bins
newdf <- newdf[newdf$PC1 >= -2 & newdf$PC1 <= 1.0,]
brays <- matrix(nrow = length(newdf$cell), ncol = 1)

# start with the first grid cell, then find a random grid cell in the same envt:
for (i in 1:length(newdf$cell)){ 
#for(i in 1: 10){ 
  gridcell <- newdf[i,]
  grid1 <- newdf[i,]$cell
  
  bin <- range(newdf[newdf$cell %in% grid1,]$PC1 - 0.15, newdf[newdf$cell %in% grid1,]$PC1 + 0.15) 
  
  # find a random grid cell within the same envt
  subset<- newdf[newdf$PC1 >= bin[1] & newdf$PC1 <= bin[2] & !newdf$cell %in% grid1, ]
  randcell <- subset[sample(x = nrow(subset) , size = 1, replace = TRUE),]
  
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
  #newdf[i,]$bimodal <- as.factor(gridcell$biboth)
  
}

brays <- data.frame(brays)
brays$cell <- newdf$cell
brays$x <- newdf$x
brays$y <- newdf$y

write.csv(newdf, "data/outputs/newdf_1rand.csv")
write.csv(brays , "data/outputs/brays_bimodal_1rand.csv")

# for fia 1 random sampling:

newdff <- fia.full[,2:44]

newdff$randcell <- NA
newdff$bimodal <- NA
newdff$PC1 <- fia.full$PC1
newdff$PC1bins <- fia.full$PC1bins
newdf <- newdff[newdff$PC1 >= -2 & newdff$PC1 <= 1.0,]
braysf <- matrix(nrow = length(newdff$cell), ncol = 1)

# start with the first grid cell, then find a random grid cell in the same envt:
for (i in 1:length(newdff$cell)){ 
  #for(i in 1: 10){ 
  gridcell <- newdff[i,]
  grid1 <- newdff[i,]$cell
  
  bin <- range(newdff[newdff$cell %in% grid1,]$PC1 - 0.15, newdff[newdff$cell %in% grid1,]$PC1 + 0.15) 
  
  # find a random grid cell within the same envt
  subset<- newdff[newdff$PC1 >= bin[1] & newdff$PC1 <= bin[2] & !newdff$cell %in% grid1, ]
  randcell <- subset[sample(x = nrow(subset) , size = 1, replace = TRUE),]
  
  diffs <- randcell
  difmean <- randcell[1,]
  # calculate the diffs between the average grid cell
  
  randmeans<- colMeans(randcell[,7:43], na.rm =TRUE)
  difmean[ 7:43] <- randmeans - gridcell[,7:43]
  
  
  # calculate the absolute difference of all the diffs
  for(j in 1:nrow(randcell)){
    braysf[i,j] <- rowSums(data.frame(lapply((randcell[j,7:43] - gridcell[,7:43]), abs)), na.rm=TRUE)/rowSums(data.frame(randcell[j,7:43] + gridcell[,7:43]), na.rm=TRUE)
  }
  
  # find the mean difference
  newdff[i, 6:42] <- data.frame( lapply(difmean[7:43], abs) )
  #newdf[i,]$randcell <- randcell$cell
  #newdf[i,]$bimodal <- as.factor(gridcell$biboth)
  
}

braysf <- data.frame(braysf)
braysf$cell <- newdff$cell
braysf$x <- newdff$x
braysf$y <- newdff$y

#newdf <- read.csv("newdf.csv")
#brays <- read.csv("brays_bimodal.csv")


#clust_plot6 <- read.csv('data/density_pls_with_clusters.csv')
clust_plot6 <- read.csv('outputs/cluster/density_pls_with_clusters.csv')

brays.2 <- merge(brays, clust_plot6[,c("x", "y", "cell", "speciescluster")])
brays.m <- melt(brays.2, id.vars = c("cell", "x", "y", "speciescluster"))

png(height = 6, width = 12,units = "in",res=300,"data/outputs/bray_curtis_dist_rand_1_hists_spec_pls.png")
ggplot(brays.m, aes(value, fill = speciescluster)) +geom_histogram()+
  scale_fill_manual(values = c('#beaed4','#ffff99','#386cb0', '#f0027f','#fdc086','#7fc97f'))+facet_wrap(~speciescluster)
dev.off()

png(height = 6, width = 6,units = "in",res=300,"data/outputs/bray_curtis_dist_rand_1_hists_pls.png")
ggplot(brays.m, aes(value)) +geom_histogram()
dev.off()

braysf.2 <- merge(braysf, clust_plot6[,c("x", "y", "cell", "speciescluster")])
braysf.m <- melt(braysf.2, id.vars = c("cell", "x", "y", "speciescluster"))

png(height = 6, width = 12,units = "in",res=300,"data/outputs/bray_curtis_dist_rand_1_hists_spec_fia.png")
ggplot(braysf.m, aes(value, fill = speciescluster)) +geom_histogram()+
  scale_fill_manual(values = c('#beaed4','#ffff99','#386cb0', '#f0027f','#fdc086','#7fc97f'))+facet_wrap(~speciescluster)
dev.off()

png(height = 6, width = 6,units = "in",res=300,"data/outputs/bray_curtis_dist_rand_1_hists_fia.png")
ggplot() +geom_histogram(data = braysf.m, aes(value), alpha = 0.5)+geom_histogram(data = brays.m, aes(value), fill = "red", alpha = 0.5)
dev.off()

braysf.m$period <- "FIA"
brays.m$period <- "PLS"
full.brays <- rbind(braysf.m, brays.m)

png(height = 6, width = 6,units = "in",res=300,"data/outputs/bray_curtis_dist_rand_1_hists_fia_pls.png")
ggplot() +geom_histogram(data = full.brays, aes(value, fill = period), alpha = 0.5 , position = "identity")
dev.off()

png(height = 6, width = 12,units = "in",res=300,"data/outputs/bray_curtis_dist_rand_1_hists_spec_fia_pls.png")
ggplot() +geom_histogram(data = full.brays, aes(value, fill = period), alpha = 0.5 , position = "identity")+facet_wrap(~speciescluster)
dev.off()