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
brays <- matrix(nrow = length(newdf[complete.cases(newdf[,7:44]),7:43]$cell), ncol = 1)
test.dist <- vegdist(newdf[complete.cases(newdf[,7:43]),7:43], method = "bray")
dist.matrix <- as.matrix(test.dist)
row.names(dist.matrix) <- newdf[complete.cases(newdf[,7:43]),]$cell

# start with the first grid cell, then find a random grid cell in the same envt:
for (i in 1:length(newdf$cell)){ 
#for(i in 1: 10){ 
  gridcell <- newdf[i,]
  grid1 <- newdf[i,]$cell
  
  bin <- range(newdf[newdf$cell %in% grid1,]$PC1 - 0.15, newdf[newdf$cell %in% grid1,]$PC1 + 0.15) 
  
  # find a random grid cell within the same envt
  subset<- newdf[newdf$PC1 >= bin[1] & newdf$PC1 <= bin[2] & !newdf$cell %in% grid1, ]
  randcell <- subset[sample(x = nrow(subset) , size = 1, replace = TRUE),]
  
  cells <- as.character(randcell$cell)

  newdf[i,]$bray_dist <- dist.matrix[row.names(dist.matrix) %in% cells, i]
  
}

#brays <- data.frame(brays)
#brays$cell <- newdf$cell
#brays$x <- newdf$x
#brays$y <- newdf$y

ggplot(data = newdf, aes(bray_dist))+geom_histogram()+facet_wrap(~PC1bins)
ggplot(data = newdf, aes(x,y, fill = bray_dist))+geom_raster()

write.csv(newdf, "data/outputs/newdf_1rand.csv")
#write.csv(brays , "data/outputs/brays_bimodal_1rand.csv")

# for fia 1 random sampling:

newdff <- fia.full[,2:44]


newdff$PC1 <- fia.full$PC1
newdff$PC1bins <- fia.full$PC1bins
#newdff$randcell <- NA
#newdff$bimodal <- NA
newdff$bray_dist <- NA

newdff <- newdff[complete.cases(newdff[,7:44]) & ! is.na(newdff$cell), ]


#newdf <- newdf[newdf$PC1 >= -2 & newdf$PC1 <= 1.0,]
brays <- matrix(nrow = length(newdff[complete.cases(newdff[,7:43]),7:43]$cell), ncol = 1)
test.dist.f <- vegdist(newdff[complete.cases(newdff[,7:44]),7:43], method = "bray")
dist.matrix.f <- as.matrix(test.dist.f)
row.names(dist.matrix.f) <- newdff[complete.cases(newdff[,7:44]),]$cell

# start with the first grid cell, then find a random grid cell in the same envt:
for (i in 1:length(newdff$cell)){ 
 # for(i in 1: 10){ 
  gridcell <- newdff[i,]
  grid1 <- newdff[i,]$cell
  
  bin <- range(newdff[newdff$cell %in% grid1,]$PC1 - 0.15, newdff[newdff$cell %in% grid1,]$PC1 + 0.15) 
  
  # find a random grid cell within the same envt
  subset <- newdff[newdff$PC1 >= bin[1] & newdff$PC1 <= bin[2] & !newdff$cell %in% grid1, ]
  randcell <- subset[sample(x = nrow(subset) , size = 1, replace = TRUE),]
  
  cells <- as.character(randcell$cell)
  
  newdff[i,]$bray_dist <- ifelse(nrow(subset) == 0,NA, dist.matrix.f[row.names(dist.matrix.f) %in% cells, i])
  
}



ggplot(data = newdff, aes(bray_dist))+geom_histogram()+facet_wrap(~PC1bins)
ggplot(data = newdff, aes(x,y, fill = bray_dist))+geom_raster()

# merge pls and fia bray estimates together:
brays <- rbind(newdf[,c("x", "y", "cell","period", "bray_dist", "PC1", "PC1bins")], newdff[,c("x", "y", "cell","period", "bray_dist", "PC1", "PC1bins")])


# ----------------------------How does species composition distances vary by the species comp. cluster?-----------
#clust_plot6 <- read.csv('data/density_pls_with_clusters.csv')
clust_plot6 <- read.csv('outputs/cluster/density_pls_with_clusters.csv')

brays.2 <- merge(brays, clust_plot6[,c("x", "y", "cell", "speciescluster")])
brays.m <- melt(brays.2, id.vars = c("cell", "x", "y", "speciescluster", "period", "PC1", "PC1bins"))

png(height = 6, width = 12,units = "in",res=300,"data/outputs/bray_curtis_dist_rand_1_hists_spec_pls.png")
ggplot(brays.m[brays.m$period %in% "PLS",], aes(value, fill = speciescluster)) +geom_histogram()+
  scale_fill_manual(values = c('#beaed4','#ffff99','#386cb0', '#f0027f','#fdc086','#7fc97f'))+facet_wrap(~speciescluster)
dev.off()

png(height = 6, width = 6,units = "in",res=300,"data/outputs/bray_curtis_dist_rand_1_hists_pls.png")
ggplot(brays.m[brays.m$period %in% "PLS",], aes(value)) +geom_histogram()
dev.off()


png(height = 6, width = 12,units = "in",res=300,"data/outputs/bray_curtis_dist_rand_1_hists_spec_fia.png")
ggplot(brays.m[brays.m$period %in% "FIA",], aes(value, fill = speciescluster)) +geom_histogram()+
  scale_fill_manual(values = c('#beaed4','#ffff99','#386cb0', '#f0027f','#fdc086','#7fc97f'))+facet_wrap(~speciescluster)
dev.off()

png(height = 6, width = 6,units = "in",res=300,"data/outputs/bray_curtis_dist_rand_1_hists_fia.png")
ggplot() +geom_histogram(data = brays.m[brays.m$period %in% "FIA",], aes(value), alpha = 0.5)#+geom_histogram(data = brays.m, aes(value), fill = "red", alpha = 0.5)
dev.off()



png(height = 6, width = 6,units = "in",res=300,"data/outputs/bray_curtis_dist_rand_1_hists_fia_pls.png")
ggplot() +geom_histogram(data = brays.m, aes(value, fill = period), alpha = 0.5 , position = "identity")
dev.off()

png(height = 6, width = 12,units = "in",res=300,"data/outputs/bray_curtis_dist_rand_1_hists_by_PC1_fia_pls.png")
ggplot() +geom_histogram(data = brays.m, aes(value, fill = period), alpha = 0.5 , position = "identity")+facet_wrap(~PC1bins)
dev.off()

png(height = 6, width = 12,units = "in",res=300,"data/outputs/bray_curtis_dist_rand_1_hists_spec_fia_pls.png")
ggplot() +geom_histogram(data = brays.m, aes(value, fill = period), alpha = 0.5 , position = "identity")+facet_wrap(~speciescluster)
dev.off()
