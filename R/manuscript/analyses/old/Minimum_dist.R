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
newdf$minbray <- NA
#newdf <- newdf[newdf$PC1 >= -2 & newdf$PC1 <= 1.0,]
brays <- matrix(nrow = length(newdf$cell), ncol = 1)

test.dist <- vegdist(newdf[complete.cases(newdf[,7:43]),7:43], method = "bray")
dist.matrix <- as.matrix(test.dist)


# start with the first grid cell, then find a random grid cell in the same envt:
for (i in 1:length(newdf[complete.cases(newdf[,7:43]),7:43]$Alder)){ 
  #for(i in 1: 10){ 
  gridcell <- newdf[i,]
  grid1 <- newdf[i,]$cell
  
  #bin <- range(newdf[newdf$cell %in% grid1,]$PC1 - 0.15, newdf[newdf$cell %in% grid1,]$PC1 + 0.15) 
  
  # find a random grid cell within the same envt
  #subset<- newdf[newdf$PC1 >= bin[1] & newdf$PC1 <= bin[2] & !newdf$cell %in% grid1, ]
  #randcell <- subset[sample(x = nrow(subset) , size = 1, replace = TRUE),]
  
  min_b <- min(dist.matrix[-i,i])
  max_b <- max(dist.matrix[-i,i])
  # find the mean difference
  newdf[i,]$minbray <- min_b
  #newdf[i,]$min <- randcell$cell
  
  
}

ggplot(newdf, aes(x = x, y=y, fill = minbray))+geom_raster()
ggplot(newdf, aes(minbray))+geom_histogram()



write.csv(newdf, "data/outputs/PLS_minimum_brays.csv")


newdff <- fia.full[,2:44]

newdff$randcell <- NA
newdff$bimodal <- NA
newdff$PC1 <- fia.full$PC1
newdff$PC1bins <- fia.full$PC1bins
newdff$minbray <- NA
#newdf <- newdf[newdf$PC1 >= -2 & newdf$PC1 <= 1.0,]
#brays <- matrix(nrow = length(newdf$cell), ncol = 1)
dist.f <- vegdist(newdff[complete.cases(newdff[,7:43]),7:43], method = "bray")
dist.matrix.f <- as.matrix(dist.f)

for (i in 1:length(newdff[complete.cases(newdff[,7:43]),7:43]$Alder)){ 
  #for(i in 1: 10){ 
  gridcell <- newdff[i,]
  grid1 <- newdff[i,]$cell
  
  #bin <- range(newdf[newdf$cell %in% grid1,]$PC1 - 0.15, newdf[newdf$cell %in% grid1,]$PC1 + 0.15) 
  
  # find a random grid cell within the same envt
  #subset<- newdf[newdf$PC1 >= bin[1] & newdf$PC1 <= bin[2] & !newdf$cell %in% grid1, ]
  #randcell <- subset[sample(x = nrow(subset) , size = 1, replace = TRUE),]
  
  min_b <- min(dist.matrix.f[-i,i])
  max_b <- max(dist.matrix.f[-i,i])
  # find the mean difference
  newdff[i,]$minbray <- min_b
  #newdf[i,]$min <- randcell$cell
  
  
}

ggplot(newdff, aes(x = x, y=y, fill = minbray))+geom_raster()
ggplot(newdff, aes(minbray))+geom_histogram()

newdff$period <- "FIA"
newdf$period <- "PLS"

full.min.brays <- rbind(newdff[,c("x", "y", "cell", "period", "minbray")], newdf[,c("x", "y", "cell", "period", "minbray")])

ggplot(full.min.brays, aes(x = x, y=y, fill = minbray))+geom_raster()+coord_equal()+facet_wrap(~period)

# plot histograms of minimum bray curtis distances:
png(height = 6, width = 6,units = "in",res=300,"data/outputs/min_bray_curtis_dist_fia_pls_hist.png")
ggplot()+geom_histogram(data = full.min.brays, aes(minbray, fill = period, alpha = 0.5),position = "identity")
dev.off()

#---------------get bray curtis distances of all the grid cells within the same climate space--------------------
full <- read.csv("outputs/cluster/full_comp_dens_df.csv")
#full <- read.csv("data/full_comp_dens_df.csv")
pls.full <- full[full$period %in% "PLS",]
fia.full <- full[full$period %in% "FIA",]

newdf <- pls.full[,2:44]
newdf <- newdf[complete.cases(newdf[,7:43]), ] # only take non-na values for veg. composition
newdf$randcell <- NA
newdf$bimodal <- NA
newdf$PC1 <- pls.full$PC1
newdf$PC1bins <- pls.full$PC1bins
newdf$minbray <- NA
newdf$maxbray <- NA
newdf$difbray <- NA
#newdf <- newdf[newdf$PC1 >= -2 & newdf$PC1 <= 1.0,]
brays <- matrix(nrow = length(newdf$cell), ncol = 1)

test.dist <- vegdist(newdf[complete.cases(newdf[,7:43]),7:43], method = "bray")
dist.matrix <- as.matrix(test.dist)
row.names(dist.matrix) <- newdf$cell

# start with the first grid cell, then find a random grid cell in the same envt:
for (i in 1:length(newdf[complete.cases(newdf[,7:43]),7:43]$Alder))
  #for (i in 1:100)
    { 
  
  gridcell <- newdf[i,]
  grid1 <- newdf[i,]$cell
  
  bin <- range(newdf[newdf$cell %in% grid1,]$PC1 - 0.15, newdf[newdf$cell %in% grid1,]$PC1 + 0.15) 
  
  # find a random grid cell within the same envt
  subset <- newdf[newdf$PC1 >= bin[1] & newdf$PC1 <= bin[2] & !newdf$cell %in% grid1, ]
  #randcell <- subset[sample(x = nrow(subset) , size = 1, replace = TRUE),]
  cells <- as.character(na.omit(subset$cell))
  
  min_b <- min(dist.matrix[row.names(dist.matrix) %in% cells, i])
  max_b <- max(dist.matrix[row.names(dist.matrix) %in% cells,i])
  # find the min difference
  newdf[i,]$minbray <- min_b
  newdf[i,]$maxbray <- max_b
  newdf[i,]$difbray <- max_b - min_b
  #newdf[i,]$min <- randcell$cell
  
  
}

ggplot(newdf, aes(x = x, y=y, fill = minbray))+geom_raster()
ggplot(newdf, aes(difbray))+geom_histogram()

#--------------------- do the same thing for FIA data:


fia.full <- full[full$period %in% "FIA",]

newdff <- fia.full[,2:44]
newdff <- newdff[complete.cases(newdff[,7:43]), ] # only take non-na values for veg. composition
newdff$randcell <- NA
newdff$bimodal <- NA
newdff$PC1 <- fia.full$PC1
newdff$PC1bins <- fia.full$PC1bins
newdff$minbray <- NA
newdff$maxbray <- NA
newdff$difbray <- NA
#newdf <- newdf[newdf$PC1 >= -2 & newdf$PC1 <= 1.0,]
brays <- matrix(nrow = length(newdff$cell), ncol = 1)

test.dist <- vegdist(newdff[complete.cases(newdff[,7:43]),7:43], method = "bray")
dist.matrix.f <- as.matrix(test.dist)
row.names(dist.matrix.f) <- newdff$cell

# start with the first grid cell, then find a random grid cell in the same envt:
for (i in 1:length(newdff[complete.cases(newdff[,7:43]),7:43]$Alder))
  #for (i in 1:100)
{ 
  
  gridcell <- newdff[i,]
  grid1 <- newdff[i,]$cell
  
  bin <- range(newdff[newdff$cell %in% grid1,]$PC1 - 0.15, newdff[newdff$cell %in% grid1,]$PC1 + 0.15) 
  
  # find a random grid cell within the same envt
  subset <- newdff[newdff$PC1 >= bin[1] & newdff$PC1 <= bin[2] & !newdff$cell %in% grid1, ]
  #randcell <- subset[sample(x = nrow(subset) , size = 1, replace = TRUE),]
  cells <- as.character(na.omit(subset$cell))
  
  min_b <- min(dist.matrix[row.names(dist.matrix) %in% cells, i])
  max_b <- max(dist.matrix[row.names(dist.matrix) %in% cells,i])
  # find the min difference
  newdff[i,]$minbray <- min_b
  newdff[i,]$maxbray <- max_b
  newdff[i,]$difbray <- max_b - min_b
  #newdf[i,]$min <- randcell$cell
  
  
}

ggplot(newdff, aes(x = x, y=y, fill = minbray))+geom_raster()
ggplot(newdff, aes(difbray))+geom_histogram()
ggplot(newdff, aes(minbray))+geom_histogram()


newdff$period <- "FIA"
newdf$period <- "PLS"

full.min.brays <- rbind(newdff[,c("x", "y", "cell", "period", "minbray", "difbray","PC1bins", "PC1")], newdf[,c("x", "y", "cell", "period", "minbray", "difbray", "PC1bins", "PC1")])

ggplot(full.min.brays[full.min.brays$minbray <= 0.2,], aes(x = x, y=y, fill = minbray))+geom_raster()+coord_equal()+facet_wrap(~period)

# plot histograms of minimum bray curtis distances:
png(height = 6, width = 6,units = "in",res=300,"data/outputs/min_bray_curtis_dist_in_group_fia_pls_hist.png")
ggplot()+geom_histogram(data = full.min.brays, aes(minbray, fill = period, alpha = 0.5),position = "identity")
dev.off()

ggplot()+geom_histogram(data = full.min.brays, aes(minbray, fill = period, alpha = 0.5),position = "identity")+facet_wrap(~PC1bins)

ggplot()+geom_histogram(data = full.min.brays, aes(difbray, fill = period, alpha = 0.5),position = "identity")+facet_wrap(~PC1bins)


ggplot(data = full.min.brays, aes(PC1, minbray)) +geom_hex() + 
  theme_bw(base_size = 10)+scale_fill_distiller(palette = "Spectral", limits = c(1,300))+facet_wrap(~period)
