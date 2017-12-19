library(ggplot2)
library(diptest)
library(grid)
library(gridExtra)
library(cowplot)
library(vegan)
library(cluster)
library(sp)
#library()

full <- read.csv("outputs/cluster/full_comp_dens_df.csv")

pls.full <- full[full$period %in% "PLS",]

# want to calculate the difference in spec. composiitions between each grid cell, and a random grid cell within the same envt bin:
# dataframe output desired: 
new <- pls.full[,2:43]
new$randcell <- NA
new$bimodal <- NA
new$PC1 <- pls.full$PC1
new$PC1bins <- pls.full$PC1bins

# start with the first grid cell, then find a random grid cell in the same envt:
for (i in 1:length(pls.full$cell)){ 
  
  gridcell <- pls.full[i,]
  grid1 <- pls.full[i,]$cell
  
  bin <- pls.full[pls.full$cell %in% grid1,]$PC1bins
  # find a random grid cell within the same envt
  subset <- pls.full[pls.full$PC1bins %in% bin & ! pls.full$cell %in% grid1, ]
  randcell <- subset[sample(x = nrow(subset) , size = 1, replace = TRUE),]
  
  diffs <- randcell
  brays <- data.frame(brays <- nrow(randcell))
  # calculate the diffs between the random grid cell and the grid cell
  
  
  diffs[,7:43] <- randcell[,7:43] - gridcell[,7:43]
  brays <- rowSums(data.frame(lapply((randcell[,7:43] - gridcell[,7:43]), abs)), na.rm=TRUE)/rowSums(data.frame(randcell[,7:43] + gridcell[,7:43]), na.rm=TRUE)
  # calculate the absolute difference of all the diffs
  
  # find the mean difference
  new[i, 6:42] <- data.frame( lapply(diffs[7:43], abs) )
  new[i,]$randcell <- randcell$cell
  new[i,]$bimodal <- as.factor(gridcell$biboth)
  
}


# below code does the same but it takes 50 random samples (with replacement)
# x, y, grid cell 1, grid cell 2, bimodality, spec compostion differences 1-29
newdf <- pls.full[,2:43]
newdf$randcell <- NA
newdf$bimodal <- NA
newdf$PC1 <- pls.full$PC1
newdf$PC1bins <- pls.full$PC1bins
brays <- matrix(nrow = length(pls.full$cell), ncol = 50)

# start with the first grid cell, then find a random grid cell in the same envt:
 for (i in length(pls.full$cell)){ 
   
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

#write.csv(newdf, "outputs/random_comp_differences.csv")

newdf <- read.csv("outputs/newdf.csv")
brays <- read.csv("outputs/brays_full.csv")


# ---------Visualize an ordination of these compositional differences-----------------
# now we want to look at the PCA of these to see if there is any structure
diffpca <- princomp(newdf[,7:43])

plot(diffpca)
biplot(diffpca)

scores <- diffpca$scores

# add scores to pls.full:
pls.full$pc1 <- scores[,1]
pls.full$pc2 <- scores[,2]


library(ggbiplot)
source("R/newggbiplot.R")

#png("outputs/cluster/pca_scree_plot.png")
ggbiplot(diffpca, pc.biplot = TRUE)+geom_point(data= pls.full, aes(x=pc1, y=pc2, color = biboth))
#dev.off()

g <- newggbiplot(diffpca, obs.scale = 1, var.scale = 1, labels.size
                 = 25,alpha = 0,color = "blue",  alpha_arrow = 1, line.size = 1.5, scale = TRUE)
g$layers <- c(geom_point(data = pls.full, aes(x = pc1, y = pc2, color = biboth)), g$layers)

png("outputs/cluster/composition_diff_unscaled_PCA.png")
g+theme(legend.position = "bottom")+guides(color=guide_legend(nrow=4,byrow=TRUE))
dev.off()


# ------------Do the comp differences cluster?---------------------------
# cacluate bray dists

# kmeans clustering on the diff values between each grid cell and a random grid cell in the same envt bin:
mydata <- new[,6:42]

# plot out the within group sum of squares
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

set.seed(7)
km2 = kmeans(mydata, 4, nstart=100)

# Examine the result of the clustering algorithm
km2

newdf$cluster6 <- km2$cluster

png("outputs/cluster/kmeans_diffs_four_single_rand.png")
ggplot(newdf, aes(x,y, fill = as.factor(cluster6)))+geom_raster()+coord_equal()+guides(fill=guide_legend(title="Cluster"))
dev.off()


#----------------doe selected more grid cells and averageing diffs make it less noisy?-------------
# kmeans clustering on the averaged values:
# make a mydata object that we will cluster on:
mydata <- newdf[,7:43]

# calculate sum of squares to make the elbow plots:
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)

png("outputs/cluster/ncluster_within_ssq.png")
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)
dev.off()

set.seed(7)
km2 = kmeans(mydata, 2, nstart=100)
km5 = kmeans(mydata, 5, nstart = 100)
km6 = kmeans(mydata, 6, nstart = 100)
km3 = kmeans(mydata, 3, nstart = 100)
km4 = kmeans(mydata, 4, nstart = 100)
km7 = kmeans(mydata, 7, nstart = 100)
km8 = kmeans(mydata, 8, nstart = 100)

# Examine the result of the clustering algorithm
km2

# add the cluster values to the "newdf"
newdf$cluster2 <- km2$cluster
newdf$cluster3 <- km3$cluster
newdf$cluster4 <- km4$cluster
newdf$cluster5 <- km5$cluster
newdf$cluster6 <- km6$cluster
newdf$cluster7 <- km7$cluster
newdf$cluster8 <- km8$cluster

#get map data for the Midwest:
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(states) <- ~long+lat
class(states)
proj4string(states) <- CRS("+proj=longlat +datum=NAD83")
mapdata <-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)



# map out the correlations:
png("outputs/cluster/kmeans_diffs_four.png")

a <- ggplot(newdf, aes(x,y, fill = as.factor(cluster4)))+geom_raster()+coord_equal()+guides(fill=guide_legend(title="Cluster"))
a
dev.off()


#X11(width = 12)
png("outputs/cluster/rand_sample_cluster_2.png")
b<- ggplot(newdf, aes(x,y, fill = as.factor(cluster2)))+geom_raster()+coord_equal()+guides(fill=guide_legend(title="Cluster"))+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
ggtitle("K = 2")+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.4,'lines'),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                       panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))

b
dev.off()

png("outputs/cluster/rand_sample_cluster_3.png")
c<- ggplot(newdf, aes(x,y, fill = as.factor(cluster3)))+geom_raster()+coord_equal()+guides(fill=guide_legend(title="Cluster"))+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
ggtitle("K = 3")+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.4,'lines'),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                       panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))

c
dev.off()

png("outputs/cluster/rand_sample_cluster_4.png")
d<- ggplot(newdf, aes(x,y, fill = as.factor(cluster4)))+geom_raster()+coord_equal()+guides(fill=guide_legend(title="Cluster"))+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
ggtitle("K = 4")+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.4,'lines'),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                       panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))

d
dev.off()

png("outputs/cluster/rand_sample_cluster_5.png")
e<- ggplot(newdf, aes(x,y, fill = as.factor(cluster5)))+geom_raster()+coord_equal()+guides(fill=guide_legend(title="Cluster"))+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
ggtitle("K = 5")+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.4,'lines'),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                       panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))

e
dev.off()

png("outputs/cluster/rand_sample_cluster_6.png")
f<- ggplot(newdf, aes(x,y, fill = as.factor(cluster6)))+geom_raster()+coord_equal()+guides(fill=guide_legend(title="Cluster"))+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
ggtitle("K = 6")+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.4,'lines'),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                       panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))

f
dev.off()

png("outputs/cluster/rand_sample_cluster_7.png")
g<- ggplot(newdf, aes(x,y, fill = as.factor(cluster7)))+geom_raster()+coord_equal()+guides(fill=guide_legend(title="Cluster"))+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
ggtitle("K = 7")+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.4,'lines'),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                       panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))
g
dev.off()

png("outputs/cluster/rand_sample_cluster_8.png")
h<- ggplot(newdf, aes(x,y, fill = as.factor(cluster8)))+geom_raster()+coord_equal()+guides(fill=guide_legend(title="Cluster"))+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
ggtitle("K = 8")+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.4,'lines'),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                       panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))
h
dev.off()

png(height = 8, width = 12, units = 'in', res = 300, "outputs/cluster/rand_sample_cluster_maps.png")
grid.arrange(b,c,d,e,f,g,h, ncol = 4)
dev.off()

ggplot(newdf, aes(x,y, fill = bimodal))+geom_raster()+coord_equal()

ggplot(newdf, aes(x,y, fill = Hemlock))+geom_raster()
ggplot(newdf, aes(x,y, fill = Poplar))+geom_raster()



# lets add xy coords to brays and calculate bimodality in brays disimilarity each grid cells
brays$x <- newdf$x
brays$y <- newdf$y
brays$dipP <- NA
brays$BC <- NA

for (i in 1:8124){
  brays[i,]$dipP <- diptest::dip.test(density(as.numeric(brays[i,2:51]))$y)$p
  brays[i,]$BC <- modes::bimodality_coefficient(as.numeric(brays[i,2:51]))
}

brays$bimodal <- ifelse(brays$dipP < 0.05 & brays$BC >= 0.55, "Bimodal brays", "Unimodal brays")

png("outputs/cluster/bimodal_brays_grid_cell_map.png")
ggplot(brays, aes(x, y, fill = bimodal))+geom_raster()+coord_equal()
dev.off()

# Are the clustered areas in cluster 4 bimodal in terms of brays overall?

brays$cluster4 <- newdf$cluster4
brays$dipPclust <- NA
brays$BCclust <- NA

for (i in 1:4){
  val <- brays[brays$cluster4 == i, 2:51]
  val.m <- melt(val)
  dipP <- diptest::dip.test(density(as.numeric(val.m$value))$y)$p
  BC <- modes::bimodality_coefficient(as.numeric(val.m$value))
  brays[brays$cluster4 == i,]$dipPclust <- dipP
  brays[brays$cluster4 == i,]$BCclust <- BC
}

brays$bimodalclust <- ifelse(brays$dipPclust <= 0.05 & brays$BCclust >= 0.55, "Bimodal brays", "Unimodal brays")

ggplot(brays, aes(x, y, fill = bimodalclust))+geom_raster()+coord_equal()

# what do these clusters really look like?
# species differences averaged for each cluster:
test <- newdf[! names(newdf) %in% c("randcell", 'period'),]
agg <- aggregate(newdf, by=list(newdf$cluster4), mean)
means.long<-melt(agg[,c(1,8:44)],id.vars="Group.1")

X11(width = 12)
ggplot(means.long,aes(x=variable,y=value,fill=factor(variable)))+
  geom_bar(stat="identity",position="dodge")+facet_wrap(~Group.1)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

common.spec <- as.character(unique(means.long[means.long$value > 0.07,]$variable))

set.seed(12)
p<- ggplot(means.long[means.long$variable %in% common.spec,],aes(x=variable,y=value,fill=factor(variable)))+
  geom_bar(stat="identity")+facet_wrap(~Group.1)+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.1))+ylab('Mean species differences within clusters')+xlab(' ')
cols <- c("#E65C5CFF", "#775CE6FF", "#CA5CE6FF", "#E6AE5CFF", "#5C93E6FF", "#E65CAEFF",
          "#5CE693FF", "#5CE6E6FF", "#77E65CFF", "#CAE65CFF")
p<- p + scale_fill_manual(values=cols)+theme_bw()+theme(legend.title = element_blank())

png(width = 7, height = 6, units = 'in',res = 200,"outputs/cluster/mean_species_diffs_clust4.png")
p
dev.off()

# plot the differences in space: 
newdf.m <- melt(newdf, id.vars = c('X.1', "x", "y", "cell", "period", "X","randcell",
                                   "bimodal", "PC1", "PC1bins", "cluster2",
                                   "cluster3", "cluster4", "cluster5", "cluster6",
                                   "cluster7", "cluster8"))

ggplot(newdf.m, aes(x, y, fill = value))+geom_raster()+coord_equal()+scale_fill_gradientn(colours = rev(terrain.colors(6)))+facet_wrap(~variable, ncol = 10)+theme(axis.ticks = element_blank(), axis.text = element_blank())
ggplot(newdf, aes(x, y, fill = Hemlock))+geom_raster()+coord_equal()
ggplot(newdf, aes(x, y, fill = Maple))+geom_raster()+coord_equal()
ggplot(newdf, aes(x, y, fill = Beech))+geom_raster()+coord_equal()
ggplot(newdf, aes(x, y, fill = Basswood))+geom_raster()+coord_equal()

summary(newdf[newdf$cluster6 == "4",])
summary(newdf[newdf$cluster6 == "3",])
summary(newdf[newdf$cluster6 == "2",])
summary(newdf[newdf$cluster6 == "1",]) 

hist(newdf[newdf$cluster6 == "4",]$bimodal)# a mix, buth mostl 1,2,3
hist(newdf[newdf$cluster6 == "3",]$bimodal)# mostly bimodal = 3 and 4
hist(newdf[newdf$cluster6 == "2",]$bimodal)# is mostly bimodal = 4 nd bimoal = 1
hist(newdf[newdf$cluster6 == "1",]$bimodal)# cluster 1 is mostly bimodal = 4, but poplar



