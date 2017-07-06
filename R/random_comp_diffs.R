library(ggplot2)
library(diptest)
library(grid)
library(gridExtra)
library(cowplot)
library(vegan)
library(cluster)

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
  subset<- pls.full[pls.full$PC1bins %in% bin & ! pls.full$cell %in% grid1, ]
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
 for (i in 1:length(pls.full$cell)){ 
   
    gridcell <- pls.full[i,]
    grid1 <- pls.full[i,]$cell
  
    bin <- pls.full[pls.full$cell %in% grid1,]$PC1bins
    # find a random grid cell within the same envt
    subset<- pls.full[pls.full$PC1bins %in% bin & ! pls.full$cell %in% grid1, ]
    randcell <- subset[sample(x = nrow(subset) , size = 50, replace = TRUE),]
    
    diffs <- randcell
    difmean<- randcell[1,]
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
    
 }


# ---------Visualize an ordination of these compositional differences-----------------
# now we want to look at the PCA of these to see if there is any structure
diffpca <- princomp(newdf[,6:42])

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
newdf.bray <- vegdist(newdf[,6:42])

test.agnes <- agnes(newdf[1:2000,])

# kmeans clustering on the diff values between each grid cell and a random grid cell in the same envt bin:
mydata <- new[,6:42]
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


#----------------does selected more grid cells and averageing diffs make it less noisy?-------------
# kmeans clustering on the averaged values:
mydata <- newdf[,6:42]
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

png("outputs/cluster/kmeans_diffs_four.png")
ggplot(newdf, aes(x,y, fill = as.factor(cluster6)))+geom_raster()+coord_equal()+guides(fill=guide_legend(title="Cluster"))
dev.off()



ggplot(newdf, aes(x,y, fill = bimodal))+geom_raster()+coord_equal()

ggplot(newdf, aes(x,y, fill = Hemlock))+geom_raster()
ggplot(newdf, aes(x,y, fill = Poplar))+geom_raster()

#newdf.m <- melt(newdf[,7:47], id.vars=c("randcell", "bimodal", "PC1", "PC1bins", "cluster6"))
agg <- aggregate(newdf,by=list(newdf$cluster6), mean)
means.long<-melt(agg[,c(1,7:43)],id.vars="Group.1")

X11(width = 12)
ggplot(means.long,aes(x=variable,y=value,fill=factor(variable)))+
  geom_bar(stat="identity",position="dodge")+facet_wrap(~Group.1)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


summary(newdf[newdf$cluster6 == "4",])
summary(newdf[newdf$cluster6 == "3",])
summary(newdf[newdf$cluster6 == "2",])
summary(newdf[newdf$cluster6 == "1",]) 

hist(newdf[newdf$cluster6 == "4",]$bimodal)# a mix, buth mostl 1,2,3
hist(newdf[newdf$cluster6 == "3",]$bimodal)# mostly bimodal = 3 and 4
hist(newdf[newdf$cluster6 == "2",]$bimodal)# is mostly bimodal = 4 nd bimoal = 1
hist(newdf[newdf$cluster6 == "1",]$bimodal)# cluster 1 is mostly bimodal = 4, but poplar
# or we could use k-mediods:
newdf.pam <- pam(newdf[,6:42], k = 4)
summary(newdf.pam)

mediods <- newdf$cell [newdf.pam$id.med]
df5 <- newdf[newdf$cell %in% mediods,] # look at the rows that have the mediods
df5

old_classes <- newdf.pam
#[1] 49221 29369 17193 16954 11274# mediods
rem_class5 <- factor(old_classes$clustering,
                     labels=c('one', # 1,
                              'two', # 2
                              'three',#3
                              "four" 
                              ))
                              
                              
                    

clust_plot5 <- data.frame(newdf, 
                          cluster = rem_class5,
                          clustNum = as.numeric(rem_class5))

b <- ggplot(clust_plot5, aes(x = x, y=y, fill=cluster))+geom_raster()
a <- ggplot(clust_plot5, aes(x = x, y=y, fill=bimodal))+geom_raster()

#X11(width = 12)
png(width = 8, height = 4, units = "in", res = 300, "outputs/cluster/kmediods_diffs_four.png")
grid.arrange(a,b, ncol = 2)
dev.off()