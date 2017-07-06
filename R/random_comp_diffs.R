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
# x, y, grid cell 1, grid cell 2, bimodality, spec compostion differences 1-29
newdf <- pls.full[,2:43]
newdf$randcell <- NA
newdf$bimodal <- NA
newdf$PC1 <- pls.full$PC1
newdf$PC1bins <- pls.full$PC1bins

# start with the first grid cell, then find a random grid cell in the same envt:
 for (i in 1:length(pls.full$cell)){ 
   
    gridcell <- pls.full[i,]
    grid1 <- pls.full[i,]$cell
  
    bin <- pls.full[pls.full$cell %in% grid1,]$PC1bins
    # find a random grid cell within the same envt
    subset<- pls.full[pls.full$PC1bins %in% bin & ! pls.full$cell %in% grid1, ]
    randcell <- subset[sample(x = nrow(subset) , size = 50, replace = TRUE),]
    
    diffs <- randcell
    # calculate the diffs between the average grid cell
      for (j in 1:length(randcell$cell)){
        diffs[j, 7:43] <- randcell[j, 7:43] - gridcell[,7:43]
      }
    # calculate the absolute difference of all the diffs
    abs.diffs <- data.frame( lapply(diffs[,7:43], abs) )
    # find the mean difference
    newdf[i, 6:42] <- colMeans(abs.diffs)
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
g
dev.off()

# ------------Do the comp differences cluster?---------------------------
# cacluate bray dists
newdf.bray <- vegdist(newdf[,6:42])

test.agnes <- agnes(newdf[1:2000,])

newdf.pam <- pam(newdf[,6:42], k = 5)
summary(newdf.pam)

mediods <- newdf$cell [newdf.pam$id.med]

# kmeans clustering:
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
ggplot(newdf, aes(x,y, fill = as.factor(cluster6)))+geom_raster()
dev.off()

ggplot(newdf, aes(x,y, fill = bimodal))+geom_raster()

ggplot(newdf, aes(x,y, fill = Hemlock))+geom_raster()
ggplot(newdf, aes(x,y, fill = Poplar))+geom_raster()



df5 <- newdf[newdf$cell %in% mediods,] # look at the rows that have the mediods
df5

old_classes <- newdf.pam
#[1] 49221 29369 17193 16954 11274# mediods
rem_class5 <- factor(old_classes$clustering,
                     labels=c('one', # 1,
                              'two', # 2
                              'three',#3
                              "four", # 4,
                              'five', 
                              "six"))
                              
                              
                    

clust_plot5 <- data.frame(newdf, 
                          cluster = rem_class5,
                          clustNum = as.numeric(rem_class5))

b <- ggplot(clust_plot5, aes(x = x, y=y, fill=cluster))+geom_raster()
a <- ggplot(clust_plot5, aes(x = x, y=y, fill=bimodal))+geom_raster()

X11(width = 12)
grid.arrange(a,b, ncol = 2)
