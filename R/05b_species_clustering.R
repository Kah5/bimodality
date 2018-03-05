version <- "1.7-5"

library(data.table)
library(reshape2)
library(dtplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(sp)
library(raster)
library(rgdal)

# load PLS data from 04_combine_umw_pls_fia.R
full.spec <- read.csv('data/outputs/plss_pct_density_composition_v1.6.csv')
full.spec <- full.spec[!is.na(full.spec$cell),]
density.full <- comps <- full.spec
summary(comps)

#----------------------k-mediods cluster analysis---------------------------------

# we want to cluster the data based on % species composition: based on tree density, not the counts
# using clusters similar to simons mediod clustering scheme: 
library(cluster)


comps <- comps[!is.na(comps$Oak),]
set.seed(11)

# use Pam for the k-mediods clustering algorithm. These take ~30 seconds to a minute each
classes.3 <- pam(comps[,4:ncol(comps)], k = 3, diss = FALSE, keep.diss = TRUE)
classes.4 <- pam(comps[,4:ncol(comps)], k = 4, diss = FALSE)
classes.5 <- pam(comps[,4:ncol(comps)], k = 5, diss = FALSE,  keep.diss = TRUE)
classes.6 <- pam(comps[,4:ncol(comps)], k = 6, diss = FALSE, keep.diss = TRUE)
classes.7 <- pam(comps[,4:ncol(comps)], k = 7, diss = FALSE, keep.diss = TRUE)
classes.8 <- pam(comps[,4:ncol(comps)], k = 8, diss = FALSE)
classes.9 <- pam(comps[,4:ncol(comps)], k = 9, diss = FALSE)
diss.6 <- as.matrix(classes.6$diss)


# Use Avg. Silhouette width to evaluate the clusters:  

# SIlhouette width close to 1 indicates the cluster clusters very well with itself. Silhoutte widith that is negative or low indicates low clustering with itself
summary(classes.9) # Avg. Silhouette width = 0.2798578
summary(classes.8) # Avg. Silhouette width = 0.2885791
summary(classes.7) # Avg. Silhouette width =  0.2842867
summary(classes.6) # Avg. Silhouette width = 0.2643707# lower than 9 classes, but the minimum width is 0.2 for all classes
summary(classes.5) # Avg. Silhouette width = 0.2335565
summary(classes.4) # Avg. Silhouette width = 0.1824538
summary(classes.3) # Avg. Silhouette width = 0.2234054

# these sihouette widths are low, but this is likely due to the large amount of data and noise in composition
#plot(classes.5)
#plot(classes.6)



# Now lets look at the clustering of 6 classes
mediods <- comps$cell [classes.6$id.med]
index <- rownames(comps[comps$cell %in% mediods,])

# get bry curtis dissimilarity to assess disimilarity within and between clusters:
brays <- vegdist(comps[,4:ncol(comps)], method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
                 na.rm = FALSE) 
brays2 <- as.matrix(brays)

# get cluster dissimilarity from each of the medoids
diss.6.dissimilarity <- brays2[,index]


# get mediods to make the cluster definitions
df6 <- comps[comps$cell %in% mediods,] # look at the rows that have the mediods
write.csv(df6, "outputs/species_comp_clusters_6_class_mediods.csv")

old_classes <- classes.6

# mediod classes:
# class 1 = 99% Oak
# class 2 = 11% Ash, 7% Basswood, 17% Beech, 10% Elm, 10% Hickory, 13 % Oak, 9.6% Maple
# class 3 = 12 % Birch, 18% Cedar.juniper, %18 Hemlock, 5% fir %12 maple, 11% Tamarck, 
# class 4 = 74 % Poplar 13 % oak
# class 5 = 10% Birch, 44% Pine, 10% Poplar, 14 % Tamarack, 
rem_class <- factor(old_classes$clustering,
                    labels=c("Oak",
                             'Elm/Maple/Hickory/Oak/Beech', #and ASH
                             'Hemlock/Beech/Cedar/Birch/Maple', # mediod 3
                             'Poplar/Oak', # mediod 4
                             'Pine/Tamarack/Poplar',# mediod 5
                             'Tamarack/Spruce/Birch/Pine/Spruce/Poplar' # mediod6 # not as much birch
                             
                      ))

classes.6$silinfo$clus.avg.widths

# note the the clus.avg.widths is not in the same order as df6,had to manually assign
ranked_class <- factor(old_classes$clustering,
                    labels=c("Low.0.10", #mediod 1
                             "High.0.71", # mediod 2
                             "Mod.0.19", # mediod 3
                             "High.0.40", #mediod 4
                             "Mod.0.16", # mediod 5
                             "High.0.42" #mediod 6
               
                    ))



clust_plot6 <- data.frame(comps, 
                          speciescluster = rem_class,
                          rank = ranked_class,
                          clustNum = as.numeric(rem_class),
                          diss1 = diss.6.dissimilarity[,1],
                          diss2 = diss.6.dissimilarity[,2],
                          diss3 = diss.6.dissimilarity[,3],
                          diss4 = diss.6.dissimilarity[,4],
                          diss5 = diss.6.dissimilarity[,5],
                          diss6 = diss.6.dissimilarity[,6])


# merge the clusters with denisty estimates:
densities <- read.csv(paste0("data/midwest_pls_full_density_alb",version,".csv"))
dens.clus <- merge(densities, clust_plot6[,c('x', "y", "cell", "speciescluster", "clustNum",'rank', 'diss1','diss2','diss3','diss4','diss5','diss6')], by = c("x","y","cell"),keep = all)

write.csv(dens.clus, "outputs/cluster/density_pls_with_clusters.csv")

# map out the clusters in space:
png(width = 6, height = 6, units= 'in',res=300,"outputs/paper_figs/six_cluster_map_pls.png")
pls.clust <- ggplot(clust_plot6, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c('#386cb0','#beaed4','#ffff99','#f0027f', '#7fc97f','#fdc086'))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                            axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(),legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.position = c(0.205, 0.32),legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()+ annotate("text", x=-90000, y=1486000,label= "B", size = 5)+ggtitle("")
pls.clust 
dev.off()

ggplot(clust_plot6, aes(x = x, y=y, fill=diss3))+geom_raster()+
   geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(),legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.position = c(0.205, 0.32),legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()+ annotate("text", x=-90000, y=1486000,label= "B", size = 5)+ggtitle("")
colnames(clust_plot6)[42:47] <- c('Elm.Maple.Hickory.Oak.Beech.diss', #mediod 1
                                  'Tamarack.Spruce.Birch.Pine.Spruce.Poplar.diss', # mediod5
                                  'Pine.Tamarack.Poplar.diss', # mediod 6
                                  
                                  "Poplar.Oak.diss", # mediod 4
                                  
                                  'Hemlock.Beech.Cedar.Birch.Maple.diss', # mediod 3
                                  "Oak.diss" )# medoid 2) #6)                                                                                             



#map out the dissimilarities in space
clust6.m <- melt(clust_plot6[,c("x", "y", "cell", "Elm.Maple.Hickory.Oak.Beech.diss",
                                "Oak.diss",
                                "Hemlock.Beech.Cedar.Birch.Maple.diss",
                                "Poplar.Oak.diss",
                                "Tamarack.Spruce.Birch.Pine.Spruce.Poplar.diss",
                                "Pine.Tamarack.Poplar.diss")], id.vars = c('x',"y","cell"))

dens.m <- melt(dens[,c("x", "y", "cell", "Elm.Maple.Hickory.Oak.Beech.diss",
                                "Oak.diss",
                                "Hemlock.Beech.Cedar.Birch.Maple.diss",
                                "Poplar.Oak.diss",
                                "Tamarack.Spruce.Birch.Pine.Spruce.Poplar.diss",
                                "Pine.Tamarack.Poplar.diss", "PC1")], id.vars = c('x',"y","cell", "PC1"))

# create relabeller
composition_names <- list(
  'Elm.Maple.Hickory.Oak.Beech.diss'="Elm/Maple/Hickory/ \n Oak/Beech",
  'Oak.diss'="Oak",
  'Hemlock.Beech.Cedar.Birch.Maple.diss'="Hemlock/Beech/Cedar/ \n Birch/Maple",
  'Poplar.Oak.diss'="Poplar/Oak",
  'Tamarack.Spruce.Birch.Pine.Spruce.Poplar.diss' = 'Tamarack/Spruce/Birch/ \n Pine/Spruce/Poplar',
  'Pine.Tamarack.Poplar.diss' = 'Pine/Tamarack/Poplar'
)

composition_labeller <- function(variable,value){
  return(composition_names[value])
}

png(width = 10, height = 6, units="in",res=300,"outputs/Composition/six_cluster_dissimilarity_maps.png")
dis.maps <- ggplot(clust6.m, aes(x,y, fill = value))+geom_raster()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 6, name = "YlGnBu"))+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                                                                                                                                                                  axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),
                                                                                                                                                                                                                                                  axis.title.y=element_blank(),legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.background = element_rect(fill=alpha('transparent', 0)))+
                                                                                                                                                                                                                                                  xlab("easting") + ylab("northing") +coord_equal()+ggtitle("")+facet_wrap(~variable, ncol = 3, labeller = composition_labeller)
dis.maps
dev.off()



# lets look at the histograms of these overall
png(width = 10, height = 6, units = "in", res=300, 'outputs/Composition/six_cluster_dissimilarity_hists.png')
dis.hist <- ggplot(clust6.m, aes(value))+geom_histogram(bw = 35)+theme_bw()+facet_wrap(~variable, ncol = 3, labeller = composition_labeller)
dis.hist
dev.off()

# save as csv for future 
write.csv(clust_plot6, "outputs/six_clust_pls_dissimilarity.csv", row.names = FALSE)


# -----------test out what the other n clusters look like:
# k = 7 mediods: 
# get mediods to make the cluster definitions
mediods7 <- comps$cell [classes.7$id.med]
index <- rownames(comps[comps$cell %in% mediods7,])

df7 <- comps[comps$cell %in% mediods7,] # look at the rows that have the mediods

old_classes <- classes.7

# mediod classes:

rem_class <- factor(old_classes$clustering,
                    labels=c("Elm/Maple/Hickory/Oak/Beech",
                             "Beech/Maple/Hemlock",
                             'Oak', #and ASH
                             'Hemlock/Beech/Cedar/Birch/Maple', # mediod 3
                             'Poplar', # mediod 4
                             'Pine/Tamarack/Poplar',# mediod 5
                             'Tamarack/Spruce/Birch/Pine/Poplar' # mediod6 # not as much birch
                             
                    ))

classes.7$silinfo$clus.avg.widths
clust_plot7 <- data.frame(comps, speciescluster = rem_class)
                       
png("outputs/Composition/seven_cluster_map_pls.png")
ggplot(clust_plot7, aes(x, y, fill = speciescluster))+geom_raster()+coord_equal()+theme_bw()
dev.off()

png(width = 6, height = 6, units= 'in',res=300,"outputs/paper_figs/seven_cluster_map_pls.png")
pls.clust7 <- ggplot(clust_plot7, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c('#beaed4', '#bf5b17','#386cb0','#ffff99','#fdc086', '#f0027f','#7fc97f'))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(),legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.position = "bottom",legend.direction = "vertical",legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()+ggtitle(" ")
pls.clust7 
dev.off()


# save as csv for future 
write.csv(clust_plot7, "outputs/seven_clust_pls_dissimilarity.csv", row.names = FALSE)


# k = 8 mediods (highest average silhouette width)
# get mediods to make the cluster definitions
mediods8 <- comps$cell [classes.8$id.med]
index <- rownames(comps[comps$cell %in% mediods8,])

df8 <- comps[comps$cell %in% mediods8,] # look at the rows that have the mediods

old_classes <- classes.8

# mediod classes:

rem_class <- factor(old_classes$clustering,
                    labels=c(
                            "Oak/Hickory",
                             "Elm/Maple/Hickory/Oak/Beech",
                              "Oak",
                             # mediod 3
                             'Poplar/Oak', # mediod 4
                             "Beech/Maple",
                             'Hemlock/Beech/Cedar/Birch/Maple',
                             'Pine/Tamarack/Poplar',# mediod 5
                             'Tamarack/Spruce/Birch/Pine/Poplar' # mediod6 # not as much birch
                             
                    ))

classes.8$silinfo$clus.avg.widths
clust_plot8 <- data.frame(comps, speciescluster = rem_class)

png("outputs/Composition/eight_cluster_map_pls.png")
ggplot(clust_plot8, aes(x, y, fill = speciescluster))+geom_raster()+coord_equal()+theme_bw()
dev.off()




#do the same clustering for FIA data and plot:
# -----------------------Clustering of FIA data----------------------

# read in fcomps csv from 04_combine_umw_pls_fia.R
fcomps <- read.csv("outputs/cluster/fullcomps.csv")
fcomps <- fcomps[fcomps$period %in% "Modern",] 
library(cluster)


#---------------------- get dissimilarity of fcomp cells from the mediod of the PLS cells:
indexpls <- rownames(comps[comps$cell %in% mediods,])

fia_with_pls_meds <- rbind(comps[comps$cell %in% mediods,], fcomps)
indexpls <- row.names(fia_with_pls_meds[fia_with_pls_meds$period %in% "Past",])

#index <- fcomps[fcomps$cell %in% mediods,]$idvar
brays.f <- vegdist(fia_with_pls_meds[,5:40], method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
                   na.rm = TRUE) 
brays.f2 <- as.matrix(brays.f)

diss.f6.dissim_fia <- brays.f2[,indexpls]


fia_pls_diss <- data.frame(fia_with_pls_meds[7:6351,], 
           diss1 = diss.f6.dissim_fia[7:6351,1],
           diss2 = diss.f6.dissim_fia[7:6351,2],
           diss3 = diss.f6.dissim_fia[7:6351,3],
           diss4 = diss.f6.dissim_fia[7:6351,4],
           diss5 = diss.f6.dissim_fia[7:6351,5],
           diss6 = diss.f6.dissim_fia[7:6351,6])




colnames(fia_pls_diss)[41:46] <- c('Elm.Maple.Hickory.Oak.Beech.diss', #mediod 1
                                  'Tamarack.Spruce.Birch.Pine.Spruce.Poplar.diss', # mediod5
                                  'Pine.Tamarack.Poplar.diss', # mediod 6
                                  
                                  "Poplar.Oak.diss", # mediod 4
                                  
                                  'Hemlock.Beech.Cedar.Birch.Maple.diss', # mediod 3
                                  "Oak.diss" )# medoid 2) #6)                                                                                             

#map out the dissimilarities in space
clust6fia.m <- melt(fia_pls_diss[,c("x", "y", "cell", "Elm.Maple.Hickory.Oak.Beech.diss",
                                "Oak.diss",
                                "Hemlock.Beech.Cedar.Birch.Maple.diss",
                                "Poplar.Oak.diss",
                                "Tamarack.Spruce.Birch.Pine.Spruce.Poplar.diss",
                                "Pine.Tamarack.Poplar.diss")], id.vars = c('x',"y","cell"))



# create relabeller
composition_names <- list(
  'Elm.Maple.Hickory.Oak.Beech.diss'="Elm/Maple/Hickory/ \n Oak/Beech",
  'Oak.diss'="Oak",
  'Hemlock.Beech.Cedar.Birch.Maple.diss'="Hemlock/Beech/Cedar/ \n Birch/Maple",
  'Poplar.Oak.diss'="Poplar/Oak",
  'Tamarack.Spruce.Birch.Pine.Spruce.Poplar.diss' = 'Tamarack/Spruce/Birch/ \n Pine/Spruce/Poplar',
  'Pine.Tamarack.Poplar.diss' = 'Pine/Tamarack/Poplar'
)

composition_labeller <- function(variable,value){
  return(composition_names[value])
}

png(width = 10, height = 6, units="in",res=300,"outputs/Composition/six_cluster_dissimilarity_maps_fia_from_pls.png")
dis.maps <- ggplot(clust6fia.m, aes(x,y, fill = value))+geom_raster()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 6, name = "YlGnBu"))+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                                                                                                                                                                                axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),
                                                                                                                                                                                                                                                                axis.title.y=element_blank(),legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.background = element_rect(fill=alpha('transparent', 0)))+
  xlab("easting") + ylab("northing") +coord_equal()+ggtitle("")+facet_wrap(~variable, ncol = 3, labeller = composition_labeller)
dis.maps
dev.off()

# lets look at the histograms of these overall
png(width = 10, height = 6, units = "in", res=300, 'outputs/Composition/six_cluster_dissimilarity_hists_fia_from_pls.png')
dis.hist <- ggplot(clust6fia.m, aes(value))+geom_histogram(bw = 35)+theme_bw()+facet_wrap(~variable, ncol = 3, labeller = composition_labeller)
dis.hist
dev.off()



png(width = 10, height = 6, units = "in", res=300, 'outputs/Composition/six_cluster_dissimilarity_hists_fia_pls.png')

dis.hist.pls.fia<- ggplot(clust6fia.m, aes(value, fill = variable))+geom_histogram(  alpha = 0.5)+geom_density(data = clust6fia.m, aes(value, 0.03*..count.., color = variable), fill = NA)+
  geom_density(data = clust6.m, aes(value, 0.05*..count.., color = variable), fill = NA, linetype = "dashed")+theme_bw()+facet_wrap(~variable, ncol = 3, labeller = composition_labeller)
dis.hist.pls.fia
dev.off()

#ggplot(fia_pls_diss, aes(Oak.diss, Hemlock.Beech.Cedar.Birch.Maple.diss))+geom_point()

#ggplot(clust_plot6, aes(Oak.diss, Hemlock.Beech.Cedar.Birch.Maple.diss))+geom_point()

# need to merge clust6fia.m with the dissimilarities from the pls era and plot them together:
#------------------------------------Fcomps fia classification----------------------------
#fcomps classifcation only
fcomps <- read.csv('data/outputs/FIA_pct_density_composition.csv')


classes.3 <- pam(fcomps[,5:ncol(fcomps)], k = 3, diss = FALSE,  keep.diss = TRUE)
classes.4 <- pam(fcomps[,5:ncol(fcomps)], k = 4, diss = FALSE,  keep.diss = TRUE)
classes.5 <- pam(fcomps[,5:ncol(fcomps)], k = 5, diss = FALSE,  keep.diss = TRUE)
classes.6 <- pam(fcomps[,5:ncol(fcomps)], k = 6, diss = FALSE,  keep.diss = TRUE)
classes.7 <- pam(fcomps[,5:ncol(fcomps)], k = 7, diss = FALSE,  keep.diss = TRUE)
classes.8 <- pam(fcomps[,5:ncol(fcomps)], k = 8, diss = FALSE,  keep.diss = TRUE)
classes.9 <- pam(fcomps[,5:ncol(fcomps)], k = 9, diss = FALSE,  keep.diss = TRUE)



summary(classes.8) # Avg. Silhouette width = 0.2352843
summary(classes.7) # Avg. Silhouette width = 0.2525737
summary(classes.6) # Avg. Silhouette width = 0.2362255
summary(classes.5) # Avg. Silhouette width = 0.2139102
summary(classes.4) # Avg. Silhouette width = 0.2081704
summary(classes.3) # Avg. Silhouette width = 0.1584407
fcomps$idvar <- 1:nrow(fcomps)

#mediods <- fcomps[fcomps$idvar %in%  classes.5$medoids,]
#mediods <- fcomps$idvar [classes.5$id.med]
mediods5 <- fcomps$cell [classes.5$id.med]
index <- rownames(fcomps[fcomps$cell %in% mediods5,])

#index <- fcomps[fcomps$cell %in% mediods,]$idvar
brays.f <- vegdist(fcomps[,4:39], method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
                 na.rm = FALSE) 
brays.f2 <- as.matrix(brays.f)

diss.f5.dissimilarity <- brays.f2[,index]

df5 <- fcomps[fcomps$cell %in% mediods5,] # look at the rows that have the mediods

old_classes <- classes.5

#[1] 1292 2201 4618 4978 4604# idvars of the mediods
rem_class5 <- factor(old_classes$clustering,
                    labels=c(  'Oak/Maple',
                               'Maple/Ash/Birch/Aspen', # 2
                              'Maple', # 1,
                              
                            'Aspen',#3
                           "Pine/Poplar" # 4
                           ))

rem_class5 <- factor(old_classes$clustering,
                     labels=c( 'Oak/Maple',
                               'Maple/Ash/Birch/Aspen', # 2
                               'Maple', # 1,
                               
                               'Aspen',#3
                               "Pine/Poplar" 
                              
                              
                     ))

clust_plot5 <- data.frame(fcomps, 
                          speciescluster = rem_class5,
                          clustNum = as.numeric(rem_class5),
                          diss1 = diss.f5.dissimilarity[,1],
                          diss2 = diss.f5.dissimilarity[,2],
                          diss3 = diss.f5.dissimilarity[,3],
                          diss4 = diss.f5.dissimilarity[,4],
                          diss5 = diss.f5.dissimilarity[,5])

ggplot(clust_plot5, aes(x = x, y=y, fill=speciescluster))+geom_raster()

classes.5$silinfo$clus.avg.widths
# they are all equally dissimilar
#0.40663776 -0.04516793  0.51842603  0.47118307  0.46843040  0.33582488

# 6 classes
mediods <- fcomps$cell [classes.6$id.med]
#mediods
#[1] 45094 27585 14273  9203 22622 15808
index <- rownames(fcomps[fcomps$cell %in% mediods,])

#index <- fcomps[fcomps$cell %in% mediods,]$idvar
brays.f <- vegdist(fcomps[,4:39], method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
                   na.rm = FALSE) 
brays.f2 <- as.matrix(brays.f)

diss.f6.dissimilarity <- brays.f2[,index]
df6 <- fcomps[fcomps$cell %in% mediods,] # look at the rows that have the mediods
write.csv(df6, "outputs/fia_species_comp_clusters_6_class_mediods.csv")

old_classes <- classes.6
rem_class <- factor(old_classes$clustering,
                    labels=c('Oak/Maple', # 1,
                             'Maple/Birch/Ash/Oak/Hickory/Otherhardwood', # 2
                             'Maple',#3
                             "Poplar", # 4,
                             'Cedar.juniper/Tamarack', #5
                             'Pine/Poplar' #6
                    ))

clust_plot6f <- data.frame(fcomps, 
                          speciescluster = rem_class,
                          clustNum = as.numeric(rem_class),
                          diss1 = diss.f6.dissimilarity[,1],
                          diss2 = diss.f6.dissimilarity[,2],
                          diss3 = diss.f6.dissimilarity[,3],
                          diss4 = diss.f6.dissimilarity[,4],
                          diss5 = diss.f6.dissimilarity[,5],
                          diss6 = diss.f6.dissimilarity[,6])

#ggplot(clust_plot6f, aes(x = x, y=y, fill=diss1))+geom_raster()

png(width = 6, height = 6, units= 'in',res=300,"outputs/paper_figs/six_cluster_map_fia.png")
fia.clust<- ggplot(clust_plot6f, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c('#386cb0','#beaed4','#e41a1c','#ffff33', '#7fc97f','#fdc086'))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                             axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                             axis.title.y=element_blank(), legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.position = c(0.205, 0.32),legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()+ annotate("text", x=-90000, y=1486000,label= "D", size = 5)+ggtitle("")
fia.clust 

dev.off()

write.csv(clust_plot6f, "outputs/cluster/density_fia_with_clusters.csv", row.names = FALSE)
# reassign names
colnames(clust_plot6f)[44:49] <- c('Oak/Maple', # 1,
                                   'Oak/Hickory/Otherhardwood/Maple/Birch/Ash', # 2
                                   'MapleComp',#3
                                   "Poplar/Spruce/Maple/Fir", # 4,
                                   'Pine/Poplar', #5,
                                   'Cedar.juniper/Poplar/Maple')
# save the fia clusters to a csv:
#map out the dissimilarities in space
clust6f.m <- melt(clust_plot6f[,c("x", "y", "cell", 'speciescluster',"clustNum",'Oak/Maple', # 1,
                                  'Oak/Hickory/Otherhardwood/Maple/Birch/Ash', # 2
                                  'MapleComp',#3
                                  "Poplar/Spruce/Maple/Fir", # 4,
                                  'Pine/Poplar', #5,
                                  'Cedar.juniper/Poplar/Maple')], id.vars = c('x',"y","cell", "speciescluster","clustNum"))



# create relabeller
composition_names <- list(
  'Oak/Maple'="Oak/Maple",
  'Oak/Hickory/Otherhardwood/Maple/Birch/Ash'="Oak/Hickory/ \n Other hardwood/ \n Maple/Birch/Ash",
  'MapleComp'="Maple",
  'Poplar/Spruce/Maple/Fir'="Poplar/Spruce/ \n Maple/Fir",
  'Pine/Poplar' = 'Pine/Poplar',
  'Cedar.juniper/Poplar/Maple' = 'Cedar juniper/Poplar/Maple'
)

composition_labeller <- function(variable,value){
  return(composition_names[value])
}

png(width = 10, height = 6, units="in",res=300,"outputs/Composition/six_cluster_dissimilarity_fia_maps.png")
dis.fia.maps <- ggplot(clust6f.m, aes(x,y, fill = value))+geom_raster()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 6, name = "YlGnBu"))+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                                                                                                                                                                                axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),
                                                                                                                                                                                                                                                                axis.title.y=element_blank(),legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.background = element_rect(fill=alpha('transparent', 0)))+
  xlab("easting") + ylab("northing") +coord_equal()+ggtitle("")+facet_wrap(~variable, ncol = 3, labeller = composition_labeller)
dis.fia.maps
dev.off()

# lets look at the histograms of these overall
png(width = 10, height = 6, units = "in", res=300, 'outputs/Composition/six_cluster_dissimilarity_hists.png')
dis.fia.hist <- ggplot(clust6f.m, aes(value))+geom_histogram()+theme_bw()+facet_wrap(~variable, ncol = 3, labeller = composition_labeller)
dis.fia.hist
dev.off()

# write as csv for future 
write.csv(clust_plot6f, "outputs/six_clust_fia_dissimilarity.csv", row.names = FALSE)

# plot pls and fia cluster figures together:
png(width = 10, height=4, units="in", res=300, "outputs/paper_figs/Fig_S1CD.png")
grid.arrange(pls.clust, fia.clust, ncol = 2)
dev.off()



# map out the clusters from the previoius FIA surveys:

prevcomps <- read.csv("outputs/cluster/fullcomps_oldsurvey.csv")
prev1980 <- prevcomps[prevcomps$period %in% "Modern-1980s",]
prev1990 <- prevcomps[prevcomps$period %in% "Modern-1990s",]


classes.5.1980 <- pam(prev1980[,5:ncol(prev1980)], k = 5, diss = FALSE,  keep.diss = TRUE)
summary(classes.5.1980) # avg silhouette width = 0.2624983

classes.5.1990 <- pam(prev1990[,5:ncol(prev1990)], k = 5, diss = FALSE,  keep.diss = TRUE)
summary(classes.5.1990) # avg silhouette width = 0.246144

mediods5.1980 <- prev1980$cell [classes.5.1980$id.med]
index <- rownames(prev1980[prev1980$cell %in% mediods5.1980,])

#index <- fcomps[fcomps$cell %in% mediods,]$idvar

df5 <- prev1980[prev1980$cell %in% mediods5.1980,] # look at the rows that have the mediods

old_classes <- classes.5.1980

#[1] 1292 2201 4618 4978 4604# idvars of the mediods
rem_class5 <- factor(old_classes$clustering,
                     labels=c(  'Oak/Maple',
                                'Maple/Oak/Ash/Poplar', # 2
                                "Pine/Poplar", # 4
                                'Maple', # 1,
                                
                                'Aspen'#3
                                
                     ))

rem_class5 <- factor(old_classes$clustering,
                     labels=c( 'Oak/Maple',
                               'Maple/Ash/Birch/Aspen', # 2
                               "Pine/Poplar",
                               'Maple', # 1,
                               
                               'Aspen'#3
                                
                               
                               
                     ))

clust_plot5.1980 <- data.frame(prev1980, 
                          speciescluster = rem_class5,
                          clustNum = as.numeric(rem_class5))
                          #diss1 = diss.f5.dissimilarity[,1],
                          #diss2 = diss.f5.dissimilarity[,2],
                          #diss3 = diss.f5.dissimilarity[,3],
                          #diss4 = diss.f5.dissimilarity[,4],
                          #diss5 = diss.f5.dissimilarity[,5])


ggplot(clust_plot5.1980, aes(x,y,fill = speciescluster))+geom_raster()



classes.5.1990 <- pam(prev1990[,5:ncol(prev1990)], k = 5, diss = FALSE,  keep.diss = TRUE)
summary(classes.5.1990) # avg silhouette width = 0.246144

mediods5.1990 <- prev1990$cell [classes.5.1990$id.med]
index <- rownames(prev1990[prev1990$cell %in% mediods5.1990,])

#index <- fcomps[fcomps$cell %in% mediods,]$idvar

df5 <- prev1990[prev1990$cell %in% mediods5.1990,] # look at the rows that have the mediods

old_classes <- classes.5.1990

#[1] 1292 2201 4618 4978 4604# idvars of the mediods
rem_class5 <- factor(old_classes$clustering,
                     labels=c( 'Oak/Maple', 
                          'Maple/Oak/Ash/Poplar', # 2
                                "Pine/Poplar", # 4
                                'Maple', # 1,
                                
                                'Aspen'#3
                                
                     ))

rem_class5 <- factor(old_classes$clustering,
                     labels=c( 'Oak/Maple',
                               'Maple/Ash/Birch/Aspen', # 2
                               "Pine/Poplar",
                               'Maple', # 1,
                               
                               'Aspen'#3
                               
                               
                               
                     ))

clust_plot5.1990 <- data.frame(prev1990, 
                               speciescluster = rem_class5,
                               clustNum = as.numeric(rem_class5))
#diss1 = diss.f5.dissimilarity[,1],
#diss2 = diss.f5.dissimilarity[,2],
#diss3 = diss.f5.dissimilarity[,3],
#diss4 = diss.f5.dissimilarity[,4],
#diss5 = diss.f5.dissimilarity[,5])


ggplot(clust_plot5.1990, aes(x,y,fill = speciescluster))+geom_raster()


write.csv(clust_plot5.1990, "outputs/cluster/density_fia_1990s_with_clusters.csv", row.names = FALSE)
write.csv(clust_plot5.1980, "outputs/cluster/density_fia_1980s_with_clusters.csv", row.names = FALSE)
