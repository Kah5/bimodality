library(data.table)
library(reshape2)
library(dtplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(sp)
library(raster)
library(rgdal)
library(ncdf4)
library(cluster)# library needed for pam
library(tidyr)
library(dplyr)

#-----------------Outline of this script----------------------------------------
# 1. Clustering of species composition data from raw data, aggregated to the 8km density level
# 2. Clustering of species composition data from Chris Paciorek's statistical product

# load PLS data from 04_combine_umw_pls_fia.R
full.spec <- read.csv('data/outputs/plss_pct_density_composition_v1.6.csv')
full.spec <- full.spec[!is.na(full.spec$cell),]
full.spec <- full.spec[!duplicated(full.spec),] # for some reason there are some duplicated rows, remove these
density.full <- comps <- full.spec
summary(comps)



#-----------------------Part 2: clustering with C.P. statistical estimates----------------------------

#----------------------k-mediods cluster analysis---------------------------------

# we want to cluster the data based on % species composition: based on tree density, not the counts
# using clusters similar to simons mediod clustering scheme: 

# make sure we are only looking at grid cells that have numbers, not NA values
comps <- comps[!is.na(comps$Oak),]
set.seed(11)

# plot some data to check it looks right
ggplot(comps, aes(x,y, fill = Oak))+geom_raster()

# we need to run a series of cluster analyses with 1:n number of clusters and then determine the # of clusters that best explains the data
# use Pam function for the k-mediods clustering algorithm. These take few minutes to complete each round of pam
# pam clusters the data base on k number of clusters, then we need to evaluate which # of clusters is the best to use
# for more info:
??pam

classes.3 <- pam(comps[,4:ncol(comps)], k = 3, diss = FALSE, keep.diss = TRUE) 
classes.4 <- pam(comps[,4:ncol(comps)], k = 4, diss = FALSE, keep.diss = TRUE)
classes.5 <- pam(comps[,4:ncol(comps)], k = 5, diss = FALSE, keep.diss = TRUE)
classes.6 <- pam(comps[,4:ncol(comps)], k = 6, diss = FALSE, keep.diss = TRUE)
classes.7 <- pam(comps[,4:ncol(comps)], k = 7, diss = FALSE, keep.diss = TRUE)
classes.8 <- pam(comps[,4:ncol(comps)], k = 8, diss = FALSE)
classes.9 <- pam(comps[,4:ncol(comps)], k = 9, diss = FALSE)
diss.6 <- as.matrix(classes.6$diss)


# Use Avg. Silhouette width to evaluate the clusters:  
# Silhouette width close to 1 indicates the cluster clusters very well with itself. Silhoutte widith that is negative or low indicates low clustering with itself
# ideally we want to select the # of clusters (k) where there is high silhouette widths
summary(classes.9) # Avg. Silhouette width = 0.2798578
pls.8class.summ <- summary(classes.8) # Avg. Silhouette width = 0.2885791
pls.7class.summ <- summary(classes.7) # Avg. Silhouette width =  0.2842867
pls.6class.summ <- summary(classes.6) # Avg. Silhouette width = 0.2643707# lower than 9 classes, but the minimum width is 0.2 for all classes
pls.5class.summ <- summary(classes.5) # Avg. Silhouette width = 0.2335565
pls.4class.summ <- summary(classes.4) # Avg. Silhouette width = 0.1824538
pls.3class.summ <- summary(classes.3) # Avg. Silhouette width = 0.2234054

# in general 7 clusters seems to be the best for the full midwest data, but you may have a different # 

# --------------- Now lets look at a map of the k=7 clusters ----------------------

# k = 7 mediods: 
# get mediods to make the cluster definitions
mediods7 <- comps$cell [classes.7$id.med]
index <- rownames(comps[comps$cell %in% mediods7,])

# look at the rows that have the mediods
df7 <- comps[comps$cell %in% mediods7,] 
df7

old_classes <- classes.7

# I assigned names to mediod classes based on the highest species % in each mediod:

rem_class <- factor(old_classes$clustering,
                    # relabel the clusters from numbers to custom names
                    labels=c("Elm/Maple/Hickory/Oak/Beech",
                             "Beech/Maple/Hemlock",
                             'Oak', #and ASH
                             'Poplar',
                             'Hemlock/Beech/Cedar/Birch/Maple', # mediod 3
                             
                             'Tamarack/Spruce/Birch/Pine/Poplar',# mediod 4
                             'Pine/Tamarack/Poplar'# mediod 5
                              # mediod6 # not as much birch
                             
                    ))

classes.7$silinfo$clus.avg.widths # get the average silohette width for each cluster
clust_plot7 <- data.frame(comps, speciescluster = rem_class)
                       


# need to set up state outlines:
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota', 'wisconsin', 'michigan', "illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)


# map out the clusters with pretty colors & save to a file:
png(width = 6, height = 6, units= 'in',res=300,"outputs/paper_figs/seven_cluster_map_pls.png")
pls.clust7 <- ggplot(clust_plot7, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c('#beaed4', '#bf5b17','#386cb0','#fdc086','#ffff99','#7fc97f','#f0027f'))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(),legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.position = "bottom",legend.direction = "vertical",legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()+ggtitle("PLS species clusters")
pls.clust7 
dev.off()


# save as csv for future 
write.csv(clust_plot7, "outputs/seven_clust_pls_dissimilarity.csv", row.names = FALSE)


#-----------------------Part 2: clustering with C.P. statistical estimates----------------------------
# estimates downloaded from here: https://paleon.geography.wisc.edu/doku.php/data_and_products;settlement_vegetation_statistical
# saved under my /data folder:
# format is a netcdf file, so I need to open it:


comp.nc <- nc_open(filename = "data/composition_v0.4.nc")

# data structure: x = 296, y = 180, sample = 250 MCMC samples, for 23 different taxa
# has x, y, sample for each taxa

# extract x and y values
x <- ncvar_get(comp.nc, "x")
y <- ncvar_get(comp.nc, "y")
n <- ncvar_get(comp.nc, "sample")

# list the names of taxa:
Taxa <- names(comp.nc$var)
nTaxa <- comp.nc$nvars #get the # of taxa

# this for loop extracts the composition draws from each taxa and puts into one giant dataframe
for(i in 1:nTaxa){
  
  total.array <- ncvar_get(comp.nc, Taxa[i]) # store the data in a 3-dimensional array
  rownames(total.array) <- x # assign column and row names to the array
  colnames(total.array) <- y
  # melt the array into a dataframe
  total.m <- melt(total.array, varnames=c("x","y","sample"), as.is = TRUE)
  colnames(total.m) <- c("x", "y", "sample", Taxa[i]) # rename the columns
  total.m$x <- as.numeric(total.m$x)
  total.m$y <- as.numeric(total.m$y)
  
  # create a new dataframe & add the extracted samples to it
  if(i == 1){ comp.df <- total.m }else{
  comp.df[,3+i] <- total.m[,Taxa[i]] 
  colnames(comp.df)[3+i] <- Taxa[i]}
  
}

nc_close(comp.nc) # close the ncdf file

# there are alot of NA grid cells for all taxathat we need to remove:
comp.df <- comp.df[!is.na(comp.df$Oak),]

# now comp.df has 250 samples for each gridcell, so lets summarize the mean composition draw for each species:
comp.long <- melt(comp.df, id.vars = c("x", "y", "sample")) # convert from wide format to long
# get the mean composition value from all the draws for each spects
comp.stat <- comp.long %>% group_by(x, y, variable) %>% summarise(mean = mean(value, na.rm=TRUE))

# save this:
write.csv(comp.stat, "data/mean_pls_statistical_comp_summary.csv", row.names = FALSE)

# make a giant plot of composition across the domain:
ggplot(comp.stat, aes(x,y, fill = mean))+geom_raster()+facet_wrap(~variable)

# make it wide again:
comp.wide <- spread(comp.stat, variable, mean)

# only select places that are in the Midwest domain:
comp.stat.mw <- comp.wide[is.na(comp.wide$Chestnut),]

ggplot(comp.stat.mw, aes(x,y, fill = Oak))+geom_raster()



#----------------------k-mediods cluster analysis---------------------------------

# now run pam with 7 classes again:
classes.7.smooth <- pam(comp.stat.mw[,4:ncol(comp.stat.mw)], k = 7, diss = FALSE, keep.diss = TRUE)
classes.6.smooth <- pam(comp.stat.mw[,4:ncol(comp.stat.mw)], k = 6, diss = FALSE, keep.diss = FALSE)
classes.8.smooth <- pam(comp.stat.mw[,4:ncol(comp.stat.mw)], k = 8, diss = FALSE, keep.diss = FALSE)


pls.7class.smooth <- summary(classes.7.smooth) # Avg. Silhouette width = 0.3454994 lower than 9 classes, but the minimum width is 0.2 for all classes
pls.6class.smooth <- summary(classes.6.smooth) # Avg. Silhouette width = 0.5534339
pls.8class.smooth <- summary(classes.8.smooth) # Avg. Silhouette width = 0.2234054

# in general 7 clusters seems to be the best for the full midwest grid level data data, but you may have a different # 


# --------------- Now lets look at a map of the k=7 clusters ----------------------

# k = 7 mediods: 
# get mediods to make the cluster definitions
mediods7 <- rownames(comp.stat.mw) [classes.7.smooth$id.med]
comp.stat.mw$cell <- rownames(comp.stat.mw)
index <- comp.stat.mw[comp.stat.mw$cell %in% mediods7,]

# look at the rows that have the mediods
df7 <- comp.stat.mw[comp.stat.mw$cell %in% mediods7,] 
data.frame(index)

old_classes <- classes.7.smooth

# I assigned names to mediod classes based on the highest species % in each mediod:
df7
rem_class <- factor(old_classes$clustering,
                    # relabel the clusters from numbers to custom names
                    labels=c(
                      'Oak/Poplar/Ash', # 1
                      "Elm/Maple/Hickory/Oak/Beech", # 2
                      "Oak", # 3
                      'Tamarack/Spruce/Birch/Pine/Poplar',# mediod 4
                      
                      'Pine/Tamarack/Poplar',# mediod 5
                      
                      
                      
                      'Hemlock/Beech/Cedar/Birch/Maple', # mediod 3
                      "Beech/Maple/Hemlock"
                      # mediod7 # not as much birch
                      
                    ))

classes.7.smooth$silinfo$clus.avg.widths # get the average silohette width for each cluster
clust_plot7 <- data.frame(comp.stat.mw, speciescluster = rem_class)

# map out the clusters with pretty colors & save to a file:
png(width = 7, height = 7, units= 'in',res=300,"outputs/paper_figs/seven_cluster_map_pls_stat_smooth.png")
pls.clust7 <- ggplot(clust_plot7, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c('#fdc087','#beaed4', '#387cb0','#7fc97f','#f0027f','#ffff99','#bf5b17'))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(),legend.key.size = unit(0.7,'lines'),legend.title=element_text(size=10),legend.position = "bottom",legend.direction = "vertical",legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()+ggtitle("PLS species clusters")
pls.clust7 
dev.off()


# save as csv for future 
write.csv(clust_plot, "outputs/seven_clust_pls_dissimilarity_stat_smooth.csv", row.names = FALSE)


# right now code below does not properly work because all density for taxa is displayed as the same
#------------------------------Composition estimated as a function of total density---------------
# open the density draws:
pls.nc <- nc_open(filename = "data/PLS_density_western_v0.999.nc")

# data structure: x = 146, y = 180, sample = 250 MCMC samples
# has x, y, sample for each taxa and for Total density

x <- ncvar_get(pls.nc, "x")
y <- ncvar_get(pls.nc, "y")
n <- ncvar_get(pls.nc, "sample")

total.array <- ncvar_get(pls.nc, "Total") # store the data in a 3-dimensional array
#nc_close(pls.nc) # close the file

dim(total.array)# an x by y by sample array
rownames(total.array) <- x # assign column and row names to the array
colnames(total.array) <- y

# melt arry into a dataframe
total.m <- melt(total.array, varnames=c("x","y","sample"), as.is = TRUE)
# convert x + y to numerics
total.m$x <- as.numeric(total.m$x)
total.m$y <- as.numeric(total.m$y)

# list the names of taxa:
Taxa <- names(pls.nc$var)
nTaxa <- pls.nc$nvars #get the # of taxa

# this for loop extracts the density draws from each taxa and puts into one giant dataframe
for(i in 1:nTaxa){
  
  total.array <- ncvar_get(pls.nc, Taxa[i]) # store the data in a 3-dimensional array
  rownames(total.array) <- x # assign column and row names to the array
  colnames(total.array) <- y
  # melt the array into a dataframe
  total.m <- melt(total.array, varnames=c("x","y","sample"), as.is = TRUE)
  colnames(total.m) <- c("x", "y", "sample", Taxa[i]) # rename the columns
  total.m$x <- as.numeric(total.m$x)
  total.m$y <- as.numeric(total.m$y)
  
  # create a new dataframe & add the extracted samples to it
  if(i == 1){ pls.df <- total.m }else{
    pls.df[,3+i] <- total.m[,Taxa[i]] 
    colnames(pls.df)[3+i] <- Taxa[i]}
  
}

nc_close(pls.nc) # close the ncdf file

# there are alot of NA grid cells for all taxathat we need to remove:
pls.df <- pls.df[!is.na(pls.df$Oak),]

# now pls.df has 250 samples for each gridcell, so lets summarize the mean plsosition draw for each species:
pls.long <- melt(pls.df, id.vars = c("x", "y", "sample")) # convert from wide format to long
# get the mean composition value from all the draws for each spects
pls.stat <- pls.long %>% group_by(x, y, variable) %>% summarise(mean = mean(value, na.rm=TRUE))

# make it wide again:
plsdens.wide <- spread(pls.stat, variable, mean)

ggplot(plsdens.wide, aes(x, y, fill = Beech))+geom_raster()

# save this:
write.csv(plsdens.wide, "data/mean_density_statistical_pls_summary.csv", row.names = FALSE)
plsdens.wide <- read.csv("data/mean_density_statistical_pls_summary.csv")

ordered.df <- plsdens.wide[ , order(names(plsdens.wide))]

#orderd.df <- ordered.df <- select(c(x,y))

ordered.df <- ordered.df %>% dplyr::select(y, everything())
ordered.df <- ordered.df %>% dplyr::select(x, everything())
plsdens.wide <- ordered.df %>% dplyr::select( -Total, everything())
plsdens.wide <- data.frame(plsdens.wide)

# come up with a better solution:
plsdens.wide$calc.total <- rowSums(plsdens.wide[,3:(ncol(plsdens.wide)-1)]) 
plsdens.wide [,3:(ncol(plsdens.wide)-2)] <- plsdens.wide [,3:(ncol(plsdens.wide)-2)]/plsdens.wide[,ncol(plsdens.wide)] # calculate the proportion of the total density that each species takes up
plscomp  <- plsdens.wide [,1:(ncol(plsdens.wide)-2)]
ggplot(plscomp, aes(x,y, fill = Spruce))+geom_raster()


#----------------------k-mediods cluster analysis---------------------------------

# now run pam with 7 classes again:
classes.7.smooth.dens <- pam(plscomp[,3:ncol(plscomp)], k = 7, diss = FALSE, keep.diss = TRUE)
classes.6.smooth.dens <- pam(plscomp[,3:ncol(plscomp)], k = 6, diss = FALSE, keep.diss = TRUE)
classes.8.smooth.dens <- pam(plscomp[,3:ncol(plscomp)], k = 8, diss = FALSE, keep.diss = TRUE)


pls.7class.smooth.dens <- summary(classes.7.smooth.dens) # Avg. Silhouette width = 0.2903842 lower than 9 classes, but the minimum width is 0.2 for all classes
pls.6class.smooth.dens <- summary(classes.6.smooth.dens) # Avg. Silhouette width = 0.2780922
pls.8class.smooth.dens <- summary(classes.8.smooth.dens) # Avg. Silhouette width = 0.3057238


# in general 7 clusters seems to be the best for the full midwest grid level data data, but you may have a different # 
# for this data, it seems that 7-8 classes may be the best cluster

# --------------- Now lets look at a map of the k=8 clusters ----------------------

# k = 8 mediods: 
# get mediods to make the cluster definitions
mediods8 <- rownames(plscomp) [classes.8.smooth.dens$id.med]
plscomp$cell <- rownames(plscomp)
index <- plscomp[plscomp$cell %in% mediods8,]

# look at the rows that have the mediods
df8 <- plscomp[plscomp$cell %in% mediods8,] 
data.frame(index)

old_classes <- classes.8.smooth.dens

# I assigned names to mediod classes based on the highest species % in each mediod:
df8
rem_class <- factor(old_classes$clustering,
                    # relabel the clusters from numbers to custom names
                    labels=c(
                      'Oak/Poplar/Ash', # 1
                      "Oak", # 2
                      "Oak/Maple/Elm/Ash", # 3
                      'Oak/Hickory',# mediod 4
                      'Spruce/Cedar/Tamarack/Poplar',# mediod 5
                      "Pine/Poplar",
                      'Hemlock/Beech/Cedar/Birch/Maple', # mediod 3
                      "Beech/Maple/Hemlock"
                      # mediod8 # not as much birch
                      
                    ))

classes.8.smooth.dens$silinfo$clus.avg.widths # get the average silohette width for each cluster

min(classes.8.smooth.dens$silinfo$clus.avg.widths)

clust_plot8 <- data.frame(plscomp, speciescluster = rem_class)

# map out the clusters with pretty colors & save to a file:
png(width = 8, height = 8, units= 'in',res=300,"outputs/paper_figs/eight_cluster_map_pls_stat_smooth.dens.png")
pls.clust8 <- ggplot(clust_plot8, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c( '#fdc088','#388cb0', '#beaed4',"#33a02c",'#8fc98f','#f0028f', '#ffff99','#bf5b18'))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(),legend.key.size = unit(0.8,'lines'),legend.title=element_text(size=10),legend.position = "bottom",legend.direction = "vertical",legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()+ggtitle("PLS species clusters")
pls.clust8 
dev.off()


# save as csv for future 
write.csv(clust_plot8, "outputs/eight_clust_pls_dissimilarity_stat_smooth.dens.csv", row.names = FALSE)

# summarize average % of each taxa in each cluster:
clusterinfo <- clust_plot8 %>% select(-x, -y, -cell)
clusterinfo.m <- melt(clusterinfo)
clusterinfo.m$value <- clusterinfo.m$value*100
summary.clusters <- clusterinfo.m %>% group_by(speciescluster, variable) %>% summarise(mean = mean(value, na.rm=TRUE),
                                                                                       sd = sd(value, na.rm = TRUE),
                                                                                       sd.low = mean(value, na.rm=TRUE) - sd(value, na.rm=TRUE),
                                                                                       sd.high = mean(value, na.rm=TRUE) + sd(value, na.rm=TRUE))

ggplot(summary.clusters, aes(x = variable, y =mean, fill = variable))+geom_bar(stat= "identity") +
  geom_errorbar(aes(ymin = sd.low, ymax = sd.high))+ facet_wrap(~speciescluster, scales = "free_y")+theme(axis.text.x = element_text(angle = 45, hjust = 1))



silinfo.df <- data.frame(silinfo = classes.8.smooth.dens$silinfo$clus.avg.widths, 
           cluster = 1:8, 
           labels=c(
             'Oak/Poplar/Ash', # 1
             "Oak", # 2
             "Oak/Maple/Elm/Ash", # 3
             'Oak/Hickory',# mediod 4
             'Spruce/Cedar/Tamarack/Poplar',# mediod 5
             "Pine/Poplar",
             'Hemlock/Beech/Cedar/Birch/Maple', # mediod 3
             "Beech/Maple/Hemlock"
             # mediod8 # not as much birch
             
           ))
write.csv(summary.clusters, "outputs/eight_clust_pls_mean_taxa_summary.csv", row.names = FALSE)
write.csv(silinfo.df, "outputs/eight_clust_pls_silinfo_summary.csv", row.names = FALSE)


#------------------------FIA clustering using statistical density estimates---------------------------

# right now code below does not properly work because all density for taxa is displayed as the same
#------------------------------Composition estimated as a function of total density---------------
# open the density draws:
fia.nc <- nc_open(filename = "data/FIA_density_v0.999.nc")

# data structure: x = 146, y = 180, sample = 250 MCMC samples
# has x, y, sample for each taxa and for Total density

x <- ncvar_get(fia.nc, "x")
y <- ncvar_get(fia.nc, "y")
n <- ncvar_get(fia.nc, "sample")

total.array <- ncvar_get(fia.nc, "Total") # store the data in a 3-dimensional array
#nc_close(fia.nc) # close the file

dim(total.array)# an x by y by sample array
rownames(total.array) <- x # assign column and row names to the array
colnames(total.array) <- y

# melt arry into a dataframe
total.m <- melt(total.array, varnames=c("x","y","sample"), as.is = TRUE)
# convert x + y to numerics
total.m$x <- as.numeric(total.m$x)
total.m$y <- as.numeric(total.m$y)

# list the names of taxa:
Taxa <- names(fia.nc$var)
nTaxa <- fia.nc$nvars #get the # of taxa

# this for loop extracts the density draws from each taxa and puts into one giant dataframe
for(i in 1:nTaxa){
  
  total.array <- ncvar_get(fia.nc, Taxa[i]) # store the data in a 3-dimensional array
  rownames(total.array) <- x # assign column and row names to the array
  colnames(total.array) <- y
  # melt the array into a dataframe
  total.m <- melt(total.array, varnames=c("x","y","sample"), as.is = TRUE)
  colnames(total.m) <- c("x", "y", "sample", Taxa[i]) # rename the columns
  total.m$x <- as.numeric(total.m$x)
  total.m$y <- as.numeric(total.m$y)
  
  # create a new dataframe & add the extracted samples to it
  if(i == 1){ fia.df <- total.m }else{
    fia.df[,3+i] <- total.m[,Taxa[i]] 
    colnames(fia.df)[3+i] <- Taxa[i]}
  
}

nc_close(fia.nc) # close the ncdf file

# there are alot of NA grid cells for all taxathat we need to remove:
fia.df <- fia.df[!is.na(fia.df$Oak),]

# now fia.df has 250 samples for each gridcell, so lets summarize the mean fiaosition draw for each species:
fia.long <- melt(fia.df, id.vars = c("x", "y", "sample")) # convert from wide format to long
# get the mean composition value from all the draws for each spects
fia.stat <- fia.long %>% group_by(x, y, variable) %>% summarise(mean = mean(value, na.rm=TRUE))

# make it wide again:
fiadens.wide <- spread(fia.stat, variable, mean)

ggplot(fiadens.wide, aes(x, y, fill = Spruce))+geom_raster()

# save this:
write.csv(fiadens.wide, "data/mean_density_statistical_fia_summary.csv", row.names = FALSE)

ordered.df <- fiadens.wide[ , order(names(fiadens.wide))]

#orderd.df <- ordered.df <- select(c(x,y))

ordered.df <- ordered.df %>% dplyr::select(y, everything())
ordered.df <- ordered.df %>% dplyr::select(x, everything())
fiadens.wide <- ordered.df %>% dplyr::select( -Total, everything())
fiadens.wide <- data.frame(fiadens.wide)

# come up with a better solution:
fiadens.wide$calc.total <- rowSums(fiadens.wide[,3:(ncol(fiadens.wide)-1)]) 
fiadens.wide [,3:(ncol(fiadens.wide)-2)] <- fiadens.wide [,3:(ncol(fiadens.wide)-2)]/fiadens.wide[,ncol(fiadens.wide)] # calculate the proportion of the total density that each species takes up
fiacomp  <- fiadens.wide [,1:(ncol(fiadens.wide)-2)]
ggplot(fiacomp, aes(x,y, fill = Other.hardwood))+geom_raster()+scale_fill_distiller(palette = "Spectral", limits = c(0,1))

# get only the grid cells also in PLS range:

fiacomp <- merge(fiacomp, plscomp[,c("x", "y")], by = c("x", "y"))
ggplot(fiacomp, aes(x,y, fill = Beech))+geom_raster()+scale_fill_distiller(palette = "Spectral", limits = c(0,1))


#----------------------k-mediods cluster analysis---------------------------------

# now run pam with 7 classes again:
classes.3.smooth.dens <- pam(fiacomp[,3:ncol(fiacomp)], k = 3, diss = FALSE, keep.diss = FALSE)
classes.4.smooth.dens <- pam(fiacomp[,3:ncol(fiacomp)], k = 4, diss = FALSE, keep.diss = FALSE)
classes.5.smooth.dens <- pam(fiacomp[,3:ncol(fiacomp)], k = 5, diss = FALSE, keep.diss = FALSE)
classes.6.smooth.dens <- pam(fiacomp[,3:ncol(fiacomp)], k = 6, diss = FALSE, keep.diss = FALSE)
classes.7.smooth.dens <- pam(fiacomp[,3:ncol(fiacomp)], k = 7, diss = FALSE, keep.diss = FALSE)
classes.8.smooth.dens <- pam(fiacomp[,3:ncol(fiacomp)], k = 8, diss = FALSE, keep.diss = FALSE)


fia.3class.smooth.dens <- summary(classes.3.smooth.dens) # avg width = 0.1834982
fia.4class.smooth.dens <- summary(classes.4.smooth.dens) # avg width = 0.2192253
fia.5class.smooth.dens <- summary(classes.5.smooth.dens) # avg width = 0.234389 # highest avg
fia.6class.smooth.dens <- summary(classes.6.smooth.dens) # Avg. Silhouette width = 0.2250767
fia.7class.smooth.dens <- summary(classes.7.smooth.dens) # Avg. Silhouette width =  0.2230915 lower than 9 classes, but the minimum width is 0.2 for all classes
fia.8class.smooth.dens <- summary(classes.8.smooth.dens) # Avg. Silhouette width =0.2065625

# in general 7 clusters seems to be the best for the full midwest grid level data data, but you may have a different # 
# for this data, it seems that 8 classes may be the best cluster

# --------------- Now lets look at a map of the k=8 clusters ----------------------

# k = 5 mediods: 
# get mediods to make the cluster definitions
mediods5 <- rownames(fiacomp) [classes.5.smooth.dens$id.med]
fiacomp$cell <- rownames(fiacomp)
index <- fiacomp[fiacomp$cell %in% mediods5,]

# look at the rows that have the mediods
df5 <- fiacomp[fiacomp$cell %in% mediods5,] 
data.frame(index)

old_classes <- classes.5.smooth.dens

# I assigned names to mediod classes based on the highest species % in each mediod:
df5
rem_class <- factor(old_classes$clustering,
                    # relabel the clusters from numbers to custom names
                    labels=c(
                      'Maple/Oak/Ash/Poplar', # 1
                      "Poplar/Cedar/Pine", # 2
                      "Oak/Maple/Pine/Poplar", # 3
                      'Oak/Maple/Other hardwoods',# mediod 4
                      'Maple/Cedar/Pine'# mediod 5
                      
                      # mediod5 # not as much birch
                      
                    ))

classes.5.smooth.dens$silinfo$clus.avg.widths # get the average silohette width for each cluster
clust_plot5 <- data.frame(fiacomp, speciescluster = rem_class)

ggplot(clust_plot5, aes(x = x, y=y, fill=speciescluster))+geom_raster()

# map out the clusters with pretty colors & save to a file:
png(width = 8, height = 8, units= 'in',res=300,"outputs/paper_figs/five_cluster_map_fia_stat_smooth.dens.png")
fia.clust5 <- ggplot(clust_plot5, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c('#f0027f'  ,'#fdc086','#a6cee3',"#beaed4",'#003c30'), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(),legend.key.size = unit(0.8,'lines'),
                                                                                                              legend.title=element_text(size=10),legend.position = "bottom",legend.direction = "vertical",legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()
fia.clust5
dev.off()


# save as csv for future 
write.csv(clust_plot5, "outputs/five_clust_fia_dissimilarity_stat_smooth.dens.csv", row.names = FALSE)

# the smoothed estimates are very smooth......

# summarize average % of each taxa in each cluster:
clusterinfo <- clust_plot5 %>% select(-x, -y, -cell)
clusterinfo.m <- melt(clusterinfo)
clusterinfo.m$value <- clusterinfo.m$value*100
summary.clusters.fia <- clusterinfo.m %>% group_by(speciescluster, variable) %>% summarise(mean = mean(value, na.rm=TRUE),
                                                                                       sd = sd(value, na.rm = TRUE),
                                                                                       sd.low = mean(value, na.rm=TRUE) - sd(value, na.rm=TRUE),
                                                                                       sd.high = mean(value, na.rm=TRUE) + sd(value, na.rm=TRUE))

ggplot(summary.clusters.fia, aes(x = variable, y =mean, fill = variable))+geom_bar(stat= "identity") +
  geom_errorbar(aes(ymin = sd.low, ymax = sd.high))+ facet_wrap(~speciescluster, scales = "free_y")+theme(axis.text.x = element_text(angle = 45, hjust = 1))



silinfo.df <- data.frame(silinfo = classes.5.smooth.dens$silinfo$clus.avg.widths, 
                         cluster = 1:5, 
                         labels=c(
                           'Maple/Oak/Ash/Poplar', # 1
                           "Poplar/Cedar/Pine", # 2
                           "Oak/Maple/Pine/Poplar", # 3
                           'Oak/Maple/Other hardwoods',# mediod 4
                           'Maple/Cedar/Pine'# mediod 5
                         ))
write.csv(summary.clusters.fia, "outputs/five_clust_fia_mean_taxa_summary.csv", row.names = FALSE)
write.csv(silinfo.df, "outputs/five_clust_fia_silinfo_summary.csv", row.names = FALSE)




#-------------------------------Run clusters with both FIA and pls composition together--------------

# read in pls and fia:
plsdens.wide <- read.csv( "data/mean_density_statistical_pls_summary.csv")
fiadens.wide <- read.csv( "data/mean_density_statistical_fia_summary.csv")

addtofia <- colnames(plsdens.wide)[!colnames(plsdens.wide) %in% colnames(fiadens.wide)]
addtopls <- colnames(fiadens.wide)[!colnames(fiadens.wide) %in% colnames(plsdens.wide)]

plsdens.wide[,addtopls] <- 0
fiadens.wide[,addtofia] <- 0


ordered.df <- plsdens.wide[ , order(names(plsdens.wide))]

#orderd.df <- ordered.df <- select(c(x,y))

ordered.df <- ordered.df %>% dplyr::select(y, everything())
ordered.df <- ordered.df %>% dplyr::select(x, everything())
plsdens.wide <- ordered.df %>% dplyr::select( -Total, everything())
plsdens.wide <- data.frame(plsdens.wide)

# come up with a better solution:
plsdens.wide$calc.total <- rowSums(plsdens.wide[,3:(ncol(plsdens.wide)-1)]) 
plsdens.wide [,3:(ncol(plsdens.wide)-2)] <- plsdens.wide [,3:(ncol(plsdens.wide)-2)]/plsdens.wide[,ncol(plsdens.wide)] # calculate the proportion of the total density that each species takes up
plscomp  <- plsdens.wide [,1:(ncol(plsdens.wide)-2)]
ggplot(plscomp, aes(x,y, fill = Other.hardwood))+geom_raster()+scale_fill_distiller(palette = "Spectral", limits = c(0,1))






ordered.df <- fiadens.wide[ , order(names(fiadens.wide))]

#orderd.df <- ordered.df <- select(c(x,y))

ordered.df <- ordered.df %>% dplyr::select(y, everything())
ordered.df <- ordered.df %>% dplyr::select(x, everything())
fiadens.wide <- ordered.df %>% dplyr::select( -Total, everything())
fiadens.wide <- data.frame(fiadens.wide)

# come up with a better solution:
fiadens.wide$calc.total <- rowSums(fiadens.wide[,3:(ncol(fiadens.wide)-1)]) 
fiadens.wide [,3:(ncol(fiadens.wide)-2)] <- fiadens.wide [,3:(ncol(fiadens.wide)-2)]/fiadens.wide[,ncol(fiadens.wide)] # calculate the proportion of the total density that each species takes up
fiacomp  <- fiadens.wide [,1:(ncol(fiadens.wide)-2)]
ggplot(fiacomp, aes(x,y, fill = Other.hardwood))+geom_raster()+scale_fill_distiller(palette = "Spectral", limits = c(0,1))

# get only the grid cells also in PLS range:

fiacomp <- merge(fiacomp, plscomp[,c("x", "y")], by = c("x", "y"))
ggplot(fiacomp, aes(x,y, fill = Other.hardwood))+geom_raster()+scale_fill_distiller(palette = "Spectral", limits = c(0,1))

# now compbine the two dfs:
colnames(fiacomp)
colnames(plscomp)

fiacomp$period <- "FIA"
plscomp$period <- "PLS"

both.comps <- rbind(plscomp, fiacomp)

# now run pam with 7 classes again:
classes.9.smooth.dens <- pam(both.comps[,3:(ncol(both.comps)-1)], k = 9, diss = FALSE, keep.diss = FALSE)
classes.10.smooth.dens <- pam(both.comps[,3:(ncol(both.comps)-1)], k = 10, diss = FALSE, keep.diss = FALSE)
classes.5.smooth.dens <- pam(both.comps[,3:(ncol(both.comps)-1)], k = 5, diss = FALSE, keep.diss = FALSE)
classes.6.smooth.dens <- pam(both.comps[,3:(ncol(both.comps)-1)], k = 6, diss = FALSE, keep.diss = FALSE)
classes.7.smooth.dens <- pam(both.comps[,3:(ncol(both.comps)-1)], k = 7, diss = FALSE, keep.diss = FALSE)
classes.8.smooth.dens <- pam(both.comps[,3:(ncol(both.comps)-1)], k = 8, diss = FALSE, keep.diss = FALSE)
classes.11.smooth.dens <- pam(both.comps[,3:(ncol(both.comps)-1)], k = 11, diss = FALSE, keep.diss = FALSE)
classes.12.smooth.dens <- pam(both.comps[,3:(ncol(both.comps)-1)], k = 12, diss = FALSE, keep.diss = FALSE)


both.9class.smooth.dens <- summary(classes.9.smooth.dens) # avg width = 0.2273168
both.10class.smooth.dens <- summary(classes.10.smooth.dens) # avg width = 0.2371311
both.11class.smooth.dens <- summary(classes.11.smooth.dens) # avg width = 0.2262053
both.12class.smooth.dens <- summary(classes.12.smooth.dens) # avg width = 0.2240107

both.5class.smooth.dens <- summary(classes.5.smooth.dens) # avg width = 0.199837 # highest avg
both.6class.smooth.dens <- summary(classes.6.smooth.dens) # Avg. Silhouette width = 0.2069075
both.7class.smooth.dens <- summary(classes.7.smooth.dens) # Avg. Silhouette width =  0.2276273 lower than 9 classes, but the minimum width is 0.2 for all classes
both.8class.smooth.dens <- summary(classes.8.smooth.dens) # Avg. Silhouette width = 0.2427202

#------------------------looks like 8 classes fits the data best:


# k = 8 mediods: 
# get mediods to make the cluster definitions
mediods8 <- rownames(both.comps) [classes.8.smooth.dens$id.med]
both.comps$cell <- rownames(both.comps)
index <- both.comps[both.comps$cell %in% mediods8,]

# look at the rows that have the mediods
df8 <- both.comps[both.comps$cell %in% mediods8,] 
data.frame(index)

old_classes <- classes.8.smooth.dens

# I assigned names to mediod classes based on the highest species % in each mediod:
df8
rem_class <- factor(old_classes$clustering,
                    # relabel the clusters from numbers to custom names
                    labels=c(
                      'Poplar/Oak-FIA', # 1
                      "Oak/Maple/Ash/Poplar-FIA", # 2
                      "Oak-PLS", # 3
                      "Oak/Maple/Other/Hickory-FIA", #4
                      "Pine/Tamarack/Fir/Hemlock-PLS", # 5
                      
                      "Poplar/Pine/Maple/Cedar/Spruce-FIA", # 6
                      "Maple/Poplar/Cedar-FIA", # 7
                      
                      'Beech/Maple-PLS'# mediod 8
                      
                      # mediod5 # not as much birch
                      
                    ))

classes.8.smooth.dens$silinfo$clus.avg.widths # get the average silohette width for each cluster
clust_plot8 <- data.frame(both.comps, speciescluster = rem_class)

ggplot(clust_plot8, aes(x = x, y=y, fill=speciescluster))+geom_raster()+facet_wrap(~period)

# map out the clusters with pretty colors & save to a file:
png(width = 8, height = 8, units= 'in',res=300,"outputs/paper_figs/8_cluster_map_fia_pls_stat_smooth.dens.png")
all.clust8 <- ggplot(clust_plot8, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c('#f0027f'  ,'#fdc086','#a6cee3',"#beaed4",'#003c30',"#666666", "#e6ab02", "#d95f02"), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(),legend.key.size = unit(0.8,'lines'),
                                                                                                              legend.title=element_text(size=10),legend.position = "bottom",legend.direction = "vertical",legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()+facet_wrap(~period)
all.clust8
dev.off()


# save as csv for future 
write.csv(clust_plot5, "outputs/five_clust_fia_dissimilarity_stat_smooth.dens.csv", row.names = FALSE)

#------------------------looks like 10 classes fits the data pretty well-------------
# choose ten if you are maximizing both the average and minimum silhoutte width:
# look at the predictions for 10 clusters because it has both high min silohette width and high avg sihloette width:


# k = 10 mediods: 
# get mediods to make the cluster definitions
mediods10 <- rownames(both.comps) [classes.10.smooth.dens$id.med]
both.comps$cell <- rownames(both.comps)
index <- both.comps[both.comps$cell %in% mediods10,]

# look at the rows that have the mediods
df10 <- both.comps[both.comps$cell %in% mediods10,] 
data.frame(index)

old_classes <- classes.10.smooth.dens

# I assigned names to mediod classes based on the highest species % in each mediod:
df10
rem_class <- factor(old_classes$clustering,
                    # relabel the clusters from numbers to custom names
                    labels=c(
                      'Poplar/Oak-FIA', # 1
                      "Oak/Maple/Ash/Poplar-FIA", # 2
                      "Oak-PLS", # 3
                      "Oak/Hickory/Elm/Maple-FIA", #4
                      "Oak/Maple/Other/Hickory-FIA", # 5
                      "Poplar/Pine/Tamarack/Fir-PLS", # 6
                      
                      "Pine/Maple/Poplar/Oak/Ash-FIA", # 7
                      'Maple/Cedar/Poplar-FIA',# 8
                      "Hemlock/Cedar/Maple-PLS", # 9
                      "Beech/Maple/Pine-PLS"
                     
                      
                      # mediod5 # not as much birch
                      
                    ))

classes.10.smooth.dens$silinfo$clus.avg.widths # get the average silohette width for each cluster
clust_plot10 <- data.frame(both.comps, speciescluster = rem_class)

ggplot(clust_plot10, aes(x = x, y=y, fill=speciescluster))+geom_raster()+facet_wrap(~period)

# map out the clusters with pretty colors & save to a file:
png(width = 10, height = 10, units= 'in',res=300,"outputs/paper_figs/10_cluster_map_fia_pls_stat_smooth.dens.png")
all.clust10 <- ggplot(clust_plot10, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c('#ff7f00'  ,'#fdc086','#386cb0',"#beaed4",'#a6cee3',"#7fc97f", "#f0027f", "#004529", "#ffff99", "#b15928"), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(),legend.key.size = unit(0.10,'lines'),
                                                                                                              legend.title=element_text(size=10),legend.position = "bottom",legend.direction = "vertical",legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()+facet_wrap(~period)
all.clust10
dev.off()


# save as csv for future 
write.csv(clust_plot10, "outputs/ten_clust_combined_dissimilarity_stat_smooth.dens.csv", row.names = FALSE)




# what happens if we do the same thing, but with the raw data estimates:
#------------------------FIA with smooth composition & masked cells where we don't have data----------------
# read in old fia density data:
dens.pr <- read.csv("data/PLS_FIA_density_climate_full.csv")
dens.fia.cells <- dens.pr[!is.na(dens.pr$FIAdensity),]
fiacomp.msk <- merge(dens.fia.cells[,c("x", "y")], fiacomp, by = c("x", "y"))
ggplot(fiacomp.msk, aes(x,y, fill = Oak))+geom_raster() # check to make sure it msked out cells where we have no FIA plots

fiacomp.msk <- fiacomp.msk[,-ncol(fiacomp.msk)]
# lets do the cluster analysis on this and see what the best clustering is:

# now run pam with 7 classes again:
classes.3.smooth.dens.msk <- pam(fiacomp.msk[,3:ncol(fiacomp.msk)], k = 3, diss = FALSE, keep.diss = FALSE)
classes.4.smooth.dens.msk <- pam(fiacomp.msk[,3:ncol(fiacomp.msk)], k = 4, diss = FALSE, keep.diss = FALSE)
classes.5.smooth.dens.msk <- pam(fiacomp.msk[,3:ncol(fiacomp.msk)], k = 5, diss = FALSE, keep.diss = FALSE)
classes.6.smooth.dens.msk <- pam(fiacomp.msk[,3:ncol(fiacomp.msk)], k = 6, diss = FALSE, keep.diss = FALSE)
classes.7.smooth.dens.msk <- pam(fiacomp.msk[,3:ncol(fiacomp.msk)], k = 7, diss = FALSE, keep.diss = FALSE)
classes.8.smooth.dens.msk <- pam(fiacomp.msk[,3:ncol(fiacomp.msk)], k = 8, diss = FALSE, keep.diss = FALSE)


fia.3class.smooth.dens.msk <- summary(classes.3.smooth.dens.msk) # avg width = 0.2973809
fia.4class.smooth.dens.msk <- summary(classes.4.smooth.dens.msk) # avg width = 0.2864922
fia.5class.smooth.dens.msk <- summary(classes.5.smooth.dens.msk) # avg width =  0.2490878 # highest avg
fia.6class.smooth.dens.msk <- summary(classes.6.smooth.dens.msk) # Avg. Silhouette width = 0.2394792
fia.7class.smooth.dens.msk <- summary(classes.7.smooth.dens.msk) # Avg. Silhouette width = 0.2428703 lower than 9 classes, but the minimum width is 0.2 for all classes
fia.8class.smooth.dens.msk <- summary(classes.8.smooth.dens.msk) # Avg. Silhouette width = 0.236274

# in general 3-4 clusters seems to be the best for the full midwest grid level data data, but you may have a different # 
# for this data, it seems that 4 classes may be the best cluster bc/ they match previous clustesr



# --------------- Now lets look at a map of the k=4 clusters ----------------------

# k = 4 mediods: 
# get mediods to make the cluster definitions
mediods4 <- rownames(fiacomp.msk) [classes.4.smooth.dens.msk$id.med]
fiacomp.msk$cell <- rownames(fiacomp.msk)

# look at the rows that have the mediods
df4 <- fiacomp.msk[fiacomp.msk$cell %in% mediods4,] 


old_classes <- classes.4.smooth.dens.msk

# I assigned names to mediod classes based on the highest species % in each mediod:
df4
rem_class <- factor(old_classes$clustering,
                    # relabel the clusters from numbers to custom names
                    labels=c(
                      'Oak/Maple/Ash/Pine/Poplar', # 1
                      "Poplar/Maple/Cedar/Fir", # 2
                      "Maple/Poplar", # 3
                      'Pine/Maple/Poplar'# mediod 4
                      
                      
                      # mediod4 # not as much birch
                      
                    ))

classes.4.smooth.dens.msk$silinfo$clus.avg.widths # get the average silohette width for each cluster
clust_plot4 <- data.frame(fiacomp.msk, speciescluster = rem_class)

ggplot(clust_plot4, aes(x = x, y=y, fill=speciescluster))+geom_raster()

# map out the clusters with pretty colors & save to a file:
png(width = 8, height = 8, units= 'in',res=300,"outputs/paper_figs/four_cluster_map_fia_stat_smooth.dens.msk.png")
fia.clust4 <- ggplot(clust_plot4, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c( '#a6cee3','#fdc086','#003c30','#f0027f' ), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(),legend.key.size = unit(0.8,'lines'),
                                                                                                              legend.title=element_text(size=10),legend.position = "bottom",legend.direction = "vertical",legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()
fia.clust4
dev.off()


# save as csv for future 
write.csv(clust_plot4, "outputs/four_clust_fia_dissimilarity_stat_smooth.dens.msk.csv", row.names = FALSE)








#----------------------- FIA old code ---------------


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

pls.6class.sum # 0.05856274 0.28633008 0.73919132 0.51385646 0.14746533 0.29704184


summary(classes.8) # Avg. Silhouette width = 0.2352843
summary(classes.7) # Avg. Silhouette width = 0.2525737
#average per cluster: 0.04640113 0.27235871 0.74034213 0.55688143 0.18348651 0.19091193 0.38198172
summary(classes.6) # Avg. Silhouette width = 0.2362255
#average per cluster:  0.40223850 -0.04396843  0.51868225  0.47351622  0.46875376  0.34022366
summary(classes.5) # Avg. Silhouette width = 0.2139102
#average per cluster: 0.41302806 -0.03862057  0.53192545  0.47751488  0.46735263
summary(classes.4) # Avg. Silhouette width = 0.2081704
#average per cluster:  0.43084298 0.07111234 0.49014174 0.47601499
summary(classes.3) # Avg. Silhouette width = 0.1584407  
#average per cluster: 0.44771444 0.05662039 0.42821212
fcomps$idvar <- 1:nrow(fcomps)

#mediods <- fcomps[fcomps$idvar %in%  classes.5$medoids,]
#mediods <- fcomps$idvar [classes.5$id.med]
mediods4 <- fcomps$cell [classes.4$id.med]
index <- rownames(fcomps[fcomps$cell %in% mediods4,])


#index <- fcomps[fcomps$cell %in% mediods,]$idvar
brays.f <- vegdist(fcomps[,4:39], method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
                 na.rm = FALSE) 
brays.f2 <- as.matrix(brays.f)

diss.f4.dissimilarity <- brays.f2[,index]

df4 <- fcomps[fcomps$cell %in% mediods4,] # look at the rows that have the mediods

old_classes <- classes.4

#[1] 1292 2201 4618 4978 4604# idvars of the mediods
rem_class4 <- factor(old_classes$clustering,
                    labels=c(  'Oak/Maple',
                               'Maple/Ash/Birch/Aspen/Oak', # 2
                             
                              
                            'Aspen',#3
                           "Pine/Aspen" #4
                           ))

rem_class4 <- factor(old_classes$clustering,
                     labels=c( 'Oak/Maple',
                               'Maple/Ash/Birch/Aspen', # 2
                               
                               
                               'Aspen',#3
                               "Pine/Poplar" 
                              
                              
                     ))

clust_plot4 <- data.frame(fcomps, 
                          speciescluster = rem_class4,
                          clustNum = as.numeric(rem_class4),
                          diss1 = diss.f4.dissimilarity[,1],
                          diss2 = diss.f4.dissimilarity[,2],
                          diss3 = diss.f4.dissimilarity[,3],
                          diss4 = diss.f4.dissimilarity[,4])

ggplot(clust_plot4, aes(x = x, y=y, fill=speciescluster))+geom_raster()


png(width = 6, height = 6, units= 'in',res=300,"outputs/paper_figs/four_cluster_map_fia.png")
fia.clust<- ggplot(clust_plot4, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c('#ffff33', '#4daf4a','#984ea3','#ff7f00'), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(), legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.position = c(0.205, 0.32),legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()+ggtitle("FIA clusters")
fia.clust 

dev.off()

write.csv(clust_plot4, "outputs/cluster/density_fia_with_clusters.csv", row.names = FALSE)
# reassign names

fia.silinfo <- data.frame(cluster = c('Oak/Maple',
                       'Maple/Ash/Birch/Aspen', # 2
                       'Aspen',#3
                       "Pine/Poplar" ), silwidths = classes.4$silinfo$clus.avg.widths)
fia.silplot<- ggplot(fia.silinfo, aes(cluster, silwidths, fill = cluster))+geom_bar(stat = "identity")+ylim(0,1)+theme_bw()+ylab("Average Cluster Silhouette Width")+ggtitle("FIA Clusters")+
  scale_fill_manual(values = c( '#ffff33', '#4daf4a','#984ea3','#ff7f00'), name = " ")+theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

# they are all equally dissimilar
#0.40663776 -0.04516793  0.51842603  0.47118307  0.46843040  0.33582488

pls.silinfo <- data.frame(cluster = c("Oak/Hickory",
                                      "Beech/Maple",
                                      'Oak', #and ASH
                                      'N. Mixed Forest', # mediod 3
                                      'Aspen', # mediod 4
                                      'Pine',# mediod 5
                                      'Boreal/Sub-Boreal' ), silwidths = pls.7class.summ$silinfo$clus.avg.widths)
pls.silplot <- ggplot(pls.silinfo, aes(cluster, silwidths, fill = cluster))+geom_bar(stat = "identity")+ylim(0,1)+theme_bw()+ylab("Average Cluster Silhouette Width")+ggtitle("PLS Clusters")+
  scale_fill_manual(values = c('#fdc086', '#bf5b17','#7fc97f','#ffff99',  '#386cb0','#beaed4','#f0027f'), name = " ")+theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

png(width = 8, height = 4, units = "in",res = 300, "outputs/silhoutte_plots_comp.png")
grid.arrange(fia.silplot, pls.silplot, ncol = 2)
dev.off()


# 6 classes
mediods <- fcomps$cell [classes.6$id.med]
#mediods
#[1] 0.43084298 0.07111234 0.49014174 0.47601499
index <- rownames(fcomps[fcomps$cell %in% mediods,])

#index <- fcomps[fcomps$cell %in% mediods,]$idvar
#brays.f <- vegdist(fcomps[,4:39], method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
 #                  na.rm = FALSE) 
#brays.f2 <- as.matrix(brays.f)

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
