# make figures for the paper & coduct bimodality analyses:
# migrated some of this code from bimodality_draft_3.1.17

library(gridExtra)
library(grid)
library(ggplot2)
library(maps)
library(sp)
library(plyr)
library(maps)

# read in density + climate data:
dens.pr <- read.csv("data/PLS_FIA_density_climate_full.csv")


# -------------------figure 1 A: Map of pls bimodality

# need to set up state outlines:
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota', 'wisconsin', 'michigan', "illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)



# fia and pls plots
sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.pr, aes(x=x, y=y, fill = PLSdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,600), name ="Tree \n Density", na.value = 'darkgrey') +
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                                legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                
                                                axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


png("/Users/kah/Documents/bimodality/outputs/paper_figs/PLS_density_map_full.png")
pls.map
dev.off()






# B: Species clusters:
# read in the species classifications from" Species_clustering.R"
clust_plot7 <- read.csv("outputs/seven_clust_pls_dissimilarity.csv")

# merge the clusters and pls density data: 
clust_7<- merge(clust_plot7, dens.pr, by = c("x", "y", "cell"))

# need to rename the clusters here (should go back and do it in the place where we originally make the clusters):
clust_7
library(plyr)
clust_7$foresttype<- revalue(clust_7$speciescluster, c("Poplar"="Aspen", "Elm/Maple/Hickory/Oak/Beech"="Oak-Hickory", "Oak" = "Oak", 
                                                               "Hemlock/Beech/Cedar/Birch/Maple" = "N. Mixed Forest", 
                                                               "Pine/Tamarack/Poplar" = "Pine", "Tamarack/Spruce/Birch/Pine/Poplar" = "Boreal/Sub-boreal", "Beech/Maple/Hemlock" = "Beech-Maple"))

clust_7$orderedforesttype<- factor(clust_7$foresttype, c("Oak", "Pine", "Aspen", "N. Mixed Forest", "Boreal/Sub-boreal", "Oak-Hickory", "Beech-Maple"))

pls.clust <- ggplot(clust_7, aes(x = x, y=y, fill=orderedforesttype))+geom_raster()+
  scale_fill_manual(values = c('#386cb0', '#f0027f','#fdc086','#ffff99','#7fc97f','#beaed4', '#bf5b17'), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.20, 0.18),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                               axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()

pls.clust
# merge clust_plot6 and dens.pr

dens.clust <- merge(dens.pr, clust_7[,c("x" ,"y", "cell", "speciescluster", "foresttype")], by = c("x", "y", "cell"), all.x = TRUE)

png("outputs/cluster/density_vs_envt_pc1_by_species_cluster.png")
ggplot(dens.clust, aes(PC1, PLSdensity, color = speciescluster))+geom_point()
dev.off()


dens.clust.m <- melt(dens.clust[,c("x", "y", "cell",  "foresttype", "PC1", "PLSdensity")], id.vars = c("x", "y", "cell", "PC1","PLSdensity" ))

png("outputs/paper_figs/density_by_7_spec_clusters.png")
ggplot(na.omit(dens.clust.m), aes(PLSdensity))+geom_histogram()+facet_wrap(~value)
dev.off()

# what areas of the PC1 environment have the highest p(bimodality?)
dens.clust$PC1bins <- cut(dens.clust$PC1, breaks = seq(-5,5.5, by = 1), labels = label.breaks(-5,4.5, 1))

dens.clust.mpc1 <- melt(dens.clust[,c("x", "y", "cell",  "foresttype", "PC1", "PLSdensity", "PC1bins")], id.vars = c("x", "y", "cell", "PC1","PLSdensity", "foresttype" ))
dens.clust.mpc1$PC_1bins_o <- factor(dens.clust.mpc1$value, c("-5 - -4", "-4 - -3", "-3 - -2", "-2 - -1",
                                                              "-1 - 0",  "0 - 1" ,"1 - 2",   "2 - 3" ,  "3 - 4", "4 - 5", NA))

png("outputs/paper_figs/PLS_hist_bins_by_envt_species_cluster.png")
ggplot(na.omit(dens.clust.mpc1), aes(PLSdensity, fill = foresttype))+geom_histogram()+facet_wrap(~PC_1bins_o)+scale_fill_manual(values = c('#bf5b17', '#beaed4','#ffff99','#386cb0','#f0027f', '#fdc086','#7fc97f'), name = " ")
dev.off()

# C: Density vs PC1 as a hexbin plot


pls.dens.pc1.hex <- ggplot(data = dens.clust, aes(PC1, PLSdensity)) +geom_hex() + 
  theme_bw(base_size = 8)+scale_fill_distiller(palette = "Spectral", limits = c(1,135))+
  xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+geom_vline(xintercept = -2.5)+geom_vline(xintercept = 1)+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
                                                                                                                                                                               legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                                                                                               legend.key.size = unit(0.35, "line"),legend.title = element_text(size = 8), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
pls.dens.pc1.hex

# D: Histogram colored by species cluster:
# make a histogram of denisty betwen -2.5 and 1 colored by species cluster:

# need to reorder the factors:
dens.clust$foresttype_ordered<- factor(dens.clust$foresttype, levels = rev(c("Boreal/Sub-boreal", "Pine", "Aspen", "Beech-Maple", "N. Mixed Forest", "Oak", "Oak-Hickory")))
dens.clust.omit <- dens.clust[ !is.na(dens.clust$foresttype_ordered),]

clust.hist <- ggplot()+ geom_density(data = dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < 1 & dens.clust$PLSdensity > 0.5, ], aes(PLSdensity, 23 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_histogram(data = dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < 1 & dens.clust$PLSdensity > 0.5, ], aes(PLSdensity, fill = foresttype_ordered, binwidth = 30))+xlim(0,600)+
  scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+coord_flip()+ylim(0,700)+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.65, 0.75),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.35, "line"),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
clust.hist




# E: map of bimodal regions:
# sample the p(forest) at each environmental bitn

pls.prob.forest <- read.csv("outputs/posterior_prob_forest_pls.csv") # from sample_density_probabilities script


cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

p.forest.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.prob.forest, aes(x=x, y=y, fill = pforest))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= cbpalette, labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.forest.map

                                                                                                                                                                  




# write out new figure 1 to a png
png(height = 14, width = 10, units = 'in', res = 300, "outputs/paper_figs/fig1_full_mi.png")
grid.arrange(pls.map + annotate("text", x=-90000, y=1486000,label= "A", size = 5), 
             pls.clust + annotate("text", x=-90000, y=1486000,label= "B", size = 5), 
             pls.dens.pc1.hex + annotate("text", x= 4, y=600,label= "C", size = 5), 
             clust.hist + annotate("text", x=600, y=15,label= "D", size = 5), 
             p.forest.map + annotate("text", x=-90000, y=1486000,label= "E", size = 5), ncol = 2)
dev.off()

# ----------------------------PLS plots for grid cells that also exist in FIA:
dens.pr <- read.csv("data/PLS_FIA_density_climate_full.csv")


# -------------------figure 1 A: Map of pls bimodality

# need to set up state outlines:
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota', 'wisconsin', 'michigan', "illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)



# fia and pls plots
sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

dens.pr<- dens.pr[!is.na(dens.pr$FIAdensity),]

pls.map.nona <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.pr, aes(x=x, y=y, fill = PLSdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,600), name ="Tree \n Density", na.value = 'darkgrey') +
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


png("/Users/kah/Documents/bimodality/outputs/paper_figs/PLS_density_map_nona.png")
pls.map.nona
dev.off()






# B: Species clusters:
# read in the species classifications from" Species_clustering.R"
clust_plot7 <- read.csv("outputs/seven_clust_pls_dissimilarity.csv")

# merge the clusters and pls density data: 
clust_7<- merge(clust_plot7, dens.pr, by = c("x", "y", "cell"))

# need to rename the clusters here (should go back and do it in the place where we originally make the clusters):
clust_7
library(plyr)
clust_7$foresttype<- revalue(clust_7$speciescluster, c("Poplar"="Aspen", "Elm/Maple/Hickory/Oak/Beech"="Oak-Hickory", "Oak" = "Oak", 
                                                       "Hemlock/Beech/Cedar/Birch/Maple" = "N. Mixed Forest", 
                                                       "Pine/Tamarack/Poplar" = "Pine", "Tamarack/Spruce/Birch/Pine/Poplar" = "Boreal/Sub-boreal", "Beech/Maple/Hemlock" = "Beech-Maple"))

clust_7$orderedforesttype<- factor(clust_7$foresttype, c("Oak", "Pine", "Aspen", "N. Mixed Forest", "Boreal/Sub-boreal", "Oak-Hickory", "Beech-Maple"))

pls.clust.nona <- ggplot(clust_7, aes(x = x, y=y, fill=orderedforesttype))+geom_raster()+
  scale_fill_manual(values = c('#386cb0', '#f0027f','#fdc086','#ffff99','#7fc97f','#beaed4', '#bf5b17'), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.20, 0.18),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                                           axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),axis.text.x=element_blank(),
                                                                                                                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                           axis.title.x=element_blank(),
                                                                                                                           axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()

pls.clust.nona
# merge clust_plot6 and dens.pr

dens.clust.nona <- merge(dens.pr, clust_7[,c("x" ,"y", "cell", "speciescluster", "foresttype")], by = c("x", "y", "cell"), all.x = TRUE)

png("outputs/cluster/density_vs_envt_pc1_by_species_cluster_nona.png")
ggplot(dens.clust, aes(PC1, PLSdensity, color = speciescluster))+geom_point()
dev.off()


dens.clust.m <- melt(dens.clust[,c("x", "y", "cell",  "foresttype", "PC1", "PLSdensity")], id.vars = c("x", "y", "cell", "PC1","PLSdensity" ))

png("outputs/paper_figs/density_by_7_spec_clusters_nona.png")
ggplot(na.omit(dens.clust.m), aes(PLSdensity))+geom_histogram()+facet_wrap(~value)
dev.off()

# what areas of the PC1 environment have the highest p(bimodality?)
dens.clust$PC1bins <- cut(dens.clust$PC1, breaks = seq(-5,5.5, by = 1), labels = label.breaks(-5,4.5, 1))

dens.clust.mpc1 <- melt(dens.clust[,c("x", "y", "cell",  "foresttype", "PC1", "PLSdensity", "PC1bins")], id.vars = c("x", "y", "cell", "PC1","PLSdensity", "foresttype" ))
dens.clust.mpc1$PC_1bins_o <- factor(dens.clust.mpc1$value, c("-5 - -4", "-4 - -3", "-3 - -2", "-2 - -1",
                                                              "-1 - 0",  "0 - 1" ,"1 - 2",   "2 - 3" ,  "3 - 4", "4 - 5", NA))

png("outputs/paper_figs/PLS_hist_bins_by_envt_species_cluster_nona.png")
ggplot(na.omit(dens.clust.mpc1), aes(PLSdensity, fill = foresttype))+geom_histogram()+facet_wrap(~PC_1bins_o)+scale_fill_manual(values = c('#bf5b17', '#beaed4','#ffff99','#386cb0','#f0027f', '#fdc086','#7fc97f'), name = " ")
dev.off()

# C: Density vs PC1 as a hexbin plot


pls.dens.pc1.hex.nona <- ggplot(data = dens.clust, aes(PC1, PLSdensity)) +geom_hex() + 
  theme_bw(base_size = 8)+scale_fill_distiller(palette = "Spectral", limits = c(1,135))+
  xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+geom_vline(xintercept = -2.5)+geom_vline(xintercept = 1)+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
                                                                                                                                                                               legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                                                                                               legend.key.size = unit(0.35, "line"),legend.title = element_text(size = 8), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
pls.dens.pc1.hex.nona

# D: Histogram colored by species cluster:
# make a histogram of denisty betwen -2.5 and 1 colored by species cluster:

# need to reorder the factors:
dens.clust.nona$foresttype_ordered<- factor(dens.clust.nona$foresttype, levels = rev(c("Boreal/Sub-boreal", "Pine", "Aspen", "Beech-Maple", "N. Mixed Forest", "Oak", "Oak-Hickory")))
dens.clust.omit <- dens.clust.nona[ !is.na(dens.clust.nona$foresttype_ordered),]

clust.hist.nona <- ggplot()+ geom_density(data = dens.clust.nona[dens.clust.nona$PC1 > -2.5 & dens.clust.nona$PC1 < 1 & dens.clust.nona$PLSdensity > 0.5, ], aes(PLSdensity, 23 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_histogram(data = dens.clust.nona[dens.clust.nona$PC1 > -2.5 & dens.clust.nona$PC1 < 1 & dens.clust.nona$PLSdensity > 0.5, ], aes(PLSdensity, fill = foresttype_ordered, binwidth = 30))+xlim(0,600)+
  scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+coord_flip()+ylim(0,700)+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.65, 0.75),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.35, "line"),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
clust.hist.nona




# E: map of bimodal regions:
# sample the p(forest) at each environmental bitn

pls.prob.forest.nonag <- read.csv("outputs/posterior_prob_forest_pls_non_ag.csv") # from sample_density_probabilities script


cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

p.forest.map.nona <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.prob.forest.nonag, aes(x=x, y=y, fill = pforest))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= cbpalette, labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.forest.map.nona





#--------------------------FIA plots------------------------
# read in FIA data with bimodality assigned within each environmental space using PC1 +/- 0.25 units
fia.bim <- read.csv("data/PLS_FIA_density_climate_full.csv")

# A: map of density
fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fia.bim, aes(x=x, y=y, fill = FIAdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,600), name ="Tree \n Density", na.value = 'darkgrey') +
  coord_equal()+theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text.x=element_blank(),
                                               legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                               axis.title.x=element_blank(),
                                               axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")

png("/Users/kah/Documents/bimodality/outputs/paper_figs/PLS_density_map.png")
fia.map + annotate("text", x=-90000, y=1486000,label= "B", size = 5)
dev.off()

# B: Map of species clusters

# from species_clustering.R:
species.clust <- read.csv("outputs/cluster/density_fia_with_clusters.csv")

species.clust$foresttype <- plyr::revalue(species.clust$speciescluster,c("Oak/Maple" = "Oak/Maple", 
                                                                   "Maple/Birch/Ash/Oak/Hickory/Otherhardwood" = "Mixed Hardwoods",
                                                                   "Maple" = "Maple", 
                                                                   "Poplar"="Aspen", 
                                                                   "Pine/Poplar"="Pine", 
                                                                   "Cedar.juniper/Tamarack"="Cedar/Tamarack"))


fia.clust <- ggplot(species.clust, aes(x = x, y=y, fill=foresttype))+geom_raster()+
  scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00',
                               '#ffff33'), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.20, 0.15),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                                           axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),axis.text.x=element_blank(),
                                                                                                                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                           axis.title.x=element_blank(),
                                                                                                                           axis.title.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()
fia.clust

# merge clust_plot6 and dens.pr

fia.dens.clust <- merge(fia.bim, species.clust[,c("x","y", "cell", "speciescluster", "foresttype")], by = c("x", "y", "cell"))

png("outputs/cluster/density_vs_envt_pc1_by_species_cluster_fia.png")
ggplot(fia.dens.clust, aes(PC1fia,FIAdensity, color = speciescluster))+geom_point()
dev.off()

# C: Density vs PC1 as a hexbin plot
fia.dens.pc1.hex <- ggplot(data = fia.dens.clust, aes(PC1fia, FIAdensity)) +geom_hex() + 
  theme_bw(base_size = 8)+scale_fill_distiller(palette = "Spectral", limits = c(1,135))+
  xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+geom_vline(xintercept = -2.5)+geom_vline(xintercept = 1)+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
                                                                                                                                                                               legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                                                                                               legend.key.size = unit(0.35, "line"),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
fia.dens.pc1.hex

# D: Histogram colored by species cluster:
# make a histogram of denisty betwen -2.5 and 1 colored by species cluster:
f.clust.hist <- ggplot()+ geom_density(data = dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < 1 & dens.clust$PLSdensity > 0.5, ], aes(PLSdensity, 23 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+
  geom_density(data = fia.dens.clust[fia.dens.clust$PC1fia > -2.5 & fia.dens.clust$PC1fia < 1, ], aes(FIAdensity, 23 *..count..),trim = TRUE , color = "black", size = 1.5)+
  geom_histogram(data = fia.dens.clust[fia.dens.clust$PC1fia > -2.5 & fia.dens.clust$PC1fia < 1, ], aes(FIAdensity, fill = foresttype))+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33'), name = " ")+coord_flip()+xlim(0,600)+ylim(0,700)+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.70, 0.75),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.35, "line"), 
                                                                                                                                                                                                                                                                                                                                                                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())
f.clust.hist

f.clust.hist.nona <- ggplot()+ geom_density(data = dens.clust.nona[dens.clust.nona$PC1 > -2.5 & dens.clust.nona$PC1 < 1 & dens.clust.nona$PLSdensity > 0.5, ], aes(PLSdensity, 23 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+
  geom_density(data = fia.dens.clust[fia.dens.clust$PC1fia > -2.5 & fia.dens.clust$PC1fia < 1, ], aes(FIAdensity, 23 *..count..),trim = TRUE , color = "black", size = 1.5)+
  geom_histogram(data = fia.dens.clust[fia.dens.clust$PC1fia > -2.5 & fia.dens.clust$PC1fia < 1, ], aes(FIAdensity, fill = foresttype))+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33'), name = " ")+coord_flip()+xlim(0,600)+ylim(0,700)+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.70, 0.75),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.35, "line"), 
                                                                                                                                                                                                                                                                                                                                                                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())
f.clust.hist.nona


#clust.hist <- ggplot()+ geom_density(data = dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC2 < 1, ], aes(PLSdensity, 25 *..count..))+ geom_histogram(data = dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC2 < 1, ], aes(PLSdensity, fill = speciescluster, binwidth = 30))+scale_fill_manual(values = c('#beaed4','#ffff99','#386cb0', '#f0027f','#fdc086','#7fc97f'))+coord_flip()+xlab("PLS tree density")+ylab("# grid cells")+theme_bw()+theme(legend.position = "none")
#clust.hist


# E: map of bimodal regions:
# read in data from the rolling bimodality:

fia.prob.forest <- read.csv("outputs/posterior_prob_forest_fia.csv") # from sample_density_probabilities script


cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

p.forest.map.f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fia.prob.forest, aes(x=x, y=y, fill = pforest))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(title="Prob(forest)")+ scale_fill_manual(values= cbpalette, labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw(base_size = 8)+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),
                                          axis.title = element_blank(),
                                          legend.key.size = unit(0.3,'lines'), legend.position = c(0.205, 0.13),
                                          legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.forest.map.f


# write out new figure 2 to a png and annotate with A-F designations
png(height = 11, width = 4, units = 'in', res = 300, "outputs/paper_figs/fig2_10panel.png")
grid.arrange(pls.map + annotate("text", x=-90000, y=1486000,label= "A", size = 3)+ggtitle("PRE-SETTLEMENT"), 
             fia.map + annotate("text", x=-90000, y=1486000,label= "F", size = 3)+ggtitle("MODERN"),
             pls.clust+ annotate("text", x=-90000, y=1486000,label= "B", size = 3),
             fia.clust + annotate("text", x=-90000, y=1486000,label= "G", size = 3), 
             pls.dens.pc1.hex + annotate("text", x=4, y=600,label= "C", size = 3),
             fia.dens.pc1.hex + annotate("text", x=4, y=600,label= "H", size = 3), 
             clust.hist + annotate("text", x=600, y=20,label= "D", size = 3),
             f.clust.hist + annotate("text", x=600, y=20,label= "I", size = 3), 
             p.forest.map + annotate("text", x=-90000, y=1486000,label= "E", size = 3),
             p.forest.map.f + annotate("text", x=-90000, y=1486000,label= "J", size = 3),
             ncol = 2)
dev.off()


png(height = 11, width = 4, units = 'in', res = 300, "outputs/paper_figs/fig2_10panel_non_ag_supplement.png")
grid.arrange(pls.map.nona + annotate("text", x=-90000, y=1486000,label= "A", size = 3)+ggtitle("PRE-SETTLEMENT"), 
             fia.map + annotate("text", x=-90000, y=1486000,label= "B", size = 3)+ggtitle("MODERN"),
             pls.clust.nona+ annotate("text", x=-90000, y=1486000,label= "C", size = 3),
             fia.clust + annotate("text", x=-90000, y=1486000,label= "D", size = 3), 
             pls.dens.pc1.hex.nona + annotate("text", x=4, y=600,label= "E", size = 3),
             fia.dens.pc1.hex + annotate("text", x=4, y=600,label= "F", size = 3), 
             clust.hist.nona + annotate("text", x=600, y=20,label= "G", size = 3),
             f.clust.hist.nona + annotate("text", x=600, y=20,label= "H", size = 3), 
             p.forest.map.nona + annotate("text", x=-90000, y=1486000,label= "I", size = 3),
             p.forest.map.f + annotate("text", x=-90000, y=1486000,label= "J", size = 3),
             ncol = 2)
dev.off()


comp.pcs <- read.csv( "outputs/full_comp_pcs.csv")
pls.pcs <- comp.pcs[comp.pcs$period %in% "Past", c("x", "y", "cell", "pc1", "pc2")]
colnames(pls.pcs) <- c("x", "y","cell", "pls_pc1", "pls_pc2")
fia.pcs <- comp.pcs[comp.pcs$period %in% "Modern", c("x", "y", "cell", "pc1", "pc2")]
colnames(fia.pcs) <- c("x", "y","cell", "fia_pc1", "fia_pc2")

pcs <- merge(fia.pcs, pls.pcs, by = c("x", "y", "cell"))

pc.clust<- merge(pcs, dens.clust, by = c("x", "y", "cell"))


colnames(pc.clust)[25] <- "pls_clust"
ggplot(pc.clust, aes(x,y, fill = pls_clust))+geom_raster()

# Messy arrow figures: pls oak category
ggplot(pc.clust[pc.clust$pls_clust %in% c("Oak" ),], aes(fia_pc1, fia_pc2))+geom_point(size = 0.2)+geom_point(data = pc.clust[pc.clust$pls_clust %in% "Oak",], aes(pls_pc1, pls_pc2, color = pls_clust), size = 0.2)+xlim(-7, 5)+ylim(-9,2)+geom_segment(aes(x = pls_pc1, y = pls_pc2, xend = fia_pc1, yend = fia_pc2), data =pc.clust[pc.clust$pls_clust %in% "Oak",], arrow = arrow(length = unit(0.5, "cm")), size = 0.05)+theme_bw()

pc.clust2 <- na.omit(pc.clust)

# plot mean arrows of all the species compositions PC values:
bimodal.region.shifts.full <- ggplot(pc.clust2, aes(fia_pc1, fia_pc2))+geom_point(size = 0.2, color = "darkgrey")+geom_point(data = pc.clust2, aes(pls_pc1, pls_pc2, color = pls_clust), size = 0.2)+scale_color_manual(values = c('#bf5b17', '#beaed4','#ffff99','#386cb0','#f0027f', '#fdc086','#7fc97f'))+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c("Oak" ),], arrow = arrow(length = unit(0.5, "cm")), size =1)+
  
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'N. Mixed Forest' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Aspen' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Oak-Hickory' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Boreal/Sub-boreal' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Beech-Maple' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Pine' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+theme_bw()+ylab("Species Composition PC2")+xlab("Species Composition PC1")+theme(legend.position = c(0.75, 0.25),legend.direction = "vertical", legend.title = element_blank(), legend.key.size = unit(1,'lines'), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ guides(colour = guide_legend(override.aes = list(size=2)))

png(width = 6, height = 5, units = "in", res = 300, "outputs/paper_figs/composition_shift_plot_full.png")
bimodal.region.shifts.full
dev.off()


# lets look at only the places where density is bimodal:

pc.clust2 <- na.omit(pc.clust[pc.clust$PC1 > -2.5 & pc.clust$PC1 < 1, ])

# plot mean arrows of all the species compositions PC values:
bimodal.region.shifts <- ggplot(pc.clust2, aes(fia_pc1, fia_pc2))+geom_point(size = 0.2, color = "darkgrey")+geom_point(data = pc.clust2, aes(pls_pc1, pls_pc2, color = pls_clust), size = 0.2)+scale_color_manual(values = c( '#bf5b17', '#beaed4','#ffff99','#386cb0','#f0027f', '#fdc086','#7fc97f'))+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$pls_pc1, na.rm = TRUE), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$pls_pc2, na.rm = TRUE), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia_pc1, na.rm = TRUE), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia_pc2, na.rm = TRUE)), data =pc.clust2[pc.clust2$pls_clust %in% c("Oak" ),], arrow = arrow(length = unit(0.5, "cm")), size =1)+
  
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$pls_pc1, na.rm = TRUE), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$pls_pc2, na.rm = TRUE), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia_pc1, na.rm = TRUE), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia_pc2, na.rm = TRUE)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'N. Mixed Forest' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$pls_pc1, na.rm = TRUE), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$pls_pc2, na.rm = TRUE), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia_pc1, na.rm = TRUE), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia_pc2, na.rm = TRUE)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Aspen' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$pls_pc1, na.rm = TRUE), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$pls_pc2, na.rm = TRUE), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia_pc1, na.rm = TRUE), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia_pc2, na.rm = TRUE)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Oak-Hickory' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$pls_pc1, na.rm = TRUE), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$pls_pc2, na.rm = TRUE), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia_pc1, na.rm = TRUE), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia_pc2, na.rm = TRUE)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Boreal/Sub-boreal' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$pls_pc1, na.rm = TRUE), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$pls_pc2, na.rm = TRUE), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia_pc1, na.rm = TRUE), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia_pc2, na.rm = TRUE)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Beech-Maple' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  
   geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$pls_pc1, na.rm = TRUE), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$pls_pc2, na.rm = TRUE), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia_pc1, na.rm = TRUE), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia_pc2, na.rm = TRUE)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Pine' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+theme_bw()+ylab("Species Composition PC2")+xlab("Species Composition PC1")+theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(2,'lines'), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ guides(colour = guide_legend(override.aes = list(size=5)))

png(width = 6, height = 5, units = "in", res = 300, "outputs/paper_figs/composition_shift_plot_bimodal_region.png")
bimodal.region.shifts +theme(legend.direction = "vertical", legend.key.size = unit(0.5, "line"), legend.position = c(0.75, 0.2) ) + guides(colour = guide_legend(override.aes = list(size=3)))
dev.off()


# add in the pca shift plots here:
shifted.pca <- bimodal.region.shifts +theme(legend.direction = "vertical", legend.key.size = unit(0.5, "line"), legend.position = c(0.75, 0.2) ) + guides(colour = guide_legend(override.aes = list(size=3)))


#-------------------------Figure 3: Plot future predictions PLS-----------------------
future.preds <- read.csv("outputs/future_predictive_samples.csv")

future.preds$pbimodal26 <- cut(future.preds$prob_forest26, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))
future.preds$pbimodal45 <- cut(future.preds$prob_forest45, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))
future.preds$pbimodal60 <- cut(future.preds$prob_forest60, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))
future.preds$pbimodal85 <- cut(future.preds$prob_forest85, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

future.preds$pbimodal26 <- as.character(future.preds$pbimodal26)
future.preds$pbimodal45 <- as.character(future.preds$pbimodal45)
future.preds$pbimodal60 <- as.character(future.preds$pbimodal60)
future.preds$pbimodal85 <- as.character(future.preds$pbimodal85)


# plot out the probability of forests in the fia region:

p.bimodal26 <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=future.preds, aes(x=x, y=y, fill = pbimodal26))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="p(forest)")+ scale_fill_manual(values= rev(cbpalette), labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.bimodal26

p.bimodal45 <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=future.preds, aes(x=x, y=y, fill = pbimodal45))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="p(forest)")+ scale_fill_manual(values= rev(cbpalette), labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.bimodal45

p.bimodal60 <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=future.preds, aes(x=x, y=y, fill = pbimodal60))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="p(forest)")+ scale_fill_manual(values= rev(cbpalette), labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.bimodal60

p.bimodal85 <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=future.preds, aes(x=x, y=y, fill = pbimodal85))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="p(forest)")+ scale_fill_manual(values= rev(cbpalette), labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.bimodal85



# predictions using FIA:
future.predsf <- read.csv("outputs/future_predictive_samples_fia.csv")

future.predsf$pbimodal26 <- cut(future.predsf$prob_forest26, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))
future.predsf$pbimodal45 <- cut(future.predsf$prob_forest45, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))
future.predsf$pbimodal60 <- cut(future.predsf$prob_forest60, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))
future.predsf$pbimodal85 <- cut(future.predsf$prob_forest85, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

future.predsf$pbimodal26 <- as.character(future.predsf$pbimodal26)
future.predsf$pbimodal45 <- as.character(future.predsf$pbimodal45)
future.predsf$pbimodal60 <- as.character(future.predsf$pbimodal60)
future.predsf$pbimodal85 <- as.character(future.predsf$pbimodal85)


# plot out the probability of forests in the fia region:

p.bimodal26f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=future.predsf, aes(x=x, y=y, fill = pbimodal26))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="p(forest)")+ scale_fill_manual(values= rev(cbpalette), labels=c("0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.bimodal26f

p.bimodal45f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=future.predsf, aes(x=x, y=y, fill = pbimodal45))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="p(forest)")+ scale_fill_manual(values= rev(cbpalette), labels=c("0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.bimodal45f

p.bimodal60f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=future.predsf, aes(x=x, y=y, fill = pbimodal60))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="p(forest)")+ scale_fill_manual(values= rev(cbpalette), labels=c("0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.bimodal60f

p.bimodal85f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=future.predsf, aes(x=x, y=y, fill = pbimodal85))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="p(forest)")+ scale_fill_manual(values= rev(cbpalette), labels=c("0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.bimodal85f

png(height = 4, width = 6, units = "in", res = 300, "outputs/paper_figs/figure_3_prob_forest_future85.png")
grid.arrange(p.bimodal85, p.bimodal85f, ncol = 2)
dev.off()


png(height = 8, width = 9, units = "in", res = 300, "outputs/paper_figs/supp_prob_forest_future85.png")
grid.arrange(p.bimodal26+ggtitle("RCP 2.6")+
               theme(plot.title = element_text(hjust = 0.5)), p.bimodal60+ggtitle("RCP 6.0")+
               theme(plot.title = element_text(hjust = 0.5)),
             p.bimodal45+ggtitle("RCP 4.5")+
               theme(plot.title = element_text(hjust = 0.5)), p.bimodal26f,p.bimodal45f,
              p.bimodal60f,
              ncol = 3, left = "MODERN RELATIONSHIP                                                 PAST RELATIONSHIP")
dev.off()
