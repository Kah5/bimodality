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
                                                
                                                axis.title=element_blank())+ggtitle("")+coord_equal()


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
clust_7$foresttype<- revalue(clust_7$speciescluster, c("Poplar"="Aspen", "Elm/Maple/Hickory/Oak/Beech"="Mesic Hardwoods", "Oak" = "Oak", 
                                                               "Hemlock/Beech/Cedar/Birch/Maple" = "Hemlock/Beech", 
                                                               "Pine/Tamarack/Poplar" = "Pine", "Tamarack/Spruce/Birch/Pine/Poplar" = "Boreal/sub-Boreal", "Beech/Maple/Hemlock" = "Beech/Maple/Hemlock"))

clust_7$orderedforesttype<- factor(clust_7$foresttype, c("Oak", "Pine", "Aspen", "Hemlock/Beech", "Boreal/sub-Boreal", "Mesic Hardwoods", "Beech/Maple/Hemlock"))

pls.clust <- ggplot(clust_7, aes(x = x, y=y, fill=orderedforesttype))+geom_raster()+
  scale_fill_manual(values = c('#386cb0', '#f0027f','#fdc086','#ffff99','#7fc97f','#beaed4', '#bf5b17'), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.25, 0.15),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                               axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank())+coord_equal()

pls.clust
# merge clust_plot6 and dens.pr

dens.clust <- merge(dens.pr, clust_7[,c("x","y", "cell", "speciescluster", "foresttype")], by = c("x", "y", "cell"))

png("outputs/cluster/density_vs_envt_pc1_by_species_cluster.png")
ggplot(dens.clust, aes(PC1, PLSdensity, color = speciescluster))+geom_point()
dev.off()



# C: Density vs PC1 as a hexbin plot


pls.dens.pc1.hex <- ggplot(data = dens.clust, aes(PC1, PLSdensity)) +geom_hex() + 
  theme_bw(base_size = 8)+scale_fill_distiller(palette = "Spectral", limits = c(1,130))+
  xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+geom_vline(xintercept = -2.5)+geom_vline(xintercept = 1)+xlim(4, -5)+ylim(0,600)+coord_fixed(ratio = 1/60)+theme(legend.position = "top",legend.direction = "horizontal", 
                                                                                                                                                                               legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                                                                                               legend.key.size = unit(0.35, "line"))
pls.dens.pc1.hex

# D: Histogram colored by species cluster:
# make a histogram of denisty betwen -2.5 and 1 colored by species cluster:

# need to reorder the factors:
dens.clust$foresttype<- factor(dens.clust$foresttype, levels = rev(c("Boreal/sub-Boreal", "Pine", "Aspen", "Beech/Maple/Hemlock", "Hemlock/Beech", "Oak", "Mesic Hardwoods")))

clust.hist <- ggplot()+ geom_density(data = dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < 1, ], aes(PLSdensity, 23 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_histogram(data = dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < 1, ], aes(PLSdensity, fill = foresttype, binwidth = 30))+xlim(0,600)+
  scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+coord_flip()+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.75, 0.75),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.35, "line"))
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
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.forest.map

                                                                                                                                                                  




# write out new figure 1 to a png
png(height = 14, width = 10, units = 'in', res = 300, "outputs/paper_figs/fig1_full_mi.png")
grid.arrange(pls.map + annotate("text", x=-90000, y=1486000,label= "A", size = 5), 
             pls.clust + annotate("text", x=-90000, y=1486000,label= "B", size = 5), 
             pls.dens.pc1.hex + annotate("text", x= 4, y=600,label= "C", size = 5), 
             clust.hist + annotate("text", x=600, y=15,label= "D", size = 5), 
             p.forest.map + annotate("text", x=-90000, y=1486000,label= "E", size = 5), ncol = 2)
dev.off()



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
                                               axis.title.y=element_blank())+ggtitle("")

png("/Users/kah/Documents/bimodality/outputs/paper_figs/PLS_density_map.png")
fia.map + annotate("text", x=-90000, y=1486000,label= "B", size = 5)
dev.off()

# B: Map of species clusters

# from species_clustering.R:
species.clust <- read.csv("outputs/cluster/density_fia_with_clusters.csv")

species.clust$foresttype <- plyr::revalue(species.clust$speciescluster,c("Oak/Maple" = "Oak/Maple", 
                                                                   "Maple/Birch/Ash/Oak/Hickory/Otherhardwood" = "Hardwoods",
                                                                   "Maple" = "Maple", 
                                                                   "Poplar"="Aspen", 
                                                                   "Pine/Poplar"="Pine/Aspen", 
                                                                   "Cedar.juniper/Tamarack"="Cedar/Tamarack"))


fia.clust <- ggplot(species.clust, aes(x = x, y=y, fill=foresttype))+geom_raster()+
  scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00',
                               '#ffff33'), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.25, 0.15),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                                           axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),axis.text.x=element_blank(),
                                                                                                                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                           axis.title.x=element_blank(),
                                                                                                                           axis.title.y=element_blank())+coord_equal()
fia.clust

# merge clust_plot6 and dens.pr

fia.dens.clust <- merge(fia.bim, species.clust[,c("x","y", "cell", "speciescluster", "foresttype")], by = c("x", "y", "cell"))

png("outputs/cluster/density_vs_envt_pc1_by_species_cluster_fia.png")
ggplot(fia.dens.clust, aes(PC1fia,FIAdensity, color = speciescluster))+geom_point()
dev.off()

# C: Density vs PC1 as a hexbin plot
fia.dens.pc1.hex <- ggplot(data = fia.dens.clust, aes(PC1fia, FIAdensity)) +geom_hex() + 
  theme_bw(base_size = 8)+scale_fill_distiller(palette = "Spectral", limits = c(1,130))+
  xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+geom_vline(xintercept = -2.5)+geom_vline(xintercept = 1)+xlim(4, -5)+ylim(0,600)+coord_fixed(ratio = 1/60)+theme(legend.position = "top",legend.direction = "horizontal", 
                                                                                                                                                                               legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                                                                                               legend.key.size = unit(0.35, "line"))
fia.dens.pc1.hex

# D: Histogram colored by species cluster:
# make a histogram of denisty betwen -2.5 and 1 colored by species cluster:
f.clust.hist <- ggplot()+ geom_density(data = dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < 1, ], aes(PLSdensity, 23 *..count..), bw = 12,linetype="dashed" , color = "darkgrey", size = 1.5)+
  geom_density(data = fia.dens.clust[fia.dens.clust$PC1fia > -2.5 & fia.dens.clust$PC1fia < 1, ], aes(FIAdensity, 23 *..count..),trim = TRUE , color = "black", size = 1.5)+
  geom_histogram(data = fia.dens.clust[fia.dens.clust$PC1fia > -2.5 & fia.dens.clust$PC1fia < 1, ], aes(FIAdensity, fill = foresttype))+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33'), name = " ")+coord_flip()+xlim(0,600)+ylim(0,600)+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.75, 0.75),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.35, "line"))
f.clust.hist

#plot(density(dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC2 < 1, ]$PLSdensity))
#plot(density(fia.dens.clust[fia.dens.clust$PC1 > -2.5 & fia.dens.clust$PC2 < 1, ]$Density))
#fia.dens.clust$ecocode <- ifelse(fia.dens.clust$Density >= 75, "Forest", ifelse(fia.dens.clust$Density >= 1, "Savanna", "Prairie" ))

#f.ecotype.hist <- ggplot()+ geom_density(data = fia.dens.clust, aes(Density, 25*..count..), color = "white", linetype = 'dashed')+ geom_histogram(data = fia.dens.clust, aes(Density, fill = ecocode, binwidth = 30))+xlim(0,600)+scale_fill_manual(values = c(
 # '#addd8e',
  #'#31a354'),limits = c( "Savanna", "Forest"), name ="Biome")+xlab("PLS tree density")+ylab("# grid cells")+theme_bw()+theme(legend.position = "none")+ggtitle("")+ theme_black(base_size = 20)

#png(height = 6, width = 7, units = "in",res = 300, "outputs/black_white_density_hist_ecotype_fia.png")
#f.ecotype.hist 
#dev.off()

# ecotype map fia:
#f.ecotype.map<- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), colour = "darkgrey", fill = NA)+
 # geom_raster(data=fia.bim, aes(x=x, y=y, fill = ecotype))+
  #geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  #labs(x="easting", y="northing")+scale_fill_manual(values = c(
   # '#addd8e',
    #'#31a354'), limits = c( "Savanna", "Forest"), name ="Biome")+
  #coord_equal()+theme_bw(base_size = 10)+theme_black()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
   #                                                           axis.text.y=element_blank(),axis.ticks=element_blank(),
    #                                                          axis.title.x=element_blank(),
     #                                                         axis.title.y=element_blank(), panel.border = element_blank()) + geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), colour = "darkgrey", fill = NA)

#png("outputs/black_white_ecotype_map_fia.png")
#f.ecotype.map
#dev.off()

# add in the pca shift plots here:
shifted.pca <- bimodal.region.shifts +theme(legend.direction = "vertical", legend.key.size = unit(0.5, "line") ) + guides(colour = guide_legend(override.aes = list(size=3)))

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
                                          legend.key.size = unit(0.3,'lines'), legend.position = c(0.205, 0.125),
                                          legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.forest.map.f


# write out new figure 2 to a png and annotate with A-F designations
png(height = 11, width = 4, units = 'in', res = 300, "outputs/paper_figs/fig2_10panel.png")
grid.arrange(pls.map + annotate("text", x=-90000, y=1486000,label= "A", size = 3)+ggtitle("PRE-SETTLEMENT"), 
             fia.map + annotate("text", x=-90000, y=1486000,label= "B", size = 3)+ggtitle("MODERN"),
             pls.clust+ annotate("text", x=-90000, y=1486000,label= "C", size = 3),
             fia.clust + annotate("text", x=-90000, y=1486000,label= "D", size = 3), 
             pls.dens.pc1.hex + annotate("text", x=4, y=600,label= "E", size = 3),
             fia.dens.pc1.hex + annotate("text", x=4, y=600,label= "F", size = 3), 
             clust.hist + annotate("text", x=600, y=20,label= "G", size = 3),
             f.clust.hist + annotate("text", x=600, y=20,label= "H", size = 3), 
             p.forest.map + annotate("text", x=-90000, y=1486000,label= "I", size = 3),
             p.forest.map.f + annotate("text", x=-90000, y=1486000,label= "J", size = 3),
             ncol = 2)
dev.off()


