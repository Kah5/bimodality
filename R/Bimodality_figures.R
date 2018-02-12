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
  scale_fill_gradientn(colours = cbpalette, limits = c(0,600), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw(base_size = 10)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank())+ggtitle("")


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

pls.clust <- ggplot(clust_7, aes(x = x, y=y, fill=foresttype))+geom_raster()+
  scale_fill_manual(values = c( '#bf5b17','#beaed4','#ffff99','#386cb0','#fdc086', '#f0027f','#7fc97f'), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(),legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.position = c(0.19, 0.3),legend.direction = "vertical",legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()+ggtitle("")
pls.clust 

# merge clust_plot6 and dens.pr

dens.clust <- merge(dens.pr, clust_7[,c("x","y", "cell", "speciescluster", "foresttype")], by = c("x", "y", "cell"))

png("outputs/cluster/density_vs_envt_pc1_by_species_cluster.png")
ggplot(dens.clust, aes(PC1, PLSdensity, color = speciescluster))+geom_point()
dev.off()



# C: Density vs PC1 as a hexbin plot
pls.dens.pc1.hex <- ggplot(data = dens.clust, aes(PC1, PLSdensity)) +geom_hex() + 
  theme_bw(base_size = 10)+scale_fill_distiller(palette = "Spectral", limits = c(1,130))+
  xlab('Environmental PC1') + ylab(" PLS Tree Density /n (stems/ha)")+geom_vline(xintercept = -2.5)+geom_vline(xintercept = 1)+xlim(4, -5)+ggtitle("")
pls.dens.pc1.hex

# D: Histogram colored by species cluster:
# make a histogram of denisty betwen -2.5 and 1 colored by species cluster:

# need to reorder the factors:
dens.clust$foresttype<- factor(dens.clust$foresttype, levels = rev(c("Boreal/sub-Boreal", "Pine", "Aspen", "Beech/Maple/Hemlock", "Hemlock/Beech", "Oak", "Mesic Hardwoods")))

clust.hist <- ggplot()+ geom_density(data = dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < 1, ], aes(PLSdensity, 23 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_histogram(data = dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < 1, ], aes(PLSdensity, fill = foresttype, binwidth = 30))+xlim(0,600)+
  scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+coord_flip()+xlab("PLS tree density")+ylab("# grid cells")+theme_bw()+theme(legend.position = c(0.85, 0.75), legend.background = element_rect(fill=alpha('white', 0.4)))+ggtitle("")+xlim(0,600)+ylim(0,600)
clust.hist




# E: map of bimodal regions:
# sample the p(forest) at each environmental bitn

pls.roll.bim <- read.csv("outputs/cluster/bimodal_widths/PLS_Dens_Bimodal_width_0.25.csv")

bimodal.map <- ggplot(pls.roll.bim, aes(x, y, fill = bimodal))+geom_raster()+coord_equal()+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+scale_fill_manual(values = c("#ca0020", "#0571b0"))+theme_bw()+ theme(legend.position = c(0.2,0.25),axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                                                                                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                                                                  axis.title.x=element_blank(),
                                                                                                                                                                  axis.title.y=element_blank())+ggtitle("")

bimodal.map 




# write out new figure 1 to a png
png(height = 14, width = 10, units = 'in', res = 300, "outputs/paper_figs/fig1_jan.png")
grid.arrange(pls.map + annotate("text", x=-90000, y=1486000,label= "A", size = 5), 
             pls.clust + annotate("text", x=-90000, y=1486000,label= "B", size = 5), 
             pls.dens.pc1.hex + annotate("text", x= 4, y=600,label= "C", size = 5), 
             clust.hist + annotate("text", x=600, y=15,label= "D", size = 5), 
             bimodal.map + annotate("text", x=-90000, y=1486000,label= "E", size = 5), ncol = 2)
dev.off()

