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



dens.pr$density_discrete <- ifelse(dens.pr$PLSdensity <= 0.5, "Prairie", 
                                   ifelse(dens.pr$PLSdensity <= 47, "Savanna",
                                          ifelse(dens.pr$PLSdensity > 47 & dens.pr$PLSdensity <= 100, "47-100",
                                                 ifelse(dens.pr$PLSdensity > 100 & dens.pr$PLSdensity <= 200, "100-200", 
                                                        ifelse(dens.pr$PLSdensity > 200 & dens.pr$PLSdensity <= 300, "200-300", 
                                                               ifelse(dens.pr$PLSdensity > 300 & dens.pr$PLSdensity <= 400, "300-400",
                                                                      ifelse(dens.pr$PLSdensity > 400 & dens.pr$PLSdensity <= 500, "400-500",
                                                                             ifelse(dens.pr$PLSdensity > 500 & dens.pr$PLSdensity <= 600, "600 +", "NA"))))))))

dens.pr$density_discrete<- factor(dens.pr$density_discrete, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500", "500-600", "600+", "NA"))


pls.map.alt.color <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.pr, aes(x=x, y=y, fill = density_discrete))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_manual(values = c('#dfc27d',
    '#8c510a',
    '#d9f0a3',
    '#addd8e',
    '#78c679',
    '#41ab5d',
    '#238443',
    '#005a32'), name ="Tree \n Density", na.value = 'darkgrey') +
  
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.3, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


png(height = 4, width = 3, units = "in", res = 300,"/Users/kah/Documents/bimodality/outputs/paper_figs/PLS_density_map_full_alt_colors.png")
pls.map.alt.color+ggtitle("PLS")
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
dens.clust <- dens.clust[!dupliated(dens.clust),] # remove any duplicates created in merge

png("outputs/cluster/density_vs_envt_pc1_by_species_cluster.png")
ggplot(dens.clust, aes(PC1, PLSdensity, color = speciescluster))+geom_point()
dev.off()

ggplot(dens.clust, aes(GS_ppet, PLSdensity, color = speciescluster))+geom_point()

dens.clust.m <- melt(dens.clust[,c("x", "y", "cell",  "foresttype", "PC1", "PLSdensity")], id.vars = c("x", "y", "cell", "PC1","PLSdensity" ))

png("outputs/paper_figs/density_by_7_spec_clusters.png")
ggplot(na.omit(dens.clust.m), aes(PLSdensity))+geom_histogram()+facet_wrap(~value)
dev.off()

# what areas of the PC1 environment have the highest p(bimodality?)
dens.clust$PC1bins <- cut(dens.clust$PC1, breaks = seq(-5,5.5, by = 0.25), labels = label.breaks(-5,5.25, 0.25))

dens.clust.mpc1 <- melt(dens.clust[,c("x", "y", "cell",  "foresttype", "PC1", "PLSdensity", "PC1bins")], id.vars = c("x", "y", "cell", "PC1","PLSdensity", "foresttype" ))
dens.clust.mpc1$PC_1bins_o <- factor(dens.clust.mpc1$value, c("-5 - -4.", "-4 - -3", "-3 - -2", "-2 - -1",
                                                              "-1 - 0",  "0 - 1" ,"1 - 2",   "2 - 3" ,  "3 - 4", "4 - 5", NA))

png("outputs/paper_figs/PLS_hist_bins_by_envt_species_cluster.png")
ggplot(na.omit(dens.clust.mpc1), aes(PLSdensity, fill = foresttype))+geom_histogram()+facet_wrap(~value)+scale_fill_manual(values = c('#bf5b17', '#beaed4','#ffff99','#386cb0','#f0027f', '#fdc086','#7fc97f'), name = " ")
dev.off()

# C: Density vs PC1 as a hexbin plot


pls.dens.pc1.hex <- ggplot(data = dens.clust, aes(PC1, PLSdensity)) +geom_hex() + 
  theme_bw(base_size = 8)+scale_fill_distiller(palette = "Spectral", limits = c(1,140))+
  xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+geom_vline(xintercept = -2.5)+geom_vline(xintercept = 1)+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
                                                                                                                                                                               legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                                                                                               legend.key.size = unit(0.35, "line"),legend.title = element_text(size = 8), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
pls.dens.pc1.hex


pls.dens.ppet.hex <- ggplot(data = dens.clust, aes(GS_ppet, PLSdensity)) +geom_hex() + 
  theme_bw(base_size = 8)+#scale_fill_distiller(palette = "Spectral", limits = c(1,140))+
  xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")#+geom_vline(xintercept = -2.5)+geom_vline(xintercept = 1)+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
                                                                                                                                                                              # legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                                                                                               #legend.key.size = unit(0.35, "line"),legend.title = element_text(size = 8), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
pls.dens.ppet.hex

smoothScatter(x =dens.clust$PC1, y = dens.clust$PLSdensity, pch = NA, nrpoints = 1000,  nbin = c(1000, 1000) )

p4 <- ggplot(dens.clust, aes(PC1, PLSdensity)) +
  stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE)+scale_fill_continuous(low = 'white', high = 'red')   

png("outputs/alt_hexbin_raster_pls.png")
p4
dev.off()

png("outputs/alt_hexbin_scale_count_pls.png")
ggplot(dens.clust, aes(x=PC1, y=PLSdensity))+ geom_count(col="tomato3", show.legend=F, alpha = 0.05) +scale_size_area()+theme_bw()
dev.off()
               
ggplot(dens.clust, aes(x=PC1, y=PLSdensity)) + geom_point(alpha = 0.2)+geom_rug(alpha = 0.01)                                                                                                                                                


sizeplot(dens.clust$PC1, dens.clust$PLSdensity, size = c(0.1,5))
count.overplot(dens.clust$PC1, dens.clust$PLSdensity)
cluster.overplot(dens.clust$PC1, dens.clust$PLSdensity)
sunflowerplot(dens.clust$PC1, dens.clust$PLSdensity)

plot(dens.clust$PC1, dens.clust$PLSdensity, main="PDF Scatterplot Example", col=rgb(0,100,0,50,maxColorValue=350), pch=16)



library(RColorBrewer)
buylrd = c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF",
           "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026") 

reds <- c('#fff7ec',
  '#fee8c8',
  '#fdd49e',
  '#fdbb84',
  '#fc8d59',
  '#ef6548',
  '#d7301f',
  '#b30000',
  '#7f0000')
myColRamp = colorRampPalette(c(reds))

# smoothed scatterplot
png("outputs/alt_hexbin_smooth_scatter_pls.png")
smoothScatter(x=dens.clust$PC1,y=dens.clust$PLSdensity,
              colramp=myColRamp,
              main=" ",
              xlab="PC 1",
              ylab="PLS density")
dev.off()
plot.smooth <- smoothScatter(x=dens.clust$PC1,y=dens.clust$PLSdensity,
                             colramp=myColRamp,
                             main="",
                             xlab="Environmental PC1",
                             ylab="Tree Density (stems/ha)")


# plot scaling overlapping histograms with density plot overlaid:

png("outputs/alt_hexbin_smooth_scatter_with_density2doverlay.png")
p1 = ggplot(dens.clust,aes(x=PC1, y=PLSdensity)) +
  geom_point(alpha = 0.1, colour="orange")+ #+ geom_count(alpha = 0.1, colour="orange")+
  geom_density2d() + 
  theme_bw()

scatter_dens_2dpls<- p1+xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+geom_vline(xintercept = -2.5)+geom_vline(xintercept = 1)+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
                                                                                                                                                                                legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                                                                                               legend.key.size = unit(0.35, "line"),legend.title = element_text(size = 8), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dev.off() 


p1 = ggplot(dens.clust,aes(x=GS_ppet, y=PLSdensity)) +
  geom_point(alpha = 0.1, colour="orange")+ #+ geom_count(alpha = 0.1, colour="orange")+
  geom_density2d() + 
  theme_bw()

scatter_dens_2dpls_ppet <- p1+xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+ylim(0,650)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
                                                                                                                                                                                                          legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                                                                                                                          legend.key.size = unit(0.35, "line"),legend.title = element_text(size = 8), panel.grid.major = element_blank(), panel.grid.minor = element_blank())


png("outputs/alt_hexbin_smooth_scatter_with_density2doverlay_gs_ppet.png")
scatter_dens_2dpls_ppet
dev.off()

p2 = ggplot(na.omit(dens.clust),aes(x=PC1, y=PLSdensity)) +
   #+ geom_count(alpha = 0.1, colour="orange")+
  stat_density_2d(geom = "polygon", 
                 aes(fill = ..level..)) + geom_point(alpha = 0.01, colour="black")+
  theme_bw()

png("outputs/alt_hexbin_smooth_scatter_with_polygon2doverlay.png")
p2+xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+geom_vline(xintercept = -2.5)+geom_vline(xintercept = 1)+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.direction = "vertical", 
                                                                                                                                                                                legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                                                                                                legend.key.size = unit(0.35, "line"),legend.title = element_text(size = 8), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()


p3 <- ggplot(dens.clust,aes(x=PC1, y=PLSdensity))+stat_density_2d(geom = "point", aes(size = ..density..), n = 30, contour = FALSE, na.rm = TRUE)+scale_size_area()

png("outputs/alt_hexbin_regular_density_dots.png")
p3+xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+geom_vline(xintercept = -2.5)+geom_vline(xintercept = 1)+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.direction = "vertical", 
                                                                                                                                                                                legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                                                                                                legend.key.size = unit(0.35, "line"),legend.title = element_text(size = 8), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dev.off()
p4 <- ggplot(dens.clust, aes(PC1, PLSdensity)) +
  stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE)+scale_fill_continuous(low = 'white', high = 'red')   

p4+xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+geom_vline(xintercept = -2.5)+geom_vline(xintercept = 1)+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
                                                                                                                                                                                legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                                                                                                legend.key.size = unit(0.35, "line"),legend.title = element_text(size = 8), panel.grid.major = element_blank(), panel.grid.minor = element_blank())




# D: Histogram colored by species cluster:

# make a histogram of denisty betwen -2.5 and 1 colored by species cluster:

# need to reorder the factors:
dens.clust$foresttype_ordered<- factor(dens.clust$foresttype, levels = rev(c("Boreal/Sub-boreal", "Pine", "Aspen", "Beech-Maple", "N. Mixed Forest", "Oak", "Oak-Hickory")))
dens.clust.omit <- dens.clust[ !is.na(dens.clust$foresttype_ordered),]
dens.clust <- dens.clust[!duplicated(dens.clust),]

clust.hist <- ggplot()+ geom_density(data = dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < 0.25 & dens.clust$PLSdensity > 0.5, ], aes(PLSdensity, 23 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_histogram(data = dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < 0.25 & dens.clust$PLSdensity > 0.5, ], aes(PLSdensity, fill = foresttype_ordered, binwidth = 30))+xlim(0,600)+
  scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+coord_flip()+ylim(0,500)+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.65, 0.75),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.35, "line"),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
clust.hist

ggplot(dens.clust[dens.clust$PC1 > -0.25 & dens.clust$PC1 < 0.5 , ], aes(x,y, fill = PLSdensity))+geom_raster()+
  geom_tile(data = dens.clust[dens.clust$PC1 > -0.25 & dens.clust$PC1 < 0.5 , ], aes(x,y, color = foresttype_ordered))+scale_color_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")


# where are the highest densities
# places with bimodal vegetation:
png(height = 10, width = 10, units = "in", res = 300, "outputs/maps_veg_by_pc1bins.png")
ggplot(dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < 1 & dens.clust$PLSdensity > 0.5, ], aes(x,y, fill = foresttype_ordered))+geom_raster()+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+coord_equal()+facet_wrap(~PC1bins)
dev.off()


png(height = 10, width = 10, units = "in", res = 300, "outputs/hist_veg_by_pc1bins.png")
ggplot(dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < 1 & dens.clust$PLSdensity > 0.5, ], aes(PLSdensity, fill = foresttype_ordered))+geom_histogram( binwidth = 30)+xlim(0,650)+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+
facet_wrap(~PC1bins)
dev.off()

# bimodal places with 100 - 200 + trees /ha
ggplot(dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < -0.25 & dens.clust$PLSdensity > 100 & dens.clust$PLSdensity <=200, ], aes(x,y, fill = foresttype_ordered))+geom_raster()+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")

# bimodal places with 200 + trees/ha
ggplot(dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < 0.25 & dens.clust$PLSdensity >= 200, ], aes(x,y, fill = foresttype_ordered))+geom_raster()+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")

png("outputs/PC1_bins_map_pls.png")
ggplot(dens.clust, aes(x,y, fill = PC1bins))+geom_raster()#+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")
dev.off()

ggplot(dens.clust, aes(x,y, fill = PC1))+geom_raster()#+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")

# E: map of bimodal regions:
# sample the p(forest) at each environmental bitn

pls.prob.forest <- read.csv("outputs/posterior_prob_forest_pls.csv") # from sample_density_probabilities script


cbpalette <- c("#ffffcc", "#c2e699", "#d73027", "#31a354", "#006837")
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

p.forest.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.prob.forest, aes(x=x, y=y, fill = pforest))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= cbpalette, labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.forest.map

                                                                                                                                                                  

# read in p(bimodality with 0.5 bins)

pls.b50 <- read.csv("outputs/posterior_prob_bimodal_pls_50bins_for100_diptest_only.csv")
pls.b50$pbimodal <- cut(pls.b50$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

rpalette <- c('#fee5d9',
  '#fcae91',
  '#fb6a4a',
  '#de2d26',
  '#a50f15')
names(rpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

pls.b50$pbimodal <- as.character(pls.b50$pbimodal)


p.bimodal50 <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.b50, aes(x=x, y=y, fill = pbimodal, alpha = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+ scale_fill_manual(values= rpalette, labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1"))+scale_alpha_discrete(range=c(0, 1))+
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")

p.bimodal50 + geom_tile(data=pls.prob.forest[pls.prob.forest$prob_forest >= 0.2 & pls.prob.forest$prob_forest <= 0.6,], aes(x=x, y=y, color = prob_forest))+scale_color_continuous(low = 'white', high = 'blue')


pls.bim50 <- merge(pls.b50, dens.clust, by = c("x", "y", "cell"))

png("outputs/prob_bimodal_vs_PC1_0.5_bins.png")
ggplot(pls.bim50[!is.na(pls.bim50$foresttype_ordered),], aes(PC1, prob_bimodal, color = foresttype_ordered))+geom_point(size = 1)+scale_color_manual(values = c('#beaed4','#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+theme_bw()+geom_jitter()
dev.off()


pls.b20 <- read.csv("outputs/posterior_prob_bimodal_pls_25bins_dipP_only.csv")
pls.b20$pbimodal <- cut(pls.b20$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

rpalette <- c('#fee5d9',
              '#fcae91',
              '#fb6a4a',
              '#de2d26',
              '#a50f15')
names(rpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

pls.b20$pbimodal <- as.character(pls.b20$pbimodal)


p.bimodal20 <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.b20, aes(x=x, y=y, fill = pbimodal, alpha = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+ scale_fill_manual(values= rpalette, labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1"))+scale_alpha_discrete(range=c(0.2, 1))+
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")

p.bimodal20 + geom_tile(data=pls.prob.forest[pls.prob.forest$prob_forest <= 0.25 | pls.prob.forest$prob_forest >= 0.6,], aes(x=x, y=y, color = prob_forest))


pls.bim20 <- merge(pls.b20, dens.clust, by = c("x", "y", "cell"))

png("outputs/prob_bimodal_vs_PC1_0.2_bins.png")
ggplot(pls.bim20[!is.na(pls.bim20$foresttype_ordered),], aes(PC1, prob_bimodal, color = foresttype_ordered))+geom_point(size = 1)+scale_color_manual(values = c('#beaed4','#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+theme_bw()
dev.off()

png(height = 10, width = 10, units = "in", res = 300,"outputs/prob_bimodal_facet_by_PC1bins.png")
ggplot(pls.bim20, aes(prob_bimodal, fill = foresttype_ordered))+geom_histogram()+facet_wrap(~PC1bins)
dev.off()


# make a plot of the maps of composition by PC bins:
png(width = 12, height = 12, units = "in", res = 300, "outputs/map_comp_facet_by_PC1bins.png")
ggplot(pls.bim20[pls.bim20$PC1 > - 2.5 & pls.bim20$PC1 <= 0.5,], aes(x,y, fill = foresttype_ordered))+geom_raster()+facet_wrap(~PC1bins)+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+scale_fill_manual(values = c('#beaed4','#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+theme_bw()+coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.background = element_rect(fill=alpha('transparent', 0)),
                                                                                                                                                                                                                              panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")
dev.off()


png(width = 12, height = 12, units = "in", res = 300, "outputs/map_comp_bimodal_-2.5_0.5.png")
ggplot(pls.bim20[pls.bim20$PC1 > - 2.5 & pls.bim20$PC1 <= 0.5,], aes(x,y, fill = foresttype_ordered))+geom_raster()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+scale_fill_manual(values = c('#beaed4','#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+theme_bw()+coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.background = element_rect(fill=alpha('transparent', 0)),
                                                                                                                                                                                                                              panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")

dev.off()

png(height = 10, width = 10, units = "in", res = 300,"outputs/density_facet_by_PC1bins.png")
ggplot(pls.bim20, aes(PLSdensity, fill = foresttype_ordered))+geom_histogram()+facet_wrap(~PC1bins)
dev.off()

# for 0.75 bins:
pls.b75 <- read.csv("outputs/posterior_prob_bimodal_pls_75bins.csv")
pls.b75$pbimodal <- cut(pls.b75$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

rpalette <- c('#fee5d9',
              '#fcae91',
              '#fb6a4a',
              '#de2d26',
              '#a50f15')
names(rpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

pls.b75$pbimodal <- as.character(pls.b75$pbimodal)


p.bimodal75 <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.b75, aes(x=x, y=y, fill = pbimodal, alpha = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+ scale_fill_manual(values= rpalette, labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1"))+scale_alpha_discrete(range=c(0.2, 1))+
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.755, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")

p.bimodal75 + geom_tile(data=pls.prob.forest[pls.prob.forest$prob_forest <= 0.25 | pls.prob.forest$prob_forest >= 0.6,], aes(x=x, y=y, color = prob_forest))


pls.bim75 <- merge(pls.b75, dens.clust, by = c("x", "y", "cell"))

png("outputs/prob_bimodal_vs_PC1_0.75_bins.png")
ggplot(pls.bim75[!is.na(pls.bim75$foresttype_ordered),], aes(PC1, prob_bimodal, color = foresttype_ordered))+geom_point(size = 1)+scale_color_manual(values = c('#beaed4','#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+theme_bw()
dev.off()


# write out new figure 1 to a png
png(height = 14, width = 10, units = 'in', res = 300, "outputs/paper_figs/fig1_full_mi.png")
grid.arrange(pls.map + annotate("text", x=-90000, y=1486000,label= "A", size = 5), 
             pls.clust + annotate("text", x=-90000, y=1486000,label= "B", size = 5), 
             pls.dens.pc1.hex + annotate("text", x= 4, y=600,label= "C", size = 5), 
             clust.hist + annotate("text", x=600, y=15,label= "D", size = 5), 
             p.forest.map + annotate("text", x=-90000, y=1486000,label= "E", size = 5), ncol = 2)
dev.off()


plot_grid(pls.map, pls.clust, pls.dens.pc1.hex, clust.hist, p.forest.map, labels = c("A", "B", "C", "D", "E"), ncol = 2, align = "hv")

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
#cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
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

fia.bim$density_discrete <- ifelse(fia.bim$FIAdensity <= 0.5, "Prairie", 
                                   ifelse(fia.bim$FIAdensity <= 47, "Savanna",
                                          ifelse(fia.bim$FIAdensity > 47 & fia.bim$FIAdensity <= 100, "47-100",
                                                 ifelse(fia.bim$FIAdensity > 100 & fia.bim$FIAdensity <= 200, "100-200", 
                                                        ifelse(fia.bim$FIAdensity > 200 & fia.bim$FIAdensity <= 300, "200-300", 
                                                               ifelse(fia.bim$FIAdensity > 300 & fia.bim$FIAdensity <= 400, "300-400",
                                                                      ifelse(fia.bim$FIAdensity > 400 & fia.bim$FIAdensity <= 500, "400-500",
                                                                             ifelse(fia.bim$FIAdensity > 500 & fia.bim$FIAdensity <= 600, "600 +", "NA"))))))))

fia.bim$density_discrete<- factor(fia.bim$density_discrete, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500", "500-600", "600+", "NA"))


fia.map.alt.color <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fia.bim, aes(x=x, y=y, fill = density_discrete))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_manual(values = c("Prairie" ='#dfc27d',
                               "Savanna"='#8c510a',
                               "47-100"='#d9f0a3',
                               "100-200"='#addd8e',
                               "200-300"='#78c679',
                               "300-400"='#41ab5d',
                               "400-500"='#238443',
                               "500-600"='#005a32'), name ="Tree \n Density", na.value = 'darkgrey') +
  
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


png(height = 4, width = 3, units = "in", res = 300,"/Users/kah/Documents/bimodality/outputs/paper_figs/FIA_density_map_full_alt_colors.png")
fia.map.alt.color+ggtitle("FIA")
dev.off()


png(height = 4, width = 6, units = "in", res = 300,"/Users/kah/Documents/bimodality/outputs/paper_figs/PLS_FIA_density_map_full_alt_colors.png")
grid.arrange(pls.map.alt.color+ggtitle("PLS"),fia.map.alt.color+ggtitle("FIA"), ncol = 2)
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
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.25, 0.10),legend.background = element_rect(fill=alpha('transparent', 0)) ,
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

# alternate to geom_hex: geom_point with overlaid density contours:
p1fia <- ggplot(fia.dens.clust,aes(x=PC1fia, y=FIAdensity, color = foresttype)) +
  geom_point(alpha = 0.1, colour="orange")+ #+ geom_count(alpha = 0.1, colour="orange")+
  geom_density2d(data = fia.dens.clust,aes(x=PC1fia, y=FIAdensity), color = "blue") + 
  theme_bw()

scatter_dens_2dfia <- p1fia+xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+geom_vline(xintercept = -2.5)+geom_vline(xintercept = 1)+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
                                                                                                                                                                                                     legend.background = element_rect(fill=alpha('transparent', 0)))
                                                                                                                                                                                                     


past.logged<- read.csv("data/FIA_plot_data/fia.by.cell.treated.2000_2017.csv")
past.survey <- read.csv("data/FIA_plot_data/fia.by.cell.out_1980_1990.csv")
past.survey$new_scale <- ifelse(past.survey$INVYRcd %in% "1990s", 775, 1000)

# get data frame w/ 1980s, 1990s, and modern survey estimates:
modern.fia <- fia.dens.clust[,c("x", "y", "cell", "FIAdensity")]
modern.fia$INVYRcd <- "2000s"

full.fia.surveys <- rbind(modern.fia, past.survey[,c("x", "y", "cell", "FIAdensity", "INVYRcd")])

full.fia.surveys$INVYRcd <- factor(full.fia.surveys$INVYRcd, c("1980s", "1990s", "2000s"))
# D: Histogram colored by species cluster:
# make a histogram of denisty betwen -2.5 and 1 colored by species cluster:
f.clust.hist <- ggplot()+ geom_density(data = dens.nodups[dens.nodups$PC1 > -2.5 & dens.nodups$PC1 < 0.25 & dens.nodups$PLSdensity > 0.5, ], aes(PLSdensity, 23 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+
  geom_density(data = fia.dens.clust[fia.dens.clust$PC1fia > -2.5 & fia.dens.clust$PC1fia < 0.25, ], aes(FIAdensity, 23 *..count..),trim = TRUE , color = "black", size = 1.5)+
  geom_histogram(data = fia.dens.clust[fia.dens.clust$PC1fia > -2.5 & fia.dens.clust$PC1fia < 0.25, ], aes(FIAdensity, fill = foresttype))+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33'), name = " ")+coord_flip()+xlim(0,600)+ylim(0,700)+xlab("FIA tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.35, 0.85),legend.background = element_blank(), legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),
                                                                                                                                                                                                                                                                                                                                                                   panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank())
f.clust.hist 


inset <- ggboxplot( data= full.fia.surveys, x = 'INVYRcd',y = 'FIAdensity', merge=TRUE,width = 0.5, fill = "INVYRcd",  palette =c("grey28", "grey40", "grey60"), outlier.size = 0.0005)+ylim(0,600) +theme_bw(base_size = 8)+theme(axis.title = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(), legend.position = "none")#+theme_bw()
 #theme_transparent()
# Box plot of the y variable
#ybp <- ggboxplot(iris$Sepal.Width, width = 0.3, fill = "lightgray") +
 # theme_transparent()
# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(inset)

# Place box plots inside the scatter plot
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

f.clust.hist.inset <- f.clust.hist + annotation_custom(grob = xbp_grob, ymin = 375, ymax = 760, xmin = 0.005, xmax = 655)

#Other way of overlaying the plots:
box.inset <- ggplot(data = full.fia.surveys, aes(x = INVYRcd, y = FIAdensity, fill= INVYRcd, size = 0.25))+geom_boxplot(size = 0.25)+ylim(0,600)+theme_transparent()+theme(axis.title = element_blank(), axis.line = element_blank(), axis.ticks = element_blank(), axis.text.y = element_blank(), panel.grid.major = element_blank(), panel.background  = element_blank(), legend.position = "none")

hist.inset <- ggdraw() +
  draw_plot(f.clust.hist + theme(legend.justification = "bottom"), 0, 0, 1, 1) +
  draw_plot(inset, 0.6, 0.025, 0.3, 0.975)# +
  #draw_plot_label(c("A", "B"), c(0, 0.5), c(1, 0.92), size = 15)

ggplot(fia.dens.clust[fia.dens.clust$PC1fia > -2.5 & fia.dens.clust$PC1fia < 0.25, ], aes(x,y, fill = foresttype))+geom_raster()

vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.3, height = 1, x = 0.8, y = 0.51)  # the inset in upper right
print(f.clust.hist, vp = vpb_)
print(box.inset, vp = vpa_)




test.plot <-print(f.clust.hist, vp = vpb_); print(box.inset, vp = vpa_)

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


cbpalette <- c("#ffffcc", "#c2e699", "#d73027", "#31a354", "#006837")
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

p.forest.map.f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fia.prob.forest, aes(x=x, y=y, fill = pforest))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(title="Prob(forest)")+ scale_fill_manual(values= cbpalette, labels=c("0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw(base_size = 8)+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),
                                          axis.title = element_blank(),
                                          legend.key.size = unit(0.3,'lines'), legend.position = c(0.205, 0.13),
                                          legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.forest.map.f


# redraw the iset histogram
hist.inset <- ggdraw() +
  draw_plot(f.clust.hist, 0, 0, 1, 1) +
  draw_plot(inset + theme(axis.text.x = element_text(angle = 45)), 0.7, 0.075, 0.3, 0.85)# +


# write out new figure 2 to a png and annotate with A-F designations
png(height = 11, width = 4, units = 'in', res = 300, "outputs/paper_figs/fig2_10panel_v2.png")
grid.arrange(pls.map.alt.color + annotate("text", x=-90000, y=1486000,label= "A", size = 3)+ggtitle("PRE-SETTLEMENT"), 
             fia.map.alt.color + annotate("text", x=-90000, y=1486000,label= "F", size = 3)+ggtitle("MODERN"),
             pls.clust+ annotate("text", x=-90000, y=1486000,label= "B", size = 3),
             fia.clust + annotate("text", x=-90000, y=1486000,label= "G", size = 3), 
             scatter_dens_2dpls + annotate("text", x=4, y=600,label= "C", size = 3),
             scatter_dens_2dfia + annotate("text", x=4, y=600,label= "H", size = 3), 
             clust.hist + annotate("text", x=600, y=20,label= "D", size = 3),
             hist.inset + annotate("text", x=600, y=20,label= "I", size = 3), 
             p.forest.map + annotate("text", x=-90000, y=1486000,label= "E", size = 3),
             p.forest.map.f + annotate("text", x=-90000, y=1486000,label= "J", size = 3),
             ncol = 2)
dev.off()

png(height = 11, width = 4, units = 'in', res = 300, "outputs/paper_figs/fig2_10panel_v2b.png")

plot_grid(pls.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")), 
          fia.map.alt.color+theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
          pls.clust+theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
          fia.clust+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
          scatter_dens_2dpls+theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
          scatter_dens_2dfia+theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
          clust.hist+theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
          p.forest.map+theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
          p.forest.map.f+theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),labels = c("A", "B", "C", "D", "E","F", "G", "H", "I", "J"), ncol = 2, align = "v")

dev.off()



png(height = 9, width = 4, units = 'in', res = 300, "outputs/paper_figs/fig2_10panel_v2b.png")
plot_grid(pls.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "A", size = 3)+ggtitle("PRE-SETTLEMENT"), 
             fia.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "F", size = 3)+ggtitle("MODERN"),
             pls.clust+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "B", size = 3),
             fia.clust+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "G", size = 3), 
             scatter_dens_2dpls + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5, margin = unit(c(0, 0, 0, 0), "mm")))+ annotate("text", x=4, y=600,label= "C", size = 3),
             scatter_dens_2dfia + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5))+ annotate("text", x=4, y=600,label= "H", size = 3), 
             clust.hist + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5))+ annotate("text", x=600, y=20,label= "D", size = 3),
             hist.inset+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)) + annotate("text", x=600, y=20,label= "I", size = 3), 
             p.forest.map + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA))+ annotate("text", x=-90000, y=1486000,label= "E", size = 3),
             p.forest.map.f+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "J", size = 3),
             ncol = 2,align = "h",axis="tb", scale = 1) #scale = c(1.2, 1.2,
                                                         #1.2,1.2,
                                                         #1.09,1.09,
                                                         #1.09,1.09,
                                                         #1.2, 1.2,
                                                         #1.2,1.2))

dev.off()


png(height = 11, width = 4, units = 'in', res = 300, "outputs/paper_figs/fig2_10panel_non_ag_supplement_v2.png")
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


#------------------------------------P - PET figures----------------------------------
# lets make a version of figure 2 that displays bimdoality w.r.t. P-PET climate:

# Maps of P-PET in Past + modern
# relationship of PC1 and P-PET
# 2d isoline relationship wbeteen density + P-PET
# histogram of densitys 
# map of p(forest) w.r.t p-pet
dens.pr <- read.csv("data/PLS_FIA_density_climate_full.csv")


ppetpalette <- c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')

pls.ppet.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.pr, aes(x=x, y=y, fill = GS_ppet))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = ppetpalette, limits =c(-170, 300),name ="P-PET", na.value = 'darkgrey') +
  coord_equal()+theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text.x=element_blank(),
                                               legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                               axis.title.x=element_blank(),
                                               axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")


fia.ppet.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.pr, aes(x=x, y=y, fill = GS_ppet_mod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = ppetpalette,limits =c(-170, 300), name ="P-PET", na.value = 'darkgrey') +
  coord_equal()+theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text.x=element_blank(),
                                               legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                               axis.title.x=element_blank(),
                                               axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")




pc1.gsppet.pls <- ggplot(dens.pr, aes(PC1, GS_ppet))+geom_point(size = 0.5)+ylab("P - PET (mm)")+xlab("Principal Component 1")

pc1.gsppet.fia <- ggplot(dens.pr, aes(PC1fia, GS_ppet_mod))+geom_point(size = 0.5)+ylab("P - PET (mm)")+xlab("Principal Component 1")


p1pls.ppet <- ggplot(fia.dens.clust,aes(x=GS_ppet, y=PLSdensity, color = foresttype)) +
  geom_point(alpha = 0.1, colour="orange")+ #+ geom_count(alpha = 0.1, colour="orange")+
  geom_density2d(data = fia.dens.clust,aes(x=GS_ppet, y=PLSdensity), color = "blue") + 
  theme_bw()

scatter_ppet_dens_2dpls <- p1pls.ppet+xlab('P-PET') + ylab("Tree Density (stems/ha)")+ylim(0,650)+geom_vline(xintercept = 65)+geom_vline(xintercept = 150)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", legend.background = element_rect(fill=alpha('transparent', 0)))


p1fia.ppet <- ggplot(fia.dens.clust,aes(x=GS_ppet_mod, y=FIAdensity, color = foresttype)) +
  geom_point(alpha = 0.1, colour="orange")+ #+ geom_count(alpha = 0.1, colour="orange")+
  geom_density2d(data = fia.dens.clust,aes(x=GS_ppet_mod, y=FIAdensity), color = "blue") + 
  theme_bw()

scatter_ppet_dens_2dfia <- p1fia.ppet+xlab('P - PET') + ylab("Tree Density (stems/ha)")+ylim(0,650)+geom_vline(xintercept = 65)+geom_vline(xintercept = 150)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", legend.background = element_rect(fill=alpha('transparent', 0)))

# cluster histograms:

# fia:
past.logged<- read.csv("data/FIA_plot_data/fia.by.cell.treated.2000_2017.csv")
past.survey <- read.csv("data/FIA_plot_data/fia.by.cell.out_1980_1990.csv")
past.survey$new_scale <- ifelse(past.survey$INVYRcd %in% "1990s", 775, 1000)

# get data frame w/ 1980s, 1990s, and modern survey estimates:
modern.fia <- fia.dens.clust[,c("x", "y", "cell", "FIAdensity")]
modern.fia$INVYRcd <- "2000s"

full.fia.surveys <- rbind(modern.fia, past.survey[,c("x", "y", "cell", "FIAdensity", "INVYRcd")])

full.fia.surveys$INVYRcd <- factor(full.fia.surveys$INVYRcd, c("1980s", "1990s", "2000s"))
# D: Histogram colored by species cluster:
# make a histogram of denisty betwen -2.5 and 1 colored by species cluster:
dens.clust <- dens.clust[!duplicated(dens.clust),]

clust.hist.ppet <- ggplot()+ geom_density(data = dens.clust[dens.clust$GS_ppet > 65 & dens.clust$GS_ppet < 150 & dens.clust$PLSdensity > 0.5, ], aes(PLSdensity, 23 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_histogram(data = dens.clust[dens.clust$GS_ppet > 65 & dens.clust$GS_ppet < 150 & dens.clust$PLSdensity > 0.5, ], aes(PLSdensity, fill = foresttype_ordered, binwidth = 30))+xlim(0,600)+
  scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+coord_flip()+ylim(0,750)+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.65, 0.75),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.35, "line"),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
clust.hist.ppet


f.clust.hist.ppet <- ggplot()+ geom_density(data = dens.clust[dens.clust$GS_ppet > 65 & dens.clust$GS_ppet < 150 & dens.clust$PLSdensity > 0.5, ], aes(PLSdensity, 23 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+
  geom_density(data = fia.dens.clust[fia.dens.clust$GS_ppet_mod > 65 & fia.dens.clust$GS_ppet_mod < 150, ], aes(FIAdensity, 23 *..count..),trim = TRUE , color = "black", size = 1.5)+
  geom_histogram(data = fia.dens.clust[fia.dens.clust$GS_ppet_mod > 65 & fia.dens.clust$GS_ppet_mod < 150, ], aes(FIAdensity, fill = foresttype))+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33'), name = " ")+coord_flip()+xlim(0,600)+ylim(0,750)+xlab("FIA tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.35, 0.85),legend.background = element_blank(), legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),
                                                                                                                                                                                                                                                                                                                                                                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank())
f.clust.hist.ppet 


hist.inset <- ggdraw() +
  draw_plot(f.clust.hist.ppet, 0, 0, 1, 1) +
  draw_plot(inset + theme(axis.text.x = element_text(angle = 45)), 0.7, 0.075, 0.3, 0.85)# +

# draw p(forest):
pls.prob.forest <- read.csv("outputs/posterior_prob_forest_pls_ppet.csv") # from sample_density_probabilities script


cbpalette <- c("#ffffcc", "#c2e699", "#d73027", "#31a354", "#006837")
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

p.forest.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.prob.forest, aes(x=x, y=y, fill = pforest_ppet))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(title="Prob(forest)")+ scale_fill_manual(values= cbpalette, labels=c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")) +
  coord_equal()+theme_bw(base_size = 8)+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),
                                                       axis.title = element_blank(),
                                                       legend.key.size = unit(0.3,'lines'), legend.position = c(0.205, 0.13),
                                                       legend.background = element_rect(fill=alpha('transparent', 0)),
                                                       panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.forest.map


fia.prob.forest <- read.csv("outputs/posterior_prob_forest_fia_ppet.csv") # from sample_density_probabilities script


cbpalette <- c("#ffffcc", "#c2e699", "#d73027", "#31a354", "#006837")
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

p.forest.map.f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fia.prob.forest, aes(x=x, y=y, fill = pforest_ppet))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(title="Prob(forest)")+ scale_fill_manual(values= cbpalette, labels=c("0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw(base_size = 8)+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),
                                                       axis.title = element_blank(),
                                                       legend.key.size = unit(0.3,'lines'), legend.position = c(0.205, 0.13),
                                                       legend.background = element_rect(fill=alpha('transparent', 0)),
                                                       panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.forest.map.f

# write out new figure 2 to a png and annotate with A-F designations
png(height = 11, width = 4, units = 'in', res = 300, "outputs/paper_figs/fig2_10panel_v2_ppet.png")
grid.arrange(pls.ppet.map + annotate("text", x=-90000, y=1486000,label= "A", size = 3)+ggtitle("PRE-SETTLEMENT"), 
             fia.ppet.map + annotate("text", x=-90000, y=1486000,label= "F", size = 3)+ggtitle("MODERN"),
             pc1.gsppet.pls+theme_bw(base_size = 8) + annotate("text", x=-5, y=200,label= "B", size = 3),
             pc1.gsppet.fia +theme_bw(base_size = 8)+ annotate("text", x=-5, y=200,label= "G", size = 3), 
             scatter_ppet_dens_2dpls + annotate("text", x=-100, y=600,label= "C", size = 3),
             scatter_ppet_dens_2dfia + annotate("text", x=-100, y=600,label= "H", size = 3), 
             clust.hist.ppet + annotate("text", x=600, y=20,label= "D", size = 3),
             f.clust.hist.ppet + annotate("text", x=600, y=20,label= "I", size = 3), 
             p.forest.map + annotate("text", x=-90000, y=1486000,label= "E", size = 3),
             p.forest.map.f + annotate("text", x=-90000, y=1486000,label= "J", size = 3),
             ncol = 2)
dev.off()

#----------------------------------Composition change figs---------------------------------
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

pc.clust2 <- na.omit(pc.clust[pc.clust$PC1 > -0.1 & pc.clust$PC1 <= 1, ])

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

# is Oak compoisition bimodal in the past?
png("outputs/Composition/Oak_hist_modern_past.png")
ggplot(comp.pcs, aes(Oak, fill = period))+geom_histogram()+facet_wrap(~period)
dev.off()


#--------------------------------Make composition shift figures between 1980 and modern forests--------------
comp.pcs <- read.csv( "outputs/full_comp_pcs.csv")
pls.pcs <- comp.pcs[comp.pcs$period %in% "Past", c("x", "y", "cell", "pc1", "pc2")]
colnames(pls.pcs) <- c("x", "y","cell", "pls_pc1", "pls_pc2")
fia.pcs <- comp.pcs[comp.pcs$period %in% "Modern", c("x", "y", "cell", "pc1", "pc2")]
colnames(fia.pcs) <- c("x", "y","cell", "fia_pc1", "fia_pc2")


comp.pcs.old <- read.csv( "outputs/full_comp_pcs_old_surveys.csv")

fia.pcs.1980 <- comp.pcs.old[comp.pcs.old$period %in% "Modern-1980s", c("x", "y", "cell", "pc1", "pc2")]
colnames(fia.pcs.1980) <- c("x", "y","cell", "fia1980_pc1", "fia1980_pc2")

fia.pcs.1990 <- comp.pcs.old[comp.pcs.old$period %in% "Modern-1990s", c("x", "y", "cell", "pc1", "pc2")]
colnames(fia.pcs.1990) <- c("x", "y","cell", "fia1990_pc1", "fia1990_pc2")



pcs <- merge(fia.pcs, pls.pcs, by = c("x", "y", "cell"))
pcs <- merge(pcs, fia.pcs.1980, by = c("x", "y", "cell"))
pcs <- merge(pcs, fia.pcs.1990, by = c("x", "y", "cell"))

pc.clust <- merge(pcs, dens.clust, by = c("x", "y", "cell"))


colnames(pc.clust)[31] <- "pls_clust"
ggplot(pc.clust, aes(x,y, fill = pls_clust))+geom_raster()

# Messy arrow figures: pls oak category
ggplot(pc.clust[pc.clust$pls_clust %in% c("Oak" ),], aes(fia_pc1, fia_pc2))+geom_point(size = 0.2)+geom_point(data = pc.clust[pc.clust$pls_clust %in% "Oak",], aes(pls_pc1, pls_pc2, color = pls_clust), size = 0.2)+xlim(-7, 5)+ylim(-9,2)+geom_segment(aes(x = pls_pc1, y = pls_pc2, xend = fia_pc1, yend = fia_pc2), data =pc.clust[pc.clust$pls_clust %in% "Oak",], arrow = arrow(length = unit(0.5, "cm")), size = 0.05)+theme_bw()

pc.clust2 <- na.omit(pc.clust)

# plot mean arrows of all the species compositions PC values:
FIA1980.1890.shifts.full <- ggplot(pc.clust2, aes(fia_pc1, fia_pc2))+geom_point(size = 0.2, color = "darkgrey")+geom_point(data = pc.clust2, aes(pls_pc1, pls_pc2, color = pls_clust), size = 0.2)+scale_color_manual(values = c('#bf5b17', '#beaed4','#ffff99','#386cb0','#f0027f', '#fdc086','#7fc97f'))+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1990_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1990_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c("Oak" ),], arrow = arrow(length = unit(0.5, "cm")), size =1)+
  
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1990_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'N. Mixed Forest' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1990_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1990_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Aspen' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1990_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1990_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Oak-Hickory' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1990_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1990_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Boreal/Sub-boreal' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1990_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1990_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Beech-Maple' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1990_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1990_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Pine' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+theme_bw()+ylab("Species Composition PC2")+xlab("Species Composition PC1")+theme(legend.position = c(0.75, 0.25),legend.direction = "vertical", legend.title = element_blank(), legend.key.size = unit(1,'lines'), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ guides(colour = guide_legend(override.aes = list(size=2)))


# just the shifts between 1980 and modern
FIA1980.shifts.full <- ggplot(pc.clust2, aes(fia_pc1, fia_pc2))+geom_point(size = 0.2, color = "darkgrey")+geom_point(data = pc.clust2, aes(pls_pc1, pls_pc2, color = pls_clust), size = 0.2)+scale_color_manual(values = c('#bf5b17', '#beaed4','#ffff99','#386cb0','#f0027f', '#fdc086','#7fc97f'))+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c("Oak" ),], arrow = arrow(length = unit(0.5, "cm")), size =1)+
  
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'N. Mixed Forest' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Aspen' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Oak-Hickory' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Boreal/Sub-boreal' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Beech-Maple' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Pine' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+theme_bw()+ylab("Species Composition PC2")+xlab("Species Composition PC1")+theme(legend.position = c(0.25, 0.75),legend.direction = "vertical", legend.title = element_blank(), legend.key.size = unit(1,'lines'), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ guides(colour = guide_legend(override.aes = list(size=2)))

png(width = 6, height = 5, units = "in", res = 300, "outputs/paper_figs/composition_shift_plot_full_1980_FIA.png")
FIA1980.shifts.full
dev.off()


PLS1980.shifts.full <- ggplot(pc.clust2, aes(fia_pc1, fia_pc2))+geom_point(size = 0.2, color = "darkgrey")+geom_point(data = pc.clust2, aes(pls_pc1, pls_pc2, color = pls_clust), size = 0.2)+scale_color_manual(values = c('#bf5b17', '#beaed4','#ffff99','#386cb0','#f0027f', '#fdc086','#7fc97f'))+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1980_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c("Oak" ),], arrow = arrow(length = unit(0.5, "cm")), size =1)+
  
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1980_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'N. Mixed Forest' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1980_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Aspen' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1980_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Oak-Hickory' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1980_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Boreal/Sub-boreal' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1980_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Beech-Maple' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1980_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Pine' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+theme_bw()+ylab("Species Composition PC2")+xlab("Species Composition PC1")+theme(legend.position = c(0.25, 0.75),legend.direction = "vertical", legend.title = element_blank(), legend.key.size = unit(1,'lines'), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ guides(colour = guide_legend(override.aes = list(size=2)))

png(width = 6, height = 5, units = "in", res = 300, "outputs/paper_figs/composition_shift_plot_full_PLS_1980.png")
PLS1980.shifts.full
dev.off()

# lets look at only the places where density is bimodal:

pc.clust2 <- na.omit(pc.clust[pc.clust$PC1 > -0.1 & pc.clust$PC1 < 1, ])

# plot mean arrows of all the species compositions PC values:
FIA1980.1990.shifts.bim <- ggplot(pc.clust2, aes(fia_pc1, fia_pc2))+geom_point(size = 0.2, color = "darkgrey")+geom_point(data = pc.clust2, aes(pls_pc1, pls_pc2, color = pls_clust), size = 0.2)+scale_color_manual(values = c('#bf5b17', '#beaed4','#ffff99','#386cb0','#f0027f', '#fdc086','#7fc97f'))+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1990_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1990_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c("Oak" ),], arrow = arrow(length = unit(0.5, "cm")), size =1)+
  
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1990_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'N. Mixed Forest' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1990_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1990_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Aspen' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1990_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1990_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Oak-Hickory' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1990_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1990_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Boreal/Sub-boreal' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1990_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1990_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Beech-Maple' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1990_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1990_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Pine' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+theme_bw()+ylab("Species Composition PC2")+xlab("Species Composition PC1")+theme(legend.position = c(0.75, 0.755),legend.direction = "vertical", legend.title = element_blank(), legend.key.size = unit(1,'lines'), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ guides(colour = guide_legend(override.aes = list(size=2)))

png(width = 6, height = 5, units = "in", res = 300, "outputs/paper_figs/composition_shift_plot_bimodal_1980_1990.png")
FIA1980.1990.shifts.bim+ggtitle("Composition shifts between 1980's and 1990's surveys (bimodal reg.)")
dev.off()

# just the shifts between 1980 and modern
FIA1980.shifts.bim <- ggplot(pc.clust2, aes(fia_pc1, fia_pc2))+geom_point(size = 0.2, color = "darkgrey")+geom_point(data = pc.clust2, aes(pls_pc1, pls_pc2, color = pls_clust), size = 0.2)+scale_color_manual(values = c('#bf5b17', '#beaed4','#ffff99','#386cb0','#f0027f', '#fdc086','#7fc97f'))+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c("Oak" ),], arrow = arrow(length = unit(0.5, "cm")), size =1)+
  
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'N. Mixed Forest' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Aspen' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Oak-Hickory' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Boreal/Sub-boreal' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Beech-Maple' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1980_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1980_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Pine' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+theme_bw()+ylab("Species Composition PC2")+xlab("Species Composition PC1")+theme(legend.position = c(0.75, 0.75),legend.direction = "vertical", legend.title = element_blank(), legend.key.size = unit(1,'lines'), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ guides(colour = guide_legend(override.aes = list(size=2)))

png(width = 6, height = 5, units = "in", res = 300, "outputs/paper_figs/composition_shift_plot_bimodal_1980_FIA.png")
FIA1980.shifts.bim+ggtitle("Composition shifts between 1980 and latest FIA survey (bimodal reg.)")
dev.off()


PLS1980.shifts.bim <- ggplot(pc.clust2, aes(fia_pc1, fia_pc2))+geom_point(size = 0.2, color = "darkgrey")+geom_point(data = pc.clust2, aes(pls_pc1, pls_pc2, color = pls_clust), size = 0.2)+scale_color_manual(values = c('#bf5b17', '#beaed4','#ffff99','#386cb0','#f0027f', '#fdc086','#7fc97f'))+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1980_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% "Oak", ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c("Oak" ),], arrow = arrow(length = unit(0.5, "cm")), size =1)+
  
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1980_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'N. Mixed Forest', ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'N. Mixed Forest' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1980_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Aspen', ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Aspen' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1980_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Oak-Hickory', ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Oak-Hickory' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1980_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Boreal/Sub-boreal', ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Boreal/Sub-boreal' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1980_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Beech-Maple', ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Beech-Maple' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+
  geom_segment(aes(
    x = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$pls_pc1), 
    y = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$pls_pc2), 
    xend = mean( pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1980_pc1), 
    yend = mean(pc.clust2[pc.clust2$pls_clust %in% 'Pine', ]$fia1980_pc2)), data =pc.clust2[pc.clust2$pls_clust %in% c( 'Pine' ),], arrow = arrow(length = unit(0.5, "cm")), size = 1)+theme_bw()+ylab("Species Composition PC2")+xlab("Species Composition PC1")+theme(legend.position = c(0.75, 0.75),legend.direction = "vertical", legend.title = element_blank(), legend.key.size = unit(1,'lines'), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ guides(colour = guide_legend(override.aes = list(size=2)))

png(width = 6, height = 5, units = "in", res = 300, "outputs/paper_figs/composition_shift_plot_full_PLS_1980.png")
PLS1980.shifts.bim+ggtitle("Composition shifts between PLS and 1980s (bimodal reg.)")
dev.off()

# add in the pca shift plots here:
shifted.pca <- bimodal.region.shifts +theme(legend.direction = "vertical", legend.key.size = unit(0.5, "line"), legend.position = c(0.75, 0.2) ) + guides(colour = guide_legend(override.aes = list(size=3)))

# is Oak compoisition bimodal in the past?
png("outputs/Composition/Oak_hist_modern_past.png")
ggplot(comp.pcs, aes(Oak, fill = period))+geom_histogram()+facet_wrap(~period)
dev.off()






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



#--------------------What happened to the places with highest densities in the past??------------

pls.d <- dens.clust
fia.d <- fia.dens.clust[,c("x", "y", "cell", 'speciescluster', "foresttype")]
colnames(fia.d) <- c("x", "y", "cell", "fia_speciescluster", "fia_foresttype")

full.clust <- merge(pls.d, fia.d, by = c("x", "y", "cell"))


full.clust$PLSdiff <-  full.clust$FIAdensity - full.clust$PLSdensity 
ggplot(full.clust, aes(PLSdensity, PLSdiff, color = foresttype))+geom_point(size = 0.5)



# median of the PLS mode above 100 trees/ha: 236.9
summary(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity > 100, ])

# median of the FIA mode in the same environmental space = 159.86
summary(full.clust[full.clust$PC1fia > -2.5 & full.clust$PC1fia < 1 , ]$FIAdensity)


ggplot(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity > 236, ], aes(x, y, fill = foresttype))+geom_raster()

ggplot(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity > 236, ], aes(x, y, fill = fia_foresttype))+geom_raster()
ggplot(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity > 236, ], aes(FIAdensity, fill = fia_foresttype))+geom_histogram()


# histograms of tree density by themselves
ggplot(na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 , ]), aes(PLSdensity, fill = foresttype_ordered))+geom_histogram()+facet_wrap(~foresttype)+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")

# plots of histograms of density by pls species cluster
png(height = 5, width = 6, units = "in",res = 300,'outputs/density_shifts/histogram_shifts_density_by_clusters.png')
ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 , ]), aes(PLSdensity, fill = foresttype_ordered))+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1, ]), aes(FIAdensity),trim = TRUE , fill = "grey",alpha = 0.5, size = 1.5)+
  facet_wrap(~foresttype)+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+ylim(0,200)+theme_bw()+ggtitle("Density of composition clusters in bimodal climate \n (grey = same grid cells in FIA)")
dev.off()

png(height = 5, width = 6, units = "in",res = 300,'outputs/density_shifts/histogram_shifts_density_by_clusters_fia.png')
ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 , ]), aes(FIAdensity, fill = fia_foresttype))+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1, ]), aes(PLSdensity),trim = TRUE , fill = "grey",alpha = 0.5, size = 1.5)+
  facet_wrap(~foresttype)+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33'), name = " ")+ylim(0,200)+theme_bw()+ggtitle("Density of composition clusters in bimodal climate \n (grey = same grid cells in PLS)")
dev.off()

# plots of histograms of density by pls species cluster in high density space:

png(height = 5, width = 6, units = "in",res = 300,'outputs/density_shifts/histogram_shifts_density_by_clusters_over_230_trees.png')
ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity > 230 , ]), aes(PLSdensity, fill = foresttype_ordered))+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1& full.clust$PLSdensity > 230, ]), aes(FIAdensity),trim = TRUE , fill = "grey",alpha = 0.5, size = 1.5)+
  facet_wrap(~foresttype)+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+ylim(0,200)+theme_bw()+ggtitle("Density of composition clusters in bimodal climate \n (grey = same grid cells in FIA)")
dev.off()

png(height = 5, width = 6, units = "in",res = 300,'outputs/density_shifts/histogram_shifts_density_by_clusters_fia_over_230_trees.png')
ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity > 230, ]), aes(FIAdensity, fill = fia_foresttype))+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity > 230, ]), aes(PLSdensity),trim = TRUE , fill = "grey",alpha = 0.5, size = 1.5)+
  facet_wrap(~foresttype)+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33'), name = " ")+ylim(0,200)+theme_bw()+ggtitle("Density of composition clusters in bimodal climate \n (grey = same grid cells in PLS)")
dev.off()


png(height = 5, width = 6, units = "in",res = 300,'outputs/density_shifts/histogram_shifts_density_by_clusters_under_230_trees.png')
ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity < 230 , ]), aes(PLSdensity, fill = foresttype_ordered))+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1& full.clust$PLSdensity < 230, ]), aes(FIAdensity),trim = TRUE , fill = "grey",alpha = 0.5, size = 1.5)+
  facet_wrap(~foresttype)+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+ylim(0,200)+theme_bw()+ggtitle("Density of composition clusters in bimodal climate \n (grey = same grid cells in FIA)")
dev.off()

png(height = 5, width = 6, units = "in",res = 300,'outputs/density_shifts/histogram_shifts_density_by_clusters_fia_under_230_trees.png')
ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity < 230, ]), aes(FIAdensity, fill = fia_foresttype))+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity < 230, ]), aes(PLSdensity),trim = TRUE , fill = "grey",alpha = 0.5, size = 1.5)+
  facet_wrap(~foresttype)+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33'), name = " ")+ylim(0,200)+theme_bw()+ggtitle("Density of composition clusters in bimodal climate \n (grey = same grid cells in PLS)")
dev.off()


# on average, the low density mode increased and the high density mode decreased in PLS:
summary(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity <100, ])
summary(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity >100, ])
summary(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity >100 & full.clust$PLSdensity <230, ])
summary(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity >100 & full.clust$PLSdensity >230, ])


# plot histograms of PLSdiff by species composition clusters:
ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity < 230 , ]), aes(PLSdiff, fill = foresttype_ordered))

png(height = 5, width = 6, units = "in",res = 300,'outputs/density_shifts/change_in_density_by_plsclusters_bimodal_region.png')
ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1  , ]), aes(PLSdiff, fill = foresttype_ordered))+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+
  geom_vline(xintercept = 0, color = "grey")+facet_wrap(~foresttype_ordered)+xlab("Change in density between PLS and FIA \n (positive = increse in density, negative = decrease in density)")+ggtitle("Change in tree density by PLS forest type (bimodal region)")
dev.off()

png(height = 5, width = 6, units = "in",res = 300,'outputs/density_shifts/change_in_density_by_plsclusters_bimodal_region_over_236.png')
ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity > 236 , ]), aes(PLSdiff, fill = foresttype_ordered))+scale_fill_manual(values = c( '#beaed4','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+
  geom_vline(xintercept = 0, color = "grey")+facet_wrap(~foresttype_ordered)+xlab("Change in density between PLS and FIA \n (positive = increse in density, negative = decrease in density)")+ggtitle("Change in tree density by PLS forest type (bimodal region) \n PLS tree density > 236")
dev.off()

png(height = 5, width = 6, units = "in",res = 300,'outputs/density_shifts/change_in_density_by_plsclusters_bimodal_region_under_236.png')
ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity < 236 , ]), aes(PLSdiff, fill = foresttype_ordered))+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+
  geom_vline(xintercept = 0, color = "grey")+facet_wrap(~foresttype_ordered)+xlab("Change in density between PLS and FIA \n (positive = increse in density, negative = decrease in density)")+ggtitle("Change in tree density by PLS forest type (bimodal region)\n PLS tree density < 236")
dev.off()


full.diff<- ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1  , ]), aes(PLSdiff, fill = foresttype_ordered))+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+
  geom_vline(xintercept = 0, color = "grey")+facet_wrap(~foresttype_ordered, ncol = 7)+xlab("Change in density between PLS and FIA \n (positive = increse in density, negative = decrease in density)")+ggtitle("Change in tree density by PLS forest type (bimodal region)")

high.diff <- ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity > 236 , ]), aes(PLSdiff, fill = foresttype_ordered))+scale_fill_manual(values = c( '#beaed4','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+
  geom_vline(xintercept = 0, color = "grey")+facet_wrap(~foresttype_ordered, ncol = 6)+xlab("Change in density between PLS and FIA \n (positive = increse in density, negative = decrease in density)")+ggtitle("Change in tree density by PLS forest type (bimodal region) \n PLS tree density > 236")

low.diff <- ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity < 236 , ]), aes(PLSdiff, fill = foresttype_ordered))+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+
  geom_vline(xintercept = 0, color = "grey")+facet_wrap(~foresttype_ordered, ncol = 7)+xlab("Change in density between PLS and FIA \n (positive = increse in density, negative = decrease in density)")+ggtitle("Change in tree density by PLS forest type (bimodal region)\n PLS tree density < 236")

png(height = 10, width = 12,units = "in",res = 300, 'outputs/density_shifts/change_in_density_by_plsclusters_bimodal_region.png')
grid.arrange(full.diff+theme_bw(), high.diff+theme_bw(), low.diff+theme_bw(), nrow = 3)
dev.off()


# map out the highest density composition clusters in space:

full.bim.clust<- ggplot()+geom_raster(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1  , ]), aes(x,y, fill = foresttype_ordered))+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+theme_bw()+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.20, 0.18),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                                           axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),axis.text.x=element_blank(),
                                                                                                                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                           axis.title.x=element_blank(),
                                                                                                                           axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()+ggtitle("Bimodal region")


high.bim.clust<- ggplot()+geom_raster(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity > 236 , ]), aes(x,y, fill = foresttype_ordered))+scale_fill_manual(values = c( '#beaed4','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+theme_bw()+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.20, 0.18),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                                           axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),axis.text.x=element_blank(),
                                                                                                                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                           axis.title.x=element_blank(),
                                                                                                                           axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()+ggtitle("Bimodal region >= 236 trees/ha")


low.bim.clust<- ggplot()+geom_raster(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity < 236 , ]), aes(x,y, fill = foresttype_ordered))+scale_fill_manual(values = c( '#beaed4','#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+theme_bw()+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.20, 0.18),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                                           axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),axis.text.x=element_blank(),
                                                                                                                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                           axis.title.x=element_blank(),
                                                                                                                           axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()+ggtitle("Bimodal region < 236 trees/ha")

png(height = 4, width = 9, units = "in", res = 300, "outputs/density_shifts/maps_bimodal_reg_clusters_by_density.png")
grid.arrange(full.bim.clust, high.bim.clust, low.bim.clust, ncol = 3)
dev.off()
# median of modern fia from high tree density greion is 178.45 TPH, but 163.58 TPH
summary(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity > 236, ])
summary(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity < 236 & full.clust$PLSdensity > 100, ])

summary(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1 & full.clust$PLSdensity > 236, ])



# what about differences between the past FIA surveys and the present:

clust_plot5.1990 <- read.csv("outputs/cluster/density_fia_1990s_with_clusters.csv")
clust_plot5.1980 <- read.csv( "outputs/cluster/density_fia_1980s_with_clusters.csv")

clust_plot5.1980$INVYRcd <- "1980s"
clust_plot5.1990$INVYRcd <- "1990s"

old.clusts <- rbind(clust_plot5.1980, clust_plot5.1990)

densitys.old <- read.csv( paste0("data/midwest_pls_fia_density_old_surveys_alb", version,".csv"))
dens.clust.fold<- merge(densitys.old, old.clusts, by = c("x", "y", "cell", "INVYRcd"))

ggplot(dens.clust.fold, aes(FIAdensity, fill = speciescluster))+geom_histogram()+facet_wrap(~INVYRcd)
clust.fold <- dens.clust.fold[,c("x", "y", "cell","PLSdensity", "FIAdensity", "period", "speciescluster")]

clust.fold$foresttype <- plyr::revalue(clust.fold$speciescluster,c("Oak/Maple" = "Oak/Maple", 
                                                                         "Maple/Ash/Birch/Aspen" = "Mixed Hardwoods",
                                                                         "Maple" = "Maple", 
                                                                         "Aspen"="Aspen", 
                                                                         "Pine/Poplar"="Pine"
                                                                         ))


clust.fold.1990 <- clust.fold[clust.fold$period %in% "Modern-1990s",]
colnames(clust.fold.1990) <- c("x", "y", "cell", "PLSdensity", "FIAdensity_1990","period", "speciescluster_1990", "foresttype_1990")

clust.fold.1980 <- clust.fold[clust.fold$period %in% "Modern-1980s",]
colnames(clust.fold.1980) <- c("x", "y", "cell", "PLSdensity", "FIAdensity_1980","period", "speciescluster_1980", "foresttype_1980")

full.clust <- merge(full.clust, clust.fold.1980[,c(c("x", "y", "cell", "FIAdensity_1980","period", "speciescluster_1980", "foresttype_1980"))], by = c("x", "y", "cell"))

full.clust <- merge(full.clust, clust.fold.1990[,c(c("x", "y", "cell", "FIAdensity_1990", "speciescluster_1990", "foresttype_1990"))], by = c("x", "y", "cell"))

full.clust$PLS1980diff <- full.clust$FIAdensity_1980 - full.clust$PLSdensity 
full.clust$PLS1990diff <- full.clust$FIAdensity_1990 - full.clust$PLSdensity 
full.clust$FIA1980_moddiff <- full.clust$FIAdensity - full.clust$FIAdensity_1980 
full.clust$FIA1990_moddiff<- full.clust$FIAdensity - full.clust$FIAdensity_1990 
full.clust$FIA1980_1990diff  <- full.clust$FIAdensity_1990 - full.clust$FIAdensity_1980 

ggplot(full.clust, aes(FIA1980_moddiff, fill =fia_foresttype ))+geom_histogram()+facet_wrap(~foresttype_1980)
ggplot(full.clust, aes(FIA1990_moddiff, fill =fia_foresttype))+geom_histogram()+facet_wrap(~foresttype_1990)

ggplot(full.clust, aes(FIA1980_1990diff, fill =foresttype_1990 ))+geom_histogram()+facet_wrap(~foresttype_1990)

ggplot(full.clust, aes(FIA1980_1990diff, fill =foresttype_1990 ))+geom_histogram()+facet_wrap(~foresttype_1990)


ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1  , ]), aes(PLS1980diff, fill = foresttype_ordered))+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+
  geom_vline(xintercept = 0, color = "grey")+facet_wrap(~foresttype_ordered)+xlab("Change in density between PLS and FIA \n (positive = increse in density, negative = decrease in density)")+ggtitle("Change in tree density by PLS forest type (bimodal region)")

ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1  , ]), aes(PLS1990diff, fill = foresttype_ordered))+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+
  geom_vline(xintercept = 0, color = "grey")+facet_wrap(~foresttype_ordered)+xlab("Change in density between PLS and FIA \n (positive = increse in density, negative = decrease in density)")+ggtitle("Change in tree density by PLS forest type (bimodal region)")

ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1  , ]), aes(FIA1980_moddiff, fill = foresttype_ordered))+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+
  geom_vline(xintercept = 0, color = "grey")+facet_wrap(~foresttype_ordered)+xlab("Change in density between PLS and FIA \n (positive = increse in density, negative = decrease in density)")+ggtitle("Change in tree density by PLS forest type (bimodal region)")

ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1  , ]), aes(FIA1990_moddiff, fill = foresttype_ordered))+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+
  geom_vline(xintercept = 0, color = "grey")+facet_wrap(~foresttype_ordered)+xlab("Change in density between PLS and FIA \n (positive = increse in density, negative = decrease in density)")+ggtitle("Change in tree density by PLS forest type (bimodal region)")

ggplot()+geom_histogram(data = na.omit(full.clust[full.clust$PC1 > -2.5 & full.clust$PC1 < 1  , ]), aes(FIA1980_1990diff, fill = foresttype_ordered))+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = "")+
  geom_vline(xintercept = 0, color = "grey")+facet_wrap(~foresttype_ordered)+xlab("Change in density between PLS and FIA \n (positive = increse in density, negative = decrease in density)")+ggtitle("Change in tree density by PLS forest type (bimodal region)")



full.clust.hist <- ggplot()+ geom_density(data = na.omit(dens.clust), aes(PLSdensity, 23 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+
  geom_density(data = na.omit(fia.dens.clust), aes(FIAdensity, 23 *..count..),trim = TRUE , color = "black", size = 1.5)+
  #geom_histogram(data = na.omit(fia.dens.clust), aes(FIAdensity, fill = foresttype))+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33', "red"), name = " ")+
  geom_density(data = na.omit(full.clust), aes(FIAdensity_1980, 23 *..count..),trim = TRUE , color = "red", size = 1)+
 geom_density(data = na.omit(full.clust), aes(FIAdensity_1990, 23 *..count..),trim = TRUE , color = "blue", size = 1)+coord_flip()+xlim(0,600)+ylim(0,1000)+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.70, 0.75),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.35, "line"), 
                                                                                                                                                                                                                                                                                                                                                                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())
full.clust.hist

bim.clust.hist <- ggplot()+ geom_density(data = dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < 1 & dens.clust$PLSdensity > 0.5, ], aes(PLSdensity, 23 *..count.., color = "PLS"),linetype="dashed" , bw = 12,size = 1.5)+
  geom_density(data = fia.dens.clust[fia.dens.clust$PC1fia > -2.5 & fia.dens.clust$PC1fia < 1, ], aes(FIAdensity, 23 *..count.., color = "FIA"),trim = TRUE , size = 1.5)+
  #geom_histogram(data = na.omit(fia.dens.clust), aes(FIAdensity, fill = foresttype))+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33', "red"), name = " ")+
  geom_density(data = full.clust[full.clust$PC1fia > - 2.5 & full.clust$PC1fia < 1,], aes(FIAdensity_1980, 23 *..count.., color = "FIA-1980's"),trim = TRUE , size = 1)+
  geom_density(data = full.clust[full.clust$PC1fia > - 2.5 & full.clust$PC1fia < 1,], aes(FIAdensity_1990, 23 *..count.., color = "FIA-1990's"),trim = TRUE , size = 1)+
  scale_color_manual(values = c('PLS' = 'grey', 'FIA' = 'black', "FIA-1980's" = "red", "FIA-1990's" = "blue" ))+coord_flip()+xlim(0,600)+ylim(0,700)+xlab("Tree Density (stems/ha)")+ylab("# grid cells")+theme_bw(base_size = 20)+theme(aspect.ratio = 1,legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(1, "line"), 
                                                                                                                                                                                                                                         panel.grid.major = element_blank(), panel.grid.minor = element_blank())
png("outputs/paper_figs/Past_present_density_surveys_bimodal_reg.png")
bim.clust.hist 
dev.off()

full.clust.hist <- ggplot()+ geom_density(data = dens.clust[dens.clust$PLSdensity > 0.5,], aes(PLSdensity, 23 *..count.., color = "PLS"),linetype="dashed" , bw = 15,size = 1.5)+
  geom_density(data = fia.dens.clust, aes(FIAdensity, 23 *..count.., color = "FIA"),trim = TRUE , size = 1.5)+
  #geom_histogram(data = na.omit(fia.dens.clust), aes(FIAdensity, fill = foresttype))+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33', "red"), name = " ")+
  geom_density(data = full.clust, aes(FIAdensity_1980, 23 *..count.., color = "FIA-1980's"),trim = TRUE , size = 1, bw = 15)+
  geom_density(data = full.clust, aes(FIAdensity_1990, 23 *..count.., color = "FIA-1990's"),trim = TRUE , size = 1, bw = 15)+
  scale_color_manual(values = c('PLS' = 'grey', 'FIA' = 'black', "FIA-1980's" = "red", "FIA-1990's" = "blue" ))+coord_flip()+xlim(0,600)+ylim(0,1500)+xlab("Tree Density (stems/ha)")+ylab("# grid cells")+theme_bw(base_size = 20)+theme(aspect.ratio = 1,legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(1, "line"), 
                                                                                                                                                                                                                                         panel.grid.major = element_blank(), panel.grid.minor = element_blank())
png("outputs/paper_figs/Past_present_density_surveys_full.png")
full.clust.hist 
dev.off()


f.clust.hist <- ggplot()+ geom_density(data = dens.clust[dens.clust$PC1 > -2.5 & dens.clust$PC1 < 1 & dens.clust$PLSdensity > 0.5, ], aes(PLSdensity, 23 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+
  geom_density(data = fia.dens.clust[fia.dens.clust$PC1fia > -2.5 & fia.dens.clust$PC1fia < 1, ], aes(FIAdensity, 23 *..count..),trim = TRUE , color = "black", size = 1.5)+
  geom_histogram(data = fia.dens.clust[fia.dens.clust$PC1fia > -2.5 & fia.dens.clust$PC1fia < 1, ], aes(FIAdensity, fill = foresttype))+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33'), name = " ")+coord_flip()+xlim(0,600)+ylim(0,700)+xlab("tree density (stems/ha)")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.70, 0.75),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.35, "line"), 
                                                                                                                                                                                                                                                                                                                                                                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())
f.clust.hist

# read in the density of managed forests:

logged.stands <- read.csv("data/FIA_plot_data/fia.by.cell.treated.2000_2017.csv")

# The number of logged cells is low compared to the total number of survey points:
ggplot()+ geom_density(data = dens.clust[dens.clust$PLSdensity > 0.5,], aes(PLSdensity, 23 *..count.., color = "PLS"),linetype="dashed" , bw = 15,size = 1.5)+
  geom_density(data = fia.dens.clust, aes(FIAdensity, 23 *..count.., color = "FIA"),trim = TRUE , size = 1.5)+
  #geom_histogram(data = na.omit(fia.dens.clust), aes(FIAdensity, fill = foresttype))+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33', "red"), name = " ")+
  geom_density(data = full.clust, aes(FIAdensity_1980, 23 *..count.., color = "FIA-1980's"),trim = TRUE , size = 1, bw = 15)+
  geom_density(data = full.clust, aes(FIAdensity_1990, 23 *..count.., color = "FIA-1990's"),trim = TRUE , size = 1, bw = 15)+
  geom_density(data = logged.stands, aes(FIAdensity, 23*..count.., color = "Logged-2000's"),trim = TRUE ,size = 1, bw = 15)+
  scale_color_manual(values = c('PLS' = 'grey', 'FIA' = 'black', "FIA-1980's" = "red", "FIA-1990's" = "blue", "Logged-2000's" = "green" ))+coord_flip()#+xlim(0,600)+ylim(0,1500)+xlab("Tree Density (stems/ha)")+ylab("# grid cells")+theme_bw(base_size = 20)#+theme(aspect.ratio = 1,legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(1, "line"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())



Allfiahist<- ggplot()+# geom_density(data = na.omit(dens.clust[dens.clust$PLSdensity > 0.5,]), aes(PLSdensity, 23 *..count.., color = "PLS"),linetype="dashed" , bw = 15,size = 1.5)+
  geom_histogram(data = na.omit(fia.dens.clust), aes(FIAdensity,fill = "FIA"),bins = 20 , alpha = 0.5)+
  #geom_histogram(data = na.omit(fia.dens.clust), aes(FIAdensity, fill = foresttype))+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33', "red"), name = " ")+
  geom_histogram(data = na.omit(full.clust), aes(FIAdensity_1980,  fill = "FIA-1980's"),bins = 20  , alpha = 0.5)+
  geom_histogram(data = na.omit(full.clust), aes(FIAdensity_1990,  fill = "FIA-1990's"),bins = 20 ,  alpha = 0.5)+
  geom_histogram(data = na.omit(logged.stands[logged.stands$FIAdensity < 600,]), aes(FIAdensity,  fill = "Logged-2000's"),bins = 20 , alpha = 0.5)+
  geom_vline(xintercept = 178)+
  scale_fill_manual(values = c("PLS"="grey",'FIA' = 'yellow', "FIA-1980's" = "red", "FIA-1990's" = "blue", "Logged-2000's" = "green" ), name = " ")+coord_flip()+theme_bw()#+xlim(0,600)+ylim(0,1500)+xlab("Tree Density (stems/ha)")+ylab("# grid cells")+theme_bw(base_size = 20)#+theme(aspect.ratio = 1,legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(1, "line"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# save to file
png(height = 4, width = 4, units ="in",res = 300, "outputs/allfia_logged_non_logged_hist.png")
Allfiahist
dev.off()

ggplot(logged.stands, aes(Maple))+geom_histogram()

logged.m <- melt(logged.stands, id.vars = c("x","y", "cell", "INVYR"))
 
ggplot(na.omit(logged.m[!logged.m$variable %in% "FIAdensity" & logged.m$value < 1000,]), aes(value))+geom_histogram()  + facet_wrap(~variable) +xlim(0,1000)                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                     
       