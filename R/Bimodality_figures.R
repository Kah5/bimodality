# make figures for the paper & coduct bimodality analyses:
# migrated some of this code from bimodality_draft_3.1.17

library(gridExtra)
library(grid)
library(ggplot2)
library(maps)
library(sp)
library(plyr)
library(maps)
library(cowplot)
library(dplyr)
library(tidyr)

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
  scale_fill_gradientn(colours = cbpalette, limits = c(0,500), name ="Tree \n Density", na.value = 'darkgrey') +
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
                                                                             ifelse(dens.pr$PLSdensity > 500 , "500+", "No data"))))))))

dens.pr$density_discrete<- factor(dens.pr$density_discrete, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500",  "500+", "No data"))


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
    '#005a32',"darkgrey"), name ="Tree Density", na.value = 'darkgrey', drop = F) +
  
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.3, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


png(height = 4, width = 3, units = "in", res = 300,"/Users/kah/Documents/bimodality/outputs/paper_figs/PLS_density_map_full_alt_colors.png")
pls.map.alt.color+ggtitle("PLS")
dev.off()


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Prob(biomodality) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
# these come from scripts run on crc to sample probabilty of bimodality from the distribution of tree density w/ given climate

# --------------------------------read in p(bimodality) base on Soil moisture with 0.1 width bins)

pls.SM0.1 <- read.csv("outputs/posterior_prob_bimodal_pls_0.1bins_dipP_only_GS_soil_by_distn.csv")
pls.SM0.1$pbimodal <- cut(pls.SM0.1$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

rbpalette <- c('#ca0020',
               '#f4a582',
               '#f7f7f7',
               '#92c5de',
               '#0571b0')
names(rbpalette) <- rev(c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1"))

pls.SM0.1$pbimodal <- as.character(pls.SM0.1$pbimodal)

p.bimodalSM0.1 <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.SM0.1, aes(x=x, y=y, fill = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+ scale_fill_manual(values= rbpalette) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")

# --------------------------------read in p(bimodality) base on P-PET with 10 width bins)
# not run yet
pls.PPET.1 <- read.csv("outputs/posterior_prob_bimodal_pls_10bins_dipP_only_PPET_by_distn.csv")
pls.PPET.1$pbimodal <- cut(pls.PPET.1$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

rbpalette <- c('#ca0020',
               '#f4a582',
               '#f7f7f7',
               '#92c5de',
               '#0571b0')
names(rbpalette) <- rev(c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1"))

pls.PPET.1$pbimodal <- as.character(pls.PPET.1$pbimodal)

p.bimodalPPET.1 <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.PPET.1, aes(x=x, y=y, fill = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= rbpalette) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")


# --------------------------------read in p(bimodality PC1 with 0.15 bins)

pls.b15 <- read.csv("outputs/posterior_prob_bimodal_pls_0.1bins_dipP_only_PC1_by_distn.csv")
pls.b15$pbimodal <- cut(pls.b15$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

rbpalette <- c('#ca0020',
               '#f4a582',
               '#f7f7f7',
               '#92c5de',
               '#0571b0')
names(rbpalette) <- rev(c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1"))

pls.b15$pbimodal <- as.character(pls.b15$pbimodal)

p.bimodal15 <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.b15, aes(x=x, y=y, fill = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= rbpalette) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")


# merge with composition cluster data
pls.b15 <- merge(clust_7, pls.b15, by = c("x", "y", "cell"))

label.breaks <- function(beg, end, splitby){
  labels.test <- data.frame(first = seq(beg, end, by = splitby), second = seq((beg + splitby), (end + splitby), by = splitby))
  labels.test <- paste (labels.test$first, '-' , labels.test$second)
  labels.test
}
# use the label.breaks function and cut to cut environmental data up into different bins

pls.b15$PC1bins <- cut(pls.b15$PC1, breaks = seq(-5.5, 4.5, by = 0.5), labels = label.breaks(-5.5, 4,  0.5))


png("outputs/prob_bimodal_vs_PC1_0.15_bins.png")
ggplot(pls.b15[!is.na(pls.b15$orderedforesttype),], aes(PC1, prob_bimodal, color = orderedforesttype))+geom_point(size = 1)+scale_color_manual(values = c('#beaed4','#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+theme_bw()
dev.off()

png(height = 10, width = 10, units = "in", res = 300,"outputs/prob_bimodal_facet_by_PC1bins.png")
ggplot(pls.b15, aes(prob_bimodal, fill = orderedforesttype))+geom_histogram()+facet_wrap(~PC1bins)
dev.off()


# make a plot of the maps of composition by PC bins:
png(width = 12, height = 12, units = "in", res = 300, "outputs/map_comp_facet_by_PC1bins_onlypbim_over_0.8.png")
ggplot(pls.b15[pls.b15$prob_bimodal < 0.8,], aes(x,y, fill = orderedforesttype))+geom_raster()+facet_wrap(~PC1bins)+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+scale_fill_manual(values = c('#beaed4','#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+theme_bw()+coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.background = element_rect(fill=alpha('transparent', 0)),
                                                                                                                                                                                                                              panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")
dev.off()



png(height = 10, width = 10, units = "in", res = 300,"outputs/density_facet_by_PC1bins.png")
ggplot(pls.b15, aes(PLSdensity, fill = orderedforesttype))+geom_histogram()+facet_wrap(~PC1bins)+xlim(0,650)
dev.off()

# <<<<<<<<<<<<<<< map bimodality evaluated on kernal density esimates: >>>>>>>>>>>>>>>>>>>>>>>>>>>>


# map bimodal based on PC1:

bimod.pc.pls <- read.csv("outputs/new_bim_surface_PC1_pls_0.1_mode_crit_1000.csv")
bimod.pc.pls$eco <- ifelse(bimod.pc.pls$PLSdensity <= 0.5, "Prairie", 
                           ifelse(bimod.pc.pls$PLSdensity <= 47, "Savanna", "Forest"))
bimod.pc.pls$bimclass_eco <- ifelse(is.na(bimod.pc.pls$bimclass),NA ,paste(bimod.pc.pls$bimclass, bimod.pc.pls$eco))
bimod.pc.pls$bimclass <- ifelse(is.na(bimod.pc.pls$bimclass), "low sample", as.character(bimod.pc.pls$bimclass)) # make NA low samples

bimod.pc.pls.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc.pls, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")

pc1.bimpct <- round(length(bimod.pc.pls[bimod.pc.pls$bimclass %in% "bimodal",]$bimclass)/length(bimod.pc.pls$bimclass)*100, digits = 2)


bimod.pc.pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc.pls, aes(x=x, y=y, fill = bimclass))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal"='#d73027',"unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", pc1.bimpct, "%"))


# pased on P-PET:

bimod.ppet.pls <- read.csv("outputs/new_bim_surface_PPET_pls_4_mode_crit_1000.csv")
bimod.ppet.pls$eco <- ifelse(bimod.ppet.pls$PLSdensity <= 0.5, "Prairie", 
                             ifelse(bimod.ppet.pls$PLSdensity <= 47, "Savanna", "Forest"))
bimod.ppet.pls$bimclass_eco <- ifelse(is.na(bimod.ppet.pls$bimclass_ppet),NA ,paste(bimod.ppet.pls$bimclass_ppet, bimod.ppet.pls$eco))

bimod.ppet.pls$bimclass_ppet <- ifelse(is.na(bimod.ppet.pls$bimclass_ppet), "low sample", as.character(bimod.ppet.pls$bimclass_ppet)) # make NA low samples


bimod.ppet.pls.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.pls, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")
ppet.bimpct <- round(length(bimod.ppet.pls[bimod.ppet.pls$bimclass_ppet %in% "bimodal",]$bimclass_ppet)/length(bimod.ppet.pls$bimclass_ppet)*100, digits = 2)


bimod.ppet.pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.pls, aes(x=x, y=y, fill = bimclass_ppet))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal" = '#d73027', "unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", ppet.bimpct, "%"))


ggplot(bimod.ppet.pls, aes(GS_ppet, PLSdensity, color = bimclass_ppet))+geom_point()

# based on soil moisture estimates:
bimod.sm.pls <- read.csv("outputs/new_bim_surface_soil_moist_pls_0.1_mode_crit_1000.csv")
bimod.sm.pls$eco <- ifelse(bimod.sm.pls$PLSdensity <= 0.5, "Prairie", 
                           ifelse(bimod.sm.pls$PLSdensity <= 47, "Savanna", "Forest"))
bimod.sm.pls$bimclass_eco <- ifelse(is.na(bimod.sm.pls$bimclass_soil),NA ,paste(bimod.sm.pls$bimclass_soil, bimod.sm.pls$eco))

bimod.sm.pls$bimclass_soil <- ifelse(is.na(bimod.sm.pls$bimclass_soil), "low sample", as.character(bimod.sm.pls$bimclass_soil)) # make NA low samples



bimod.sm.pls.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.sm.pls, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


sm.bimpct <- round(length(bimod.sm.pls[bimod.sm.pls$bimclass_soil %in% "bimodal",]$bimclass_soil)/length(bimod.sm.pls$bimclass_soil)*100, digits = 2)


bimod.sm.pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.sm.pls, aes(x=x, y=y, fill = bimclass_soil))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", sm.bimpct, "%"))




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
                                                                                                               axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),legend.key = element_rect(color = "black", linetype = "solid"),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()

pls.clust
# merge clust_plot6 and dens.pr

dens.clust <- merge(dens.pr, clust_7[,c("x" ,"y", "cell", "speciescluster", "foresttype")], by = c("x", "y", "cell"), all.x = TRUE)
dens.clust <- merge(dens.clust, bimod.pc.pls[,c("x","y", "bimclass")], by = c("x", "y"))

dens.clust <- dens.clust[!duplicated(dens.clust),] # remove any duplicates created in merge

png("outputs/cluster/density_vs_envt_pc1_by_species_cluster.png")
ggplot(dens.clust, aes(PC1, PLSdensity, color = speciescluster))+geom_point()
dev.off()

png("outputs/cluster/density_vs_envt_JJA_soil_by_species_cluster.png")
ggplot(dens.clust, aes(mean_GS_soil, PLSdensity, color = speciescluster))+geom_point(size = 0.5)+ylim(0, 650)
dev.off()

ggplot(dens.clust, aes(GS_ppet, PLSdensity, color = speciescluster))+geom_point(size = 0.5)+ylim(0, 650)

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
# get the range of PC1 for most bimodal cells:
hist(pls.b15[pls.b15$prob_bimodal >= 0.6,]$PC1, breaks = 50)
hist(pls.b15[pls.b15$prob_bimodal >= 0.6 & pls.b15$PC1 <1.8 & pls.b15$PC1 > -2,]$PC1, breaks = 50)

# low limit of bimodality:
low.pc1.bim <- min(pls.b15[pls.b15$prob_bimodal >= 0.6 & pls.b15$PC1 <1.8 & pls.b15$PC1 > -2,]$PC1)
low.pc1.bim.1 <- min(bimod.pc.pls[bimod.pc.pls$PC1 < 2 & bimod.pc.pls$bimclass %in% "bimodal",]$PC1)
low.pc1.bim.2 <- min(bimod.pc.pls[bimod.pc.pls$PC1 > 2 & bimod.pc.pls$bimclass %in% "bimodal",]$PC1)
# high estimate high limit of bimodality:
high.pc1.bim <- max(pls.b15[pls.b15$prob_bimodal >= 0.6 & pls.b15$PC1 <1.8 & pls.b15$PC1 > -2,]$PC1)
high.pc1.bim.1 <- max(bimod.pc.pls[bimod.pc.pls$PC1 < 2 & bimod.pc.pls$bimclass %in% "bimodal",]$PC1)
high.pc1.bim.2 <- max(bimod.pc.pls[bimod.pc.pls$PC1 > 2 & bimod.pc.pls$bimclass %in% "bimodal",]$PC1)

pls.PC1bimcell.1 <- dens.clust[dens.clust$bimclass %in% "bimodal" & dens.clust$PC1 > low.pc1.bim.1 & dens.clust$PC1 < high.pc1.bim.1,]$cell
pls.PC1bimcell.2 <- dens.clust[dens.clust$bimclass %in% "bimodal" & dens.clust$PC1 > low.pc1.bim.2 & dens.clust$PC1 < high.pc1.bim.2,]$cell



pls.dens.pc1.hex <- ggplot(data = dens.clust, aes(PC1, PLSdensity)) +geom_hex() + 
  theme_bw(base_size = 8)+scale_fill_distiller(palette = "Spectral", limits = c(1,140))+
  xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+geom_vline(xintercept = low.pc1.bim.1, color = "red")+geom_vline(xintercept = high.pc1.bim.1, color = "red")+geom_vline(xintercept = low.pc1.bim.2, color = "black")+geom_vline(xintercept = high.pc1.bim.2, color = "black")+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
                                                                                                                                                                               legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                                                                                               legend.key.size = unit(0.35, "line"),legend.title = element_text(size = 8), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
pls.dens.pc1.hex


pls.dens.ppet.hex <- ggplot(data = dens.clust, aes(GS_ppet, PLSdensity)) +geom_hex() + 
  theme_bw(base_size = 8)+#scale_fill_distiller(palette = "Spectral", limits = c(1,140))+
  xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+ylim(0,650)#+geom_vline(xintercept = -2.5)+geom_vline(xintercept = 1)+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
                                                                                                                                                                              # legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                                                                                               #legend.key.size = unit(0.35, "line"),legend.title = element_text(size = 8), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
pls.dens.ppet.hex

pls.dens.moist.hex <- ggplot(data = dens.clust, aes(mean_GS_soil, PLSdensity)) +geom_hex() + 
  theme_bw(base_size = 8)+scale_fill_distiller(palette = "Spectral", limits = c(1,140))+
  xlab('mean JJA soil moisture') + ylab("Tree Density (stems/ha)")+ylim(0,650)#+geom_vline(xintercept = -2.5)+geom_vline(xintercept = 1)+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
# legend.background = element_rect(fill=alpha('transparent', 0)), 
#legend.key.size = unit(0.35, "line"),legend.title = element_text(size = 8), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
pls.dens.moist.hex

smoothScatter(x =dens.clust$PC1, y = dens.clust$PLSdensity, pch = NA, nrpoints = 1000,  nbin = c(1000, 1000) )

p4 <- ggplot(dens.clust, aes(PC1, PLSdensity)) +
  stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE)+scale_fill_continuous(low = 'white', high = 'red')   

png("outputs/alt_hexbin_raster_pls.png")
p4
dev.off()

png("outputs/alt_hexbin_scale_count_pls.png")
ggplot(dens.clust, aes(x=PC1, y=PLSdensity))+ geom_count(col="tomato3", show.legend=F, alpha = 0.05) +scale_size_area()+theme_bw()
dev.off()
               



# plot scaling overlapping histograms with density plot overlaid:

png("outputs/alt_hexbin_smooth_scatter_with_density2doverlay.png")
p1 = ggplot(dens.clust,aes(x=PC1, y=PLSdensity)) +
  geom_point(alpha = 0.1, colour="orange")+ #+ geom_count(alpha = 0.1, colour="orange")+
  geom_density2d() + 
  theme_bw()

scatter_dens_2dpls<- p1+xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+geom_vline(xintercept = low.pc1.bim.1, color = "red")+geom_vline(xintercept = high.pc1.bim.1, color = "red")+geom_vline(xintercept = low.pc1.bim.2, color = "black")+geom_vline(xintercept = high.pc1.bim.2, color = "black")+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
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


p1 = ggplot(dens.clust,aes(x=mean_GS_soil, y=PLSdensity)) +
  geom_point(alpha = 0.1, colour="orange")+ #+ geom_count(alpha = 0.1, colour="orange")+
  geom_density2d(bins = 15) + 
  theme_bw()

scatter_dens_2dpls_mjjassoil <- p1+xlab('Mean JJA soil moisture') + ylab("Tree Density (stems/ha)")+ylim(0,650)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
                                                                                                            legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                            legend.key.size = unit(0.35, "line"),legend.title = element_text(size = 8), panel.grid.major = element_blank(), panel.grid.minor = element_blank())


png("outputs/alt_hexbin_smooth_scatter_with_density2doverlay_jja_soils.png")
scatter_dens_2dpls_mjjassoil
dev.off()





p4 <- ggplot(dens.clust, aes(mean_GS_soil, PLSdensity)) +
  stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE)+scale_fill_continuous(low = 'white', high = 'red')   

p4
# >>>>>>>>>>>>>>>>>>>>>>>.new figures with 2d density plots: <<<<<<<<<<<<<<<<<<<<<<<
pls.df <- dens.clust
library(ks)
library(ggplotify)
# for PC1:
H <- Hpi.diag(x=na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550))
#points(na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)


interp.densp <- function(pc1val){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - pc1val)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  points <- data.frame(x=rep(pc1val, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(pc1val, 601 ), y=0:600) # points all at the pc1 value, and along a grid of density
  
  if(max(kde(x=na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    dipP <- diptest::dip.test(sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE))$p.value
    dipP <- diptest::dip.test( df$freq)$p.value
    
  }
  dipP
}

interp.densp.new <- function(pc1val){
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - pc1val)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  points <- data.frame(x=rep(pc1val, maxy+1 ), y=0:maxy)
  
  if(max(kde(x=na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    # df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df[pls.df$PC1fia <= pc1val + 0.05 & pls.df$PC1fia >= pc1val - 0.05, ]$PC1fia, pls.df[pls.df$PC1fia <= pc1val + 0.05 & pls.df$PC1fia >= pc1val - 0.05, ]$FIAdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    df <- data.frame(na.omit(pls.df[pls.df$PC1 <= pc1val + 0.04 & pls.df$PC1 >= pc1val - 0.04, c("PC1", "PLSdensity")]),
                     freq = predict(fhat, x = na.omit(pls.df[pls.df$PC1 <= pc1val + 0.04 & pls.df$PC1 >= pc1val - 0.04, c("PC1", "PLSdensity")])))
    dip <- rep(NA, 100)
    rand.sample <- rep(NA, 100, 100)
    for(i in 1:1000){
      rand.sample <- sample(1:length(df$freq), 1000, replace = TRUE)
      dip[i] <- diptest::dip.test(df[rand.sample,]$freq)$p.value
    }
    dipP <- mean(dip)
    
    #dipP <- diptest::dip.test(predict(fhat, x = na.omit(pls.df[pls.df$PC1fia <= pc1val + 0.04 & pls.df$PC1fia >= pc1val - 0.04, c("PC1fia", "FIAdensity")])))$p.value
    #dipP <- diptest::dip.test(predict(fhat, x = pls.df[rand, c("PC1fia", "FIAdensity")]))$p.value
    #dipP <- diptest::dip.test(sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE))$p.value
  }
  
  dipP
  
}

interp.densp <- function(pc1val){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - pc1val)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(pc1val, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(pc1val, 401 ), y=0:400)
  points <- expand.grid(seq(pc1val - 0.05, pc1val + 0.05, by = 0.001), y=0:400)
  colnames(points) <- c("x", "y")
  if(max(kde(x=na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    dipP <- diptest::dip.test(sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE))$p.value
    #dipP <- diptest::dip.test( df$freq)$p.value
    
  }
  dipP
}


#pc1.bim.line <- data.frame(PC1 = seq(from = -5, to = 4, by = 0.05), pval = NA, bimodal = NA)
#pc1.bim.line$pval <- apply(data.frame(pc1.bim.line$PC1), MARGIN= 1, FUN=interp.densp)
#pc1.bim.line$bimodal <- ifelse(pc1.bim.line$pval <= 0.05, "bimodal", "unimodal")
#pc1.bim.line$y <- -37

#bimodal.pls.pc1 <- read.csv("outputs/new_bim_surface_PC1_pls_0.1_mode_crit_1000.csv")
bimodal.pls.pc1 <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_PC1_kde.rds")
#bimodal.pls.pc1$dip <- ifelse(bimodal.pls.pc1$pvalue == 1, NA, bimodal.pls.pc1$dip)
#bimodal.pls.pc1$pvalue <- ifelse(bimodal.pls.pc1$pvalue == 1, NA, bimodal.pls.pc1$pvalue)


pvalues <- bimodal.pls.pc1 %>% group_by(pc1_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                              median.p = median(pvalue, na.rm = TRUE),
                                                              ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                              ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                              mean.d = mean(dip, na.rm = TRUE),
                                                              median.d = median(dip, na.rm = TRUE),
                                                              ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                              ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pvalues$pc1_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(pc1_bins = unique(unique(pvalues$pc1_bins)),
                           pc1_mids = as.numeric(mids)+0.125)





bimodal.pls.pc1 <- merge(pvalues, ordered.cuts, by = "pc1_bins")
bimodal.pls.pc1$bimclass <- ifelse(bimodal.pls.pc1$median.p <= 0.05, "bimodal", "unimodal")
pc1.bim.line <- data.frame(PC1 = unique(bimodal.pls.pc1[bimodal.pls.pc1$bimclass %in% "bimodal",]$pc1_mids), y = -37, bimodal = "bimodal")
pls.kde.plot.pc1 <- recordPlot()
ggplot(bimodal.pls.pc1, aes(PC1, fill = bimclass))+geom_histogram()

library(base2grob)
pls.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550)))


# make the plot with GGPLOT:
pls.kde.plot.pc1.gg <- ggplot(pls.df, aes(x=PC1, y=PLSdensity) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_distiller(palette= c("YlOrRd"), direction=1 )+ylab("Tree Density")+theme(legend.position = "none")
  
  
pls.kde.plot.pc1.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7 ) + points(pc1.bim.line[pc1.bim.line$bimodal %in% "bimodal",]$PC1 , pc1.bim.line[pc1.bim.line$bimodal %in% "bimodal",]$y, col = "darkblue", pch = 15, cex = 1)+ text(-5.5, 500, "A"))
pls.kde.plot.pc1.gg

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7 ) + points(data = pc1.bim.line[pc1.bim.line$bimodal %in% "bimodal",], y~PC1, cex = 0.9,  pch = 15,col = "darkblue") + text(-6,500, "A")

ylow2redscale <- c('#ffffcc',
    '#ffeda0',
    '#fed976',
    '#feb24c',
    '#fd8d3c',
    '#fc4e2a',
    '#e31a1c',
    '#bd0026',
    '#800026')

#ggplot(pls.df, aes(x=PC1, y=PLSdensity) ) +
 # stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_manual(palette= ylow2redscale)


# for P-PET:
H <- Hpi.diag(x=na.omit(cbind(pls.df$GS_ppet, pls.df$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(pls.df$GS_ppet, pls.df$PLSdensity)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,75,80,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(0,550))
#points(na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
pls.kde.plot.ppet <- recordPlot()
pls.kde.plot.ppet.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(0,550), xlim = c(-200, 300)))


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)


interp.densp <- function(pc1val){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - pc1val)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(pc1val, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  points <- data.frame(x=rep(pc1val, 551 ), y=0:550)
  
  if(max(kde(x=na.omit(cbind(pls.df[pls.df$GS_ppet <= pls.df$GS_ppet + 4 & pls.df$GS_ppet >= pls.df$GS_ppet - 4, ]$GS_ppet, pls.df[pls.df$GS_ppet <= pls.df$GS_ppet + 4 & pls.df$GS_ppet >= pls.df$GS_ppet - 4, ]$PLSdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df[pls.df$GS_ppet <= pls.df$GS_ppet + 4 & pls.df$GS_ppet >= pls.df$GS_ppet - 4, ]$GS_ppet, pls.df[pls.df$GS_ppet <= pls.df$GS_ppet + 4 & pls.df$GS_ppet >= pls.df$GS_ppet - 4,]$PLSdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    #dipP <- diptest::dip.test(sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE))$p.value
    dipP <- diptest::dip.test( df$freq)$p.value
    
  }
  dipP
}


interp.densp(pc1val = 200)
#ppet.bim.line <- data.frame(PPET = seq(from = -200, to = 200, by = 1), pval = NA, bimodal = NA)
#ppet.bim.line$pval <- apply(data.frame(ppet.bim.line$PPET), MARGIN= 1, FUN=interp.densp)
#ppet.bim.line$bimodal <- ifelse(ppet.bim.line$pval <= 0.05, "bimodal", "unimodal")
#ppet.bim.line$y <- -37

#bimodal.pls.ppet <- read.csv("outputs/new_bim_surface_PPET_pls_4_mode_crit_1000.csv")
bimodal.pls.ppet <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_PPET.rds")
#bimodal.pls.ppet$dip <- ifelse(bimodal.pls.ppet$pvalue == 1, NA, bimodal.pls.ppet$dip)
#bimodal.pls.ppet$pvalue <- ifelse(bimodal.pls.ppet$pvalue == 1, NA, bimodal.pls.ppet$pvalue)

pvalues <- bimodal.pls.ppet %>% group_by(ppet_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                               median.p = median(pvalue, na.rm = TRUE),
                                                               ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                               ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                               mean.d = mean(dip, na.rm = TRUE),
                                                               median.d = median(dip, na.rm = TRUE),
                                                               ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                               ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

first <- strsplit(as.character(unique(pvalues$ppet_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(ppet_bins = pvalues$ppet_bins,
                           ppet_mids = as.numeric(mids)+7.5)


bimodal.pls.ppet <- merge(pvalues, ordered.cuts, by = "ppet_bins")
bimodal.pls.ppet$bimclass_ppet <- ifelse(bimodal.pls.ppet$median.p <= 0.05, "bimodal", "unimodal")


ppet.bim.line <- data.frame(PPET = unique(bimodal.pls.ppet[bimodal.pls.ppet$bimclass_ppet %in% "bimodal",]$ppet_mids), y = -37, bimodal = "bimodal")


# ggplotify the kde plots here:
pls.kde.plot.ppet.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim = c(-200, 300), cex.axis = 0.7) + points(data = ppet.bim.line[ppet.bim.line$bimodal %in% "bimodal",], y~PPET, cex = 1,  pch = 15,col = "darkblue") + text(-170,500, "B"))
pls.kde.plot.ppet.gg

#pls.kde.plot.ppet.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim = c(-200, 300), cex.axis = 0.7) + rug(ppet.bim.line[ppet.bim.line$bimodal %in% "bimodal",]$PPET, ticksize = 0.01, lwd = 0.01, col = "darkblue") + text(-170,500, "B"))
#pls.kde.plot.ppet.gg

#ggplot(bimodal.pls.ppet, aes(GS_ppet, fill = bimclass_ppet))+geom_histogram()
#pls.kde.plot.ppet.gg <- ggplot(pls.df, aes(x=PC1, y=PLSdensity) ) +
 # stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_distiller(palette= c("Spectral"), direction=2 )+ylab("Tree Density")+theme(legend.position = "none")

#pls.kde.plot.ppet.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim = c(-200, 250)))

#pls.kde.plot.ppet.gg <-
 # ggplot(na.omit(pls.df[, c("GS_ppet", "PLSdensity")]), aes(x=GS_ppet, y=PLSdensity) ) +
  #stat_density_2d(aes(fill = ..level..), geom = "polygon", n = 200, h = c( 24.63, 73.9, 24.63, 73.9), contour = TRUE)+  scale_fill_gradientn(colours=rev(heat.colors(5)))+ylab("Tree Density")+theme(legend.position = "none")


#pls.kde.plot.sm.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim=c(0, 1.75)))
  
  
  # for soil moisture/bucket model

H <- Hpi.diag(x=na.omit(cbind(pls.df$mean_GS_soil, pls.df$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(pls.df$mean_GS_soil, pls.df$PLSdensity)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,65,75,80,85,95), xlab = "Growing season soil", ylab = "Tree density", ylim = c(-40,550))
#points(na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), cex=0.3, pch=16)
abline(h = -40, col = "purple", lwd = 10)
plot(fhat, display="slice", cont=c(85), add = TRUE)
pls.kde.plot.sm <- recordPlot()
pls.kde.plot.sm.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "soil moisture", ylab = "Tree density", ylim = c(0,550), xlim=c(0, 1.5)))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)


interp.densp <- function(pc1val){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - pc1val)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(pc1val, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  points <- data.frame(x=rep(pc1val, 551 ), y=0:550)
  if(max(kde(x=na.omit(cbind(pls.df$mean_GS_soil, pls.df$PLSdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df[pls.df$mean_GS_soil <= pls.df$mean_GS_soil + 0.015 & pls.df$mean_GS_soil >= pls.df$mean_GS_soil - 0.015,]$mean_GS_soil, pls.df[pls.df$mean_GS_soil <= pls.df$mean_GS_soil + 0.05 & pls.df$mean_GS_soil >= pls.df$mean_GS_soil - 0.05,]$PLSdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    dipP <- diptest::dip.test(sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE))$p.value
    #dipP <- diptest::dip.test( df$freq)$p.value
    
  }
  dipP
}

# predict bimodality at evenly spaced envt values
#sm.bim.line <- data.frame(SM = seq(from = 0, to = 2, by = 0.001), pval = NA, bimodal = NA)
#sm.bim.line$pval <- apply(data.frame(sm.bim.line$SM), MARGIN= 1, FUN=interp.densp)
#sm.bim.line$bimodal <- ifelse(sm.bim.line$pval <= 0.05, "bimodal", "unimodal")
#sm.bim.line$y <- -37

#bimodal.pls.soil <- read.csv("outputs/new_bim_surface_soil_moist_pls_0.1_mode_crit_1000.csv")
bimodal.pls.soil<- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_soil_15bins_kde_sample.rds")
# if pvalues == 1, then we dont evaluate over it

pvalues <- bimodal.pls.soil %>% group_by(soil_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                               median.p = median(pvalue, na.rm = TRUE),
                                                               ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                               ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                               mean.d = mean(dip, na.rm = TRUE),
                                                               median.d = median(dip, na.rm = TRUE),
                                                               ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                               ci.high.d = quantile(dip, 0.975, na.rm = TRUE))


# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

first <- strsplit(as.character(unique(pvalues$soil_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(soil_bins = unique(pvalues$soil_bins),
                           soil_mids = as.numeric(mids)+0.025)

bimodal.pls.soil <- merge(pvalues, ordered.cuts, by.x = "soil_bins")
bimodal.pls.soil$bimclass_soil <- ifelse(bimodal.pls.soil$median.p <= 0.05, "bimodal", "unimodal")

sm.bim.line <- data.frame(SM = unique(bimodal.pls.soil[bimodal.pls.soil$bimclass_soil %in% "bimodal",]$soil_mids), y = -37, bimodal = "bimodal")

# ggplotify the kde plots here:
pls.kde.plot.sm.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7) + points(data = sm.bim.line[sm.bim.line$bimodal %in% "bimodal",], y~SM, cex = 1,  pch = 15,col = "darkblue") + text(-0.05,500, "C"))
pls.kde.plot.sm.gg

#pls.kde.plot.sm.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7) + rug(sm.bim.line[sm.bim.line$bimodal %in% "bimodal",]$SM, ticksize = 0.01, lwd = 0.01,col = "darkblue") + text(-0.05,500, "C"))
#pls.kde.plot.sm.gg
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>D: Histogram colored by species cluster:

# make a histogram of denisty betwen -2.5 and 1 colored by species cluster:

# need to reorder the factors:
dens.clust$foresttype_ordered <- factor(dens.clust$foresttype, levels = rev(c("Boreal/Sub-boreal", "Pine", "Aspen", "Beech-Maple", "N. Mixed Forest", "Oak", "Oak-Hickory")))
dens.clust.omit <- dens.clust[ !is.na(dens.clust$foresttype_ordered),]
dens.clust <- dens.clust[!duplicated(dens.clust),]


# clust.hist full
# remove dens.clust with no forest type assigned:

clust.hist.full <- ggplot()+ geom_density(data = dens.clust[dens.clust$PLSdensity >= 0.5 & !is.na(dens.clust$foresttype_ordered),], aes(PLSdensity, 22 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_histogram(data = dens.clust[dens.clust$PLSdensity >= 0.5 & !is.na(dens.clust$foresttype_ordered),], aes(PLSdensity, fill = foresttype_ordered), binwidth =  20)+xlim(0,600)+
  scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ", drop = TRUE)+coord_flip()+ylim(0,1050)+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.44, 0.85),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.4, "line"),legend.key = element_rect(color = "black", linetype ="solid"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
clust.hist.full




clust.hist <- ggplot()+ geom_density(data = dens.clust[dens.clust$cell %in% pls.PC1bimcell.1 | dens.clust$cell %in% pls.PC1bimcell.2 , ], aes(PLSdensity, 23 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_histogram(data = dens.clust[dens.clust$cell %in% pls.PC1bimcell.1 | dens.clust$cell %in% pls.PC1bimcell.2, ], aes(PLSdensity, fill = foresttype_ordered), binwidth =  20)+xlim(0,600)+
  scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+coord_flip()+ylim(0,950)+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.65, 0.75),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.35, "line"),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
clust.hist


clust.hist.jja_soil <- ggplot(data = dens.clust[dens.clust$mean_GS_soil > 0.8 & dens.clust$mean_GS_soil < 1.2  & dens.clust$PLSdensity > 0.5, ], aes(PLSdensity, fill = foresttype_ordered))+geom_histogram()+xlim(0,650)+scale_fill_manual(values = c( '#beaed4', '#386cb0','#ffff99','#bf5b17','#f0027f','#fdc086', '#7fc97f'), name = " ")+coord_flip()+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.65, 0.75),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.35, "line"),panel.grid.major = element_blank(), panel.grid.minor = element_blank())

png(height = 5, width = 5, units = "in", res = 300, "outputs/paper_figs/hist_by_community_JJA_soil.png")
clust.hist.jja_soil
dev.off()




# B: Map of FIA species clusters
dens.pr$density_discretef <- ifelse(dens.pr$FIAdensity <= 0.5, "Prairie", 
                                   ifelse(dens.pr$FIAdensity <= 47, "Savanna",
                                          ifelse(dens.pr$FIAdensity > 47 & dens.pr$FIAdensity <= 100, "47-100",
                                                 ifelse(dens.pr$FIAdensity > 100 & dens.pr$FIAdensity <= 200, "100-200", 
                                                        ifelse(dens.pr$FIAdensity > 200 & dens.pr$FIAdensity <= 300, "200-300", 
                                                               ifelse(dens.pr$FIAdensity > 300 & dens.pr$FIAdensity <= 400, "300-400",
                                                                      ifelse(dens.pr$FIAdensity > 400 & dens.pr$FIAdensity <= 500, "400-500",
                                                                             ifelse(dens.pr$FIAdensity > 500 & dens.pr$FIAdensity <= 600, "500 +", "No data"))))))))

dens.pr$density_discretef <- factor(dens.pr$density_discretef, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500", "500+", "No data"))


fia.map.alt.color <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.pr[!is.na(dens.pr$density_discretef),], aes(x=x, y=y, fill = density_discretef))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_manual(values = c('#dfc27d',
                               '#8c510a',
                               '#d9f0a3',
                               '#addd8e',
                               '#78c679',
                               '#41ab5d',
                               '#238443',
                               '#005a32',"darkgrey" ), name ="Tree Density", na.value = 'darkgrey', drop = F) +
  
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.3, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()




# from species_clustering.R:
species.clust <- read.csv("outputs/cluster/density_fia_with_clusters.csv")

species.clust$foresttype <- plyr::revalue(species.clust$speciescluster,c("Oak/Maple" = "Oak/Maple", 
                                                                         "Maple/Ash/Birch/Aspen" = "Mixed Hardwoods",
                                                                         
                                                                         "Pine/Poplar"="Pine", "Aspen" = "Poplar"))



fia.clust <- ggplot(species.clust, aes(x = x, y=y, fill=foresttype))+geom_raster()+
  scale_fill_manual(values = c('#fdc086', '#003c30','#a6cee3','#f0027f'), name = " ", c("Poplar", "Mixed Hardwoods", "Oak/Maple", "Pine"))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+
  theme(legend.position=c(0.20, 0.18),legend.background = element_rect(fill=alpha('transparent', 0)) , legend.key = element_rect(color = "black", linetype = "solid"),
  axis.line=element_blank(),legend.key.size = unit(0.3,'lines'),legend.text=element_text(size=5),axis.text.x=element_blank(),
  axis.text.y=element_blank(),axis.ticks=element_blank(),
  axis.title.x=element_blank(),
  axis.title.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()

fia.clust

# merge clust_plot6 and dens.pr
fia.bim <- read.csv("data/PLS_FIA_density_climate_full.csv")
fia.dens.clust <- merge(fia.bim, species.clust[,c("x","y", "cell", "speciescluster", "foresttype")], by = c("x", "y", "cell"))

png("outputs/cluster/density_vs_envt_pc1_by_species_cluster_fia.png")
ggplot(fia.dens.clust, aes(PC1fia, FIAdensity, color = speciescluster))+geom_point()
dev.off()

# C: Density vs PC1 as a hexbin plot
fia.dens.pc1.hex <- ggplot(data = fia.dens.clust, aes(PC1fia, FIAdensity)) +geom_hex() + 
  theme_bw(base_size = 8)+scale_fill_distiller(palette = "Spectral", limits = c(1,135))+
  xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+geom_vline(xintercept = low.pc1.bim.1, color = "red")+geom_vline(xintercept = high.pc1.bim.1, color = "red")+geom_vline(xintercept = low.pc1.bim.2, color = "black")+geom_vline(xintercept = high.pc1.bim.2, color = "black")+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
                                                                                                                                                                               legend.background = element_rect(fill=alpha('transparent', 0)), 
                                                                                                                                                                               legend.key.size = unit(0.35, "line"),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
fia.dens.pc1.hex

# alternate to geom_hex: geom_point with overlaid density contours:
p1fia <- ggplot(fia.dens.clust,aes(x=PC1fia, y=FIAdensity, color = foresttype)) +
  geom_point(alpha = 0.1, colour="orange")+ #+ geom_count(alpha = 0.1, colour="orange")+
  geom_density2d(data = fia.dens.clust,aes(x=PC1fia, y=FIAdensity), color = "blue") + 
  theme_bw()

scatter_dens_2dfia <- p1fia+xlab('Environmental PC1') + ylab("Tree Density (stems/ha)")+geom_vline(xintercept = low.pc1.bim.1, color = "red")+geom_vline(xintercept = high.pc1.bim.1, color = "red")+geom_vline(xintercept = low.pc1.bim.2, color = "black")+geom_vline(xintercept = high.pc1.bim.2, color = "black")+xlim(4, -5)+ylim(0,650)+coord_fixed(ratio = 1/60)+theme(legend.position = c(0.85, 0.85),legend.direction = "vertical", 
                                                                                                                                                                                                     legend.background = element_rect(fill=alpha('transparent', 0)))
                                                                                                                                                                                                     
# read in the past survey data and past logged data:

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
f.clust.hist <- ggplot()+ geom_density(data = dens.clust[dens.clust$cell %in% pls.PC1bimcell.1 | dens.clust$cell %in% pls.PC1bimcell.2 , ], aes(PLSdensity, 23 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+
  geom_density(data = fia.dens.clust[fia.dens.clust$PC1fia >= low.pc1.bim.1 & fia.dens.clust$PC1fia <= high.pc1.bim.1,] , aes(FIAdensity, 23 *..count..),trim = TRUE , color = "black", size = 1.5)+
  geom_histogram(data = fia.dens.clust[fia.dens.clust$PC1fia >= low.pc1.bim.1 & fia.dens.clust$PC1fia <= high.pc1.bim.1 , ], aes(FIAdensity, fill = foresttype), binwidth = 20)+ scale_fill_manual(values = c('#fdc086', '#003c30','#a6cee3','#f0027f'), name = " ", c("Poplar", "Mixed Hardwoods", "Oak/Maple", "Pine"))+coord_flip()+xlim(0,600)+ylim(0,950)+xlab("FIA tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.35, 0.85),legend.background = element_blank(), legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),
                                                                                                                                                                                                                                                                                                                                                                   panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank())
f.clust.hist 


f.clust.hist.full <- ggplot()+ geom_density(data = dens.clust[dens.clust$PLSdensity >= 0.5,], aes(PLSdensity, 22 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+
  geom_density(data = fia.dens.clust , aes(FIAdensity, 23 *..count..),trim = TRUE , color = "black", size = 1.5)+
  geom_histogram(data = fia.dens.clust, aes(FIAdensity, fill = foresttype), binwidth = 18)+ scale_fill_manual(values = c('#fdc086', '#003c30','#a6cee3','#f0027f'), name = " ", c("Poplar", "Mixed Hardwoods", "Oak/Maple", "Pine"))+
  coord_flip()+xlim(0,600)+ylim(0,1050)+xlab("FIA tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.36, 0.87),legend.background = element_blank(), legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=6), legend.key = element_rect(color= "black", linetype = "solid"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank())
f.clust.hist.full 

library(ggpubr)
inset <- ggboxplot( data= full.fia.surveys, x = 'INVYRcd',y = 'FIAdensity', merge=TRUE,width = 0.5, fill = "INVYRcd",  palette =c("grey28", "grey40", "grey60"), outlier.size = 0.0005)+ylim(0,600) +theme_bw()+theme(axis.title = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(), legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1))

# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(inset)

# Place box plots inside the histogram plot
full.fia.surveys$INV_place <- ifelse(full.fia.surveys$INVYRcd %in% "2000s", 1000, 
       ifelse(full.fia.surveys$INVYRcd %in% "1990s", 900,
              ifelse(full.fia.surveys$INVYRcd %in% "1980s", 800, NA )))


hist.1 <- ggplot()+ geom_density(data = dens.clust[dens.clust$PLSdensity >= 0.5,], aes(PLSdensity, 22 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+
  geom_density(data = fia.dens.clust , aes(FIAdensity, 23 *..count..),trim = TRUE , color = "black", size = 1.5)+
  geom_histogram(data = fia.dens.clust, aes(FIAdensity, fill = foresttype), binwidth = 18)+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33'), name = " ")+
  scale_x_discrete(breaks=seq(0, 750, 250))
  bx.plt <- ggplot()+geom_boxplot(data = full.fia.surveys, aes(y = FIAdensity, x = INV_place, group = INV_place))+coord_flip()



f.clust.hist.full + geom_boxplot(data = full.fia.surveys, aes(y = INV_place, x = FIAdensity, group = INVYRcd))
f.clust.hist+scale_x_discrete(breaks=seq(0, 750, 250))+geom_boxplot(data = full.fia.surveys, aes(x = INV_place, y = FIAdensity, group = INVYRcd))+coord_flip()

f.clust.hist.inset <- f.clust.hist+scale_x_discrete(breaks=seq(0, 750, 250)) + annotation_custom(grob = xbp_grob, ymin = 375, ymax = 760, xmin = 0.005, xmax = 655)
f.clust.hist.inset.full <- f.clust.hist.full+ scale_y_continuous(breaks=c(0,250,500,750)) + annotation_custom(grob = xbp_grob, ymin = 750, ymax = 1100, xmin = 0.005, xmax = 655)

#Other way of overlaying the plots:
box.inset <- ggplot(data = full.fia.surveys, aes(x = INVYRcd, y = FIAdensity, fill= INVYRcd, size = 0.25))+geom_boxplot(size = 0.25)+ylim(0,600)#+theme_transparent()#+theme(axis.title.y = element_blank(), axis.line = element_blank(), axis.ticks = element_blank(), axis.text.y = element_blank(), panel.grid.major = element_blank(), panel.background  = element_blank(), legend.position = "none")

hist.inset <- ggdraw() +
  draw_plot(f.clust.hist + theme(legend.justification = "bottom"), 0, 0, 1, 1) +
  draw_plot(box.inset, x = 0.6, y = -0.045,width= 0.5, height=1, scale = 1.05)
hist.inset
# +
  #draw_plot_label(c("A", "B"), c(0, 0.5), c(1, 0.92), size = 15)


vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.3, height = 1, x = 0.8, y = 0.51)  # the inset in upper right
print(f.clust.hist, vp = vpb_)
print(box.inset, vp = vpa_)


test.plot <-print(f.clust.hist, vp = vpb_); print(box.inset, vp = vpa_)


#f.clust.hist.full <- ggplot()+ geom_density(data = dens.clust[dens.clust$PLSdensity >= 0.5,], aes(PLSdensity, 22 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+
 # geom_density(data = fia.dens.clust , aes(FIAdensity, 23 *..count..),trim = TRUE , color = "black", size = 1.5)+
  #geom_histogram(data = fia.dens.clust, aes(FIAdensity, fill = foresttype), binwidth = 18)+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33'), name = " ")+coord_flip()+xlim(0,600)+ylim(0,1050)+xlab("FIA tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.35, 0.85),legend.background = element_blank(), legend.key.size = unit(0.3,'lines'),legend.text=element_text(size=6))
                                                                                                                                                                                                                                                                                                                       
density.pls <- ggdensity(dens.clust[dens.clust$PLSdensity >= 0.5,], x = "PLSdensity", y = "23*..count..", size = 3, alpha = 0.6)+coord_flip()
density.fia<- ggdensity(fia.dens.clust, x = "FIAdensity", y = "23*..count..", size = 3, alpha = 0.6)+coord_flip()


f.clust.hist.nona <- ggplot()+ geom_density(data = dens.clust.nona[dens.clust.nona$cell %in% pls.PC1bimcell, ], aes(PLSdensity,  23*..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+
  geom_density(data = fia.dens.clust[fia.dens.clust$PC1fia >= low.pc1.bim & fia.dens.clust$PC1fia <= high.pc1.bim, ], aes(FIAdensity, 23 *..count..),trim = TRUE , color = "black", size = 1.5)+
  geom_histogram(data = fia.dens.clust[fia.dens.clust$PC1fia >= low.pc1.bim & fia.dens.clust$PC1fia <= high.pc1.bim, ], aes(FIAdensity, fill = foresttype))+ scale_fill_manual(values = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33'), name = " ")+coord_flip()+xlim(0,600)+ylim(0,400)+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.70, 0.75),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.35, "line"), 
                                                                                                                                                                                                                                                                                                                                                                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())
f.clust.hist.nona





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


# <<<<<<<<<<<<<<< map bimodality evaluated on kernal density esimates: >>>>>>>>>>>>>>>>>>>>>>>>>>>>


# map bimodal based on PC1:

bimod.pc.fia <- read.csv("outputs/new_bim_surface_PC1_fia_0.1_mode_crit_1000.csv")
bimod.pc.fia$eco <- ifelse(bimod.pc.fia$FIAdensity <= 0.5, "Prairie", 
                           ifelse(bimod.pc.fia$FIAdensity <= 47, "Savanna", 
                                  ifelse(bimod.pc.fia$FIAdensity > 47,"Forest", NA)))
bimod.pc.fia$bimclass_eco <- ifelse(is.na(bimod.pc.fia$bimclass_f) | is.na(bimod.pc.fia$eco),NA , paste(bimod.pc.fia$bimclass_f, bimod.pc.fia$eco))

bimod.pc.fia$bimclass_f <- ifelse(is.na(bimod.pc.fia$bimclass_f), "low sample", as.character(bimod.pc.fia$bimclass_f))

bimod.pc.fia.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc.fia, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")

pc.bimpct.f <- round(length(bimod.pc.fia[bimod.ppet.fia$bimclass_f %in% "bimodal",]$bimclass_f)/length(bimod.pc.fia$bimclass_f)*100, digits = 2)


bimod.pc.fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc.fia, aes(x=x, y=y, fill = bimclass_f))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c('bimodal'='#d73027', 'unimodal' ='#4575b4', 'low sample'='tan')) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region = ", pc.bimpct.f, "%"))



# pased on P-PET:

bimod.ppet.fia <- read.csv("outputs/new_bim_surface_PPET_fia_4_mode_crit_1000.csv")
bimod.ppet.fia$eco <- ifelse(bimod.ppet.fia$FIAdensity <= 0.5, "Prairie", 
                             ifelse(bimod.ppet.fia$FIAdensity <= 47, "Savanna", "Forest"))
bimod.ppet.fia$bimclass_eco <- ifelse(is.na(bimod.ppet.fia$bimclass_ppet_f) | is.na(bimod.ppet.fia$eco),NA ,paste(bimod.ppet.fia$bimclass_ppet_f, bimod.ppet.fia$eco))

bimod.ppet.fia$bimclass_ppet_f <- ifelse(is.na(bimod.ppet.fia$bimclass_ppet_f), "low sample", as.character(bimod.ppet.fia$bimclass_ppet_f))


bimod.ppet.fia.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.fia, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")

ppet.bimpct.f <- round(length(bimod.ppet.fia[bimod.ppet.fia$bimclass_ppet_f %in% "bimodal",]$bimclass_ppet_f)/length(bimod.sm.fia$bimclass_ppet_f)*100, digits = 2)

bimod.ppet.fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.fia, aes(x=x, y=y, fill = bimclass_ppet_f))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c( 'bimodal' = '#d73027', "unimodal"='#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("bimodal region = 0%")



# based on soil moisture estimates:
bimod.sm.fia <- read.csv("outputs/new_bim_surface_soil_moist_fia_0.01_mode_crit_1000.csv")
bimod.sm.fia$eco <- ifelse(bimod.sm.fia$FIAdensity <= 0.5, "Prairie", 
                           ifelse(bimod.sm.fia$FIAdensity <= 47, "Savanna", "Forest"))
bimod.sm.fia$bimclass_eco <- ifelse(is.na(bimod.sm.fia$bimclass_soil_f) | is.na(bimod.sm.fia$eco), NA ,paste(bimod.sm.fia$bimclass_soil_f, bimod.sm.fia$eco))
bimod.sm.fia$bimclass_soil_f <- ifelse(is.na(bimod.sm.fia$bimclass_soil_f), "low sample", as.character(bimod.sm.fia$bimclass_soil_f))

bimod.sm.fia.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.sm.fia, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


sm.bimpct.f <- round(length(bimod.sm.fia[bimod.sm.fia$bimclass_soil_f %in% "bimodal",]$bimclass_soil)/length(bimod.sm.fia$bimclass_soil_f)*100, digits = 2)

bimod.sm.fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.sm.fia, aes(x=x, y=y, fill = bimclass_soil_f))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", sm.bimpct.f, "%"))




# now merge all of these together to make a map of 1, 2, 3, bimodal metrics:
bim.class.m.f <- merge(bimod.pc.fia[,c("x", "y", "bimclass_f")],bimod.sm.fia[,c("x", "y", "bimclass_soil_f")], by = c("x", "y"), all = TRUE)
bim.class.m.f <- merge(bim.class.m.f, bimod.ppet.fia[,c("x", "y", "bimclass_ppet_f")])

bim.class.m.f$nbimod <- as.character(rowSums(bim.class.m.f[,3:5] == "bimodal", na.rm = TRUE))

three.color.bimodal.plots.fia <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bim.class.m.f, aes(x=x, y=y, fill = nbimod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red"
  ), labels = c("0","1", "2", "3")) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


bim.class.m.f$nbimod <- factor(bim.class.m.f$nbimod, levels = c("0","1", "2", "3", "No data"))

three.color.bimodal.plots.fia <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bim.class.m.f, aes(x=x, y=y, fill = nbimod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red","darkgrey"
  ), labels = c("0","1", "2", "3", "No data"), drop = F) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.23),legend.background = element_rect(fill=alpha('transparent', 0 )),legend.key = element_rect(colour = 'black',  size = 0.5, linetype='solid'),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "H", size = 4)



# >>>>>>>>>>>>>>>>>>>>>>>.new figures with 2d density plots: <<<<<<<<<<<<<<<<<<<<<<<
pls.df <- dens.clust
library(ks)
# for PC1:
H <- Hpi.diag(x=na.omit(cbind(pls.df$PC1fia, pls.df$FIAdensity)) )
fhat <- kde(x=na.omit(cbind(pls.df$PC1fia, pls.df$FIAdensity)), H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density")
#points(na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["16%"])[[1]])
contour_95 <- data.frame(contour_95)


interp.densp <- function(pc1val){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - pc1val)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  points <- data.frame(x=rep(pc1val, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(pc1val, 551 ), y=0:550)
 
   if(max(kde(x=na.omit(cbind(pls.df$PC1fia, pls.df$FIAdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    #df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df[pls.df$PC1fia <= (pc1val + 0.05) & pls.df$PC1fia >= (pc1val - 0.05), ]$PC1fia, pls.df[pls.df$PC1fia <= (pc1val + 0.05) & pls.df$PC1fia >= (pc1val - 0.05), ]$FIAdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$GS_ppet_mod, pls.df$FIAdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    
    #dipP <- diptest::dip.test(sample(x=df$points, prob = df$freq, size = 10000, replace = TRUE))$p.value
    dipP <- diptest::dip.test( df$freq)$p.value
   
  }
  dipP
}

# diptest on surface over a range of envt:
interp.densp.new <- function(pc1val){
      contour_95 <- contour_95[contour_95$y >=0,]
      closest <- contour_95[which.min(abs(contour_95$x - pc1val)),]
      maxy <- ceiling(closest$y) # get the closest y value and round up
      points <- data.frame(x=rep(pc1val, maxy+1 ), y=0:maxy)
      #points <- expand.grid(seq(from = pc1val - 0.05, to = pc1val + 0.05, by = 0.001), 1:maxy)
      if(max(kde(x=na.omit(cbind(pls.df$PC1fia, pls.df$FIAdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
       dipP <- NA
      }else{
        #df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$PC1fia, pls.df$FIAdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
        
        df <- data.frame(points = points$Var1, freq = kde(x=na.omit(cbind(pls.df[pls.df$PC1fia <= pc1val + 0.05 & pls.df$PC1fia >= pc1val - 0.05, ]$PC1fia, pls.df[pls.df$PC1fia <= pc1val + 0.05 & pls.df$PC1fia >= pc1val - 0.05, ]$FIAdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
       # df <- data.frame(na.omit(pls.df[pls.df$PC1fia <= pc1val + 0.04 & pls.df$PC1fia >= pc1val - 0.04, c("PC1fia", "FIAdensity")]),
        #      freq = predict(fhat, x = na.omit(pls.df[pls.df$PC1fia <= pc1val + 0.04 & pls.df$PC1fia >= pc1val - 0.04, c("PC1fia", "FIAdensity")])))
        
        dip <- rep(NA, 100)
        rand.sample <- rep(NA, 100, 100)
        
        for(i in 1:1000){
        rand.sample <- sample(1:length(df$freq), 1000, replace = TRUE)
        dip[i] <- diptest::dip.test(df[rand.sample,]$freq)$p.value
        }
        dipP <- mean(dip)
        
      #dipP <- diptest::dip.test(predict(fhat, x = na.omit(pls.df[pls.df$PC1fia <= pc1val + 0.04 & pls.df$PC1fia >= pc1val - 0.04, c("PC1fia", "FIAdensity")])))$p.value
      #dipP <- diptest::dip.test(predict(fhat, x = pls.df[rand, c("PC1fia", "FIAdensity")]))$p.value
      #dipP <- diptest::dip.test(sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE))$p.value
      }
      
      dipP

}

interp.densp <- function(pc1val){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - pc1val)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(pc1val, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(pc1val, 401 ), y=0:400)
  points <- expand.grid(seq(pc1val - 0.05, pc1val + 0.05, by = 0.001), y=0:400)
  colnames(points) <- c("x", "y")
  if(max(kde(x=na.omit(cbind(pls.df$PC1fia, pls.df$FIAdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$PC1fia, pls.df$FIAdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    dipP <- diptest::dip.test(sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE))$p.value
    #dipP <- diptest::dip.test( df$freq)$p.value
    
  }
  dipP
}


interp.densp(0.37403230)
#bimodal.fia.pc1 <- read.csv("outputs/new_bim_surface_PC1_fia_0.1_mode_crit_1000.csv")
bimodal.fia.pc1 <- readRDS("outputs/bimodal_bins_p_value_dipP_FIA_pc1_bins_kde_sample.rds")


pvalues <- bimodal.fia.pc1 %>% group_by(pc1_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                       median.p = median(pvalue, na.rm = TRUE),
                                                                       ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                       ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                       mean.d = mean(dip, na.rm = TRUE),
                                                                       median.d = median(dip, na.rm = TRUE),
                                                                       ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                       ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)


# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pvalues$pc1_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(pc1_bins_f = unique(unique(pvalues$pc1_bins)),
                           pc1_mids_f = as.numeric(mids)+0.125)



colnames(pvalues)[1] <- "pc1_bins_f"

bimodal.fia.pc1 <- merge(pvalues, ordered.cuts, by = "pc1_bins_f")
bimodal.fia.pc1$bimclass_f <- ifelse(bimodal.fia.pc1$median.p <= 0.05, "bimodal", "unimodal")

pc1.f.bim.line <- data.frame(PC1 = ifelse(is.null(nrow(unique(bimodal.fia.pc1[bimodal.fia.pc1$bimclass_f %in% "bimodal",]$mids))),NA, 
                                          unique(bimodal.fia.pc1[bimodal.fia.pc1$bimclass_f %in% "bimodal",]$mids)), y = -37, bimodal = "bimodal")


#pc1.f.bim.line <- data.frame(PC1 = seq(from = -6, to =6, by = 0.05), pval = NA, bimodal = NA)
#pc1.f.bim.line$pval <- apply(data.frame(pc1.f.bim.line$PC1), MARGIN= 1, FUN=interp.densp)
#pc1.f.bim.line$bimodal <- ifelse(pc1.f.bim.line$pval <= 0.05, "bimodal", "unimodal")
#pc1.f.bim.line$y <- -37
#fia.kde.plot.pc1 <- recordPlot()
library(base2grob)
fia.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), cex.axis=1.5
                                         )+ text(-6,500, "B"))

#plot_grid(pls.kde.plot.pc1.grob, three.color.bimodal.plots, ncol = 2)

# make the plot with GGPLOT:
fia.kde.plot.pc1.gg <- ggplot(pls.df, aes(x=PC1, y=FIAdensity) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_distiller(palette= c("YlOrRd"), direction=1 )+ylab("Tree Density")+theme(legend.position = "none")


fia.kde.plot.pc1.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550),  yaxt="n", cex.axis=0.7) + points(data = pc1.f.bim.line[pc1.f.bim.line$bimodal %in% "bimodal",], y~PC1, cex = 0.9,  pch = 15,col = "red")+ text(-5.5,500, "E"))+ xlab("P-PET")
fia.kde.plot.pc1.gg + xlab("PC1")

#fia.kde.plot.pc1 <- recordPlot()
#fia.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density"))


# for P-PET:
H <- Hpi.diag(x=na.omit(cbind(pls.df$GS_ppet_mod, pls.df$FIAdensity)) )
fhat <- kde(x=na.omit(cbind(pls.df$GS_ppet_mod, pls.df$FIAdensity)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,75,80,85,95), xlab = "P-PET", ylab = "Tree density")
#points(na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
fia.kde.plot.ppet <- recordPlot()
fia.kde.plot.ppet.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density"))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)


interp.densp <- function(pc1val){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - pc1val)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(pc1val, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  points <- data.frame(x=rep(pc1val, 551 ), y=0:550)
  if(max(kde(x=na.omit(cbind(pls.df$GS_ppet_mod, pls.df$FIAdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$GS_ppet_mod, pls.df$FIAdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    dipP <- diptest::dip.test(sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE))$p.value
    #dipP <- diptest::dip.test( df$freq )$p.value
    
  }
  dipP
}

#ppet.f.bim.line <- data.frame(PPET = seq(from = -100, to =300, by = 10), pval = NA, bimodal = NA)
#ppet.f.bim.line$pval <- apply(data.frame(ppet.f.bim.line$PPET), MARGIN= 1, FUN=interp.densp)
#ppet.f.bim.line$bimodal <- ifelse(ppet.f.bim.line$pval <= 0.05, "bimodal", "unimodal")
#ppet.f.bim.line$y <- -37

#bimodal.fia.ppet <- read.csv("outputs/new_bim_surface_PPET_fia_4_mode_crit_1000.csv")
bimodal.fia.ppet <- readRDS("outputs/bimodal_bins_p_value_dipP_FIA_PPET_kde.rds")


pvalues <- bimodal.fia.ppet %>% group_by(ppet_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                       median.p = median(pvalue, na.rm = TRUE),
                                                                       ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                       ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                       mean.d = mean(dip, na.rm = TRUE),
                                                                       median.d = median(dip, na.rm = TRUE),
                                                                       ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                       ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pvalues$ppet_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(ppet_bins_f = unique(unique(pvalues$ppet_bins)),
                           ppet_mids_f = as.numeric(mids)+7.5)


colnames(pvalues)[1] <- "ppet_bins_f"



bimodal.fia.ppet <- merge(pvalues, ordered.cuts, by = "ppet_bins_f")
bimodal.fia.ppet$bimclass_ppet_f <- ifelse(bimodal.fia.ppet$median.p <= 0.05, "bimodal", "unimodal")

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

ppet.f.bim.line <- data.frame(PPET = ifelse(is.null(nrow(unique(bimodal.fia.ppet[bimodal.fia.ppet$bimclass_ppet_f %in% "bimodal",]$GS_ppet_mod))),NA, 
                                            unique(bimodal.fia.ppet[bimodal.fia.ppet$bimclass_ppet_f %in% "bimodal",]$GS_ppet_mod)), y = -37, bimodal = "bimodal")


fia.kde.plot.pc1 <- recordPlot()
library(base2grob)
fia.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PPET", ylab = NA, ylim = c(-40,550)))


# make the plot with GGPLOT:
fia.kde.plot.ppet.gg <- ggplot(pls.df, aes(x=PC1, y=FIAdensity) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_distiller(palette= c("YlOrRd"), direction=1 )+ylab("Tree Density")+theme(legend.position = "none")


fia.kde.plot.ppet.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab="P-PET",ylab=NA,  ylim = c(-40,550), xlim = c(-200, 300),  yaxt="n" , cex.axis=0.7) + points(data = ppet.f.bim.line[ppet.f.bim.line$bimodal %in% "bimodal",], y~PPET, cex = 0.9,  pch = 15,col = "red")+ text(-170,500, "F"))+xlab("P-PET")
fia.kde.plot.ppet.gg +xlab("P-PET")


# for soil moisture/bucket model

H <- Hpi.diag(x=na.omit(cbind(pls.df$mean_GS_soil_m, pls.df$FIAdensity)) )
fhat <- kde(x=na.omit(cbind(pls.df$mean_GS_soil_m, pls.df$FIAdensity)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,65,75,80,85,95), xlab = "Growing season soil", ylab = "Tree density")
#points(na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
fia.kde.plot.sm <- recordPlot()
fia.kde.plot.sm.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "soil moisture", ylab = "Tree density"))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)


interp.densp <- function(pc1val){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - pc1val)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(pc1val, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  points <- data.frame(x=rep(pc1val, 551 ), y=0:550)
  if(max(kde(x=na.omit(cbind(pls.df$mean_GS_soil_m, pls.df$FIAdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$mean_GS_soil_m, pls.df$FIAdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    dipP <- diptest::dip.test(sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE))$p.value
    #dipP <- diptest::dip.test( df$freq)$p.value
    
  }
  dipP
}

#sm.f.bim.line <- data.frame(SM = seq(from = 0, to = 1.5, by = 0.05), pval = NA, bimodal = NA)
#sm.f.bim.line$pval <- apply(data.frame(sm.f.bim.line$SM), MARGIN= 1, FUN=interp.densp)
#sm.f.bim.line$bimodal <- ifelse(sm.f.bim.line$pval <= 0.05, "bimodal", "unimodal")
#sm.f.bim.line$y <- -37

#bimodal.fia.sm <- read.csv("outputs/new_bim_surface_soil_moist_fia_0.01_mode_crit.csv")
bimodal.fia.sm <- readRDS("outputs/bimodal_bins_p_value_dipP_FIA_soil_15bins_kde_sample.rds")


pvalues <- bimodal.fia.sm %>% group_by(soil_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                         median.p = median(pvalue, na.rm = TRUE),
                                                                         ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                         ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                         mean.d = mean(dip, na.rm = TRUE),
                                                                         median.d = median(dip, na.rm = TRUE),
                                                                         ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                         ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pvalues$soil_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(soil_bins_f = unique(unique(pvalues$soil_bins)),
                           soil_mids_f = as.numeric(mids)+0.025)

colnames(pvalues)[1] <- "soil_bins_f"



bimodal.fia.sm <- merge(pvalues, ordered.cuts, by = "soil_bins_f")
bimodal.fia.sm$bimclass_soil_f <- ifelse(bimodal.fia.sm$median.p <= 0.05, "bimodal", "unimodal")

sm.f.bim.line <- data.frame(SM = ifelse(is.null(nrow(unique(bimodal.fia.sm[bimodal.fia.sm$bimclass_soil_f %in% "bimodal",]$mean_GS_soil_m))),NA, 
                                        unique(bimodal.fia.sm[bimodal.fia.sm$bimclass_soil_f %in% "bimodal",]$mean_GS_soil_m)), y = -37, bimodal = "bimodal")



fia.kde.plot.pc1 <- recordPlot()
library(base2grob)
fia.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550)))

#plot_grid(pls.kde.plot.pc1.grob, three.color.bimodal.plots, ncol = 2)

# make the plot with GGPLOT:
fia.kde.plot.sm.gg <- ggplot(pls.df, aes(x=mean_GS_soil_m, y=FIAdensity) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_distiller(palette= c("YlOrRd"), direction=1 )+ylab("Tree Density")+theme(legend.position = "none")


fia.kde.plot.sm.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = NA, ylim = c(-40,550), yaxt="n",  cex.axis=0.9) + points(data = sm.f.bim.line[sm.f.bim.line$bimodal %in% "bimodal",], y~SM, cex = 0.8,  pch = 15,col = "red")+ text(-0.05,500, "G"))
fia.kde.plot.sm.gg 


# make ggplot figures of cluster density

# need to merge together all of the bimodal/unimodal tags
library(ggplotify)

pls.df$pc1_bins <- cut(pls.df$PC1, breaks=seq(-5.5, 4.5, by = 0.25))
kde.surf.pc1.pls.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass"], bimodal.pls.pc1, by = "pc1_bins")

pls.df$ppet_bins <- cut(pls.df$GS_ppet, breaks=seq(-170, 205, by = 15))
kde.surf.ppet.pls.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass_ppet"], bimodal.pls.ppet, by = "ppet_bins")

pls.df$soil_bins <- cut(pls.df$mean_GS_soil, breaks=seq(0, 1.8, by = 0.05))
kde.surf.soil.pls.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass_soil"], bimodal.pls.soil, by = "soil_bins")


# now merge all of these together to make a map of 1, 2, 3, bimodal metrics:
bim.class.m <- merge(kde.surf.pc1.pls.df[,c("x", "y", "bimclass")],kde.surf.soil.pls.df [,c("x", "y", "bimclass_soil")], by = c("x", "y"))
bim.class.m <- merge(bim.class.m, kde.surf.ppet.pls.df[,c("x", "y", "bimclass_ppet", "PLSdensity")])

bim.class.m$nbimod <- as.character(rowSums(bim.class.m[,3:5] == "bimodal", na.rm = TRUE))
# define nbimod as a category:
bim.class.m$nbimod <- factor(bim.class.m$nbimod, levels = c("0","1", "2", "3", "No data"))

three.color.bimodal.plots <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bim.class.m, aes(x=x, y=y, fill = nbimod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red","darkgrey"
  ), labels = c("0","1", "2", "3", "No data"), drop = F) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.23),legend.background = element_rect(fill=alpha('transparent', 0 )),legend.key = element_rect(colour = 'black',  size = 0.5, linetype='solid'),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "D", size = 4)


one.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "1",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
two.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "2",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
three.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "3",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)

one.bimpct <- one.bimpct + two.bimpct + three.bimpct
two.bimpct <-  two.bimpct + three.bimpct
three.bimpct <-  three.bimpct



# merge for fia
pls.df$pc1_bins_f <- cut(pls.df$PC1fia, breaks=seq(-5.5, 4.5, by = 0.25))
kde.surf.pc1.fia.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass_f"], bimodal.fia.pc1, by = "pc1_bins_f")

pls.df$ppet_bins_f <- cut(pls.df$GS_ppet_mod, breaks=seq(-125, 310, by = 15))
kde.surf.ppet.fia.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass_ppet_f"], bimodal.fia.ppet, by = "ppet_bins_f")

pls.df$soil_bins_f <- cut(pls.df$mean_GS_soil_m, breaks=seq(0, 1.8, by = 0.05))
kde.surf.soil.fia.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass_soil_f"], bimodal.fia.sm, by = "soil_bins_f")


# FIA three color maps
# now merge all of these together to make a map of 1, 2, 3, bimodal metrics:
bim.class.m.f <- merge(kde.surf.pc1.fia.df[,c("x", "y", "bimclass_f")],kde.surf.soil.fia.df [,c("x", "y", "bimclass_soil_f")], by = c("x", "y"))
bim.class.m.f <- merge(bim.class.m.f, kde.surf.ppet.fia.df[,c("x", "y", "bimclass_ppet_f", "FIAdensity")])

bim.class.m.f$nbimod <- as.character(rowSums(bim.class.m.f[,3:5] == "bimodal", na.rm = TRUE))
# define nbimod as a category:
bim.class.m.f$nbimod <- factor(bim.class.m.f$nbimod, levels = c("0","1", "2", "3", "No data"))

three.color.bimodal.plots.fia <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bim.class.m.f[!is.na(bim.class.m.f$FIAdensity),], aes(x=x, y=y, fill = nbimod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red","darkgrey"
  ), labels = c("0","1", "2", "3", "No data"), drop = F) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.23),legend.background = element_rect(fill=alpha('transparent', 0 )),legend.key = element_rect(colour = 'black',  size = 0.5, linetype='solid'),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "D", size = 4)


one.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "1",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
two.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "2",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
three.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "3",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)

one.bimpct <- one.bimpct + two.bimpct + three.bimpct
two.bimpct <-  two.bimpct + three.bimpct
three.bimpct <-  three.bimpct



# merge fia and pls bimodal dfs together:
kde.surf.pc1.df <- merge(kde.surf.pc1.pls.df[,c("x", "y",  "PLSdensity",  "bimclass","pc1_bins", "pc1_mids")], kde.surf.pc1.fia.df[,c("x", "y",  "FIAdensity","bimclass_f","pc1_bins_f", "pc1_mids_f")], by = c("x", "y"), all = TRUE)
kde.surf.ppet.df <- merge(kde.surf.ppet.pls.df[,c("x", "y",  "PLSdensity",  "bimclass_ppet","ppet_bins", "ppet_mids")], kde.surf.ppet.fia.df[,c("x", "y",  "FIAdensity","bimclass_ppet_f", "ppet_bins_f","ppet_mids_f")], by = c("x", "y"), all = TRUE)
kde.surf.soilm.df <- merge(kde.surf.soil.pls.df[,c("x", "y",  "PLSdensity",  "bimclass_soil", "soil_bins", "soil_mids")], kde.surf.soil.fia.df[,c("x", "y",  "FIAdensity","bimclass_soil_f", "soil_bins_f","soil_mids_f")], by = c("x", "y"), all = TRUE)




flipped.pc1.hist <- ggplot(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",], aes(PLSdensity))+geom_density(color = "blue")+
  geom_density(data = kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",], aes(FIAdensity), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

flipped.ppet.hist <- ggplot(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",], aes(PLSdensity))+geom_density(color = "blue")+
  geom_density(data = kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",], aes(FIAdensity), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

flipped.soilm.hist <- ggplot(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",], aes(PLSdensity))+geom_density(color = "blue")+
  geom_density(data = kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",], aes(FIAdensity), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)


# alternative: get density lines then ggplotify them to align:
pls.soilm.density.df <- data.frame(y = density(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$PLSdensity)$y, 
           x = density(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$PLSdensity)$x)

fia.soilm.density.df <- data.frame(y = density(na.omit(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$FIAdensity))$y, 
                                 x = density(na.omit(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$FIAdensity))$x)
flipped.soilm.hist.gg <- as.ggplot(~plot(pls.soilm.density.df[pls.soilm.density.df$x < 550 & pls.soilm.density.df$x > -41,], type = "l", col = "blue", ylim = c(-40, 550), yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(fia.soilm.density.df[fia.soilm.density.df$x < 550,] , type = "l", col = "red"))



pls.ppet.density.df <- data.frame(y = density(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$PLSdensity)$y, 
                                 x = density(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$PLSdensity)$x)

fia.ppet.density.df <- data.frame(y = density(na.omit(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$FIAdensity))$y, 
                                 x = density(na.omit(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$FIAdensity))$x)
flipped.ppet.hist.gg <- as.ggplot(~plot(pls.ppet.density.df[pls.ppet.density.df$x < 550 & pls.ppet.density.df$x > -41,], type = "l", col = "blue", ylim = c(-40, 550),yaxt="n", ylab = NA, xlab = NA, xaxt = "n",xlim = c(0, 0.005)) + lines( fia.ppet.density.df[fia.ppet.density.df$x < 550,], type = "l", col = "red"))

flipped.ppet.hist.gg <- as.ggplot(~plot(fia.ppet.density.df[fia.ppet.density.df$x < 550,], type = "l", col = "red", ylim = c(-40, 550),yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines( pls.ppet.density.df[pls.ppet.density.df$x < 550 & pls.ppet.density.df$x > -41,], type = "l", col = "blue"))

pls.pc1.density.df <- data.frame(y = density(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$PLSdensity)$y, 
                                  x = density(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$PLSdensity)$x)

fia.pc1.density.df <- data.frame(y = density(na.omit(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$FIAdensity))$y, 
                                  x = density(na.omit(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$FIAdensity))$x)
flipped.pc1.hist.gg <- as.ggplot(~plot(fia.pc1.density.df[fia.pc1.density.df$x < 550 ,], type = "l", col = "red", ylim = c(-41, 550), yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.pc1.density.df[pls.pc1.density.df$x < 550,], type = "l", col = "blue"))

library(gtable)
g1 <- ggplotGrob(pls.kde.plot.pc1.gg+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-0.1), "cm")))
g2 <- ggplotGrob(fia.kde.plot.pc1.gg+theme(plot.margin=unit(c(-0.7,-1,-0.5,-1), "cm")))
g3 <- ggplotGrob(flipped.pc1.hist.gg+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g4 <- ggplotGrob(pls.kde.plot.ppet.gg+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g5 <- ggplotGrob(fia.kde.plot.ppet.gg+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g6 <- ggplotGrob(flipped.ppet.hist.gg+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g7 <- ggplotGrob(pls.kde.plot.sm.gg+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g8 <- ggplotGrob(fia.kde.plot.sm.gg+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g9 <- ggplotGrob(flipped.soilm.hist.gg+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))
g10 <- ggplotGrob(three.color.bimodal.plots)
g11 <- ggplotGrob(three.color.bimodal.plots.fia)

g <- cbind(g1 ,g2, g3, size = "first")
g$heights <-unit.pmax(g1$heights, g2$heights, g3$heights)#, 

grow2 <- cbind(g4 ,g5, g6, size = "first")
grow2$heights <-unit.pmax(g4$heights, g5$heights, g6$heights)#, 

grow3 <- cbind(g7 ,g8, g9, size = "first")
grow3$heights <-unit.pmax(g7$heights, g8$heights, g9$heights)#, 
grow4 <- cbind(g10, g11, size = "first")
grow4$heights <-unit.pmax(g10$heights, g11$heights)
#png(height = 9, width = 6, units = "in", res = 300, "outputs/paper_figs/new_figure_3.png")
grid.arrange(arrangeGrob(g1,g2,g3, ncol=3, nrow=1, widths = c(1,1,0.2)), 
             arrangeGrob(g4, g5, g6, ncol = 3, nrow = 1, widths = c(1,1,0.2)) ,
             arrangeGrob(g7,g8, g9, ncol = 3, nrow = 1, widths = c(1,1,0.2)), 
             arrangeGrob(g10, g11, ncol = 3, nrow = 1, widths = c(1,1, 0)))
#dev.off()


png(height = 10, width = 6, units = "in", res = 500, "outputs/paper_figs/new_figure_3_kde_plot_with_hist.png")
fig3 <- grid.arrange(g, grow2, grow3,grow4, ncol = 1)
fig3
dev.off()



# redraw the iset histogram
hist.inset <- ggdraw() +
  draw_plot(f.clust.hist, 0, 0, 1, 1) +
  draw_plot(inset + theme(axis.text.x = element_text(angle = 45)), 0.7, 0.075, 0.3, 0.85)# +

hist.inset2 <- ggdraw() +
  draw_plot(f.clust.hist, 0, 0, 1, 1) +
  draw_plot(inset + theme(axis.text.x = element_text(angle = 45)), x= 0.7, y = 0.15, width = 0.3, height=0.7)# +

# write out new figure 2 to a png and annotate with A-F designations
png(height = 11, width = 4, units = 'in', res = 300, "outputs/paper_figs/fig2_10panel_v3.png")
grid.arrange(pls.map.alt.color + annotate("text", x=-90000, y=1486000,label= "A", size = 3)+ggtitle("PRE-SETTLEMENT"), 
             fia.map.alt.color + annotate("text", x=-90000, y=1486000,label= "F", size = 3)+ggtitle("MODERN"),
             pls.clust+ annotate("text", x=-90000, y=1486000,label= "B", size = 3),
             fia.clust + annotate("text", x=-90000, y=1486000,label= "G", size = 3), 
             scatter_dens_2dpls + annotate("text", x=4, y=600,label= "C", size = 3),
             scatter_dens_2dfia + annotate("text", x=4, y=600,label= "H", size = 3), 
             clust.hist + annotate("text", x=600, y=20,label= "D", size = 3),
             hist.inset + annotate("text", x=600, y=20,label= "I", size = 3), 
             bimod.pc.pls.map + annotate("text", x=-90000, y=1486000,label= "E", size = 3),
             bimod.pc.fia.map + annotate("text", x=-90000, y=1486000,label= "J", size = 3),
             ncol = 2)
dev.off()

png(height = 11, width = 4, units = 'in', res = 300, "outputs/paper_figs/fig2_10panel_v4.png")
grid.arrange(pls.map.alt.color + annotate("text", x=-90000, y=1486000,label= "A", size = 3)+ggtitle("PRE-SETTLEMENT"), 
             fia.map.alt.color + annotate("text", x=-90000, y=1486000,label= "E", size = 3)+ggtitle("MODERN"),
             pls.clust+ annotate("text", x=-90000, y=1486000,label= "B", size = 3),
             fia.clust + annotate("text", x=-90000, y=1486000,label= "F", size = 3), 
             scatter_dens_2dpls + annotate("text", x=4, y=600,label= "C", size = 3),
             scatter_dens_2dfia + annotate("text", x=4, y=600,label= "G", size = 3), 
             clust.hist + annotate("text", x=600, y=20,label= "D", size = 3),
             hist.inset2 + annotate("text", x=600, y=20,label= "H", size = 3), 
             #bimod.pc.pls.map + annotate("text", x=-90000, y=1486000,label= "E", size = 3),
            # bimod.pc.fia.map + annotate("text", x=-90000, y=1486000,label= "J", size = 3),
             ncol = 2)
dev.off()

png(heightk = 11, width = 4, units = 'in', res = 300, "outputs/paper_figs/fig2_10panel_v3b.png")

plot_grid(pls.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")), 
          fia.map.alt.color+theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
          pls.clust+theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
          fia.clust+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
          scatter_dens_2dpls+theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
          scatter_dens_2dfia+theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
          clust.hist+theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
          p.bimodal15+theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
          p.bimodal15.f+theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),labels = c("A", "B", "C", "D", "E","F", "G", "H", "I", "J"), ncol = 2, align = "v")

dev.off()



png(height = 9, width = 4, units = 'in', res = 300, "outputs/paper_figs/fig2_10panel_v3c.png")
plot_grid(pls.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "A", size = 3)+ggtitle("PRE-SETTLEMENT"), 
             fia.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "F", size = 3)+ggtitle("MODERN"),
             pls.clust+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "B", size = 3),
             fia.clust+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "G", size = 3), 
             scatter_dens_2dpls + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5, margin = unit(c(0, 0, 0, 0), "mm")))+ annotate("text", x=4, y=600,label= "C", size = 3),
             scatter_dens_2dfia + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5))+ annotate("text", x=4, y=600,label= "H", size = 3), 
             clust.hist + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5))+ annotate("text", x=600, y=20,label= "D", size = 3),
             hist.inset+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)) + annotate("text", x=600, y=20,label= "I", size = 3), 
             p.bimodal15 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA))+ annotate("text", x=-90000, y=1486000,label= "E", size = 3),
             p.bimodal15.f+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "J", size = 3),
             ncol = 2,align = "h",axis="tb", scale = 1) 

dev.off()



png(height = 11, width = 4, units = 'in', res = 300, "outputs/paper_figs/fig2_10panel_non_ag_supplement_v3.png")
grid.arrange(pls.map.nona + annotate("text", x=-90000, y=1486000,label= "A", size = 3)+ggtitle("PRE-SETTLEMENT"), 
             fia.map + annotate("text", x=-90000, y=1486000,label= "B", size = 3)+ggtitle("MODERN"),
             pls.clust.nona+ annotate("text", x=-90000, y=1486000,label= "C", size = 3),
             fia.clust + annotate("text", x=-90000, y=1486000,label= "D", size = 3), 
             scatter_dens_2dpls.nona + annotate("text", x=4, y=600,label= "E", size = 3),
             scatter_dens_2dfia.nona + annotate("text", x=4, y=600,label= "F", size = 3), 
             clust.hist.nona + annotate("text", x=600, y=20,label= "G", size = 3),
             f.clust.hist.nona + annotate("text", x=600, y=20,label= "H", size = 3), 
             p.bimodal15.nona + annotate("text", x=-90000, y=1486000,label= "I", size = 3),
             p.bimodal15.f + annotate("text", x=-90000, y=1486000,label= "J", size = 3),
             ncol = 2)
dev.off()


# figure 2 new again:
inset2 <- ggboxplot( data= full.fia.surveys, x = 'INVYRcd',y = 'FIAdensity', merge=TRUE,width = 0.5, fill = "INVYRcd",  palette =c("grey28", "grey40", "grey60"), outlier.size = 0.0005)+ylim(0,600) +theme_bw(base_size = 8)+theme(axis.title = element_blank(),axis.ticks.y= element_blank(), axis.text.y= element_blank(),legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob2 <- ggplotGrob(inset2)


f.clust.hist.inset.full <- f.clust.hist.full+ scale_y_continuous(breaks=c(0,250,500,750))+theme(plot.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ annotation_custom(grob = xbp_grob2, ymin = 700, ymax = 1100, xmin = -140, xmax = 648)#+ annotation_custom(grob = xbp_grob, ymin = 750, ymax = 1100, xmin = -141, xmax = 648)

box <- data.frame(
  x = 0.75,
  y = 0
)

f.clust.hist.full2 <- ggdraw(f.clust.hist.full) + 
  geom_rect(data = box, aes(xmin = x, xmax = x + .15, ymin = y, ymax = y + .15),
            colour = "gray60", fill = "gray80")

#Other way of overlaying the plots:
box.inset <- ggplot(data = full.fia.surveys, aes(x = INVYRcd, y = FIAdensity, fill= INVYRcd, size = 0.25))+geom_boxplot(size = 0.25)+ylim(0,600)+theme_transparent()+theme(axis.title = element_blank(), axis.line = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), panel.grid.major = element_blank(), panel.background  = element_blank(), legend.position = "none")

hist.inset <- ggdraw() +
  draw_plot(f.clust.hist.full + theme(legend.justification = "bottom")) +
  draw_plot(box.inset, 0.6, 0.025, 0.3, 0.975)# +
#draw_plot_label(c("A", "B"), c(0, 0.5), c(1, 0.92), size = 15)



png(height = 8.4, width = 4, units = 'in', res = 300, "outputs/paper_figs/fig2_6panel_trans.png")
plot_grid(pls.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "A", size = 3), 
          fia.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "D", size = 3),
          pls.clust+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "B", size = 3),
          fia.clust+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "E", size = 3), 
          clust.hist.full + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5))+ annotate("text", x=600, y=20,label= "C", size = 3),
          #hist.inset+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)) + annotate("text", x=600, y=20,label= "F", size = 3), 
          
          f.clust.hist.inset.full+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)) + annotate("text", x=600, y=20,label= "F", size = 3), 
          ncol = 2, align = "h", axis="tb", scale = 1) 
dev.off()




# figure 3 new:
require(gridGraphics)
png(height = 8, width = 4, units = 'in', res = 300, "outputs/paper_figs/fig3_kde_3color_maps_v1.png")
plot_grid(pls.kde.plot.pc1, pls.kde.plot.ppet, pls.kde.plot.sm, three.color.bimodal.plots, 
          fia.kde.plot.pc1, fia.kde.plot.ppet, fia.kde.plot.sm, three.color.bimodal.plots.fia, ncol = 2, align = "h")#,
          #ncol = 2,align = "h",axis="tb", scale = 1)
dev.off()


gA$widths[2:5] <- as.list(maxWidth) 
gB$widths[2:5] <- as.list(maxWidth) 
grid.arrange(gA, gB, ncol=1)


grid.draw(cbind(
  rbind(ggplotGrob(pls.kde.plot.pc1.gg + ggtitle("Past")+theme(title = element_text(size = 4, hjust = 0.65))),
                ggplotGrob(fia.kde.plot.pc1.gg+theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())+ ggtitle("Modern") +theme(title = element_text(size = 8, hjust = 0.65))),
                ggplotGrob(flipped.pc1.hist+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5))), size="last")
, size = "last"))

png(height = 8, width = 5, units = "in", res = 300, "outputs/paper_figs/fig3_kde_3color_maps.png")
grid.arrange(pls.kde.plot.pc1.gg + ggtitle("Past")+theme(title = element_text(size = 4, hjust = 0.65)), 
             fia.kde.plot.pc1.gg+theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())+ ggtitle("Modern"),
             flipped.pc1.hist.gg,#+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)), 
             pls.kde.plot.ppet.gg, 
             fia.kde.plot.ppet.gg, 
             flipped.ppet.hist+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)),
             pls.kde.plot.sm.gg,
             fia.kde.plot.sm.gg,  
             flipped.soilm.hist+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.35)),
             three.color.bimodal.plots, 
             three.color.bimodal.plots.fia, ncol = 3, heights = c(2,2,2,2), widths = c(1,1,0.6))
dev.off()



pushViewport(viewport(layout = grid.layout(1, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y) 
print(pls.kde.plot.pc1.gg , vp = vplayout(1, 1))
print(fia.kde.plot.pc1.gg, vp = vplayout(1, 2))
print(flipped.pc1.hist, vp = vplayout(1, 3))


png(height = 8, width = 5, units = "in", res = 300, "outputs/paper_figs/fig3_kde_3color_maps2.png")
plot_grid(pls.kde.plot.pc1.gg + ggtitle("Past")+theme(title = element_text(size = 4, hjust = 0.65)), 
          fia.kde.plot.pc1.gg+theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())+ ggtitle("Modern") +theme(title = element_text(size = 8, hjust = 0.65)),
          flipped.pc1.hist.gg+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)), 
          pls.kde.plot.ppet.gg, 
          fia.kde.plot.ppet.gg, 
          flipped.ppet.hist+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)),
          pls.kde.plot.sm.gg,
          fia.kde.plot.sm.gg,  
          flipped.soilm.hist+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.35)),
          three.color.bimodal.plots, 
          three.color.bimodal.plots.fia, ncol = 3, align = "v")
dev.off()


# ----------------------------Future 3 color bimodal plots ----------------------------------
# >>>>>>>>>>for pc1 predicted by past:
bimod.pc1.85.pls <- read.csv("outputs/new_bim_surface_PC1_future_8.5_pred_by_pls_0.1_mode_crit_1000.csv")
bimod.pc1.85.pls$eco <- ifelse(bimod.pc1.85.pls$PLSdensity <= 0.5, "Prairie", 
                                ifelse(bimod.pc1.85.pls$PLSdensity <= 47, "Savanna", "Forest"))
#bimod.pc1.85.pls$bimclass_f_pred_pls_85 <- ifelse(bimod.pc1.85.pls$dipPint_f_pred_pls_pc1 <= 0.05 , "bimodal", "unimodal")
bimod.pc1.85.pls$bimclass_f_pred_pls_85 <- ifelse(is.na(bimod.pc1.85.pls$bimclass_f_pred_pls_85), "out-of-sample", as.character(bimod.pc1.85.pls$bimclass_f_pred_pls_85))

bimod.pc1.85.pls$bimclass_eco <- ifelse(is.na(bimod.pc1.85.pls$bimclass_f_pred_pls_85 ) | is.na(bimod.pc1.85.pls$eco), NA ,paste(bimod.pc1.85.pls$bimclass_f_pred_pls_85 , bimod.pc1.85.pls$eco))

bimod.pc1.85.pls.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc1.85.pls, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


pc1.bimpct.f <- round(length(bimod.pc1.85.pls[bimod.pc1.85.pls$bimclass_f_pred_pls_85%in% "bimodal",]$bimclass_f_pred_pls_85)/length(bimod.pc1.85.pls$bimclass_f_pred_pls_85)*100, digits = 2)

bimod.pc1.85.pls$insample <- ifelse( bimod.pc1.85.pls$PC1_cc85 >= range(bimod.pc1.85.pls$PC1, na.rm=TRUE)[1] & bimod.pc1.85.pls$PC1_cc85 <=  range(bimod.pc1.85.pls$PC1, na.rm=TRUE)[2], "in-sample", "out-of-sample")
bimod.pc1.85.pls$bimclass_f_pred_pls_85_spl <- ifelse( bimod.pc1.85.pls$insample %in% "in-sample", as.character(bimod.pc1.85.pls$bimclass_f_pred_pls_85), "out-of-sample")


bimod.pc1.85.pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc1.85.pls, aes(x=x, y=y, fill = bimclass_f_pred_pls_85_spl))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample"="tan"
  ), labels = c("bimodal", "unimodal")) +
  coord_equal()+theme_bw(base_size = 12)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.5,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", pc1.bimpct.f, "%"))+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "A", size = 4)

pc.bimod.85.hist <- ggplot(bimod.pc1.85.pls, aes(PC1_cc85,fill = bimclass_f_pred_pls_85_spl))+geom_histogram(position = "stack", binwidth = 0.25)+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample"="tan"), name = "")+xlim(-6.4, 4.5)


# new estimates of bimodality:

bimod.pc1.85.pls$pc1_bins_8.5 <- cut(bimod.pc1.85.pls$PC1_cc85, breaks=seq(-5.5, 4.5, by = 0.25))
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>> for pc1 predicted by modern: <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bimod.pc1.85.fia <- read.csv("outputs/new_bim_surface_PC1_future_8.5_pred_by_fia_1000_mode_crit.csv")
bimod.pc1.85.fia$eco <- ifelse(bimod.pc1.85.fia$FIAdensity <= 0.5, "Prairie", 
                                ifelse(bimod.pc1.85.fia$FIAdensity <= 47, "Savanna", "Forest"))
bimod.pc1.85.fia$bimclass_f_pred_fia_85 <- ifelse(bimod.pc1.85.fia$dipPint_f_pred_fia_85 <= 0.05 , "bimodal", "unimodal")
bimod.pc1.85.fia$bimclass_f_pred_fia_85 <- ifelse(is.na(bimod.pc1.85.fia$bimclass_f_pred_fia_85), "out-of-sample", as.character(bimod.pc1.85.fia$bimclass_f_pred_fia_85))

bimod.pc1.85.fia$bimclass_eco <- ifelse(is.na(bimod.pc1.85.fia$bimclass_f_pred_fia_85) | is.na(bimod.pc1.85.fia$eco), NA ,paste(bimod.pc1.85.fia$bimclass_f_pred_fia_85, bimod.pc1.85.fia$eco))

bimod.pc1.85.fia.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc1.85.fia, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


pc1.bimpct.f <- round(length(bimod.pc1.85.fia[bimod.pc1.85.fia$bimclass_f_pred_fia_85 %in% "bimodal",]$bimclass_f_pred_fia_85)/length(bimod.pc1.85.fia$bimclass_f_pred_fia_85)*100, digits = 2)
#omit out of sample:
bimod.pc1.85.fia$insample <- ifelse( bimod.pc1.85.fia$PC1_cc85 >= range(bimod.pc1.85.fia$PC1fia, na.rm=TRUE)[1] & bimod.pc1.85.fia$PC1_cc85 <=  range(bimod.pc1.85.fia$PC1fia, na.rm=TRUE)[2], "in-sample", "out-of-sample")
bimod.pc1.85.fia$bimclass_f_pred_fia_85_spl <- ifelse( bimod.pc1.85.fia$insample %in% "in-sample", as.character(bimod.pc1.85.fia$bimclass_f_pred_fia_85), "out-of-sample")


bimod.pc1.85.fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc1.85.fia, aes(x=x, y=y, fill = bimclass_f_pred_fia_85_spl))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal"= '#d73027', "unimodal"='#4575b4', "out-of-sample"="tan")) +
  coord_equal()+theme_bw(base_size = 12)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.5,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", pc1.bimpct.f, "%"))+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "B", size = 4)


pc.bimod.85.hist.fia <- ggplot(bimod.pc1.85.fia, aes(PC1_cc85, fill = bimclass_f_pred_fia_85_spl))+geom_histogram(position = "stack",  binwidth = 0.25)+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample"="tan"), name = "")+xlim(-6.4, 4.5)


bimod.pc1.85.fia$pc1_bins_f_8.5 <- cut(bimod.pc1.85.fia$PC1_cc85, breaks=seq(-5.5, 4.5, by = 0.25))
# >>>>>>>>>>>>>>>>>>>>>> for PPET predicted by past: <<<<<<<<<<<<<<<<<<<<<<<<<
bimod.ppet.85.pls <- read.csv("outputs/new_bim_surface_PPET_rcp85_pred_by_pls_1000_mode_crit.csv")
bimod.ppet.85.pls$eco <- ifelse(bimod.ppet.85.pls$PLSdensity <= 0.5, "Prairie", 
                              ifelse(bimod.ppet.85.pls$PLSdensity <= 47, "Savanna", "Forest"))
bimod.ppet.85.pls$bimclass_f_pred_pls_ppet <- ifelse(bimod.ppet.85.pls$dipPint_f_pred_pls_ppet <= 0.05 , "bimodal", "unimodal")
bimod.ppet.85.pls$bimclass_f_pred_pls_ppet <- ifelse(is.na(bimod.ppet.85.pls$bimclass_f_pred_pls_ppet), "out-of-sample", as.character(bimod.ppet.85.pls$bimclass_f_pred_pls_ppet))


bimod.ppet.85.pls$bimclass_eco <- ifelse(is.na(bimod.ppet.85.pls$bimclass_f_pred_pls_ppet ) | is.na(bimod.ppet.85.pls$eco), NA ,paste(bimod.ppet.85.pls$bimclass_f_pred_pls_ppet , bimod.ppet.85.pls$eco))

bimod.ppet.85.pls.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.85.pls, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


ppet.bimpct.f <- round(length(bimod.ppet.85.pls[bimod.ppet.85.pls$bimclass_f_pred_pls_ppet %in% "bimodal",]$bimclass_f_pred_pls_ppet)/length(bimod.ppet.85.pls$bimclass_f_pred_pls_ppet)*100, digits = 2)
#omit out of sample
bimod.ppet.85.pls$insample <- ifelse( bimod.ppet.85.pls$mean_ppet_GS_8.5 >= range(bimod.ppet.85.pls$GS_ppet, na.rm=TRUE)[1] & bimod.ppet.85.pls$mean_ppet_GS_8.5 <=  range(bimod.ppet.85.pls$GS_ppet, na.rm=TRUE)[2], "in-sample", "out-of-sample")
bimod.ppet.85.pls$bimclass_f_pred_pls_85_ppet_spl <- ifelse( bimod.ppet.85.pls$insample %in% "in-sample", as.character(bimod.ppet.85.pls$bimclass_f_pred_pls_85_ppet), "out-of-sample")

bimod.ppet.85.pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.85.pls, aes(x=x, y=y, fill = bimclass_f_pred_pls_85_ppet_spl))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample" = "tan"), drop = FALSE)+
  
  coord_equal()+theme_bw(base_size = 12)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.5,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", ppet.bimpct.f, "%"))+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "C", size = 4)


ppet.bimod.85.hist.pls <- ggplot(bimod.ppet.85.pls, aes(mean_ppet_GS_8.5, fill = bimclass_f_pred_pls_85_ppet_spl))+geom_histogram(position = "stack", binwidth = 10)+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample"="tan"), name = "")+xlim(-170, 300)


bimod.ppet.85.pls$ppet_bins_8.5 <- cut(bimod.ppet.85.pls$mean_ppet_GS_8.5, breaks=seq(-125, 310, by = 15))
# >>>>>>>>>>>>>>>>>>>>>>>> for PPET predicted by modern: <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bimod.ppet.85.fia <- read.csv("outputs/new_bim_surface_PPET_rcp85_pred_by_fia_1000_mode_crit.csv")
#bimod.ppet.85.fia$bimclass_f_pred_fia_85_ppet <- ifelse(is.na(bimod.ppet.85.fia$bimclass_f_pred_fia_85_ppet), "out-of-sample", bimod.ppet.85.fia$bimclass_f_pred_fia_85_ppet)
bimod.ppet.85.fia$bimclass_f_pred_fia_85_ppet <- ifelse(is.na(bimod.ppet.85.fia$bimclass_f_pred_fia_85_ppet), "out-of-sample", as.character(bimod.ppet.85.fia$bimclass_f_pred_fia_85_ppet))

bimod.ppet.85.fia$eco <- ifelse(bimod.ppet.85.fia$FIAdensity <= 0.5, "Prairie", 
                              ifelse(bimod.ppet.85.fia$FIAdensity <= 47, "Savanna", "Forest"))
#bimod.ppet.85.fia$bimclass_f_pred_fia_ppet <- ifelse(bimod.ppet.85.fia$dipPint_f_pred_pls_ppet <= 0.05 , "bimodal", "unimodal")

bimod.ppet.85.fia$bimclass_eco <- ifelse(is.na(bimod.ppet.85.fia$bimclass_f_pred_fia_ppet) | is.na(bimod.ppet.85.fia$eco), NA ,paste(bimod.ppet.85.fia$bimclass_f_pred_fia_ppet, bimod.ppet.85.fia$eco))
bimod.ppet.85.fia.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.85.fia, aes(x=x, y=y, fill = dipPint_f_pred_fia_ppet))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


ppet.bimpct.f <- round(length(bimod.ppet.85.fia[bimod.ppet.85.fia$bimclass_f_pred_fia_ppet %in% "bimodal",]$bimclass_f_pred_fia_ppet)/length(bimod.ppet.85.fia$bimclass_f_pred_fia_ppet)*100, digits = 2)

# omit out of sample
bimod.ppet.85.fia$insample <- ifelse( bimod.ppet.85.fia$mean_ppet_GS_8.5 >= range(bimod.ppet.85.fia[! is.na(bimod.ppet.85.fia$FIAdensity),]$GS_ppet_mod, na.rm=TRUE)[1] & bimod.ppet.85.fia$mean_ppet_GS_8.5 <=  range(bimod.ppet.85.fia[! is.na(bimod.ppet.85.fia$FIAdensity),]$GS_ppet_mod, na.rm=TRUE)[2], "in-sample", "out-of-sample")
bimod.ppet.85.fia$bimclass_f_pred_fia_85_ppet_spl <- ifelse( bimod.ppet.85.fia$insample %in% "in-sample", as.character(bimod.ppet.85.fia$bimclass_f_pred_fia_85_ppet), "out-of-sample")

bimod.ppet.85.fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.85.fia, aes(x=x, y=y, fill = bimclass_f_pred_fia_85_ppet_spl))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample" = "tan"), drop = F)+
  coord_equal()+theme_bw(base_size = 12)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.5,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", ppet.bimpct.f, "%"))+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "D", size = 4)



ppet.bimod.85.hist.fia <- ggplot(bimod.ppet.85.fia, aes(mean_ppet_GS_8.5, fill = bimclass_f_pred_fia_85_ppet_spl))+geom_histogram(position = "stack", binwidth = 10)+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample"="tan"), name = "")+xlim(-170, 300)

bimod.ppet.85.fia$ppet_bins_f_8.5 <- cut(bimod.ppet.85.fia$mean_ppet_GS_8.5, breaks=seq(-125, 310, by = 15))

# >>>>>>>>>>>>>>>>>>>>>>>>>> for soil moisture predicted by past: <<<<<<<<<<<<<<<<<<<<<<<<<<
bimod.sm.85.pls <- read.csv("outputs/new_bim_surface_soil_m_rcp85_pred_by_pls.csv")
bimod.sm.85.pls$eco <- ifelse(bimod.sm.85.pls$PLSdensity <= 0.5, "Prairie", 
                              ifelse(bimod.sm.85.pls$PLSdensity <= 47, "Savanna", "Forest"))
bimod.sm.85.pls$bimclass_eco <- ifelse(is.na(bimod.sm.85.pls$bimclass_f_pred_pls_85_soil) | is.na(bimod.sm.85.pls$eco), NA ,paste(bimod.sm.85.pls$bimclass_f_pred_pls_85_soil, bimod.sm.85.pls$eco))

bimod.sm.85.pls.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.sm.85.pls, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac', "grey", "black", "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


sm.bimpct.f <- round(length(bimod.sm.85.pls[bimod.sm.85.pls$bimclass_f_pred_fia_85_soil %in% "bimodal",]$bimclass_f_pred_fia_85_soil)/length(bimod.sm.85.pls$bimclass_f_pred_fia_85_soil)*100, digits = 2)

# omit out of sample sites
bimod.sm.85.pls$insample <- ifelse( bimod.sm.85.pls$mean_GS_soil_8.5 >= range(bimod.sm.85.pls$mean_GS_soil, na.rm=TRUE)[1] & bimod.sm.85.pls$mean_GS_soil_8.5 <=  range(bimod.sm.85.pls$mean_GS_soil, na.rm=TRUE)[2], "in-sample", "out-of-sample")
bimod.sm.85.pls$bimclass_f_pred_pls_85_soil_spl <- ifelse( bimod.sm.85.pls$insample %in% "in-sample", as.character(bimod.sm.85.pls$bimclass_f_pred_pls_85_soil), "out-of-sample")


bimod.sm.85.pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.sm.85.pls, aes(x=x, y=y, fill = bimclass_f_pred_pls_85_soil_spl))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample"="tan"
  )) +
  coord_equal()+theme_bw(base_size = 12)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.5,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", sm.bimpct.f, "%"))+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "E", size = 4)


sm.bimod.85.hist.pls <- ggplot(bimod.sm.85.pls, aes(mean_GS_soil_8.5, fill = bimclass_f_pred_pls_85_soil_spl))+geom_histogram(position = "stack", binwidth = 0.05)+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample"="tan"), name = "")+xlim(0,1.75)

# cut into the same bins as pls and fia soils:
bimod.sm.85.pls$soil_bins_8.5 <- cut(bimod.sm.85.pls$mean_GS_soil_8.5, breaks=seq(0, 1.8, by = 0.05))
# write the mids for the future:
# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(bimod.sm.85.pls$soil_bins_8.5)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(soil_bins_8.5 = unique(unique(bimod.sm.85.pls$soil_bins_8.5)),
                           soil_mids_8.5 = as.numeric(mids)+0.025)

bimod.sm.85.pls <- merge(bimod.sm.85.pls, ordered.cuts, by = c("soil_bins_8.5"))

# >>>>>>>>>>>>>>>>>>>>>>  for soil moisture predicted by modern: <<<<<<<<<<<<<<<<<<<<<<<<<<<
bimod.sm.85.fia <- read.csv("outputs/new_bim_surface_soil_m_rcp85_pred_by_fia.csv")
bimod.sm.85.fia$eco <- ifelse(bimod.sm.85.fia$FIAdensity <= 0.5, "Prairie", 
                           ifelse(bimod.sm.85.fia$FIAdensity <= 47, "Savanna", "Forest"))
bimod.sm.85.fia$bimclass_eco <- ifelse(is.na(bimod.sm.85.fia$bimclass_f_pred_fia_85_soil) | is.na(bimod.sm.85.fia$eco), NA ,paste(bimod.sm.85.fia$bimclass_f_pred_fia_85_soil, bimod.sm.85.fia$eco))

bimod.sm.85.fia.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.sm.85.fia, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


  sm.bimpct.f <- round(length(bimod.sm.85.fia[bimod.sm.85.fia$bimclass_f_pred_fia_85_soil %in% "bimodal",]$bimclass_f_pred_fia_85_soil)/length(bimod.sm.85.fia$bimclass_f_pred_fia_85_soil)*100, digits = 2)

  # omit out of sample sites
  bimod.sm.85.fia$insample <- ifelse( bimod.sm.85.fia$mean_GS_soil_8.5 >= range(bimod.sm.85.fia$mean_GS_soil_m, na.rm=TRUE)[1] & bimod.sm.85.fia$mean_GS_soil_8.5 <=  range(bimod.sm.85.fia$mean_GS_soil_m, na.rm=TRUE)[2], "in-sample", "out-of-sample")
  bimod.sm.85.fia$bimclass_f_pred_fia_85_soil_spl <- ifelse( bimod.sm.85.fia$insample %in% "in-sample", as.character(bimod.sm.85.fia$bimclass_f_pred_fia_85_soil), "out-of-sample")
  
  
bimod.sm.85.fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.sm.85.fia, aes(x=x, y=y, fill = bimclass_f_pred_fia_85_soil_spl))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample"="tan")) +
  coord_equal()+theme_bw(base_size = 12)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.5,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", sm.bimpct.f, "%"))+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "F", size = 4)

sm.bimod.85.hist.fia <- ggplot(bimod.sm.85.fia, aes(mean_GS_soil_8.5, fill = bimclass_f_pred_fia_85_soil_spl))+geom_histogram(position = "stack", binwidth  = 0.05)+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample"="tan"), name = "")+xlim(0,1.75)
bimod.sm.85.fia$soil_bins_f_8.5 <- cut(bimod.sm.85.fia$mean_GS_soil_8.5, breaks=seq(0, 1.8, by = 0.05))
# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(bimod.sm.85.fia$soil_bins_f_8.5)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(soil_bins_f_8.5 = unique(unique(bimod.sm.85.fia$soil_bins_f_8.5)),
                           soil_mids_f_8.5 = as.numeric(mids)+0.025)

bimod.sm.85.fia <- merge(bimod.sm.85.fia, ordered.cuts, by = c("soil_bins_f_8.5"))


# Plot all 3 fia and all 3 pls figures
png(height = 16, width = 8, units = "in", res = 300, "outputs/paper_figs/future_8.5_preds_bimodal_out_of_sample.png")
plot_grid(bimod.pc1.85.pls.map + ggtitle(" "), 
          bimod.pc1.85.fia.map+ ggtitle(" "),
          bimod.ppet.85.pls.map+ ggtitle(" "),
          bimod.ppet.85.fia.map + ggtitle(" "),
          bimod.sm.85.pls.map+ ggtitle(" "),
          bimod.sm.85.fia.map+ ggtitle(" "), cols = 2)
dev.off()


# plot histograms of pls, fia, and future bimodal regions across climate space:
#-----Plot histograms for PC1
pc1.df <- merge(kde.surf.pc1.df[,c("x", "y","bimclass", "pc1_bins", "pc1_mids","bimclass_f","pc1_bins_f" ,"pc1_mids_f")], bimod.pc1.85.pls, by = c("x", "y"))
pc1.df <- merge(pc1.df, dens.clust[,c("x", "y", "foresttype_ordered")], by = c("x", "y"), all.x = TRUE)
pc1.df <- merge(pc1.df, species.clust[,c("x", "y", "foresttype")], by = c("x","y"), all.x = TRUE)
pc1.df$bimclass <- ifelse(is.na(pc1.df$bimclass), "low-sample-unimodal", as.character(pc1.df$bimclass))
pc1.df$bimclass_f <- ifelse(is.na(pc1.df$bimclass_f) & ! is.na(pc1.df$FIAdensity), "low-sample-unimodal", as.character(pc1.df$bimclass_f))


pc.bimod.pls <- ggplot(pc1.df[!is.na(pc1.df$PLSdensity),], aes(pc1_mids, fill = bimclass))+geom_bar(width = 0.25)+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(-6.4, 4.5)+xlab("PLS PC1")+ylim(0,1000)
pc.bimod.fia <- ggplot(pc1.df[!is.na(pc1.df$FIAdensity),], aes(pc1_mids_f,fill = bimclass_f))+geom_bar(width = 0.25)+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(-6.4, 4.5)+xlab("FIA PC1")+ylim(0,1000)

#pc.bimod.pls <- ggplot(pc1.df[!is.na(pc1.df$PLSdensity),], aes(PC1,fill = bimclass))+geom_histogram(position = "stack", bins = 35)+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(-6.4, 4.5)+xlab("PLS PC1")+ylim(0,1000)
#pc.bimod.fia <- ggplot(pc1.df[!is.na(pc1.df$FIAdensity),], aes(PC1fia,fill = bimclass_f))+geom_histogram(position = "stack", bins = 35)+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(-6.4, 4.5)+xlab("FIA PC1")+ylim(0,1000)

# color unimodal places by forest structure:
pc1.df$eco_fia <- ifelse(pc1.df$FIAdensity <= 0.5, "Prairie", 
                         ifelse(pc1.df$FIAdensity >= 47 & pc1.df$FIAdensity < 100, "Savanna", 
                                ifelse(pc1.df$FIAdensity >= 100, "Forest", NA)))
eco.mode.pc1.pls <- pc1.df[,c("eco", "pc1_mids", "pc1_bins")] %>% group_by(pc1_mids, pc1_bins) %>% dplyr::summarize(ecomode = names(which.max((table(eco)))))
eco.mode.pc1.fia <- pc1.df[,c("eco_fia", "pc1_mids_f", "pc1_bins_f")] %>% group_by(pc1_mids_f, pc1_bins_f)%>% dplyr::summarize(ecomode_f = names(which.max((table(eco_fia)))))

pc1.df <- merge(pc1.df, eco.mode.pc1.pls, by = c("pc1_mids", "pc1_bins"))
pc1.df <- merge(pc1.df, eco.mode.pc1.fia, by = c("pc1_mids_f", "pc1_bins_f"))

pc1.df$bimod_struct <- ifelse(pc1.df$bimclass %in% c("unimodal", "low-sample-unimodal"), as.character(pc1.df$ecomode), pc1.df$bimclass)
pc1.df$bimod_struct_f <- ifelse(pc1.df$bimclass_f %in% c("unimodal", "low-sample-unimodal"), as.character(pc1.df$ecomode_f), pc1.df$bimclass_f)

pc1.df$bimod_struct_actual <- ifelse(pc1.df$bimclass %in% c("unimodal","low-sample-unimodal"), as.character(pc1.df$eco), pc1.df$bimclass)
pc1.df$bimod_struct_f_actual <- ifelse(pc1.df$bimclass_f %in% c("unimodal","low-sample-unimodal"), as.character(pc1.df$eco_fia), pc1.df$bimclass_f)

# barplots with actual values for savanna vs. forest
pc1.bimod.pls.struct.act <- ggplot(pc1.df[!is.na(pc1.df$PLSdensity),], aes(pc1_mids, fill = bimod_struct_actual))+geom_bar(width = 0.25)+
  scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlim(-6.4, 4.5)+xlab("PLS PC1")+ylim(0,1000)

pc1.bimod.fia.struct.act <- ggplot(pc1.df[!is.na(pc1.df$bimod_struct_f_actual),], aes(pc1_mids_f, fill = bimod_struct_f_actual))+geom_bar(width = 0.25)+
  scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlim(-6.4, 4.5)+xlab("FIA PC1")+ylim(0,1000)


pc.bimod.pls.struct <- ggplot(pc1.df[!is.na(pc1.df$bimod_struct),], aes(pc1_mids,fill = bimod_struct))+geom_bar(position = "identity", width = 0.25)+
  scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlim(-6.4, 4.5)+xlab("PLS PC1")+ylim(0,1000)

pc.bimod.fia.struct <- ggplot(pc1.df[!is.na(pc1.df$bimod_struct_f),], aes(pc1_mids_f, fill = bimod_struct_f))+geom_bar(position = "identity", width = 0.25)+
  scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlim(-6.4, 4.5)+xlab("PLS PC1")+ylim(0,1000)

pc1.pval.pls <- ggplot(bimodal.pls.pc1, aes(pc1_mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "red", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.05, linetype = "dashed")+xlab("PLS PC1")+ylab("P value")+xlim(-6.4, 4.5)
pc1.pval.fia <- ggplot(bimodal.fia.pc1, aes(pc1_mids_f, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "red", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.05)+xlab("FIA PC1")+ylab("P value")+xlim(-6.4, 4.5)


pc1.dip.pls <- ggplot(bimodal.pls.pc1, aes(pc1_mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS PC1")+ylab("DIP value")+xlim(-6.4, 4.5)
pc1.dip.fia <- ggplot(bimodal.fia.pc1, aes(pc1_mids_f, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02)+xlab("FIA PC1")+ylab("DIP value")+xlim(-6.4, 4.5)


# color unimodal places by forest composition:
pc1.df$bimod_comps <- ifelse(pc1.df$bimclass %in% c("unimodal","low-sample-unimodal"), as.character(pc1.df$foresttype_ordered), pc1.df$bimclass)
pc1.df$bimod_comps_f <- ifelse(pc1.df$bimclass_f %in% c("unimodal","low-sample-unimodal"), as.character(pc1.df$foresttype), pc1.df$bimclass_f)

pc.bimod.pls.comp <- ggplot(pc1.df[!is.na(pc1.df$bimod_comps),], aes(PC1, fill = bimod_comps))+geom_histogram(position = "stack", binwidth = 0.25)+scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Oak-Hickory"='#beaed4', "Oak"='#386cb0',"N. Mixed Forest"='#ffff99',"Beech-Maple"='#bf5b17',"Aspen"='#f0027f',"Pine"='#fdc086', "Boreal/Sub-boreal"='#7fc97f'), name = " ")+xlim(-6.4, 4.5)#+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(-6.4, 4.5)+xlab("PLS PC1")+ylim(0,1000)
pc.bimod.fia.comp <- ggplot(pc1.df[!is.na(pc1.df$bimod_comps_f),], aes(PC1fia, fill = bimod_comps_f))+geom_histogram(position = "stack", binwidth = 0.25)+scale_fill_manual(values = c("Pine"='#fdc086', "Mixed Hardwoods"='#003c30',"Oak/Maple"='#a6cee3',"Poplar"='#f0027f', "bimodal"='#d73027', "low-sample-unimodal"="grey"), name = " ")+xlim(-6.4, 4.5)+xlab("FIA PC1")+ylim(0,1000)
# future hists for ppet:
# future soil predictions:
pc1.fut.fia <- merge(unique(pc1.df[,c("pc1_mids_f", "pc1_bins_f", "bimod_struct_f", "bimclass_f","bimod_struct_f_actual")]), bimod.pc1.85.fia,by.x = "pc1_bins_f", by.y = c("pc1_bins_f_8.5"))
pc1.fut.pls <- merge(unique(pc1.df[,c("pc1_mids", "pc1_bins", "bimod_struct", "bimclass","bimod_struct_actual")]), bimod.pc1.85.pls,by.x= "pc1_bins", by.y= "pc1_bins_8.5")

pc1.bimod.85.hist.pls <- ggplot(pc1.fut.pls[!is.na(pc1.fut.pls$bimod_struct),], aes(pc1_mids, fill = bimod_struct))+geom_bar(width = 0.25)+scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("RCP 8.5 PC1")+xlim(-6.4, 4.5)#+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(-6.4, 4.5)+xlab("PLS sm")+ylim(0,1000)
pc1.bimod.85.hist.fia <- ggplot(pc1.fut.fia[!is.na(pc1.fut.fia$bimod_struct_f),], aes(pc1_mids_f, fill = bimod_struct_f))+geom_bar(width = 0.25)+scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("RCP 8.5 PC1")+xlim(-6.4, 4.5)


# plot future with actual classes from past
pc1.fut.pls$bimod_struct_8.5.act <- ifelse(is.na(pc1.fut.pls$bimod_struct_actual) & !is.na(pc1.fut.pls$PLSdensity), "out-of-sample", pc1.fut.pls$bimod_struct_actual)
pc1.fut.fia$bimod_struct_f_8.5.act <- ifelse(is.na(pc1.fut.fia$bimod_struct_f_actual) & !is.na(pc1.fut.fia$FIAdensity), "out-of-sample", pc1.fut.fia$bimod_struct_f_actual)

pc1.fut.fia <- pc1.fut.fia[!duplicated(pc1.fut.fia[,c("x", "y", "pc1_bins_f", "bimod_struct_f","bimod_struct_f_8.5.act")]),]
pc1.fut.pls <- pc1.fut.pls[!duplicated(pc1.fut.pls[,c("x", "y", "pc1_bins", "bimod_struct", "bimod_struct_8.5.act")]),]

pc1.bimod.85.hist.pls.act <- ggplot(pc1.fut.pls[!pc1.fut.pls$bimod_struct_8.5.act %in% "out-of-sample" & !is.na(pc1.fut.pls$bimod_struct_8.5.act) ,], aes(pc1_mids, fill = bimod_struct_8.5.act))+geom_bar(width = 0.25)+scale_fill_manual(values = c("low-sample-unimodal"="grey","out-of-sample"="darkgrey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a", "NA" = "grey"), name = " ")+xlab("RCP 8.5 PC1")+xlim(-6.4, 4.5)#+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(-6.4, 4.5)+xlab("PLS sm")+ylim(0,1000)
pc1.bimod.85.hist.fia.act <- ggplot(pc1.fut.fia[! pc1.fut.fia$bimod_struct_f_8.5.act %in% "out-of-sample" & !is.na(pc1.fut.fia$bimod_struct_f_8.5.act) ,], aes(pc1_mids_f, fill = bimod_struct_f_8.5.act))+geom_bar(width = 0.25)+scale_fill_manual(values = c("low-sample-unimodal"="grey", "out-of-sample"="darkgrey","bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("RCP 8.5 PC1")+xlim(-6.4, 4.5)



# ---------------plot histograms for ppet:
ppet.df <- merge(kde.surf.ppet.df[,c("x", "y","bimclass_ppet", "bimclass_ppet_f","ppet_mids","ppet_bins","bimclass_ppet_f","ppet_bins_f" ,"ppet_mids_f")], bimod.ppet.85.pls, by = c("x", "y"), all.y = TRUE)
ppet.df <- merge(ppet.df, dens.clust[,c("x", "y", "foresttype_ordered")], by = c("x", "y"), all.x = TRUE)
ppet.df <- merge(ppet.df, species.clust[,c("x", "y", "foresttype")], by = c("x","y"), all.x = TRUE)


ppet.df$bimclass_ppet <- ifelse(is.na(ppet.df$bimclass_ppet), "low-sample-unimodal", as.character(ppet.df$bimclass_ppet))
ppet.df$bimclass_ppet_f <- ifelse(is.na(ppet.df$bimclass_ppet_f) & !is.na(ppet.df$FIAdensity), "low-sample-unimodal", as.character(ppet.df$bimclass_ppet_f))



ppet.bimod.pls <- ggplot(ppet.df[!is.na(ppet.df$PLSdensity),], aes(GS_ppet, fill = bimclass_ppet))+geom_histogram(position = "identity", bins = 35)+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(-170, 300)+xlab("PLS P-PET")+ylim(0,1200)
ppet.bimod.fia <- ggplot(ppet.df[!is.na(ppet.df$FIAdensity),], aes(GS_ppet_mod, fill = bimclass_ppet_f))+geom_histogram(position = "identity", bins = 35)+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(-170, 300)+xlab("FIA P-PET")+ylim(0,1200)


# plot structure colors on histograms for ppet
ppet.df$eco_fia <- ifelse(ppet.df$FIAdensity <= 0.5, "Prairie", 
                         ifelse(ppet.df$FIAdensity >= 47 & ppet.df$FIAdensity < 100, "Savanna", 
                                ifelse(ppet.df$FIAdensity >= 100, "Forest", NA)))

# find structure mode:
eco.mode.ppet.pls <- ppet.df[,c("eco", "ppet_mids", "ppet_bins")] %>% group_by(ppet_mids, ppet_bins) %>% dplyr::summarize(ecomode = names(which.max((table(eco)))))
eco.mode.ppet.fia <- ppet.df[!is.na(ppet.df$eco_fia),c("eco_fia", "ppet_mids_f", "ppet_bins_f")] %>% group_by(ppet_mids_f, ppet_bins_f)%>% dplyr::summarize(ecomode_f = names(which.max(table(eco_fia))))

ppet.df <- merge(ppet.df, eco.mode.ppet.pls, by = c("ppet_mids", "ppet_bins"))
ppet.df <- merge(ppet.df, eco.mode.ppet.fia, by = c("ppet_mids_f", "ppet_bins_f"), all.x = TRUE)


ppet.df$bimod_struct <- ifelse(ppet.df$bimclass_ppet %in% c("unimodal","low-sample-unimodal"), as.character(ppet.df$ecomode), ppet.df$bimclass_ppet)
ppet.df$bimod_struct_f <- ifelse(ppet.df$bimclass_ppet_f %in% c("unimodal","low-sample-unimodal"), as.character(ppet.df$ecomode_f), ppet.df$bimclass_ppet_f)

ppet.df$bimod_struct_actual <- ifelse(ppet.df$bimclass_ppet %in% c("unimodal","low-sample-unimodal"), as.character(ppet.df$eco), ppet.df$bimclass_ppet)
ppet.df$bimod_struct_f_actual <- ifelse(ppet.df$bimclass_ppet_f %in% c("unimodal","low-sample-unimodal"), as.character(ppet.df$eco_fia), ppet.df$bimclass_ppet_f)

# barplots with actual values for savanna vs. forest
ppet.bimod.pls.struct.act <- ggplot(ppet.df[!is.na(ppet.df$PLSdensity),], aes(ppet_mids, fill = bimod_struct_actual))+geom_bar(width = 10)+
  scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("PLS P-PET")+xlim(-170, 300)

ppet.bimod.fia.struct.act <- ggplot(ppet.df[!is.na(ppet.df$bimod_struct_f_actual),], aes(ppet_mids_f, fill = bimod_struct_f_actual))+geom_bar(width = 10)+
  scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("FIA P-PET")+xlim(-170, 300)

# barplots with mode values for savanna vs. forest
ppet.bimod.pls.struct <- ggplot(ppet.df[!is.na(ppet.df$PLSdensity),], aes(ppet_mids, fill = bimod_struct))+geom_bar(width = 10)+
  scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("PLS P-PET")+xlim(-170, 300)

ppet.bimod.fia.struct <- ggplot(ppet.df[!is.na(ppet.df$bimod_struct_f),], aes(ppet_mids_f, fill = bimod_struct_f))+geom_bar(width = 10)+
  scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("FIA P-PET")+xlim(-170, 300)

# color unimodal places by forest composition:
ppet.df$bimod_comps <- ifelse(ppet.df$bimclass_ppet %in% c("unimodal", "low-sample-unimodal"), as.character(ppet.df$foresttype_ordered), ppet.df$bimclass_ppet)
ppet.df$bimod_comps_f <- ifelse(ppet.df$bimclass_ppet_f %in% c("unimodal", "low-sample-unimodal"), as.character(ppet.df$foresttype), ppet.df$bimclass_ppet_f)

ppet.bimod.pls.comp <- ggplot(ppet.df[!is.na(ppet.df$bimod_comps),], aes(GS_ppet,fill = bimod_comps))+geom_histogram(position = "stack", binwidth = 10)+scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Oak-Hickory"='#beaed4', "Oak"='#386cb0',"N. Mixed Forest"='#ffff99',"Beech-Maple"='#bf5b17',"Aspen"='#f0027f',"Pine"='#fdc086', "Boreal/Sub-boreal"='#7fc97f'), name = " ")+xlim(-170, 300)#+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(-6.4, 4.5)+xlab("PLS ppet")+ylim(0,1000)
ppet.bimod.fia.comp <- ggplot(ppet.df[!is.na(ppet.df$bimod_comps_f),], aes(GS_ppet_mod, fill = bimod_comps_f))+geom_histogram(position = "stack", binwidth = 10)+scale_fill_manual(values = c("Pine"='#fdc086', "Mixed Hardwoods"='#003c30',"Oak/Maple"='#a6cee3',"Poplar"='#f0027f', "bimodal"='#d73027', "low-sample-unimodal"="grey"), name = " ")+xlab("FIA P-PET")+xlim(-170, 300)


ppet.pval.pls <- ggplot(bimodal.pls.ppet, aes(ppet_mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "red", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.05, linetype = "dashed")+xlab("PLS P-PET")+ylab("P value")+xlim(-170, 300)
ppet.pval.fia <- ggplot(bimodal.fia.ppet, aes(ppet_mids_f, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "red", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.05)+xlab("FIA P-PET")+ylab("P value")+xlim(-170, 300)


ppet.dip.pls <- ggplot(bimodal.pls.ppet, aes(ppet_mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS P-PET")+ylab("DIP value")+xlim(-170, 300)
ppet.dip.fia <- ggplot(bimodal.fia.ppet, aes(ppet_mids_f, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02)+xlab("FIA P-PET")+ylab("DIP value")+xlim(-170, 300)

# future hists for ppet:
# future soil predictions:
ppet.fut.fia <- merge(unique(ppet.df[,c("ppet_mids_f", "ppet_bins_f", "bimod_struct_f", "bimod_struct_f_actual","bimclass_ppet_f")]), bimod.ppet.85.fia, by.x = c("ppet_bins_f"), by.y = c("ppet_bins_f_8.5") )

ppet.fut.pls <- merge(unique(ppet.df[,c("ppet_mids", "ppet_bins", "bimod_struct","bimod_struct_actual","bimclass_ppet")]), bimod.ppet.85.pls,by.x= "ppet_bins", by.y= "ppet_bins_8.5")

# plot future with actual classes from past
ppet.fut.pls$bimod_struct_8.5.act <- ifelse(is.na(ppet.fut.pls$bimod_struct_actual) & !is.na(ppet.fut.pls$PLSdensity), "out-of-sample", ppet.fut.pls$bimod_struct_actual)
ppet.fut.fia$bimod_struct_f_8.5.act <- ifelse(is.na(ppet.fut.fia$bimod_struct_f_actual) & !is.na(ppet.fut.fia$FIAdensity), "out-of-sample", ppet.fut.fia$bimod_struct_f_actual)

ppet.fut.fia <- ppet.fut.fia[!duplicated(ppet.fut.fia[,c("x", "y", "ppet_bins_f", "bimod_struct_f","bimod_struct_f_8.5.act")]),]
ppet.fut.pls <- ppet.fut.pls[!duplicated(ppet.fut.pls[,c("x", "y", "ppet_bins", "bimod_struct", "bimod_struct_8.5.act")]),]

ppet.bimod.85.hist.pls.act <- ggplot(ppet.fut.pls[! ppet.fut.pls$bimod_struct_8.5.act %in% "out-of-sample" & !is.na(ppet.fut.pls$bimod_struct_8.5.act) ,], aes(ppet_mids, fill = bimod_struct_8.5.act))+geom_bar(width = 10)+scale_fill_manual(values = c("low-sample-unimodal"="grey","out-of-sample"="darkgrey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a", "NA" = "grey"), name = " ")+xlab("RCP 8.5 P-PET")+xlim(-170, 300)#+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(-6.4, 4.5)+xlab("PLS sm")+ylim(0,1000)
ppet.bimod.85.hist.fia.act <- ggplot(ppet.fut.fia[! ppet.fut.fia$bimod_struct_f_8.5.act %in% "out-of-sample" & !is.na(ppet.fut.fia$bimod_struct_f_8.5.act) ,], aes(ppet_mids_f, fill = bimod_struct_f_8.5.act))+geom_bar(width = 10)+scale_fill_manual(values = c("low-sample-unimodal"="grey", "out-of-sample"="darkgrey","bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("RCP 8.5 P-PET")+xlim(-170, 300)


ppet.bimod.85.hist.pls <- ggplot(ppet.fut.pls[!is.na(ppet.fut.pls$bimod_struct),], aes(ppet_mids, fill = bimod_struct))+geom_bar(width = 10)+scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("RCP 8.5 P-PET")+xlim(-170, 300)#+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(-6.4, 4.5)+xlab("PLS sm")+ylim(0,1000)
ppet.bimod.85.hist.fia <- ggplot(ppet.fut.fia[!is.na(ppet.fut.fia$bimod_struct_f),], aes(ppet_mids_f, fill = bimod_struct_f))+geom_bar(width = 10)+scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("RCP 8.5 P-PET")+xlim(-170, 300)


# plot histograms for soil moisuture:
sm.df <- merge(kde.surf.soilm.df[,c("x", "y","bimclass_soil", "bimclass_soil_f","soil_mids","soil_bins","bimclass_soil_f","soil_bins_f" ,"soil_mids_f")], bimod.sm.85.pls, by = c("x", "y"), all = TRUE)
sm.df <- merge(sm.df, dens.clust[,c("x", "y", "foresttype_ordered")], by = c("x", "y"), all.x = TRUE)
sm.df <- merge(sm.df, species.clust[,c("x", "y", "foresttype")], by = c("x","y"), all.x = TRUE)

sm.df$bimclass_soil <- ifelse(is.na(sm.df$bimclass_soil), "low-sample-unimodal", as.character(sm.df$bimclass_soil))
sm.df$bimclass_soil_f <- ifelse(is.na(sm.df$bimclass_soil_f) & ! is.na(sm.df$FIAdensity), "low-sample-unimodal", as.character(sm.df$bimclass_soil_f))



sm.bimod.pls <- ggplot(sm.df[!is.na(sm.df$PLSdensity),], aes(mean_GS_soil, fill = bimclass_soil))+geom_histogram(position = "identity", binwidth = 0.05)+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(0,1.75)+xlab("PLS soil moisture")+ylim(0,800)+xlim(0,1.75)
sm.bimod.fia <- ggplot(sm.df[!is.na(sm.df$FIAdensity),], aes(mean_GS_soil_m, fill = bimclass_soil_f))+geom_histogram(position = "identity", binwidth = 0.05)+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(0,1.75)+xlab("FIA soil moisture")+ylim(0,800)+xlim(0,1.75)


# plot structure colors on histograms for ppet
sm.df$eco_fia <- ifelse(sm.df$FIAdensity <= 0.5, "Prairie", 
                          ifelse(sm.df$FIAdensity >= 47 & sm.df$FIAdensity < 100, "Savanna", 
                                 ifelse(sm.df$FIAdensity >= 100, "Forest", NA)))

sm.df$eco <- ifelse(sm.df$PLSdensity <= 0.5, "Prairie", 
                        ifelse(sm.df$PLSdensity >= 47 & sm.df$PLSdensity < 100, "Savanna", 
                               ifelse(sm.df$PLSdensity >= 100, "Forest", NA)))

# find structure mode:
eco.mode.soil.pls <- sm.df[,c("eco", "soil_mids", "soil_bins")] %>% group_by(soil_mids,soil_bins) %>% dplyr::summarize(ecomode = names(which.max((table(eco)))))
eco.mode.soil.fia <- sm.df[!is.na(sm.df$eco_fia),c("eco_fia", "soil_mids_f", "soil_bins_f")] %>% group_by(soil_mids_f, soil_bins_f)%>% dplyr::summarize(ecomode_f = names(which.max(table(eco_fia))))

sm.df <- merge(sm.df, eco.mode.soil.pls, by = c("soil_mids", "soil_bins"), all.x = TRUE)
sm.df <- merge(sm.df, eco.mode.soil.fia, by = c("soil_mids_f", "soil_bins_f"), all.x = TRUE)

sm.df$bimod_struct_actual <- ifelse(sm.df$bimclass_soil %in% c("unimodal","low-sample-unimodal"), as.character(sm.df$eco), sm.df$bimclass_soil)
sm.df$bimod_struct_f_actual <- ifelse(sm.df$bimclass_soil_f %in% c("unimodal","low-sample-unimodal"), as.character(sm.df$eco_fia), sm.df$bimclass_soil_f)

# barplots with actual values for savanna vs. forest
sm.bimod.pls.struct.act <- ggplot(sm.df[!is.na(sm.df$bimod_struct_actual),], aes(soil_mids, fill = bimod_struct_actual))+geom_bar(width = 0.025)+
  scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("PLS soil moisture")+xlim(0,1.75)

sm.bimod.fia.struct.act <- ggplot(sm.df[!is.na(sm.df$bimod_struct_f),], aes(soil_mids_f, fill = bimod_struct_f_actual))+geom_bar(width = 0.025)+
  scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("FIA soil moisture")+xlim(0,1.75)



sm.df$bimod_struct <- ifelse(sm.df$bimclass_soil %in% c("unimodal", "low-sample-unimodal"), as.character(sm.df$ecomode), sm.df$bimclass_soil)
sm.df$bimod_struct_f <- ifelse(sm.df$bimclass_soil_f %in% c("unimodal","low-sample-unimodal"), as.character(sm.df$ecomode_f), sm.df$bimclass_soil_f)

sm.bimod.pls.struct <- ggplot(sm.df[!is.na(sm.df$PLSdensity),], aes(soil_mids, fill = bimod_struct))+geom_bar()+
  scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("PLS soil moisture")+xlim(0,1.75)

sm.bimod.fia.struct <- ggplot(sm.df[!is.na(sm.df$bimod_struct_f),], aes(soil_mids_f, fill = bimod_struct_f))+geom_bar()+
  scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("FIA soil moisture")+xlim(0,1.75)



sm.pval.pls <- ggplot(bimodal.pls.soil, aes(soil_mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "red", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.05, linetype = "dashed")+xlab("PLS soil moisture")+ylab("P value")+xlim(0,1.75)
sm.pval.fia <- ggplot(bimodal.fia.sm, aes(soil_mids_f, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "red", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.05)+xlab("FIA soil moisture")+ylab("P value")+xlim(0,1.75)


sm.dip.pls <- ggplot(bimodal.pls.soil, aes(soil_mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS soil moisture")+ylab("DIP value")+xlim(0,1.75)
sm.dip.fia <- ggplot(bimodal.fia.sm, aes(soil_mids_f, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02)+xlab("FIA soil moisture")+ylab("DIP value")+xlim(0,1.75)


# color unimodal places by forest composition:
sm.df$bimod_comps <- ifelse(sm.df$bimclass_soil %in% c("unimodal","low-sample-unimodal"), as.character(sm.df$foresttype_ordered), sm.df$bimclass_soil)
sm.df$bimod_comps_f <- ifelse(sm.df$bimclass_soil_f %in% c("unimodal","low-sample-unimodal"), as.character(sm.df$foresttype), sm.df$bimclass_soil_f)

soil.bimod.pls.comp <- ggplot(sm.df[!is.na(sm.df$bimod_comps),], aes(mean_GS_soil,fill = bimod_comps))+geom_histogram(position = "stack", binwidth = 0.05)+scale_fill_manual(values = c("low-sample-unimodal"="grey", "bimodal"='#d73027', "Oak-Hickory"='#beaed4', "Oak"='#386cb0',"N. Mixed Forest"='#ffff99',"Beech-Maple"='#bf5b17',"Aspen"='#f0027f',"Pine"='#fdc086', "Boreal/Sub-boreal"='#7fc97f'), name = " ")+xlim(0,1.75)+xlab("PLS Soil Moisture")#+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(-6.4, 4.5)+xlab("PLS sm")+ylim(0,1000)
soil.bimod.fia.comp <- ggplot(sm.df[!is.na(sm.df$bimod_comps_f),], aes(mean_GS_soil_m, fill = bimod_comps_f))+geom_histogram(position = "stack", binwidth = 0.05)+scale_fill_manual(values = c("Pine"='#fdc086', "Mixed Hardwoods"='#003c30',"Oak/Maple"='#a6cee3',"Poplar"='#f0027f', "bimodal"='#d73027', "low-sample-unimodal"="grey"), name = " ")+xlab("FIA Soil Moisture")+xlim(0,1.75)


# future soil predictions:
sm.fut.fia <- merge(unique(sm.df[,c("soil_mids_f", "soil_bins_f", "bimod_struct_f", "bimod_struct_f_actual", "bimclass_soil_f")]), bimod.sm.85.fia, by.x = c("soil_bins_f", "soil_mids_f"), by.y=c("soil_bins_f_8.5", "soil_mids_f_8.5"), all.y = TRUE)
sm.fut.pls <- merge(unique(sm.df[,c("soil_mids", "soil_bins", "bimod_struct","bimod_struct_actual", "bimclass_soil")]), bimod.sm.85.pls,by.x= c("soil_bins", "soil_mids"), by.y= c("soil_bins_8.5", "soil_mids_8.5"), all.y =TRUE)

# plot future with actual classes from past
sm.fut.pls$bimod_struct_8.5.act <- ifelse(is.na(sm.fut.pls$bimod_struct_actual), "out-of-sample", sm.fut.pls$bimod_struct_actual)
sm.fut.fia$bimod_struct_f_8.5.act <- ifelse(is.na(sm.fut.fia$bimod_struct_f_actual), "out-of-sample", sm.fut.fia$bimod_struct_f_actual)

sm.bimod.85.hist.pls.act <- ggplot(sm.fut.pls[!sm.fut.pls$bimod_struct_8.5.act %in% "out-of-sample",], aes(soil_mids, fill = bimod_struct_8.5.act))+geom_bar(width = 0.025)+scale_fill_manual(values = c("low-sample-unimodal"="grey","out-of-sample"="darkgrey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a", "NA" = "grey"), name = " ")+xlim(0,1.75)+xlab("RCP 8.5 Soil Moisture")#+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(-6.4, 4.5)+xlab("PLS sm")+ylim(0,1000)
sm.bimod.85.hist.fia.act <- ggplot(sm.fut.fia[!sm.fut.fia$bimod_struct_f_8.5.act %in% "out-of-sample",], aes(soil_mids_f, fill = bimod_struct_f_8.5.act))+geom_bar(width = 0.025)+scale_fill_manual(values = c("low-sample-unimodal"="grey", "out-of-sample"="darkgrey","bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("RCP 8.5 Soil Moisture")+xlim(0,1.75)


# plot with mode of classes from teh past
sm.fut.pls$bimod_struct_8.5 <- ifelse(is.na(sm.fut.pls$bimod_struct), "out-of-sample", sm.fut.pls$bimod_struct)
sm.fut.fia$bimod_struct_f_8.5 <- ifelse(is.na(sm.fut.fia$bimod_struct_f), "out-of-sample", sm.fut.fia$bimod_struct_f)

sm.bimod.85.hist.pls <- ggplot(sm.fut.pls, aes(soil_mids, fill = bimod_struct_8.5))+geom_bar()+scale_fill_manual(values = c("low-sample-unimodal"="grey","out-of-sample"="darkgrey", "bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a", "NA" = "grey"), name = " ")+xlim(0,1.75)+xlab("RCP 8.5 Soil Moisture")#+scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "low-sample-unimodal"="grey"), name = "")+xlim(-6.4, 4.5)+xlab("PLS sm")+ylim(0,1000)
sm.bimod.85.hist.fia <- ggplot(sm.fut.fia, aes(soil_mids_f, fill = bimod_struct_f_8.5))+geom_bar()+scale_fill_manual(values = c("low-sample-unimodal"="grey", "out-of-sample"="darkgrey","bimodal"='#d73027', "Forest" = "#01665e", "Prairie" = "#f6e8c3", "Savanna" = "#8c510a"), name = " ")+xlab("RCP 8.5 Soil Moisture")+xlim(0,1.75)



# output all plots to files:
png(height = 4, width = 10, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_hists_pc1.png")
plot_grid(
  pc.bimod.pls + ggtitle("PLS"),
  pc.bimod.fia + ggtitle("FIA"),
  pc.bimod.85.hist + ggtitle("RCP-8.5")+ylim(0,1000),
  pc.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,1000), ncol = 2, align = "v"
)
dev.off()

png(height = 4, width = 10, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_hists_ppet.png")
plot_grid(
  ppet.bimod.pls+ ggtitle("PLS")+ylim(0,4100),
  ppet.bimod.fia+ ggtitle("FIA")+ylim(0,4100),
  ppet.bimod.85.hist.pls+ ggtitle("RCP-8.5")+ylim(0,4100),
  ppet.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,4100), ncol = 2, align = "v"
)
dev.off()

png(height = 4, width = 10, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_hists_sm.png")
plot_grid(
  sm.bimod.pls+ ggtitle("PLS")+ylim(0,2000),
  sm.bimod.fia+ ggtitle("FIA")+ylim(0,2000),
  sm.bimod.85.hist.pls+ ggtitle("RCP-8.5")+ylim(0,2000),
  sm.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,2000), ncol = 2, align = "v"
)
dev.off()

# plot out the histograms for bimodal vs. unimodal forest/unimodal forest:

png(height = 4, width = 10, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_hists_pc1_structure.png")
plot_grid(
  pc.bimod.pls.struct + ggtitle("PLS")+ylim(0,1000),
  pc.bimod.fia.struct + ggtitle("FIA")+ylim(0,1000),
  pc.bimod.85.hist + ggtitle("RCP-8.5")+ylim(0,1000),
  pc.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,1000), ncol = 2, align = "v"
)
dev.off()

png(height = 4, width = 10, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_hists_ppet_struct.png")
plot_grid(
  ppet.bimod.pls.struct+ ggtitle("PLS")+ylim(0,1000),
  ppet.bimod.fia.struct+ ggtitle("FIA")+ylim(0,1000),
  ppet.bimod.85.hist.pls+ ggtitle("RCP-8.5")+ylim(0,4100),
  ppet.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,4100), ncol = 2, align = "v"
)
dev.off()

png(height = 4, width = 10, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_hists_sm_struct.png")
plot_grid(
  sm.bimod.pls.struct+ xlab("PLS soil moisture")+ggtitle("PLS")+ylim(0,900),
  sm.bimod.fia.struct+ xlab("FIA soil moisture")+ ggtitle("FIA")+ylim(0,900),
  sm.bimod.85.hist.pls+ ggtitle("RCP-8.5")+ylim(0,2000),
  sm.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,2000), ncol = 2, align = "v"
)
dev.off()


# plot out the histograms for bimodal vs. unimodal forest/unimodal forest:

png(height = 4, width = 10, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_hists_pc1_comp.png")
plot_grid(
  pc.bimod.pls.comp + ggtitle("PLS")+ylim(0,1000),
  pc.bimod.fia.comp + ggtitle("FIA")+ylim(0,1000),
  pc.bimod.85.hist + ggtitle("RCP-8.5")+ylim(0,1000),
  pc.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,1000), ncol = 2, align = "v"
)
dev.off()

png(height = 4, width = 10, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_hists_ppet_comp.png")
plot_grid(
  ppet.bimod.pls.comp+ ggtitle("PLS")+ylim(0,1200),
  ppet.bimod.fia.comp+ ggtitle("FIA")+ylim(0,1200),
  ppet.bimod.85.hist.pls+ ggtitle("RCP-8.5")+ylim(0,4100),
  ppet.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,4100), ncol = 2, align = "v"
)
dev.off()

# plot out histograms for composition:
png(height = 4, width = 10, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_hists_sm_comp.png")
plot_grid(
  soil.bimod.pls.comp+ xlab("PLS soil moisture")+ggtitle("PLS")+ylim(0,900),
  soil.bimod.fia.comp+ xlab("FIA soil moisture")+ ggtitle("FIA")+ylim(0,900),
  sm.bimod.85.hist.pls+ ggtitle("RCP-8.5")+ylim(0,2000),
  sm.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,2000), ncol = 2, align = "v"
)
dev.off()

# plot all stuct and comp plots combined:

png(height = 6, width = 10, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_hists_pc1_all.png")
plot_grid(
  pc.bimod.pls.struct + ggtitle("PLS")+ylim(0,1000),
  pc.bimod.fia.struct + ggtitle("FIA")+ylim(0,1000),
  pc.bimod.pls.comp + ggtitle("PLS")+ylim(0,1000),
  pc.bimod.fia.comp + ggtitle("FIA")+ylim(0,1000),
  pc.bimod.85.hist + ggtitle("RCP-8.5")+ylim(0,1000),
  pc.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,1000), ncol = 2, align = "v"
)
dev.off()

png(height = 6, width = 10, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_hists_ppet_all.png")
plot_grid(
  ppet.bimod.pls.struct+ ggtitle("PLS")+ylim(0,1200),
  ppet.bimod.fia.struct+ ggtitle("FIA")+ylim(0,1200),
  ppet.bimod.pls.comp+ ggtitle("PLS")+ylim(0,1200),
  ppet.bimod.fia.comp+ ggtitle("FIA")+ylim(0,1200),
  ppet.bimod.85.hist.pls+ ggtitle("RCP-8.5")+ylim(0,4100),
  ppet.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,4100), ncol = 2, align = "v"
)
dev.off()

# plot out histograms for composition:
png(height = 6, width = 10, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_hists_sm_all.png")
plot_grid(
  sm.bimod.pls.struct+ xlab("PLS soil moisture")+ggtitle("PLS")+ylim(0,900),
  sm.bimod.fia.struct+ xlab("FIA soil moisture")+ ggtitle("FIA")+ylim(0,900),
  soil.bimod.pls.comp+ xlab("PLS soil moisture")+ggtitle("PLS")+ylim(0,900),
  soil.bimod.fia.comp+ xlab("FIA soil moisture")+ ggtitle("FIA")+ylim(0,900),
  sm.bimod.85.hist.pls+ ggtitle("RCP-8.5")+ylim(0,2000),
  sm.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,2000), ncol = 2, align = "v"
)
dev.off()

# structure only but with bimodal stats below:
png(height = 6, width = 10, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_hists_pc1_all.png")
plot_grid(
  pc.bimod.pls.struct + ggtitle("PLS")+ylim(0,1000),
  pc.bimod.fia.struct + ggtitle("FIA")+ylim(0,1000),
  pc.bimod.pls.comp + ggtitle("PLS")+ylim(0,1000),
  pc.bimod.fia.comp + ggtitle("FIA")+ylim(0,1000),
  pc.bimod.85.hist + ggtitle("RCP-8.5")+ylim(0,1000),
  pc.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,1000), ncol = 2, align = "v"
)
dev.off()

png(height = 6, width = 10, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_hists_ppet_all.png")
plot_grid(
  ppet.bimod.pls.struct+ ggtitle("PLS")+ylim(0,1200),
  ppet.bimod.fia.struct+ ggtitle("FIA")+ylim(0,1200),
  ppet.bimod.pls.comp+ ggtitle("PLS")+ylim(0,1200),
  ppet.bimod.fia.comp+ ggtitle("FIA")+ylim(0,1200),
  ppet.bimod.85.hist.pls+ ggtitle("RCP-8.5")+ylim(0,4100),
  ppet.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,4100), ncol = 2, align = "v"
)
dev.off()

# plot out new barplots and the pvalue and dips

png(height = 8, width = 12, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_bars_and_dips_pc1_struct.png")
plot_grid(
  pc.bimod.pls.struct+ xlab("PLS PC1")+ggtitle("PLS")+ylim(0,900),
  pc.bimod.fia.struct+ xlab("FIA PC1")+ ggtitle("FIA")+ylim(0,900),
  pc1.dip.pls+ xlab("PLS PC1")+ylim(0,0.1),
  pc1.dip.fia+ xlab("FIA PC1")+ylim(0,0.1),
  pc1.pval.pls+ xlab("PLS PC1")+ylim(0,1),
  pc1.pval.fia+ xlab("FIA PC1")+ylim(0,1),
  pc1.bimod.85.hist.pls+ ggtitle("RCP-8.5")+ylim(0,1000),
  pc1.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,1000), ncol = 2, align = "v", axis = "lr"
)
dev.off()

png(height = 8, width = 12, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_bars_and_dips_ppet_struct.png")
plot_grid(
  ppet.bimod.pls.struct+ xlab("PLS P-PET")+ggtitle("PLS")+ylim(0,1500),
  ppet.bimod.fia.struct+ xlab("FIA P-PET")+ ggtitle("FIA")+ylim(0,1500),
  ppet.dip.pls+ xlab("PLS P-PET")+ylim(0,0.1),
  ppet.dip.fia+ xlab("FIA P-PET")+ylim(0,0.1),
  ppet.pval.pls+ xlab("PLS P-PET")+ylim(0,1),
  ppet.pval.fia+ xlab("FIA P-PET")+ylim(0,1),
  ppet.bimod.85.hist.pls+ ggtitle("RCP-8.5")+ylim(0,4100),
  ppet.bimod.85.hist.fia+ ggtitle("RCP-8.5")+ylim(0,4100), ncol = 2, align = "v", axis = "lr"
)
dev.off()

png(height = 8, width = 12, units = "in", res = 300,"outputs/paper_figs/out_of_sample_climate_bars_and_dips_sm_struct.png")
plot_grid(
  sm.bimod.pls.struct+ xlab("PLS soil moisture")+ggtitle("PLS")+ylim(0,900),
  sm.bimod.fia.struct+ xlab("FIA soil moisture")+ ggtitle("FIA")+ylim(0,900),
  sm.dip.pls+ylim(0,0.1)+theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()),
  sm.dip.fia+ylim(0,0.1)+theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()),
  sm.pval.pls+ylim(0,1)+theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()),
  sm.pval.fia+ylim(0,1)+theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()),
  sm.bimod.85.hist.pls + ggtitle("RCP-8.5")+ylim(0,3000),
  sm.bimod.85.hist.fia + ggtitle("RCP-8.5")+ylim(0,2000), ncol = 2, align = "v", axis = "lr",  rel_heights = c(1,0.5,0.5, 1)
)
dev.off()


# plot with actual biome types as colors for barplots:
png(height = 8, width = 12, units = "in", res = 300,"outputs/paper_figs/fig_3_out_of_sample_climate_bars_struct_actual.png")

pls.title <- ggdraw() + 
  draw_label("Past Environmental Space",
             fontface = 'bold')

fia.title <- ggdraw() + 
  draw_label("Modern Environmental Space",
             fontface = 'bold')


pls.plts <-  plot_grid(
  pc1.bimod.pls.struct.act+ xlab("PLS PC1")+ylim(0,900) + theme(legend.position = "none"),
  sm.bimod.pls.struct.act+ xlab("PLS soil moisture")+ylim(0,900)+ theme(legend.position = "none"),
  ppet.bimod.pls.struct.act+ xlab("PLS P-PET")+ylim(0,1500)+ theme(legend.position = "none"),
  
  
  pc1.bimod.85.hist.pls.act+ylim(0,1000)+ theme(legend.position = "none"),
  sm.bimod.85.hist.pls.act +ylim(0,3000)+ theme(legend.position = "none"),
  ppet.bimod.85.hist.pls.act+ylim(0,4100)+ theme(legend.position = "none"),ncol = 3, align = "v", axis = "lr"
)

fia.plts<- plot_grid(
  pc1.bimod.fia.struct.act+ xlab("FIA PC1")+ylim(0,900)+ theme(legend.position = "none"),
  sm.bimod.fia.struct.act+ xlab("FIA soil moisture")+ylim(0,900)+ theme(legend.position = "none"),
  ppet.bimod.fia.struct.act+ xlab("FIA P-PET")+ylim(0,1500)+ theme(legend.position = "none"),
  
  pc1.bimod.85.hist.fia.act+ylim(0,1000)+ theme(legend.position = "none"),
  sm.bimod.85.hist.fia.act +ylim(0,2000)+ theme(legend.position = "none"),
  ppet.bimod.85.hist.fia.act+ylim(0,4100)+ theme(legend.position = "none"), ncol = 3, align = "v", axis = "lr")

legend <- get_legend(pc1.bimod.85.hist.pls.act)
plot_grid(
  plot_grid(fia.title, fia.plts, pls.title, pls.plts, ncol = 1, rel_heights = c(0.1, 1, 0.1, 1), labels = "AUTO"), # rel_heights values control title margins
  plot_grid(legend, ncol=1),rel_widths=c(1, 0.2))


dev.off()

# updating figure 3: futures with mode:
png(height = 8, width = 12, units = "in", res = 300,"outputs/paper_figs/fig_3_out_of_sample_climate_bars_struct.png")

    pls.title <- ggdraw() + 
      draw_label("Past Environmental Space",
                 fontface = 'bold')
    
    fia.title <- ggdraw() + 
      draw_label("Modern Environmental Space",
                 fontface = 'bold')
    
      
     pls.plts <-  plot_grid(
      pc.bimod.pls.struct+ xlab("PLS PC1")+ylim(0,900) + theme(legend.position = "none"),
    sm.bimod.pls.struct+ xlab("PLS soil moisture")+ylim(0,900)+ theme(legend.position = "none"),
    ppet.bimod.pls.struct+ xlab("PLS P-PET")+ylim(0,1500)+ theme(legend.position = "none"),
    
    
    pc1.bimod.85.hist.pls+ylim(0,1000)+ theme(legend.position = "none"),
    sm.bimod.85.hist.pls +ylim(0,3000)+ theme(legend.position = "none"),
    ppet.bimod.85.hist.pls+ylim(0,4100)+ theme(legend.position = "none"),ncol = 3, align = "v", axis = "lr"
    )
    
    fia.plts<- plot_grid(
    pc.bimod.fia.struct+ xlab("FIA PC1")+ylim(0,900)+ theme(legend.position = "none"),
    sm.bimod.fia.struct+ xlab("FIA soil moisture")+ylim(0,900)+ theme(legend.position = "none"),
    ppet.bimod.fia.struct+ xlab("FIA P-PET")+ylim(0,1500)+ theme(legend.position = "none"),
    
    pc1.bimod.85.hist.fia+ylim(0,1000)+ theme(legend.position = "none"),
    sm.bimod.85.hist.fia +ylim(0,2000)+ theme(legend.position = "none"),
    ppet.bimod.85.hist.fia+ylim(0,4100)+ theme(legend.position = "none"), ncol = 3, align = "v", axis = "lr")
    
    plot_grid(
    plot_grid(fia.title, fia.plts, pls.title, pls.plts, ncol = 1, rel_heights = c(0.1, 1, 0.1, 1), labels = "AUTO"), # rel_heights values control title margins
    plot_grid(legend, ncol=1),rel_widths=c(1, 0.2))


dev.off()





# >>>>>>>>>>>>>>>>>>>>>>>>>>>> Future predictions MAPS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# now make the 3 colored maps for the future--need to deal with out of sample points for sm and ppet

# now merge all of these together to make a map of 1, 2, 3, bimodal metrics:
bim.class.f <- merge(pc1.fut.pls[,c("x", "y", "bimclass")],sm.fut.pls[,c("x", "y", "bimclass_soil")], by = c("x", "y"), all = TRUE)
colnames(bim.class.f)[4] <- "bimclass_f_pred_pls_85_sm"
bim.class.f <- merge(bim.class.f, ppet.fut.pls[,c("x", "y", "bimclass_ppet")])

bim.class.f$nbimod <- as.character(rowSums(bim.class.f[,3:5] == "bimodal", na.rm = TRUE))

three.color.bimodal.plots.f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bim.class.f, aes(x=x, y=y, fill = nbimod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red"
  ), labels = c("0","1", "2", "3", "0")) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


one.bimpct <- round(length(bim.class.f[bim.class.f$nbimod %in% "1",]$nbimod)/length(bim.class.f$nbimod)*100, digits = 2)
two.bimpct <- round(length(bim.class.f[bim.class.f$nbimod %in% "2",]$nbimod)/length(bim.class.f$nbimod)*100, digits = 2)
three.bimpct <- round(length(bim.class.f[bim.class.f$nbimod %in% "3",]$nbimod)/length(bim.class.f$nbimod)*100, digits = 2)

one.bimpct <- one.bimpct + two.bimpct + three.bimpct
two.bimpct <-  two.bimpct + three.bimpct
three.bimpct <-  three.bimpct

# make 3 bimodal plots for future predicted by fia:

bim.class.f.m <- merge(pc1.fut.fia[,c("x", "y", "bimclass_f")],sm.fut.fia[,c("x", "y", "bimclass_soil_f")], by = c("x", "y"), all = TRUE)
colnames(bim.class.f)[4] <- "bimcalss_f_pred_fia_85_sm"
bim.class.f.m <- merge(bim.class.f.m, ppet.fut.fia[,c("x", "y", "bimclass_ppet_f")])

bim.class.f.m$nbimod <- as.character(rowSums(bim.class.f.m[,3:5] == "bimodal", na.rm = TRUE))

three.color.bimodal.plots.f.m <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bim.class.f.m, aes(x=x, y=y, fill = nbimod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red"
  ), labels = c("0","1", "2", "3", "0")) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


# >>>>>>>>>>for pc1 predicted by past:
bimod.pc1.85.pls <- read.csv("outputs/new_bim_surface_PC1_future_8.5_pred_by_pls_0.1_mode_crit_1000.csv")
bimod.pc1.85.pls$eco <- ifelse(bimod.pc1.85.pls$PLSdensity <= 0.5, "Prairie", 
                               ifelse(bimod.pc1.85.pls$PLSdensity <= 47, "Savanna", "Forest"))
#bimod.pc1.85.pls$bimclass_f_pred_pls_85 <- ifelse(bimod.pc1.85.pls$dipPint_f_pred_pls_pc1 <= 0.05 , "bimodal", "unimodal")
bimod.pc1.85.pls$bimclass_f_pred_pls_85 <- ifelse(is.na(bimod.pc1.85.pls$bimclass_f_pred_pls_85), "out-of-sample", as.character(bimod.pc1.85.pls$bimclass_f_pred_pls_85))

bimod.pc1.85.pls$bimclass_eco <- ifelse(is.na(bimod.pc1.85.pls$bimclass_f_pred_pls_85 ) | is.na(bimod.pc1.85.pls$eco), NA ,paste(bimod.pc1.85.pls$bimclass_f_pred_pls_85 , bimod.pc1.85.pls$eco))

bimod.pc1.85.pls.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc1.85.pls, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


pc1.bimpct.f <- round(length(bimod.pc1.85.pls[bimod.pc1.85.pls$bimclass_f_pred_pls_85%in% "bimodal",]$bimclass_f_pred_pls_85)/length(bimod.pc1.85.pls$bimclass_f_pred_pls_85)*100, digits = 2)

bimod.pc1.85.pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc1.85.pls, aes(x=x, y=y, fill =bimclass_f_pred_pls_85))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample"="tan"
  ), labels = c("bimodal", "unimodal")) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.5,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", pc1.bimpct.f, "%"))+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "A", size = 4)



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>> for pc1 predicted by modern: <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bimod.pc1.85.fia <- read.csv("outputs/new_bim_surface_PC1_future_8.5_pred_by_fia_1000_mode_crit.csv")
bimod.pc1.85.fia$eco <- ifelse(bimod.pc1.85.fia$FIAdensity <= 0.5, "Prairie", 
                               ifelse(bimod.pc1.85.fia$FIAdensity <= 47, "Savanna", "Forest"))
bimod.pc1.85.fia$bimclass_f_pred_fia_85 <- ifelse(bimod.pc1.85.fia$dipPint_f_pred_fia_85 <= 0.05 , "bimodal", "unimodal")
bimod.pc1.85.fia$bimclass_f_pred_fia_85 <- ifelse(is.na(bimod.pc1.85.fia$bimclass_f_pred_fia_85), "out-of-sample", as.character(bimod.pc1.85.fia$bimclass_f_pred_fia_85))

bimod.pc1.85.fia$bimclass_eco <- ifelse(is.na(bimod.pc1.85.fia$bimclass_f_pred_fia_85) | is.na(bimod.pc1.85.fia$eco), NA ,paste(bimod.pc1.85.fia$bimclass_f_pred_fia_85, bimod.pc1.85.fia$eco))

bimod.pc1.85.fia.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc1.85.fia, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.5,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "B", size = 4)


pc1.bimpct.f <- round(length(bimod.pc1.85.fia[bimod.pc1.85.fia$bimclass_f_pred_fia_85 %in% "bimodal",]$bimclass_f_pred_fia_85)/length(bimod.pc1.85.fia$bimclass_f_pred_fia_85)*100, digits = 2)

bimod.pc1.85.fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc1.85.fia, aes(x=x, y=y, fill = bimclass_f_pred_fia_85))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal"= '#d73027', "unimodal"='#4575b4', "out-of-sample"="tan")) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.5,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", pc1.bimpct.f, "%"))+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "B", size = 4)





# >>>>>>>>>>>>>>>>>>>>>> for PPET predicted by past: <<<<<<<<<<<<<<<<<<<<<<<<<
bimod.ppet.85.pls <- read.csv("outputs/new_bim_surface_PPET_rcp85_pred_by_pls_1000_mode_crit.csv")
bimod.ppet.85.pls$eco <- ifelse(bimod.ppet.85.pls$PLSdensity <= 0.5, "Prairie", 
                                ifelse(bimod.ppet.85.pls$PLSdensity <= 47, "Savanna", "Forest"))

bimod.ppet.85.pls$bimclass_f_pred_pls_ppet <- ifelse(bimod.ppet.85.pls$dipPint_f_pred_pls_ppet <= 0.05 , "bimodal", "unimodal")
bimod.ppet.85.pls$bimclass_f_pred_pls_ppet_spl <- ifelse(is.na(bimod.ppet.85.pls$bimclass_f_pred_pls_ppet), "out-of-sample", as.character(bimod.ppet.85.pls$bimclass_f_pred_pls_ppet))


bimod.ppet.85.pls$bimclass_eco <- ifelse(is.na(bimod.ppet.85.pls$bimclass_f_pred_pls_ppet ) | is.na(bimod.ppet.85.pls$eco), NA ,paste(bimod.ppet.85.pls$bimclass_f_pred_pls_ppet , bimod.ppet.85.pls$eco))

bimod.ppet.85.pls.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.85.pls, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


ppet.bimpct.f <- round(length(bimod.ppet.85.pls[bimod.ppet.85.pls$bimclass_f_pred_pls_ppet %in% "bimodal",]$bimclass_f_pred_pls_ppet)/length(bimod.ppet.85.pls$bimclass_f_pred_pls_ppet)*100, digits = 2)

bimod.ppet.85.pls$insample <- ifelse( bimod.ppet.85.pls$mean_ppet_GS_8.5 >= range(bimod.ppet.85.pls$GS_ppet)[1] & bimod.ppet.85.pls$mean_ppet_GS_8.5 <=  range(bimod.ppet.85.pls$GS_ppet)[2], "in-sample", "out-of-sampl")

bimod.ppet.85.pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.85.pls, aes(x=x, y=y, fill = bimclass_f_pred_pls_ppet))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample" = "tan"), drop = F)+
  
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.5,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", ppet.bimpct.f, "%"))+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "C", size = 4)



# >>>>>>>>>>>>>>>>>>>>>>>> for PPET predicted by modern: <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bimod.ppet.85.fia <- read.csv("outputs/new_bim_surface_PPET_rcp85_pred_by_fia_1000_mode_crit.csv")
#bimod.ppet.85.fia$bimclass_f_pred_fia_85_ppet <- ifelse(is.na(bimod.ppet.85.fia$bimclass_f_pred_fia_85_ppet), "out-of-sample", bimod.ppet.85.fia$bimclass_f_pred_fia_85_ppet)
bimod.ppet.85.fia$bimclass_f_pred_fia_85_ppet <- ifelse(is.na(bimod.ppet.85.fia$bimclass_f_pred_fia_85_ppet), "out-of-sample", as.character(bimod.ppet.85.fia$bimclass_f_pred_fia_85_ppet))

bimod.ppet.85.fia$eco <- ifelse(bimod.ppet.85.fia$FIAdensity <= 0.5, "Prairie", 
                                ifelse(bimod.ppet.85.fia$FIAdensity <= 47, "Savanna", "Forest"))
#bimod.ppet.85.fia$bimclass_f_pred_fia_ppet <- ifelse(bimod.ppet.85.fia$dipPint_f_pred_pls_ppet <= 0.05 , "bimodal", "unimodal")

bimod.ppet.85.fia$bimclass_eco <- ifelse(is.na(bimod.ppet.85.fia$bimclass_f_pred_fia_ppet) | is.na(bimod.ppet.85.fia$eco), NA ,paste(bimod.ppet.85.fia$bimclass_f_pred_fia_ppet, bimod.ppet.85.fia$eco))
bimod.ppet.85.fia.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.85.fia, aes(x=x, y=y, fill = dipPint_f_pred_fia_ppet))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


ppet.bimpct.f <- round(length(bimod.ppet.85.fia[bimod.ppet.85.fia$bimclass_f_pred_fia_ppet %in% "bimodal",]$bimclass_f_pred_fia_ppet)/length(bimod.ppet.85.fia$bimclass_f_pred_fia_ppet)*100, digits = 2)

bimod.ppet.85.fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.85.fia, aes(x=x, y=y, fill = bimclass_f_pred_fia_85_ppet))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample" = "tan"))+
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", ppet.bimpct.f, "%"))




# >>>>>>>>>>>>>>>>>>>>>>>>>> for soil moisture predicted by past: <<<<<<<<<<<<<<<<<<<<<<<<<<
bimod.sm.85.pls <- read.csv("outputs/new_bim_surface_soil_m_rcp85_pred_by_pls.csv")
bimod.sm.85.pls$eco <- ifelse(bimod.sm.85.pls$PLSdensity <= 0.5, "Prairie", 
                              ifelse(bimod.sm.85.pls$PLSdensity <= 47, "Savanna", "Forest"))
bimod.sm.85.pls$bimclass_eco <- ifelse(is.na(bimod.sm.85.pls$bimclass_f_pred_pls_85_ppet) | is.na(bimod.sm.85.pls$eco), NA ,paste(bimod.sm.85.pls$bimclass_f_pred_pls_85_ppet, bimod.sm.85.pls$eco))

bimod.sm.85.pls.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.sm.85.pls, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


sm.bimpct.f <- round(length(bimod.sm.85.pls[bimod.sm.85.pls$bimclass_f_pred_fia_85_soil %in% "bimodal",]$bimclass_f_pred_fia_85_soil)/length(bimod.sm.85.pls$bimclass_f_pred_fia_85_soil)*100, digits = 2)

bimod.sm.85.pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.sm.85.pls, aes(x=x, y=y, fill = bimclass_f_pred_pls_85_soil))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample"="tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", sm.bimpct.f, "%"))



# >>>>>>>>>>>>>>>>>>>>>>  for soil moisture predicted by modern: <<<<<<<<<<<<<<<<<<<<<<<<<<<
bimod.sm.85.fia <- read.csv("outputs/new_bim_surface_soil_m_rcp85_pred_by_fia.csv")
bimod.sm.85.fia$eco <- ifelse(bimod.sm.85.fia$FIAdensity <= 0.5, "Prairie", 
                              ifelse(bimod.sm.85.fia$FIAdensity <= 47, "Savanna", "Forest"))
bimod.sm.85.fia$bimclass_eco <- ifelse(is.na(bimod.sm.85.fia$bimclass_f_pred_fia_85_soil) | is.na(bimod.sm.85.fia$eco), NA ,paste(bimod.sm.85.fia$bimclass_f_pred_fia_85_soil, bimod.sm.85.fia$eco))

bimod.sm.85.fia.eco.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.sm.85.fia, aes(x=x, y=y, fill = bimclass_eco))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title=" ")+ scale_fill_manual(values= c('#01665e','#d8b365','#8c510a',
                                                                          '#c7eae5',
                                                                          '#f6e8c3',
                                                                          '#5ab4ac'
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


sm.bimpct.f <- round(length(bimod.sm.85.fia[bimod.sm.85.fia$bimclass_f_pred_fia_85_soil %in% "bimodal",]$bimclass_f_pred_fia_85_soil)/length(bimod.sm.85.fia$bimclass_f_pred_fia_85_soil)*100, digits = 2)

bimod.sm.85.fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.sm.85.fia, aes(x=x, y=y, fill = bimclass_f_pred_fia_85_soil))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal"='#4575b4', "out-of-sample"="tan")) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", sm.bimpct.f, "%"))



# >>>>>>>>>>>>>>>>>>>>>>>>>>>> Future predictions Figures <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# now make the 3 colored maps for the future--need to deal with out of sample points for sm and ppet

# now merge all of these together to make a map of 1, 2, 3, bimodal metrics:
bim.class.f <- merge(bimod.pc1.85.pls[,c("x", "y", "bimclass_f_pred_pls_85")], bimod.sm.85.pls[,c("x", "y", "bimclass_f_pred_pls_85_soil")], by = c("x", "y"), all = TRUE)
colnames(bim.class.f)[4] <- "bimcalss_f_pred_pls_85_sm"
bim.class.f <- merge(bim.class.f, bimod.ppet.85.pls[,c("x", "y", "bimclass_f_pred_pls_ppet")])

bim.class.f$nbimod <- as.character(rowSums(bim.class.f[,3:5] == "bimodal", na.rm = TRUE))
bim.class.f$nbimod <- factor(x = bim.class.f$nbimod, levels = c("0", "1", "2","3", "No data"))


three.color.bimodal.plots.f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bim.class.f, aes(x=x, y=y, fill = nbimod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red","darkgrey"
  ), labels = c("0","1", "2", "3", "No data"), drop = F) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.23),legend.background = element_rect(fill=alpha('transparent', 0 )),legend.key = element_rect(colour = 'black',  size = 0.5, linetype='solid'),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "A", size = 4)



one.bimpct <- round(length(bim.class.f[bim.class.f$nbimod %in% "1",]$nbimod)/length(bim.class.f$nbimod)*100, digits = 2)
two.bimpct <- round(length(bim.class.f[bim.class.f$nbimod %in% "2",]$nbimod)/length(bim.class.f$nbimod)*100, digits = 2)
three.bimpct <- round(length(bim.class.f[bim.class.f$nbimod %in% "3",]$nbimod)/length(bim.class.f$nbimod)*100, digits = 2)

one.bimpct <- one.bimpct + two.bimpct + three.bimpct
two.bimpct <-  two.bimpct + three.bimpct
three.bimpct <-  three.bimpct

# make 3 bimodal plots for future predicted by fia:

bim.class.f.m <- merge(bimod.pc1.85.fia[,c("x", "y", "bimclass_f_pred_fia_85")],bimod.sm.85.fia[,c("x", "y", "bimclass_f_pred_fia_85_soil")], by = c("x", "y"), all = TRUE)
colnames(bim.class.f)[4] <- "bimcalss_f_pred_fia_85_sm"
bim.class.f.m <- merge(bim.class.f.m, bimod.ppet.85.fia[,c("x", "y", "bimclass_f_pred_fia_85_ppet")])

bim.class.f.m$nbimod <- as.character(rowSums(bim.class.f.m[,3:5] == "bimodal", na.rm = TRUE))
bim.class.f.m$nbimod <- factor(x = bim.class.f.m$nbimod, levels = c("0", "1", "2","3", "No data"))


three.color.bimodal.plots.f.m <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bim.class.f.m, aes(x=x, y=y, fill = nbimod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red","darkgrey"
  ), labels = c("0","1", "2", "3", "No data"), drop = F) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.23),legend.background = element_rect(fill=alpha('transparent', 0 )),legend.key = element_rect(colour = 'black',  size = 0.5, linetype='solid'),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "B", size = 4)




one.bimpct <- round(length(bim.class.f.m[bim.class.f.m$nbimod %in% "1",]$nbimod)/length(bim.class.f.m$nbimod)*100, digits = 2)
two.bimpct <- round(length(bim.class.f.m[bim.class.f.m$nbimod %in% "2",]$nbimod)/length(bim.class.f.m$nbimod)*100, digits = 2)
three.bimpct <- round(length(bim.class.f.m[bim.class.f.m$nbimod %in% "3",]$nbimod)/length(bim.class.f.m$nbimod)*100, digits = 2)

one.bimpct <- one.bimpct + two.bimpct + three.bimpct
two.bimpct <-  two.bimpct + three.bimpct
three.bimpct <-  three.bimpct


# write to png
png(height = 4, width = 6, units = "in", res = 300, "outputs/paper_figs/future_rcp8.5_3bimodal_maps.png")
plot_grid(three.color.bimodal.plots.f, three.color.bimodal.plots.f.m)
dev.off()



png(height = 10, width = 6, units = "in", res = 300, "outputs/paper_figs/future_rcp8.5_3bimodal_maps_all.png")
plot_grid(bimod.pc1.85.pls.map+ggtitle(" "),bimod.pc1.85.fia.map+ggtitle(" "),
          bimod.ppet.85.pls.map+ggtitle(" "),bimod.ppet.85.fia.map+ggtitle(" "), 
          bimod.sm.85.pls.map+ggtitle(" "),bimod.sm.85.fia.map+ggtitle(" "), ncol = 2, labels = c("A", "B", "C", "D", "E", "F"))
dev.off()

png(height = 10, width = 6, units = "in", res = 300, "outputs/paper_figs/past_modern_bimodal_maps_all.png")
plot_grid(bimod.pc.pls.map+ggtitle(" "),bimod.pc.fia.map+ggtitle(" "),
          bimod.ppet.pls.map+ggtitle(" "),bimod.ppet.fia.map+ggtitle(" "), 
          bimod.sm.pls.map+ggtitle(" "),bimod.sm.fia.map+ggtitle(" "), ncol = 2, labels = c("A", "B", "C", "D", "E", "F"))
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
                                                                                                                                                                                                                                                                     
       