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
dens.pr <- read.csv("/Users/kellyheilman/bimodality/data/PLS_FIA_density_
                    /data/PLS_FIA_density_climate_full.csv")

ggplot(dens.pr, aes(PLSdensity, FIAdensity))+geom_point()+ylim(0,500)+xlim(0,500)+geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey")


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
                                                                                                                                                                                                                                                                     
       
