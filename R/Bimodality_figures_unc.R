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
library(ks)
library(ggplotify)
library(dplyr)

# read in old PLS density + climate data:
dens.pr <- read.csv("data/PLS_FIA_density_climate_full.csv")

# read in draws of total PLS density:
total.m <- read.csv("data/extracted_total_PLS_density_draws.csv")

dens.summary <- total.m %>% group_by(x, y) %>% dplyr::summarize(mean_dens = mean(value, na.rm=TRUE),
                                                                media_dens = median(value, na.rm=TRUE),
                                                         ci.low_dens = quantile(value, 0.025, na.rm=TRUE), 
                                                         ci.high_dens = quantile(value, 0.975, na.rm=TRUE))


dens.width.pls <- total.m %>% dplyr::summarize(mean_dens = mean(value, na.rm=TRUE),
                                                                media_dens = median(value, na.rm=TRUE),
                                                                ci.low_dens = quantile(value, 0.025, na.rm=TRUE), 
                                                                ci.high_dens = quantile(value, 0.975, na.rm=TRUE))


ggplot(dens.summary, aes(x,y, fill =  media_dens))+geom_raster()+ scale_fill_distiller(palette = "Spectral")

dens <- merge(dens.pr, dens.summary, by = c("x", "y"), all.y = TRUE)

dens <- dens[!is.na(dens$mean_dens), ]

ggplot(dens[dens$mean_dens <= 0.5,], aes(x,y, fill =  mean_dens))+geom_raster()+ scale_fill_distiller(palette = "Spectral")
write.csv(dens, "outputs/density_full_unc.csv", row.names = FALSE)
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
  geom_raster(data=dens, aes(x=x, y=y, fill = mean_dens))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,775), name ="Tree \n Density", na.value = 'darkgrey') +
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                                legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                
                                                axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


png("/Users/kah/Documents/bimodality/outputs/paper_figs_unc/PLS_density_map_full.png")
pls.map
dev.off()



dens$density_discrete <- ifelse(dens$mean_dens <= 0.5, "Prairie", 
                                   ifelse(dens$mean_dens <= 47, "Savanna",
                                          ifelse(dens$mean_dens > 47 & dens$mean_dens <= 100, "47-100",
                                                 ifelse(dens$mean_dens > 100 & dens$mean_dens <= 200, "100-200", 
                                                        ifelse(dens$mean_dens > 200 & dens$mean_dens <= 300, "200-300", 
                                                               ifelse(dens$mean_dens > 300 & dens$mean_dens <= 400, "300-400",
                                                                      ifelse(dens$mean_dens > 400 & dens$mean_dens <= 500, "400-500",
                                                                             ifelse(dens$mean_dens > 500 , "500+", "No data"))))))))

dens$density_discrete <- factor(dens$density_discrete, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500",  "500+", "No data"))




pls.map.alt.color <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens, aes(x=x, y=y, fill = density_discrete))+
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



pls.map.alt.color.msk <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens[!is.na(dens$FIAdensity),], aes(x=x, y=y, fill = density_discrete))+
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




png(height = 4, width = 3, units = "in", res = 300,"/Users/kah/Documents/bimodality/outputs/paper_figs_unc/PLS_smooth_density_map_full_alt_colors.png")
pls.map.alt.color+ggtitle("PLS mean density smoothed")
dev.off()

# plot median density

dens$density_discrete_median <- ifelse(dens$media_dens <= 0.5, "Prairie", 
                                ifelse(dens$media_dens <= 47, "Savanna",
                                       ifelse(dens$media_dens > 47 & dens$media_dens <= 100, "47-100",
                                              ifelse(dens$media_dens > 100 & dens$media_dens <= 200, "100-200", 
                                                     ifelse(dens$media_dens > 200 & dens$media_dens <= 300, "200-300", 
                                                            ifelse(dens$media_dens > 300 & dens$media_dens <= 400, "300-400",
                                                                   ifelse(dens$media_dens > 400 & dens$media_dens <= 500, "400-500",
                                                                          ifelse(dens$media_dens > 500 , "500+", "No data"))))))))

dens$density_discrete_median<- factor(dens$density_discrete_median, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500",  "500+", "No data"))

dens$density_discrete_low <- ifelse(dens$ci.low_dens <= 0.5, "Prairie", 
                                       ifelse(dens$ci.low_dens <= 47, "Savanna",
                                              ifelse(dens$ci.low_dens > 47 & dens$ci.low_dens <= 100, "47-100",
                                                     ifelse(dens$ci.low_dens > 100 & dens$ci.low_dens <= 200, "100-200", 
                                                            ifelse(dens$ci.low_dens > 200 & dens$ci.low_dens <= 300, "200-300", 
                                                                   ifelse(dens$ci.low_dens > 300 & dens$ci.low_dens <= 400, "300-400",
                                                                          ifelse(dens$ci.low_dens > 400 & dens$ci.low_dens <= 500, "400-500",
                                                                                 ifelse(dens$ci.low_dens > 500 , "500+", "No data"))))))))

dens$density_discrete_low <- factor(dens$density_discrete_low, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500",  "500+", "No data"))




pls.map.alt.color.low <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens, aes(x=x, y=y, fill =density_discrete_low))+
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


png(height = 4, width = 3, units = "in", res = 300,"/Users/kah/Documents/bimodality/outputs/paper_figs_unc/PLS_smooth_density_map_full_alt_colors.png")
pls.map.alt.color+ggtitle("PLS mean density smoothed")
dev.off()


ggplot()+geom_errorbar(data= dens, aes(ymin = ci.low_dens, ymax = ci.high_dens), color = "grey")+
  geom_point(data = dens, aes(PLSdensity, mean_dens), size = 0.2)#+geom_abline(intercept = 0, slope = 1, color = "red")+theme_bw()


pred.old.plot <- ggplot(dens, aes(PLSdensity, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(PLSdensity, mean_dens), color = "black", size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+theme_bw()+ylab("Smoothed Density")+xlab("Previous grid cell density")

png(height = 5, width = 5, units = "in", res = 200, "outputs/chris_estimates_vs_previous_estimates.png")
pred.old.plot
dev.off()



pc1_unc <- ggplot(dens, aes(PC1, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(PC1, mean_dens), color = "black", size = 0.05)+theme_bw()+ylab("PLS Density (trees/ha)")+xlab("PC1")

ppet_unc <- ggplot(dens, aes(GS_ppet, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(GS_ppet, mean_dens), color = "black", size = 0.05)+theme_bw()+ylab("PLS Density (trees/ha)")+xlab("growing season P-PET")


soil_unc <- ggplot(dens, aes(mean_GS_soil, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(mean_GS_soil, mean_dens), color = "black", size = 0.05)+theme_bw()+ylab("PLS Density (trees/ha)")+xlab("Growing season soil moisture")


# save plots of environment vs density estimates + CI:
png(height = 10, width = 4, units = "in", res = 300, "outputs/paper_figs_unc/smooth_dens_vs_envts.png")
cowplot::plot_grid(pc1_unc, ppet_unc, soil_unc, ncol = 1)
dev.off()

ggplot()+geom_histogram(data = dens, aes(mean_dens), fill = "red")+geom_histogram(data= dens, aes(ci.high_dens), aes = 2, color = "grey")+geom_histogram(data= dens, aes(ci.low_dens), alpha = 0.2, fill = "blue")

# ------------------------------ figure 1C map of pls species clusters with smoothed 8 clusters ------------------------------------

clust_plot8 <- read.csv("outputs/eight_clust_pls_dissimilarity_stat_smooth.dens.csv")

# merge the clusters and pls density data: 
clust_8 <- merge(clust_plot8, dens, by = c("x", "y"))

# need to rename the clusters here (should go back and do it in the place where we originally make the clusters):
clust_8
library(plyr)
clust_8$foresttype <- revalue(clust_8$speciescluster, c("Oak/Poplar/Ash"="Aspen", "Oak/Maple/Elm/Ash"="Elm/Oak/Maple", "Oak" = "Oak", 
                                                       "Hemlock/Beech/Cedar/Birch/Maple" = "N. Mixed Forest", "Oak/Hickory" = "Oak-Hickory",
                                                       "Pine/Poplar" = "Pine", "Spruce/Cedar/Tamarack/Poplar" = "Boreal/Sub-boreal", "Beech/Maple/Hemlock" = "Beech-Maple"))

clust_8$orderedforesttype<- factor(clust_8$foresttype, c("Oak", "Pine", "Aspen", "N. Mixed Forest", "Boreal/Sub-boreal","Elm/Oak/Maple", "Oak-Hickory", "Beech-Maple"))

pls.clust <- ggplot(clust_8, aes(x = x, y=y, fill=orderedforesttype))+geom_raster()+
  scale_fill_manual(values = c('#386cb0', '#f0028f','#fdc088','#ffff99','#8fc98f','#beaed4','#33a02c', '#bf5b18'), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.20, 0.18),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                                           axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),legend.key = element_rect(color = "black", linetype = "solid"),axis.text.x=element_blank(),
                                                                                                                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                           axis.title.x=element_blank(),
                                                                                                                           axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()

pls.clust
# merge clust_plot8 and dens.pr

pls.clust.msk <- ggplot(clust_8[!is.na(clust_8$FIAdensity),], aes(x = x, y=y, fill=orderedforesttype))+geom_raster()+
  scale_fill_manual(values = c('#386cb0', '#f0028f','#fdc088','#ffff99','#8fc98f','#beaed4','#33a02c', '#bf5b18'), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.20, 0.18),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                                           axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),legend.key = element_rect(color = "black", linetype = "solid"),axis.text.x=element_blank(),
                                                                                                                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                           axis.title.x=element_blank(),
                                                                                                                           axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()

pls.clust.msk

# ----------------------- Figure 1D total PLS density histgram colored by species composition-----------------
dens.clust <- merge(dens, clust_8[,c("x" ,"y", "speciescluster", "foresttype")], by = c("x", "y"), all.x = TRUE)
dens.clust$foresttype_ordered <- factor(dens.clust$foresttype, levels = rev(c("Boreal/Sub-boreal", "Pine", "Aspen",  "Elm/Oak/Maple","Oak-Hickory","Beech-Maple","N. Mixed Forest", "Oak")))
dens.clust.omit <- dens.clust[ !is.na(dens.clust$foresttype_ordered),]
dens.clust <- dens.clust[!duplicated(dens.clust),]

myColors <- c( '#386cb0','#ffff99','#bf5b18','#beaed4','#33a02c','#fdc088','#f0028f','#8fc98f')
names(myColors) <- levels(dens.clust$foresttype_ordered)

clust.hist.full <- ggplot()+ geom_density(data = dens.clust[dens.clust$mean_dens >= 0.5,], aes(mean_dens, 22 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_histogram(data = dens.clust[dens.clust$mean_dens >= 0.5,], aes(mean_dens, fill = foresttype_ordered), binwidth =  20)+xlim(0,600)+
  scale_fill_manual(values = myColors, name = " ", drop = TRUE)+coord_flip()+ylim(0,1050)+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.44, 0.85),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.4, "line"),legend.key = element_rect(color = "black", linetype ="solid"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
clust.hist.full


clust.hist.full.no.aspect <- ggplot()+ geom_density(data = dens.clust[dens.clust$mean_dens >= 0.5,], aes(mean_dens, 22 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_histogram(data = dens.clust[dens.clust$mean_dens >= 0.5,], aes(mean_dens, fill = foresttype_ordered), binwidth =  20)+xlim(0,600)+
  scale_fill_manual(values = compColors, name = " ", drop = TRUE)+coord_flip()+ylim(0,1050)+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(legend.position = c(0.5, 0.85),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.2, "line"),legend.key = element_rect(color = "black", linetype ="solid"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

clust.hist.full.msk <- ggplot()+ geom_density(data = dens.clust[dens.clust$mean_dens >= 0.5 & !is.na(dens.clust$FIAdensity),], aes(mean_dens, 22 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_histogram(data = dens.clust[dens.clust$mean_dens >= 0.5 & !is.na(dens.clust$FIAdensity),], aes(mean_dens, fill = foresttype_ordered), binwidth =  20)+xlim(0,600)+
  scale_fill_manual(values = myColors, name = " ", drop = TRUE)+coord_flip()+ylim(0,1050)+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.44, 0.85),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.4, "line"),legend.key = element_rect(color = "black", linetype ="solid"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
clust.hist.full.msk

# ------------------make the same figures but with combined pls and fia clusters-----------------
# ------------------------------ figure 1C map of pls species clusters with smoothed 8 clusters ------------------------------------

clust_plot10 <- read.csv("outputs/ten_clust_combined_dissimilarity_stat_smooth.dens.csv")
clust_plot_pls <- clust_plot10[clust_plot10$period %in% "PLS",]
# merge the clusters and pls density data: 
clust_10 <- merge(clust_plot_pls, dens, by = c("x", "y"))

# need to rename the clusters here (should go back and do it in the place where we originally make the clusters):
head(clust_10)
library(plyr)
clust_10$foresttype <- revalue(clust_10$speciescluster, c("Poplar/Oak-FIA"="Aspen", 
                                                          "Oak/Maple/Ash/Poplar-FIA"="Oak/Maple/Ash", 
                                                          "Oak-PLS" = "Oak", 
                                                          "Hemlock/Cedar/Maple-PLS" = "N. Mixed Forest", 
                                                          "Oak/Hickory/Elm/Maple-FIA" = "Oak-Hickory",
                                                          "Maple/Poplar/Oak/Ash-FIA" = "Pine", 
                                                          "Pine/Poplar/Tamarack/Fir-PLS" = "Boreal/Sub-boreal", 
                                                          "Beech/Maple/Pine-PLS" = "Beech-Maple",
                                                          "Maple/Cedar/Poplar-FIA" = "Maple Mixed Forest",
                                                          "Oak/Maple/Other/Hickory-FIA" = "Oak-Mixed"))

clust_10$orderedforesttype <- factor(clust_10$foresttype, c("Oak", "Pine", "Aspen", "N. Mixed Forest", "Boreal/Sub-boreal","Oak/Maple/Ash", "Oak-Hickory", "Beech-Maple", "Maple Mixed Forest", "Oak-Mixed"))

# create a stable coloring scheme:
compColors <- c('#386cb0',"#f0027f",'#ff7f00',"#ffff99","#7fc97f", "#beaed4",'#a6cee3',"#b15928",  "#004529",  '#fdc086')
names(compColors) <- levels(clust_10$orderedforesttype)


pls.clust.both <- ggplot(clust_10, aes(x = x, y=y, fill=orderedforesttype))+geom_raster()+
  scale_fill_manual(values = compColors, name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 10)+ theme(legend.position=c(0.20, 0.24),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                                            axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),legend.key = element_rect(color = "black", linetype = "solid"),axis.text.x=element_blank(),
                                                                                                                            axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                            axis.title.x=element_blank(),
                                                                                                                            axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()

pls.clust.both

# merge clust_plot10 and dens.pr

pls.clust.both.msk <- ggplot(clust_10[!is.na(clust_10$FIAdensity),], aes(x = x, y=y, fill=orderedforesttype))+geom_raster()+
  scale_fill_manual(values = compColors, name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 10)+ theme(legend.position=c(0.20, 0.24),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                                           axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),legend.key = element_rect(color = "black", linetype = "solid"),axis.text.x=element_blank(),
                                                                                                                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                           axis.title.x=element_blank(),
                                                                                                                           axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()

pls.clust.both.msk

# ----------------------- Figure 1D total PLS density histgram colored by species composition-----------------
dens.clust <- merge(dens, clust_10[,c("x" ,"y", "speciescluster", "foresttype")], by = c("x", "y"), all.x = TRUE)
dens.clust$foresttype_ordered <- factor(dens.clust$foresttype, levels = rev(c("Aspen", "Maple Mixed Forest", "Oak-Mixed","Oak/Maple/Ash","Oak-Hickory","Pine","Oak",  "Boreal/Sub-boreal", "N. Mixed Forest",  "Beech-Maple")))
dens.clust.omit <- dens.clust[ !is.na(dens.clust$foresttype_ordered),]
dens.clust <- dens.clust[!duplicated(dens.clust),]

#myColors <- rev(c('#386cb0',"#f0027f",'#ff7f00',"#ffff99","#7fc97f", "#beaed4",'#a6cee3',"#b15928", '#fdc086',  "#004529"))
#names(myColors) <- levels(dens.clust$foresttype_ordered)

clust.hist.full.both <- ggplot()+ geom_density(data = dens.clust[dens.clust$mean_dens >= 0.5,], aes(mean_dens, 22 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_histogram(data = dens.clust[dens.clust$mean_dens >= 0.5,], aes(mean_dens, fill = foresttype_ordered), binwidth =  20)+xlim(0,600)+
  scale_fill_manual(values = compColors, name = " ", drop = TRUE)+coord_flip()+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 10)+theme(aspect.ratio = 1,legend.position = c(0.44, 0.105),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.4, "line"),legend.key = element_rect(color = "black", linetype ="solid"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
clust.hist.full.both


clust.hist.full.both.no.aspect <- ggplot()+ geom_density(data = dens.clust[dens.clust$mean_dens >= 0.5,], aes(mean_dens, 22 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_histogram(data = dens.clust[dens.clust$mean_dens >= 0.5,], aes(mean_dens, fill = foresttype_ordered), binwidth =  20)+xlim(0,600)+
  scale_fill_manual(values = compColors, name = " ", drop = TRUE)+coord_flip()+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 6)+theme(legend.position = c(0.54, 0.82),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.095, "line"),legend.key = element_rect(color = "black", linetype ="solid"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

clust.hist.full.both.no.aspect.msk <- ggplot()+ geom_density(data = dens.clust[dens.clust$mean_dens >= 0.5 & !is.na(dens.clust$FIAdensity),], aes(mean_dens, 22 *..count..),linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_histogram(data = dens.clust[dens.clust$mean_dens >= 0.5 & !is.na(dens.clust$FIAdensity),], aes(mean_dens, fill = foresttype_ordered), binwidth =  20)+xlim(0,600)+
  scale_fill_manual(values = compColors, name = " ", drop = TRUE)+coord_flip()+ylim(0,2050)+xlab("PLS tree density")+ylab("# grid cells")+theme_bw(base_size = 6)+theme(legend.position = c(0.54, 0.82),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.095, "line"),legend.key = element_rect(color = "black", linetype ="solid"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
clust.hist.full.both.no.aspect.msk

# --------------------------------Figure 1 FIA maps & Hists ---------------------------------------
# first lets pull in the full species clustesrs

# <<<<<<<<<<<<<<< map bimodality evaluated on samples from  esimates: >>>>>>>>>>>>>>>>>>>>>>>>>>>>

# map bimodal based on PC1:
out.df <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_PC1_stat.rds")
pvalues <- out.df  %>% group_by(pc1_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                              median.p = median(pvalue, na.rm = TRUE),
                                                              ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                              ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                              mean.d = mean(dip, na.rm = TRUE),
                                                              median.d = median(dip, na.rm = TRUE),
                                                              ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                              ci.high.d = quantile(dip, 0.975, na.rm = TRUE))


dens$pc1_bins <- cut(dens$PC1, breaks=seq(-5.5, 4.5, by = 0.25))

ordered.cuts <- data.frame(pc1_bins = levels(cut(dens[order(dens$PC1),]$PC1, breaks=seq(-5.5, 4.5, by = 0.25))),
                           mids = seq(-5.375, 4.5, by = 0.25))
pvalues <- left_join(ordered.cuts, pvalues, by = "pc1_bins")
bimod.pc.pls <- left_join(dens, pvalues, by = c("pc1_bins"))

# if there are too few grid cells (<50), then we won't evaluature the bimodality

bin.counts <- bimod.pc.pls %>% group_by(pc1_bins) %>% dplyr::summarise(ncells_pc1 = length(mean.p))

bimod.pc.pls <- merge(bimod.pc.pls, bin.counts, by = "pc1_bins")

# merge with the envt + pc data
bimod.pc.pls$bimclass_lowsamp <- ifelse(bimod.pc.pls$ncells_pc1 <= 50, "low-sample", "okay")
bimod.pc.pls$bimclass <- ifelse(bimod.pc.pls$mean.p <= 0.05 & bimod.pc.pls$bimclass_lowsamp %in% "okay", "bimodal", "unimodal")



# merge with the envt + pc data

bimod.pc.pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc.pls, aes(x=x, y=y, fill = bimclass))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal"='#d73027',"unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")

pvalues <- merge(pvalues, bin.counts, by = "pc1_bins")
pvalues$lowsamp <- ifelse(pvalues$ncells_pc1 <= 50, "low-sample", "okay")
write.csv(pvalues,"outputs/n_cells_in_pc1_pls_bins.csv", row.names = FALSE)# save bin.counts to use in predicting the future:


pc1.dip.pls <- ggplot(pvalues[ pvalues$lowsamp %in% "okay",], aes(mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS PC1")+ylab("DIP value")+xlim(-6.4, 4.5)
pc1.pval.pls <- ggplot(pvalues[ pvalues$lowsamp %in% "okay",], aes(mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS PC1")+ylab("P value")+xlim(-6.4, 4.5)


png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/pls_dip_pvalues_unc_ppet.png")
plot_grid(pc1.dip.pls, pc1.pval.pls)
dev.off()

# pased on P-PET:

out.df <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_PPET_stat.rds")
pvalues <- out.df  %>% group_by(ppet_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                               median.p = median(pvalue, na.rm = TRUE),
                                                               ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                               ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                               mean.d = mean(dip, na.rm = TRUE),
                                                               median.d = median(dip, na.rm = TRUE),
                                                               ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                               ci.high.d = quantile(dip, 0.975, na.rm = TRUE))


dens$ppet_bins <- cut(dens$GS_ppet, breaks=seq(-170, 205, by = 15))

ordered.cuts <- data.frame(ppet_bins = unique(cut(dens[order(dens$GS_ppet),]$GS_ppet, breaks=seq(-170, 205, by = 15))),
                           mids = seq(-167.5, 205, by = 15))
pvalues <- left_join(ordered.cuts, pvalues, by = "ppet_bins")

bimod.ppet.pls <- left_join(dens, pvalues, by = c("ppet_bins"))

bin.counts <- bimod.ppet.pls %>% group_by(ppet_bins) %>% dplyr::summarise(ncells_ppet = length(mean.p))
bimod.ppet.pls <- merge(bimod.ppet.pls, bin.counts, by = "ppet_bins")

# merge with the envt + ppet data
bimod.ppet.pls$bimclass_lowsamp <- ifelse(bimod.ppet.pls$ncells_ppet <= 50, "low-sample", "okay")
bimod.ppet.pls$bimclass_ppet <- ifelse(bimod.ppet.pls$mean.p <= 0.05 & bimod.ppet.pls$bimclass_lowsamp %in% "okay", "bimodal", "unimodal")



# merge with the envt + pc data

bimod.ppet.pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.pls, aes(x=x, y=y, fill = bimclass_ppet))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal" = '#d73027', "unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")


pvalues <- merge(pvalues, bin.counts, by = "ppet_bins")
pvalues$lowsamp <- ifelse(pvalues$ncells_ppet <= 50, "low-sample", "okay")

write.csv(pvalues,"outputs/n_cells_in_ppet_pls_bins.csv", row.names = FALSE)# save bin.counts to use in predicting the future:

# plot p values and dip values by bin
ppet.dip.pls <- ggplot(pvalues[ pvalues$lowsamp %in% "okay",], aes(mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS P-PET")+ylab("DIP value")
ppet.pval.pls <- ggplot(pvalues[ pvalues$lowsamp %in% "okay",], aes(mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS P-PET")+ylab("P value")

png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/pls_dip_pvalues_unc_ppet.png")
plot_grid(ppet.dip.pls, ppet.pval.pls)
dev.off()

# based on soil moisture estimates:
out.df <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_soil_15bins_kde_stat.rds")

pvalues <- out.df  %>% group_by(soil_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                median.p = median(pvalue, na.rm = TRUE),
                                                                ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                mean.d = mean(dip, na.rm = TRUE),
                                                                median.d = median(dip, na.rm = TRUE),
                                                                ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                ci.high.d = quantile(dip, 0.975, na.rm = TRUE))


dens$soil_bins <- cut(dens$mean_GS_soil, breaks=seq(0, 1.8, by = 0.05))

ordered.cuts <- data.frame(soil_bins = levels(unique(cut(dens[order(dens$mean_GS_soil),]$mean_GS_soil, breaks=seq(0, 1.8, by = 0.05)))),
                           mids = seq(0.025, 1.8, by = 0.05))
pvalues <- left_join(ordered.cuts, pvalues, by = "soil_bins")

bimod.sm.pls <- left_join(dens, pvalues, by = c("soil_bins"))


bin.counts <- bimod.sm.pls %>% group_by(soil_bins) %>% dplyr::summarise(ncells_soil = length(mean.p))
bimod.sm.pls <- merge(bimod.sm.pls, bin.counts, by = "soil_bins")

# merge with the envt + soil data
bimod.sm.pls$bimclass_lowsamp <- ifelse(bimod.sm.pls$ncells_soil <= 50, "low-sample", "okay")
bimod.sm.pls$bimclass_soil <- ifelse(bimod.sm.pls$mean.p <= 0.05 & bimod.sm.pls$bimclass_lowsamp %in% "okay", "bimodal", "unimodal")




bimod.sm.pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.sm.pls, aes(x=x, y=y, fill = bimclass_soil))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")


# plot out the p and dip values by bin class:
pvalues <- merge(pvalues, bin.counts, by = "soil_bins")
pvalues$lowsamp <- ifelse(pvalues$ncells_soil <= 50, "low-sample", "okay")

write.csv(pvalues,"outputs/n_cells_in_soil_pls_bins.csv", row.names = FALSE)# save bin.counts to use in predicting the future:

soil.dip.pls <- ggplot(pvalues[ pvalues$lowsamp %in% "okay",], aes(mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS Soil moisture")+ylab("DIP value")
soil.pval.pls <- ggplot(pvalues[ pvalues$lowsamp %in% "okay",], aes(mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS Soil moisture")+ylab("P value")


png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/pls_dip_pvalues_unc_ppet.png")
plot_grid(soil.dip.pls, soil.pval.pls)
dev.off()


png(height = 10, width = 10, units = "in", res = 300, "outputs/paper_figs_unc/bimodal_maps_with_pvalues.png")
plot_grid(bimod.pc.pls.map, bimod.ppet.pls.map, bimod.sm.pls.map,
          pc1_unc, ppet_unc, soil_unc,
          pc1.dip.pls + ylim(0,0.1), ppet.dip.pls+ylim(0,0.1), soil.dip.pls+ylim(0,0.1),
          pc1.pval.pls, ppet.pval.pls, soil.pval.pls, 
          ncol = 3, rel_heights = c(1,1,0.5, 0.5))
dev.off()



# -------------------------Read in FIA density statistical estimates-----------------------


total.m <- read.csv("data/extracted_total_FIA_density_draws.csv")

dens.summary.fia <- total.m %>% group_by(x, y) %>% dplyr::summarize(mean_dens_fia = mean(value, na.rm=TRUE),
                                                                media_dens_fia = median(value, na.rm=TRUE),
                                                                ci.low_dens_fia = quantile(value, 0.025, na.rm=TRUE), 
                                                                ci.high_dens_fia = quantile(value, 0.975, na.rm=TRUE))

dens.width.fia <- total.m %>% dplyr::summarize(mean_dens = mean(value, na.rm=TRUE),
                                               media_dens = median(value, na.rm=TRUE),
                                               ci.low_dens = quantile(value, 0.025, na.rm=TRUE), 
                                               ci.high_dens = quantile(value, 0.975, na.rm=TRUE))


dens.width.pls$width <- dens.width.pls$ci.high_dens - dens.width.pls$ci.low_dens
dens.width.fia$width <- dens.width.fia$ci.high_dens - dens.width.fia$ci.low_dens

dens.width.fia$width/dens.width.pls$width

dens.width.fia$width/dens.width.pls$width
ggplot(dens.summary.fia, aes(x,y, fill = mean_dens_fia))+geom_raster()+ scale_fill_distiller(palette = "Spectral")

dens <- merge(dens, dens.summary.fia, by = c("x", "y"), all.x = TRUE)

dens <- dens[!is.na(dens.full$mean_dens_fia), ]

ggplot(dens, aes(x,y, fill =  mean_dens_fia))+geom_raster()+ scale_fill_distiller(palette = "Spectral")
write.csv(dens, "outputs/density_full_FIA_PLS_unc.csv", row.names = FALSE)
# -------------------figure 1 A: Map of FIA density

# need to set up state outlines:
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota', 'wisconsin', 'michigan', "illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)



# fia and FIA plots
sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


FIA.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens, aes(x=x, y=y, fill = mean_dens_fia))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,775), name ="Tree \n Density", na.value = 'darkgrey') +
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


png("/Users/kah/Documents/bimodality/outputs/paper_figs_unc/FIA_density_map_full.png")
FIA.map
dev.off()



dens$density_discrete <- ifelse(dens$mean_dens_fia <= 0.5, "Prairie", 
                                ifelse(dens$mean_dens_fia <= 47, "Savanna",
                                       ifelse(dens$mean_dens_fia > 47 & dens$mean_dens_fia <= 100, "47-100",
                                              ifelse(dens$mean_dens_fia > 100 & dens$mean_dens_fia <= 200, "100-200", 
                                                     ifelse(dens$mean_dens_fia > 200 & dens$mean_dens_fia <= 300, "200-300", 
                                                            ifelse(dens$mean_dens_fia > 300 & dens$mean_dens_fia <= 400, "300-400",
                                                                   ifelse(dens$mean_dens_fia > 400 & dens$mean_dens_fia <= 500, "400-500",
                                                                          ifelse(dens$mean_dens_fia > 500 , "500+", "No data"))))))))

dens$density_discrete<- factor(dens$density_discrete, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500",  "500+", "No data"))




FIA.map.alt.color <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens, aes(x=x, y=y, fill = density_discrete))+
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


png(height = 4, width = 3, units = "in", res = 300,"/Users/kah/Documents/bimodality/outputs/paper_figs_unc/FIA_smooth_density_map_full_alt_colors.png")
FIA.map.alt.color+ggtitle("FIA mean density smoothed")
dev.off()

# plot median density

dens$density_discrete_median <- ifelse(dens$media_dens <= 0.5, "Prairie", 
                                       ifelse(dens$media_dens <= 47, "Savanna",
                                              ifelse(dens$media_dens > 47 & dens$media_dens <= 100, "47-100",
                                                     ifelse(dens$media_dens > 100 & dens$media_dens <= 200, "100-200", 
                                                            ifelse(dens$media_dens > 200 & dens$media_dens <= 300, "200-300", 
                                                                   ifelse(dens$media_dens > 300 & dens$media_dens <= 400, "300-400",
                                                                          ifelse(dens$media_dens > 400 & dens$media_dens <= 500, "400-500",
                                                                                 ifelse(dens$media_dens > 500 , "500+", "No data"))))))))

dens$density_discrete_median<- factor(dens$density_discrete_median, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500",  "500+", "No data"))

dens$density_discrete_low <- ifelse(dens$ci.low_dens_fia <= 0.5, "Prairie", 
                                    ifelse(dens$ci.low_dens_fia <= 47, "Savanna",
                                           ifelse(dens$ci.low_dens_fia > 47 & dens$ci.low_dens_fia <= 100, "47-100",
                                                  ifelse(dens$ci.low_dens_fia > 100 & dens$ci.low_dens_fia <= 200, "100-200", 
                                                         ifelse(dens$ci.low_dens_fia > 200 & dens$ci.low_dens_fia <= 300, "200-300", 
                                                                ifelse(dens$ci.low_dens_fia > 300 & dens$ci.low_dens_fia <= 400, "300-400",
                                                                       ifelse(dens$ci.low_dens_fia > 400 & dens$ci.low_dens_fia <= 500, "400-500",
                                                                              ifelse(dens$ci.low_dens_fia > 500 , "500+", "No data"))))))))

dens$density_discrete_low <- factor(dens$density_discrete_low, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500",  "500+", "No data"))




FIA.map.alt.color.low <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens, aes(x=x, y=y, fill =density_discrete_low))+
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


png(height = 4, width = 3, units = "in", res = 300,"/Users/kah/Documents/bimodality/outputs/paper_figs_unc/FIA_smooth_density_map_full_alt_colors.png")
FIA.map.alt.color+ggtitle("FIA mean density smoothed")
dev.off()


FIA.map.alt.color.msk <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens[!is.na(dens$FIAdensity),], aes(x=x, y=y, fill =density_discrete))+
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


png(height = 4, width = 3, units = "in", res = 300,"/Users/kah/Documents/bimodality/outputs/paper_figs_unc/FIA_smooth_density_map_full_alt_colors_msk.png")
FIA.map.alt.color.msk+ggtitle("FIA mean density smoothed")
dev.off()

pred.old.plot <- ggplot(dens, aes(FIAdensity, mean_dens_fia))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens_fia, ymax=ci.high_dens_fia), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(FIAdensity, mean_dens_fia), color = "black", size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+theme_bw()+ylab("Smoothed Density")+xlab("Previous grid cell density")

png(height = 5, width = 5, units = "in", res = 200, "outputs/chris_estimates_vs_previous_estimates_FIA.png")
pred.old.plot
dev.off()



pc1_unc_fia <- ggplot(dens, aes(PC1fia, mean_dens_fia))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens_fia, ymax=ci.high_dens_fia), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(PC1fia, mean_dens_fia), color = "black", size = 0.05)+theme_bw()+ylab("FIA Density (trees/ha)")+xlab("PC1")+geom_smooth()

ppet_unc_fia <- ggplot(dens, aes(GS_ppet_mod, mean_dens_fia))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens_fia, ymax=ci.high_dens_fia), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(GS_ppet_mod, mean_dens_fia), color = "black", size = 0.05)+theme_bw()+ylab("FIA Density (trees/ha)")+xlab("growing season P-PET")+geom_smooth()


soil_unc_fia <- ggplot(dens, aes(mean_GS_soil_m, mean_dens_fia))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens_fia, ymax=ci.high_dens_fia), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(mean_GS_soil_m, mean_dens_fia), color = "black", size = 0.05)+theme_bw()+ylab("FIA Density (trees/ha)")+xlab("Growing season soil moisture")+geom_smooth()


# save plots of environment vs density estimates + CI:
png(height = 10, width = 4, units = "in", res = 300, "outputs/paper_figs_unc/smooth_dens_vs_envts_FIA.png")
cowplot::plot_grid(pc1_unc_fia, ppet_unc_fia, soil_unc_fia, ncol = 1)
dev.off()

ggplot()+geom_histogram(data = dens, aes(mean_dens_fia), fill = "red")+geom_histogram(data= dens, aes(ci.high_dens_fia), aes = 2, color = "grey")+geom_histogram(data= dens, aes(ci.low_dens_fia), alpha = 0.2, fill = "blue")

# ----------------------- FIA histogram of smoothed, unmasked density by species classes----------------------------
clust_plot5 <- read.csv("outputs/five_clust_fia_dissimilarity_stat_smooth.dens.csv")

# merge the clusters and pls density data: 
clust_5 <- merge(clust_plot5 , dens, by = c("x", "y"))

# need to rename the clusters here (should go back and do it in the place where we originally make the clusters):
clust_5
library(plyr)
#clust_8$foresttype <- revalue(clust_5$speciescluster, c("Maple/Oak/Poplar/Ash"="Maple/Oak/Other", "Poplar/Pine/Cedar/Spruce"="Poplar", 
 #                                                       "Oak/Maple/Elm/Poplar/Ash" = "Oak/Maple/Hickory", "Oak/Maple/Other/Hickory"
  #                                                      "Pine/Poplar" = "Pine", "Spruce/Cedar/Tamarack/Poplar" = "Boreal/Sub-boreal", "Beech/Maple/Hemlock" = "Beech-Maple"))

#clust_8$orderedforesttype<- factor(clust_8$foresttype, c("Oak", "Pine", "Aspen", "N. Mixed Forest", "Boreal/Sub-boreal","Elm/Oak/Maple", "Oak-Hickory", "Beech-Maple"))

fia.clust <- ggplot(clust_5, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c('#f0027f'  ,'#003c30','#a6cee3',"#beaed4", '#fdc086'), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.20, 0.18),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                                           axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),legend.key = element_rect(color = "black", linetype = "solid"),axis.text.x=element_blank(),
                                                                                                                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                           axis.title.x=element_blank(),
                                                                                                                           axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()

fia.clust
# merge clust_plot5 and dens.pr



# ----------------------- Figure 1D total FIA density histgram colored by species composition-----------------
dens.clust <- merge(dens, clust_5[,c("x" ,"y", "speciescluster")], by = c("x", "y"), all.x = TRUE)
dens.clust <- dens.clust[!duplicated(dens.clust),]



myColors <- c('#f0027f'  ,'#003c30','#a6cee3',"#beaed4", '#fdc086')
#names(myColors) <- levels(dens.clust$foresttype_ordered)

clust.hist.fia.full <- ggplot()+ geom_density(data = dens.clust[dens.clust$mean_dens_fia >= 0.5,], aes(mean_dens, 22 *..count..), linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_density(data = dens.clust[dens.clust$mean_dens_fia >= 0.5,], aes(mean_dens_fia, 22 *..count..), linetype="solid" , color = "black", bw = 12,size = 1.5)+
  geom_histogram(data = dens.clust[dens.clust$mean_dens_fia >= 0.5,], aes(mean_dens_fia, fill = speciescluster), binwidth =  20)+xlim(0,600)+
  scale_fill_manual(values = myColors, name = " ", drop = TRUE)+
  coord_flip()+xlab("FIA tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.44, 0.85),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.4, "line"),legend.key = element_rect(color = "black", linetype ="solid"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
clust.hist.fia.full


# # ----------------------- FIA histogram of smoothed, unmasked density by species classes----------------------------
clust_plot5 <- read.csv("outputs/five_clust_fia_dissimilarity_stat_smooth.dens.csv")

# merge the clusters and pls density data: 
clust_5 <- merge(clust_plot5 , dens, by = c("x", "y"))

# need to rename the clusters here (should go back and do it in the place where we originally make the clusters):
clust_5
library(plyr)
#clust_8$foresttype <- revalue(clust_5$speciescluster, c("Maple/Oak/Poplar/Ash"="Maple/Oak/Other", "Poplar/Pine/Cedar/Spruce"="Poplar", 
#                                                       "Oak/Maple/Elm/Poplar/Ash" = "Oak/Maple/Hickory", "Oak/Maple/Other/Hickory"
#                                                      "Pine/Poplar" = "Pine", "Spruce/Cedar/Tamarack/Poplar" = "Boreal/Sub-boreal", "Beech/Maple/Hemlock" = "Beech-Maple"))

#clust_8$orderedforesttype<- factor(clust_8$foresttype, c("Oak", "Pine", "Aspen", "N. Mixed Forest", "Boreal/Sub-boreal","Elm/Oak/Maple", "Oak-Hickory", "Beech-Maple"))

fia.clust <- ggplot(clust_5, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c('#f0027f'  ,'#003c30','#a6cee3',"#beaed4", '#fdc086'), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.20, 0.18),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                                           axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),legend.key = element_rect(color = "black", linetype = "solid"),axis.text.x=element_blank(),
                                                                                                                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                           axis.title.x=element_blank(),
                                                                                                                           axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()

fia.clust
# merge clust_plot5 and dens.pr



# ----------------------- 10 species comp total FIA density histgram colored by species composition-----------------
clust_plot10 <- read.csv("outputs/ten_clust_combined_dissimilarity_stat_smooth.dens.csv")
clust_plot_fia <- clust_plot10[clust_plot10$period %in% "FIA",]
# merge the clusters and pls density data: 
clust_10_fia <- merge(clust_plot_fia, dens, by = c("x", "y"))

# need to rename the clusters here (should go back and do it in the place where we originally make the clusters):
head(clust_10_fia)
library(plyr)
clust_10_fia$foresttype <- revalue(clust_10_fia$speciescluster, c("Poplar/Oak-FIA"="Aspen", 
                                                          "Oak/Maple/Ash/Poplar-FIA"="Oak/Maple/Ash", 
                                                          "Oak-PLS" = "Oak", 
                                                          "Hemlock/Cedar/Maple-PLS" = "N. Mixed Forest", 
                                                          "Oak/Hickory/Elm/Maple-FIA" = "Oak-Hickory",
                                                          "Maple/Poplar/Oak/Ash-FIA" = "Pine", 
                                                          "Pine/Poplar/Tamarack/Fir-PLS" = "Boreal/Sub-boreal", 
                                                          "Beech/Maple/Pine-PLS" = "Beech-Maple",
                                                          "Maple/Cedar/Poplar-FIA" = "Maple Mixed Forest",
                                                          "Oak/Maple/Other/Hickory-FIA" = "Oak-Mixed"))

clust_10_fia$orderedforesttype <- factor(clust_10_fia$foresttype, c("Oak", "Pine", "Aspen", "N. Mixed Forest", "Boreal/Sub-boreal","Oak/Maple/Ash", "Oak-Hickory", "Beech-Maple", "Oak-Mixed", "Maple Mixed Forest"))

# create a stable coloring scheme:
compColors <- c('#386cb0',"#f0027f",'#ff7f00',"#ffff99","#7fc97f", "#beaed4",'#a6cee3',"#b15928", '#fdc086',  "#004529")
names(compColors) <- levels(clust_10_fia$orderedforesttype)


#dens.clust10 <- merge(dens, clust_5[,c("x" ,"y", "speciescluster")], by = c("x", "y"), all.x = TRUE)
#dens.clust$foresttype_ordered <- factor(dens.clust$foresttype, levels = rev(c("Boreal/Sub-boreal", "Pine", "Aspen",  "Elm/Oak/Maple","Oak-Hickory","Beech-Maple","N. Mixed Forest", "Oak")))
#dens.clust.omit <- dens.clust[ !is.na(dens.clust$foresttype_ordered),]
clust_10_fia <- clust_10_fia[!duplicated(clust_10_fia),]

#myColors <- c('#f0027f'  ,'#003c30','#a6cee3',"#beaed4", '#fdc086')
#names(myColors) <- levels(dens.clust$foresttype_ordered)

clust.hist.fia.full.both.no.aspect <- ggplot()+ geom_density(data = clust_10_fia[clust_10_fia$mean_dens_fia >= 0.5,], aes(mean_dens, 22 *..count..), linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_density(data = clust_10_fia[clust_10_fia$mean_dens_fia >= 0.5,], aes(mean_dens_fia, 22 *..count..), linetype="solid" , color = "black", bw = 12,size = 1.5)+
  geom_histogram(data = clust_10_fia[clust_10_fia$mean_dens_fia >= 0.5,], aes(mean_dens_fia, fill = orderedforesttype), binwidth =  20)+xlim(0,600)+
  scale_fill_manual(values = compColors, name = " ", drop = TRUE)+
  coord_flip()+xlab("FIA tree density")+ylab("# grid cells")+theme_bw(base_size = 7)+theme(legend.position = c(0.5, 0.85),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.1, "line"),legend.key = element_rect(color = "black", linetype ="solid"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
clust.hist.fia.full.both.no.aspect

# plot out the map:

fia.clust.both <- ggplot(clust_10_fia, aes(x = x, y=y, fill = orderedforesttype))+geom_raster()+
  scale_fill_manual(values = compColors, name = " ", drop = F)+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.20, 0.22),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                                           axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),legend.key = element_rect(color = "black", linetype = "solid"),axis.text.x=element_blank(),
                                                                                                                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                           axis.title.x=element_blank(),
                                                                                                                           axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()

fia.clust.both


# now mask out cells without fia plots:
clust_10_fia_msk <- clust_10_fia[!is.na(clust_10_fia$FIAdensity),]
clust.hist.fia.full.both.msk <- ggplot()+ geom_density(data = clust_10_fia_msk[clust_10_fia_msk$mean_dens_fia >= 0.5,], aes(mean_dens, 22 *..count..), linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_density(data = clust_10_fia_msk[clust_10_fia_msk$mean_dens_fia >= 0.5,], aes(mean_dens_fia, 22 *..count..), linetype="solid" , color = "black", bw = 12,size = 1.5)+
  geom_histogram(data = clust_10_fia_msk[clust_10_fia_msk$mean_dens_fia >= 0.5,], aes(mean_dens_fia, fill = orderedforesttype), binwidth =  20)+xlim(0,600)+
  scale_fill_manual(values = compColors, name = " ", drop = TRUE)+
  coord_flip()+xlab("FIA tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.44, 0.85),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.4, "line"),legend.key = element_rect(color = "black", linetype ="solid"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
clust.hist.fia.full.both.msk


clust_10_fia_msk <- clust_10_fia_msk[!is.na(clust_10_fia_msk$FIAdensity),]
clust.hist.fia.full.both.no.aspect.msk <- ggplot()+ geom_density(data = clust_10_fia_msk[clust_10_fia_msk$mean_dens_fia >= 0.5,], aes(mean_dens, 22 *..count..), linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_density(data = clust_10_fia_msk[clust_10_fia_msk$mean_dens_fia >= 0.5,], aes(mean_dens_fia, 22 *..count..), linetype="solid" , color = "black", bw = 12,size = 1.5)+
  geom_histogram(data = clust_10_fia_msk[clust_10_fia_msk$mean_dens_fia >= 0.5,], aes(mean_dens_fia, fill = orderedforesttype), binwidth =  20)+xlim(0,600)+
  scale_fill_manual(values = compColors, name = " ", drop = TRUE)+
  coord_flip()+xlab("FIA tree density")+ylab("# grid cells")+theme_bw(base_size = 6)+theme(legend.position = c(0.44, 0.85),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.4, "line"),legend.key = element_rect(color = "black", linetype ="solid"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
clust.hist.fia.full.both.no.aspect.msk

# plot out the map:

fia.clust.both.msk <- ggplot(clust_10_fia_msk, aes(x = x, y=y, fill = orderedforesttype))+geom_raster()+
  scale_fill_manual(values = compColors, name = " ", drop = F)+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.20, 0.24),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                                           axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),legend.key = element_rect(color = "black", linetype = "solid"),axis.text.x=element_blank(),
                                                                                                                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                           axis.title.x=element_blank(),
                                                                                                                           axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()

fia.clust.both.msk


# now get previous FIA surveys:

# read in the past survey data and past logged data:

past.logged <- read.csv("data/FIA_plot_data/fia.by.cell.treated.2000_2017.csv")
past.survey <- read.csv("data/FIA_plot_data/fia.by.cell.out_1980_1990.csv")
past.survey$new_scale <- ifelse(past.survey$INVYRcd %in% "1990s", 775, 1000)

# get data frame w/ 1980s, 1990s, and modern survey estimates:
modern.fia <- dens.clust[,c("x", "y", "cell", "mean_dens_fia")]
modern.fia$INVYRcd <- "2000s"
colnames(modern.fia) <- c("x", "y", "cell", "FIAdensity", "INVYRcd")
modern.fia$INVYR <- 2000

full.fia.surveys <- rbind(modern.fia, past.survey[,c("x", "y", "cell", "FIAdensity", "INVYRcd", "INVYR")])

full.fia.surveys$INVYRcd <- factor(full.fia.surveys$INVYRcd, c("1980s", "1990s", "2000s"))
# D: Histogram colored by species cluster:
# make a histogram of denisty betwen -2.5 and 1 colored by species cluster:
rownames(full.fia.surveys) <- 1:length(full.fia.surveys$x)
mean.fia.surveys <- full.fia.surveys %>% group_by(x,y,cell, INVYRcd) %>% dplyr::summarise(Density = mean(FIAdensity, na.rm = TRUE))

fia.surveys.wide <- mean.fia.surveys %>% group_by(x,y, cell) %>% spread(key = INVYRcd, value = Density)

fia.surveys.wide$pct_inc_1980_1990 <- fia.surveys.wide$`1990s` - fia.surveys.wide$`1980s`
fia.surveys.wide$pct_inc_1990_2000 <- fia.surveys.wide$`2000s` - fia.surveys.wide$`1990s`
fia.surveys.wide$pct_inc_1980_2000 <- fia.surveys.wide$`2000s` - fia.surveys.wide$`1980s`

fia.surveys.wide$dens.bins.1980 <- cut(fia.surveys.wide$`1980s`, breaks = c(0,50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650))
fia.surveys.wide$dens.bins.1990 <- cut(fia.surveys.wide$`1990s`, breaks = c(0,50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650))
fia.surveys.wide$dens.bins.2000 <- cut(fia.surveys.wide$`2000s`, breaks = c(0,50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650))


# get numbe cells changing between 1980 and 1990:
ncell.change.1980.1990 <- fia.surveys.wide %>% group_by(dens.bins.1980) %>% dplyr::summarise(n_inc = sum(pct_inc_1980_1990 > 5, na.rm = TRUE),
                                                                     n_dec = sum(pct_inc_1980_1990 < -5, na.rm = TRUE),
                                                                     n_nochange = sum(pct_inc_1980_1990 >= -5 & pct_inc_1980_1990 <= 5 , na.rm = TRUE))

ncell.change.1980.1990[ncell.change.1980.1990 == 0] <- NA # change 0 ncells to NA

ncell.change.1980.1990$start.bin <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650)
ncell.change.1980.1990$end.bin <- c( 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700)
ncell.change.1980.1990$xval.nochange <- "2"
ncell.change.1980.1990$xval.inc <- "1"
ncell.change.1980.1990$xval.dec <- "3"

# pct change plots
ncell.pct.change.1980.1990 <- ncell.change.1980.1990
ncell.pct.change.1980.1990$total_cells <- rowSums(ncell.pct.change.1980.1990[,c("n_inc", "n_dec", "n_nochange")], na.rm =TRUE)

ncell.pct.change.1980.1990$pct_inc <- (ncell.pct.change.1980.1990$n_inc/ncell.pct.change.1980.1990$total_cells)*100
ncell.pct.change.1980.1990$pct_dec <- (ncell.pct.change.1980.1990$n_dec/ncell.pct.change.1980.1990$total_cells)*100
ncell.pct.change.1980.1990$pct_nochange <- (ncell.pct.change.1980.1990$n_nochange/ncell.pct.change.1980.1990$total_cells)*100


# make a general plot with arrows for increasing and decreasing:
ncell.pct.change.1980.1990.plot <- ggplot(ncell.pct.change.1980.1990, aes(xval.inc, start.bin))+geom_segment(aes(xend = xval.inc, yend = end.bin-20, size = pct_inc/2),
                                                                                         arrow = arrow(length = unit(0.1,"cm")), color = "#2166ac")+
  geom_point(data = ncell.pct.change.1980.1990, aes(xval.nochange, start.bin+20, size = pct_nochange/2), color = "#636363")+
  geom_segment(aes(x = xval.dec, y = end.bin-20, xend = xval.dec, yend = start.bin, size = pct_dec/2), arrow = arrow(length = unit(0.1,"cm")), color = "#b2182b")+
  scale_size(range = c(0, 2))+scale_x_discrete(labels=c("2" = "No Change", "1" = "Increasing",
                            "3" = "Decreasing"))+theme_bw()+ylim(0,600)+theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x  = element_blank(), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +ylab("Tree Density") +ggtitle("% of grid cells changing density \n between 1980's and 1990's")




# make a general plot with arrows for increasing and decreasing:
ncell.change.1980.1990.plot <- ggplot(ncell.change.1980.1990, aes(xval.inc, start.bin))+geom_segment(aes(xend = xval.inc, yend = end.bin-20, size = n_inc/2),
                                                                                 arrow = arrow(length = unit(0.15,"cm")), color = "#2166ac")+
  geom_point(data = ncell.change.1980.1990, aes(xval.nochange, start.bin+20, size = n_nochange/2), color = "#636363")+
  geom_segment(aes(x = xval.dec, y = end.bin-20, xend = xval.dec, yend = start.bin, size = n_dec/2), arrow = arrow(length = unit(0.15,"cm")), color = "#b2182b")+
  scale_size(range = c(0, 2))+scale_x_discrete(labels=c("2" = "No Change", "1" = "Increasing",
                            "3" = "Decreasing"))+theme_bw()+ylim(0,600)+theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title  = element_blank(), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank() )  



# get number of cells changing between 1990 to 2000:
ncell.change.1990.2000 <- fia.surveys.wide %>% group_by(dens.bins.1990) %>% dplyr::summarise(n_inc = sum(pct_inc_1990_2000 > 5, na.rm = TRUE),
                                                                                      n_dec = sum(pct_inc_1990_2000 < -5, na.rm = TRUE),
                                                                                      n_nochange = sum(pct_inc_1990_2000 >= -5 & pct_inc_1990_2000 <= 5 , na.rm = TRUE))

ncell.change.1990.2000[ncell.change.1990.2000 == 0] <- NA # change 0 ncells to NA

ncell.change.1990.2000$start.bin <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650)
ncell.change.1990.2000$end.bin <- c( 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700)
ncell.change.1990.2000$xval.nochange <- "2"
ncell.change.1990.2000$xval.inc <- "1"
ncell.change.1990.2000$xval.dec <- "3"

ncell.pct.change.1990.2000 <- ncell.change.1990.2000
ncell.pct.change.1990.2000$total_cells <- rowSums(ncell.pct.change.1990.2000[,c("n_inc", "n_dec", "n_nochange")], na.rm =TRUE)

ncell.pct.change.1990.2000$pct_inc <- (ncell.pct.change.1990.2000$n_inc/ncell.pct.change.1990.2000$total_cells)*100
ncell.pct.change.1990.2000$pct_dec <- (ncell.pct.change.1990.2000$n_dec/ncell.pct.change.1990.2000$total_cells)*100
ncell.pct.change.1990.2000$pct_nochange <- (ncell.pct.change.1990.2000$n_nochange/ncell.pct.change.1990.2000$total_cells)*100


# make a general plot with arrows for increasing and decreasing:
ncell.pct.change.1990.2000.plot <- ggplot(ncell.pct.change.1990.2000, aes(xval.inc, start.bin))+geom_segment(aes(xend = xval.inc, yend = end.bin-20, size = pct_inc/2),
                                                                                                             arrow = arrow(length = unit(0.1,"cm")), color = "#2166ac")+
  geom_point(data = ncell.pct.change.1990.2000, aes(xval.nochange, start.bin+20, size = pct_nochange/2), color = "#636363")+
  geom_segment(aes(x = xval.dec, y = end.bin-20, xend = xval.dec, yend = start.bin, size = pct_dec/2), arrow = arrow(length = unit(0.1,"cm")), color = "#b2182b")+
  scale_size(range = c(0, 2))+scale_x_discrete(labels=c("2" = "No Change", "1" = "Increasing",
                            "3" = "Decreasing"))+theme_bw()+ylim(0,600)+theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x  = element_blank(), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +ylab("Tree Density") +ggtitle("% of grid cells changing density \n between 1990's and 2000's")


# make a general plot with arrows for increasing and decreasing:
ncell.change.1990.2000.plot <- ggplot(ncell.change.1990.2000, aes(xval.inc, start.bin))+geom_segment(aes(xend = xval.inc, yend = end.bin-20, size = n_inc/2),
                                                                                                     arrow = arrow(length = unit(0.15,"cm")), color = "#2166ac")+
  geom_point(data = ncell.change.1990.2000, aes(xval.nochange, start.bin+20, size = n_nochange/2), color = "#636363")+
  geom_segment(aes(x = xval.dec, y = end.bin-20, xend = xval.dec, yend = start.bin, size = n_dec/2), arrow = arrow(length = unit(0.15,"cm")), color = "#b2182b")+
  scale_size(range = c(0, 2))+scale_x_discrete(labels=c("2" = "No Change", "1" = "Increasing",
                            "3" = "Decreasing"))+theme_bw()+ylim(0,600)+theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title  = element_blank(), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank() )  




# get number of cells changing between 2000 to 2000:
ncell.change.1980.2000 <- fia.surveys.wide %>% group_by(dens.bins.1980) %>% dplyr::summarise(n_inc = sum(pct_inc_1980_2000 > 5, na.rm = TRUE),
                                                                                      n_dec = sum(pct_inc_1980_2000 < -5, na.rm = TRUE),
                                                                                      n_nochange = sum(pct_inc_1980_2000 >= -5 & pct_inc_1980_2000 <= 5 , na.rm = TRUE))

ncell.change.1980.2000[ncell.change.1980.2000 == 0] <- NA # change 0 ncells to NA

ncell.change.1980.2000$start.bin <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650)
ncell.change.1980.2000$end.bin <- c( 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700)
ncell.change.1980.2000$xval.nochange <- "2"
ncell.change.1980.2000$xval.inc <- "1"
ncell.change.1980.2000$xval.dec <- "3"

ncell.pct.change.1980.2000 <- ncell.change.1980.2000
ncell.pct.change.1980.2000$total_cells <- rowSums(ncell.pct.change.1980.2000[,c("n_inc", "n_dec", "n_nochange")], na.rm =TRUE)

ncell.pct.change.1980.2000$pct_inc <- (ncell.pct.change.1980.2000$n_inc/ncell.pct.change.1980.2000$total_cells)*100
ncell.pct.change.1980.2000$pct_dec <- (ncell.pct.change.1980.2000$n_dec/ncell.pct.change.1980.2000$total_cells)*100
ncell.pct.change.1980.2000$pct_nochange <- (ncell.pct.change.1980.2000$n_nochange/ncell.pct.change.1980.2000$total_cells)*100


# make a general plot with arrows for increasing and decreasing:
ncell.pct.change.1980.2000.plot <- ggplot(ncell.pct.change.1980.2000, aes(xval.inc, start.bin))+geom_segment(aes(xend = xval.inc, yend = end.bin-20, size = pct_inc/2),
                                                                                                             arrow = arrow(length = unit(0.1,"cm")), color = "#2166ac")+
  geom_point(data = ncell.pct.change.1980.2000, aes(xval.nochange, start.bin+20, size = pct_nochange/2), color = "#636363")+
  geom_segment(aes(x = xval.dec, y = end.bin-20, xend = xval.dec, yend = start.bin, size = pct_dec/2), arrow = arrow(length = unit(0.1,"cm")), color = "#b2182b")+
  scale_size(range = c(0, 2))+scale_x_discrete(labels=c("2" = "No Change", "1" = "Increasing",
                            "3" = "Decreasing"))+theme_bw()+ylim(0,600)+theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x  = element_blank(), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +ylab("Tree Density") +ggtitle("% of grid cells changing density \n between 1980's and 2000's")



# make a general plot with arrows for increasing and decreasing:
ncell.change.1980.2000.plot <- ggplot(ncell.change.1980.2000, aes(xval.inc, start.bin))+geom_segment(aes(xend = xval.inc, yend = end.bin-20, size = n_inc/2),
                                                                                                     arrow = arrow(length = unit(0.15,"cm")), color = "#2166ac")+
  geom_point(data = ncell.change.1980.2000, aes(xval.nochange, start.bin+20, size = n_nochange), color = "#636363")+
  geom_segment(aes(x = xval.dec, y = end.bin-20, xend = xval.dec, yend = start.bin, size = n_dec/2), arrow = arrow(length = unit(0.15,"cm")), color = "#b2182b")+
  scale_size(range = c(0, 2))+scale_x_discrete(labels=c("2" = "No Change", "1" = "Increasing",
                            "3" = "Decreasing"))+theme_bw()+ylim(0,600)+theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title  = element_blank(), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank() )  






library(ggpubr)
inset <- ggboxplot( data= full.fia.surveys, x = 'INVYRcd',y = 'FIAdensity', merge=TRUE,width = 0.5, fill = "INVYRcd",  palette =c("grey28", "grey40", "grey60"), outlier.size = 0.0005)+ylim(0,600) +theme_bw()+theme(axis.title = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(), legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1))

# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(inset)

# Place box plots inside the histogram plot
full.fia.surveys$INV_place <- ifelse(full.fia.surveys$INVYRcd %in% "2000s", 1000, 
                                     ifelse(full.fia.surveys$INVYRcd %in% "1990s", 900,
                                            ifelse(full.fia.surveys$INVYRcd %in% "1980s", 800, NA )))

# -----------------------make arrow figure for increasing/decreasing between pls and FIA----------

# find the # of grid cells that decreased between pls and fia:
dens.clust$dens.clust.bins <- cut(dens.clust$mean_dens, breaks = c(0,50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650))
dens.msk <- dens.clust[!is.na(dens.clust$mean_dens_fia),]
dens.msk$fiaminuspls <- dens.msk$mean_dens_fia - dens.msk$mean_dens
ncell.change <- dens.msk %>% group_by(dens.clust.bins) %>% dplyr::summarise(n_inc = sum(fiaminuspls > 5),
                                                     n_dec = sum(fiaminuspls < -5),
                                                     n_nochange = sum(fiaminuspls >= -5 & fiaminuspls <= 5 ))

ncell.change[ncell.change == 0] <- NA # change 0 ncells to NA

ncell.change$start.bin <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600)
ncell.change$end.bin <- c( 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650)
ncell.change$xval.nochange <- "2"
ncell.change$xval.inc <- "1"
ncell.change$xval.dec <- "3"



# test example with all arrows over top, but lets do it 
ncell.change.ppet <- dens.msk %>% group_by(dens.clust.bins, ppet_bins) %>% dplyr::summarise(n_inc = sum(fiaminuspls > 5),
                                                                            n_dec = sum(fiaminuspls < -5),
                                                                            n_nochange = sum(fiaminuspls >= -5 & fiaminuspls <= 5 ))

ncell.change.ppet <- merge(ncell.change.ppet, ncell.change[, c("dens.clust.bins", "start.bin", "end.bin")], by = "dens.clust.bins")
#ggplot(ncell.change.ppet, aes(ppet_bins, dens.clust.bins, color=n_dec))+geom_point()
ncell.change.ppet[ncell.change.ppet == 0] <- NA 
ggplot(ncell.change.ppet, aes(ppet_bins, start.bin))+geom_segment(aes(xend = ppet_bins, yend = end.bin-20, size = n_inc/2),
                                                            arrow = arrow(length = unit(0.15,"cm")), color = "#2166ac")+
  geom_point(data = ncell.change.ppet, aes(ppet_bins, start.bin+20, size = n_nochange/2), color = "#636363")+
  geom_segment(aes( y = end.bin-20, xend = ppet_bins, yend = start.bin, size = n_dec/2), arrow = arrow(length = unit(0.15,"cm")), color = "#b2182b")


ncell.pct.change.ppet <- ncell.change.ppet

ncell.pct.change.ppet$total_cells <- rowSums(ncell.pct.change.ppet[,c("n_inc", "n_dec", "n_nochange")], na.rm =TRUE)

ncell.pct.change.ppet$pct_inc <- (ncell.pct.change.ppet$n_inc/ncell.pct.change.ppet$total_cells)*100
ncell.pct.change.ppet$pct_dec <- (ncell.pct.change.ppet$n_dec/ncell.pct.change.ppet$total_cells)*100
ncell.pct.change.ppet$pct_nochange <- (ncell.pct.change.ppet$n_nochange/ncell.pct.change.ppet$total_cells)*100

# ideally we want to have it be the most common class, but this will do for now
pct.inc.ppet <- ggplot(ncell.pct.change.ppet[ncell.pct.change.ppet$pct_inc >=50,], aes(ppet_bins, start.bin))+geom_segment(aes(xend = ppet_bins, yend = end.bin-20, size = pct_inc),
                                                                  arrow = arrow(length = unit(0.15,"cm")), color = "#2166ac")+
  geom_point(data = ncell.pct.change.ppet[ncell.pct.change.ppet$pct_nochange >= 20,], aes(ppet_bins, start.bin+20, size = pct_nochange), color = "#636363")+
  geom_segment(data =  ncell.pct.change.ppet[ncell.pct.change.ppet$pct_dec >=50,], aes( y = end.bin-20, xend = ppet_bins, yend = start.bin, size = pct_dec/2), arrow = arrow(length = unit(0.15,"cm")), color = "#b2182b")+ylab("Tree density (stems/Ha)")



#------------ do the ncell pct change for PC1:
# test example with all arrows over top, but lets do it 
ncell.change.pc1 <- dens.msk %>% group_by(dens.clust.bins, pc1_bins) %>% dplyr::summarise(n_inc = sum(fiaminuspls > 5),
                                                                                            n_dec = sum(fiaminuspls < -5),
                                                                                            n_nochange = sum(fiaminuspls >= -5 & fiaminuspls <= 5 ))

ncell.change.pc1 <- merge(ncell.change.pc1, ncell.change[, c("dens.clust.bins", "start.bin", "end.bin")], by = "dens.clust.bins")
#ggplot(ncell.change.pc1, aes(pc1_bins, dens.clust.bins, color=n_dec))+geom_point()
ncell.change.pc1[ncell.change.pc1 == 0] <- NA 
ggplot(ncell.change.pc1, aes(pc1_bins, start.bin))+geom_segment(aes(xend = pc1_bins, yend = end.bin-20, size = n_inc/2),
                                                                  arrow = arrow(length = unit(0.15,"cm")), color = "#2166ac")+
  geom_point(data = ncell.change.pc1, aes(pc1_bins, start.bin+20, size = n_nochange/2), color = "#636363")+
  geom_segment(aes( y = end.bin-20, xend = pc1_bins, yend = start.bin, size = n_dec/2), arrow = arrow(length = unit(0.15,"cm")), color = "#b2182b")


ncell.pct.change.pc1 <- ncell.change.pc1

ncell.pct.change.pc1$total_cells <- rowSums(ncell.pct.change.pc1[,c("n_inc", "n_dec", "n_nochange")], na.rm =TRUE)

ncell.pct.change.pc1$pct_inc <- (ncell.pct.change.pc1$n_inc/ncell.pct.change.pc1$total_cells)*100
ncell.pct.change.pc1$pct_dec <- (ncell.pct.change.pc1$n_dec/ncell.pct.change.pc1$total_cells)*100
ncell.pct.change.pc1$pct_nochange <- (ncell.pct.change.pc1$n_nochange/ncell.pct.change.pc1$total_cells)*100

# ideally we want to have it be the most common class, but this will do for now
pct.inc.pc1 <- ggplot(ncell.pct.change.pc1[ncell.pct.change.pc1$pct_inc >=50,], aes(pc1_bins, start.bin))+geom_segment(aes(xend = pc1_bins, yend = end.bin-20, size = pct_inc),
                                                                                                                           arrow = arrow(length = unit(0.15,"cm")), color = "#2166ac")+
  geom_point(data = ncell.pct.change.pc1[ncell.pct.change.pc1$pct_nochange >= 20,], aes(pc1_bins, start.bin+20, size = pct_nochange), color = "#636363")+
  geom_segment(data =  ncell.pct.change.pc1[ncell.pct.change.pc1$pct_dec >=50,], aes( y = end.bin-20, xend = pc1_bins, yend = start.bin, size = pct_dec/2), arrow = arrow(length = unit(0.15,"cm")), color = "#b2182b")+ylab("Tree density (stems/Ha)")



#------------ do the ncell pct change for soil:
# test example with all arrows over top, but lets do it 
ncell.change.soil <- dens.msk %>% group_by(dens.clust.bins, soil_bins) %>% dplyr::summarise(n_inc = sum(fiaminuspls > 5),
                                                                                          n_dec = sum(fiaminuspls < -5),
                                                                                          n_nochange = sum(fiaminuspls >= -5 & fiaminuspls <= 5 ))

ncell.change.soil <- merge(ncell.change.soil, ncell.change[, c("dens.clust.bins", "start.bin", "end.bin")], by = "dens.clust.bins")
#ggplot(ncell.change.soil, aes(soil_bins, dens.clust.bins, color=n_dec))+geom_point()
ncell.change.soil[ncell.change.soil == 0] <- NA 
ggplot(ncell.change.soil, aes(soil_bins, start.bin))+geom_segment(aes(xend = soil_bins, yend = end.bin-20, size = n_inc/2),
                                                                arrow = arrow(length = unit(0.15,"cm")), color = "#2166ac")+
  geom_point(data = ncell.change.soil, aes(soil_bins, start.bin+20, size = n_nochange/2), color = "#636363")+
  geom_segment(aes( y = end.bin-20, xend = soil_bins, yend = start.bin, size = n_dec/2), arrow = arrow(length = unit(0.15,"cm")), color = "#b2182b")


ncell.pct.change.soil <- ncell.change.soil

ncell.pct.change.soil$total_cells <- rowSums(ncell.pct.change.soil[,c("n_inc", "n_dec", "n_nochange")], na.rm =TRUE)

ncell.pct.change.soil$pct_inc <- (ncell.pct.change.soil$n_inc/ncell.pct.change.soil$total_cells)*100
ncell.pct.change.soil$pct_dec <- (ncell.pct.change.soil$n_dec/ncell.pct.change.soil$total_cells)*100
ncell.pct.change.soil$pct_nochange <- (ncell.pct.change.soil$n_nochange/ncell.pct.change.soil$total_cells)*100

# ideally we want to have it be the most common class, but this will do for now
pct.inc.soil <- ggplot(ncell.pct.change.soil[ncell.pct.change.soil$pct_inc >=50,], aes(soil_bins, start.bin))+geom_segment(aes(xend = soil_bins, yend = end.bin-20, size = pct_inc),
                                                                                                                       arrow = arrow(length = unit(0.15,"cm")), color = "#2166ac")+
  geom_point(data = ncell.pct.change.soil[ncell.pct.change.soil$pct_nochange >= 20,], aes(soil_bins, start.bin+20, size = pct_nochange), color = "#636363")+
  geom_segment(data =  ncell.pct.change.soil[ncell.pct.change.soil$pct_dec >=50,], aes( y = end.bin-20, xend = soil_bins, yend = start.bin, size = pct_dec/2), arrow = arrow(length = unit(0.15,"cm")), color = "#b2182b")+ylab("Tree density (stems/Ha)")



# make a general plot with arrows for increasing and decreasing:
ncell.change.plot <- ggplot(ncell.change, aes(xval.inc, start.bin))+geom_segment(aes(xend = xval.inc, yend = end.bin-20, size = n_inc/2),
                                                            arrow = arrow(length = unit(0.15,"cm")), color = "#2166ac")+
  geom_point(data = ncell.change, aes(xval.nochange, start.bin+20, size = n_nochange/2), color = "#636363")+
  geom_segment(aes(x = xval.dec, y = end.bin-20, xend = xval.dec, yend = start.bin, size = n_dec/2), arrow = arrow(length = unit(0.15,"cm")), color = "#b2182b")+
  scale_size(range = c(0, 2))+scale_x_discrete(labels=c("2" = "No Change", "1" = "Increasing",
                              "3" = "Decreasing"))+theme_bw()+ylim(0,600)+theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title  = element_blank(), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank() )  
  
# cant get ncell.change.plot to align with clust.hist.full

png(height = 6, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/test_align_arrows.png")
plot_grid(clust.hist.full.no.aspect, ncell.change.plot, align = "hv", rel_widths = c(1,0.5))
dev.off()

#--------------------Make arrow figure but with % of grid cells not the total # of grid cells:------
# find the # of grid cells that decreased between pls and fia:
ncell.pct.change <- ncell.change
ncell.pct.change$total_cells <- rowSums(ncell.pct.change[,c("n_inc", "n_dec", "n_nochange")], na.rm =TRUE)

ncell.pct.change$pct_inc <- (ncell.pct.change$n_inc/ncell.pct.change$total_cells)*100
ncell.pct.change$pct_dec <- (ncell.pct.change$n_dec/ncell.pct.change$total_cells)*100
ncell.pct.change$pct_nochange <- (ncell.pct.change$n_nochange/ncell.pct.change$total_cells)*100


# make a general plot with arrows for increasing and decreasing:
ncell.pct.change.plot <- ggplot(ncell.pct.change, aes(xval.inc, start.bin))+geom_segment(aes(xend = xval.inc, yend = end.bin-25, size = pct_inc/2),
                                                                                 arrow = arrow(length = unit(1.5,"mm")), color = "#2166ac")+
  geom_point(data = ncell.pct.change, aes(xval.nochange, start.bin+25, size = pct_nochange/2), color = "#636363")+
  geom_segment(aes(x = xval.dec, y = end.bin-25, xend = xval.dec, yend = start.bin, size = pct_dec/2), arrow = arrow(length = unit(1.5,"mm")), color = "#b2182b")+
  scale_size(range = c(0, 2))+scale_x_discrete(labels=c("2" = "No Change", "1" = "Increasing",
                            "3" = "Decreasing"))+theme_bw()+ylim(0,600)+theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title  = element_blank(), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank() )  



# figure 2 new again:
inset1 <- ncell.change.plot

# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob1 <- ggplotGrob(inset1)

clust.hist.inset.full <- clust.hist.full+ylim(0,4500)+ scale_y_continuous(breaks=c(0,1000,2000), limits = c(0,6100))+theme(plot.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ annotation_custom(grob = xbp_grob1, ymin = 3500, ymax = 6620, xmin = -200, xmax = 655)#+ annotation_custom(grob = xbp_grob, ymin = 750, ymax = 1100, xmin = -141, xmax = 648)

#-----------------------put together figure 1---------------------------

# figure 2 new again:
inset2 <- ggboxplot( data= full.fia.surveys, x = 'INVYRcd',y = 'FIAdensity', merge=TRUE,width = 0.5, fill = "INVYRcd",  palette =c("grey28", "grey40", "grey60"), outlier.size = 0.000005, outlier.colour = "grey")+ylim(0,600) +theme_bw(base_size = 8)+theme(axis.title = element_blank(),axis.ticks.y= element_blank(), axis.text.y= element_blank(),legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob2 <- ggplotGrob(inset2)
clust.hist.fia.full <- clust.hist.fia.full+ylim(0,6100)

f.clust.hist.inset.full <- clust.hist.fia.full+ scale_y_continuous(breaks=c(0,250,500,750))+theme(plot.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ annotation_custom(grob = xbp_grob2, ymin = 700, ymax = 1100, xmin = -140, xmax = 648)#+ annotation_custom(grob = xbp_grob, ymin = 750, ymax = 1100, xmin = -141, xmax = 648)
f.clust.hist.inset.full <- clust.hist.fia.full+ scale_y_continuous(breaks=c(0,1000,2000,3000, 4000), limits = c(0,6100))+theme(plot.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ annotation_custom(grob = xbp_grob2, ymin = 4500, ymax = 6600, xmin = -140, xmax = 648)#+ annotation_custom(grob = xbp_grob, ymin = 750, ymax = 1100, xmin = -141, xmax = 648)


dev.off()
dev.off()
dev.off()
dev.off()
dev.off()
dev.off()
dev.off()


png(height = 8.4, width = 4, units = 'in', res = 300, "outputs/paper_figs_unc/fig1_6panel_trans_arrow_inset.png")
plot_grid(pls.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "A", size = 3), 
          FIA.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "D", size = 3),
          pls.clust+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "B", size = 3),
          fia.clust+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "E", size = 3), 
          clust.hist.inset.full + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5))+ annotate("text", x=600, y=20,label= "C", size = 3),
          #hist.inset+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)) + annotate("text", x=600, y=20,label= "F", size = 3), 
          
          f.clust.hist.inset.full+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)) + annotate("text", x=600, y=20,label= "F", size = 3), 
          ncol = 2, align = "h", axis="tb", scale = 1) 
dev.off()

png(height = 8.4, width = 4, units = 'in', res = 300, "outputs/paper_figs_unc/fig1_6panel_trans_arrow_inset.png")
plot_grid(
plot_grid(pls.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "A", size = 3), 
          FIA.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "D", size = 3), ncol = 2, align = "h", axis = "tb", rel_widths = c(1,1)),
          
plot_grid(pls.clust.both+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "B", size = 3),
          fia.clust.both+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "E", size = 3), ncol = 2, align = "h", axis = "tb", rel_widths = c(1,1)),

plot_grid( clust.hist.full.both.no.aspect + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5))+ annotate("text", x=600, y=20,label= "C", size = 3)+ xlab("Tree Density (stems/ha)"),
           ncell.change.plot+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.text.x =element_text(size = 5)+ annotate("text", x=1, y=600,label= "F", size = 3) ),
          clust.hist.fia.full.both.no.aspect+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)) + annotate("text", x=600, y=20,label= "G", size = 3),
          inset2 + annotate("text", x=1, y=600,label= "H", size = 3),
          ncol = 4, align = "h", axis = "tb", rel_widths = c(1,0.5, 1, 0.5)), 
          
ncol = 1, align = "h", axis="tb", scale = 1) 
dev.off()


png(height = 8.4, width = 4, units = 'in', res = 300, "outputs/paper_figs_unc/fig1_6panel_trans_arrow_inset_pct.png")
plot_grid(
  plot_grid(pls.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "A", size = 3), 
            FIA.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "B", size = 3), ncol = 2, align = "h", axis = "tb", rel_widths = c(1,1)),
  
  plot_grid(pls.clust.both+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "C", size = 3),
            fia.clust.both+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "D", size = 3), ncol = 2, align = "h", axis = "tb", rel_widths = c(1,1)),
  
  plot_grid( clust.hist.full.both.no.aspect + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5))+ annotate("text", x=600, y=100,label= "E", size = 3)+ xlab("Tree Density (stems/ha)"),
             ncell.pct.change.plot+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.text.x =element_text(size = 5) ) + annotate("text", x=1, y=600,label= "F", size = 3),
             clust.hist.fia.full.both.no.aspect+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5), axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank()) + annotate("text", x=600, y=400,label= "G", size = 3),
             inset2+ annotate("text", x=1, y=600,label= "H", size = 3),
             ncol = 4, align = "h", axis = "tb", rel_widths = c(1,0.5, 1, 0.5)), 
  
  ncol = 1, align = "h", axis="tb", scale = 1) 
dev.off()


# horizonatal figure 1:

png(height = 6, width = 10, units = 'in', res = 300, "outputs/paper_figs_unc/fig1_two_row_trans_arrow_inset_pct.png")
plot_grid(
  plot_grid(pls.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "A", size = 3), 
            pls.clust.both+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "B", size = 3),
            FIA.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "C", size = 3),
            fia.clust.both+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "D", size = 3), ncol = 4, align = "h", axis = "tb", rel_widths = c(1,1, 1, 1)),
  
 
  plot_grid( clust.hist.full.both.no.aspect + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5))+ annotate("text", x=600, y=100,label= "E", size = 3)+ xlab("Tree Density (stems/ha)"),
             ncell.pct.change.plot+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.text.x =element_text(size = 5) ) + annotate("text", x=1, y=600,label= "F", size = 3),
             clust.hist.fia.full.both.no.aspect+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5), axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank()) + annotate("text", x=600, y=400,label= "G", size = 3),
             ncell.pct.change.1980.1990.plot+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.text.x =element_text(size = 5), axis.ticks.y = element_blank(), axis.text.y = element_blank(), title = element_blank() ) + annotate("text", x=1, y=600,label= "H", size = 3),
             ncell.pct.change.1990.2000.plot+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.text.x =element_text(size = 5) , axis.ticks.y = element_blank(), axis.text.y = element_blank(),title = element_blank()) + annotate("text", x=1, y=600,label= "I", size = 3),
             inset2+ annotate("text", x=1, y=600,label= "J", size = 3),
             ncol = 6, align = "h", axis = "tb", rel_widths = c(1,0.5, 1, 0.5, 0.5, 0.5)), 
  
  ncol = 1, align = "h", axis="tb", scale = 1) 
dev.off()


png(height = 8.4, width = 4, units = 'in', res = 300, "outputs/paper_figs_unc/fig1_6panel_trans_arrow_inset_pct_nolabels.png")
plot_grid(
  plot_grid(pls.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)), 
            FIA.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) , ncol = 2, align = "h", axis = "tb", rel_widths = c(1,1)),
  
  plot_grid(pls.clust.both+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)),
            fia.clust.both+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) , ncol = 2, align = "h", axis = "tb", rel_widths = c(1,1)),
  
  plot_grid( clust.hist.full.both.no.aspect + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5))+ xlab("Tree Density (stems/ha)"),
             ncell.pct.change.plot+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.text.x =element_text(size = 5) ) ,
             clust.hist.fia.full.both.no.aspect+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)),
             inset2,
             ncol = 4, align = "h", axis = "tb", rel_widths = c(1,0.5, 1, 0.5)), 
  
  ncol = 1, align = "h", axis="tb", scale = 1) 
dev.off()

# make figure 1 but with grid cells with no fia plots masked out
png(height = 8.4, width = 4, units = 'in', res = 300, "outputs/paper_figs_unc/fig1_6panel_trans_arrow_inset_msk.png")
plot_grid(
  plot_grid(pls.map.alt.color.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "A", size = 3), 
            FIA.map.alt.color.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "B", size = 3), ncol = 2, align = "h", axis = "tb", rel_widths = c(1,1)),
  
  plot_grid(pls.clust.both.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "C", size = 3),
            fia.clust.both.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "D", size = 3), ncol = 2, align = "h", axis = "tb", rel_widths = c(1,1)),
  
  plot_grid( clust.hist.full.both.no.aspect.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5))+ annotate("text", x=600, y=75,label= "E", size = 3)+ylim(0,1000) + xlab("Tree Density (stems/ha)"),
             ncell.change.plot+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.text.x =element_text(size = 5) )+ annotate("text", x=1, y=600,label= "F", size = 3),
             clust.hist.fia.full.both.no.aspect.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text.x = element_text(size = 5), axis.title.x =  element_text(size = 5), axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) + annotate("text", x=600, y=100,label= "G", size = 3),
             inset2+ annotate("text", x=1, y=600,label= "H", size = 3),
             ncol = 4, align = "h", axis = "tb", rel_widths = c(1,0.5, 1, 0.5)), 
  
  ncol = 1, align = "h", axis="tb", scale = 1) 
dev.off()

png(height = 8.4, width = 4, units = 'in', res = 300, "outputs/paper_figs_unc/fig1_6panel_trans_arrow_inset_msk_pct_change.png")
plot_grid(
  plot_grid(pls.map.alt.color.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "A", size = 3), 
            FIA.map.alt.color.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "D", size = 3), ncol = 2, align = "h", axis = "tb", rel_widths = c(1,1)),
  
  plot_grid(pls.clust.both.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "B", size = 3),
            fia.clust.both.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "E", size = 3), ncol = 2, align = "h", axis = "tb", rel_widths = c(1,1)),
  
  plot_grid( clust.hist.full.both.no.aspect.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5))+ annotate("text", x=600, y=20,label= "C", size = 3),
             ncell.pct.change.plot+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.text.x =element_text(size = 5) ),
             clust.hist.fia.full.both.no.aspect.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)) + annotate("text", x=600, y=20,label= "F", size = 3),
             inset2,
             ncol = 4, align = "h", axis = "tb", rel_widths = c(1,0.5, 1, 0.5)), 
  
  ncol = 1, align = "h", axis="tb", scale = 1) 
dev.off()


png(height = 8.4, width = 4, units = 'in', res = 400, "outputs/paper_figs_unc/fig1_6panel_trans_arrow_inset_msk_pct_change_nolabels.png")
plot_grid(
  plot_grid(pls.map.alt.color.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)), 
            FIA.map.alt.color.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) , ncol = 2, align = "h", axis = "tb", rel_widths = c(1,1)),
  
  plot_grid(pls.clust.both.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) ,
            fia.clust.both.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)), ncol = 2, align = "h", axis = "tb", rel_widths = c(1,1)),
  
  plot_grid( clust.hist.full.both.no.aspect.msk +ylim(0,1000)+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)),
             ncell.pct.change.plot+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.text.x =element_text(size = 5) ),
             clust.hist.fia.full.both.no.aspect.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)) ,
             inset2,
             ncol = 4, align = "h", axis = "tb", rel_widths = c(1,0.5, 1, 0.5)), 
  
  ncol = 1, align = "h", axis="tb", scale = 1) 
dev.off()


# plot of the modern fia surveys next to pls and FIA histograms:
png(height = 5, width = 18, units = 'in', res = 300, "outputs/paper_figs_unc/pct_change_all_time_periods.png")
plot_grid( clust.hist.full.both.no.aspect.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5))+ annotate("text", x=600, y=20,label= "C", size = 3),
           ncell.pct.change.plot+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.text.x =element_text(size = 5) ),
           clust.hist.fia.full.both.no.aspect.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)) + annotate("text", x=600, y=20,label= "F", size = 3),
           ncell.pct.change.1980.1990.plot, 
           ncell.pct.change.1990.2000.plot,
           ncell.pct.change.1980.2000.plot,
           ncol = 6, align = "h", axis = "tb", rel_widths = c(0.75,0.5,0.75, 0.5, 0.5, 0.5))
dev.off()


png(height = 3, width = 6, units = 'in', res = 300, "outputs/paper_figs_unc/pct_change_fia_time_periods.png")
plot_grid( clust.hist.fia.full.both.no.aspect.msk  + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)) + annotate("text", x=600, y=20,label= "F", size = 3),
           ncell.pct.change.1980.1990.plot + ggtitle("") , 
           ncell.pct.change.1990.2000.plot+ ggtitle("") ,
           ncell.pct.change.1980.2000.plot+ ggtitle("") ,
           ncol = 4, align = "h", axis = "tb", rel_widths = c(1,1,1,1), labels = "AUTO")
dev.off()


# ------------------------------Plot figure 1, but with FIA values masked out-------------------------

# read in the 4 species clusters made from masked out data:

clust_4 <- read.csv("outputs/four_clust_fia_dissimilarity_stat_smooth.dens.msk.csv")

# merge the clusters and pls density data: 
clust_4 <- merge(clust_4 , dens, by = c("x", "y"))

# need to rename the clusters here (should go back and do it in the place where we originally make the clusters):
clust_4
library(plyr)
#clust_8$foresttype <- revalue(clust_5$speciescluster, c("Maple/Oak/Poplar/Ash"="Maple/Oak/Other", "Poplar/Pine/Cedar/Spruce"="Poplar", 
#                                                       "Oak/Maple/Elm/Poplar/Ash" = "Oak/Maple/Hickory", "Oak/Maple/Other/Hickory"
#                                                      "Pine/Poplar" = "Pine", "Spruce/Cedar/Tamarack/Poplar" = "Boreal/Sub-boreal", "Beech/Maple/Hemlock" = "Beech-Maple"))

#clust_8$orderedforesttype<- factor(clust_8$foresttype, c("Oak", "Pine", "Aspen", "N. Mixed Forest", "Boreal/Sub-boreal","Elm/Oak/Maple", "Oak-Hickory", "Beech-Maple"))

fia.clust.msk <- ggplot(clust_4, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c( '#003c30','#a6cee3','#f0027f', '#fdc086'), name = " ")+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 8)+ theme(legend.position=c(0.20, 0.18),legend.background = element_rect(fill=alpha('transparent', 0)) ,
                                                                                                                           axis.line=element_blank(),legend.key.size = unit(0.2,'lines'),legend.text=element_text(size=5),legend.key = element_rect(color = "black", linetype = "solid"),axis.text.x=element_blank(),
                                                                                                                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                           axis.title.x=element_blank(),
                                                                                                                           axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_equal()

fia.clust.msk
# merge clust_plot5 and dens.pr



# ----------------------- Figure 1D total FIA density histgram colored by species composition-----------------

dens.clust <- merge(dens, clust_4[,c("x" ,"y", "speciescluster")], by = c("x", "y"), all.x = TRUE)
#dens.clust$foresttype_ordered <- factor(dens.clust$foresttype, levels = rev(c("Boreal/Sub-boreal", "Pine", "Aspen",  "Elm/Oak/Maple","Oak-Hickory","Beech-Maple","N. Mixed Forest", "Oak")))
dens.clust.omit <- dens.clust[ !is.na(dens.clust$speciescluster),]
dens.clust <- dens.clust[!duplicated(dens.clust),]

myColors <- c('#f0027f'  ,'#003c30','#a6cee3',"#beaed4", '#fdc086')
#names(myColors) <- levels(dens.clust$foresttype_ordered)

clust.hist.fia.full.msk <- ggplot()+ geom_density(data = dens.clust[dens.clust$mean_dens_fia >= 0.5,], aes(mean_dens, 22 *..count..), linetype="dashed" , color = "darkgrey", bw = 12,size = 1.5)+ 
  geom_density(data = dens.clust.omit[dens.clust.omit$mean_dens_fia >= 0.5,], aes(mean_dens_fia, 22 *..count..), linetype="solid" , color = "black", bw = 12,size = 1.5)+
  geom_histogram(data = dens.clust.omit[dens.clust.omit$mean_dens_fia >= 0.5,], aes(mean_dens_fia, fill = speciescluster), binwidth =  20)+xlim(0,600)+
  scale_fill_manual(values = myColors, name = " ", drop = TRUE)+
  coord_flip()+xlab("FIA tree density")+ylab("# grid cells")+theme_bw(base_size = 8)+theme(aspect.ratio = 1,legend.position = c(0.44, 0.85),legend.background = element_rect(fill=alpha('transparent', 0)), legend.key.size = unit(0.4, "line"),legend.key = element_rect(color = "black", linetype ="solid"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
clust.hist.fia.full.msk

# get updated modern FIA surveys:

past.logged <- read.csv("data/FIA_plot_data/fia.by.cell.treated.2000_2017.csv")
past.survey <- read.csv("data/FIA_plot_data/fia.by.cell.out_1980_1990.csv")
past.survey$new_scale <- ifelse(past.survey$INVYRcd %in% "1990s", 775, 1000)

# get data frame w/ 1980s, 1990s, and modern survey estimates:
modern.fia <- dens.clust.omit[,c("x", "y", "cell", "mean_dens_fia")]
modern.fia$INVYRcd <- "2000s"
colnames(modern.fia) <- c("x", "y", "cell", "FIAdensity", "INVYRcd")

msk.fia.surveys <- rbind(modern.fia, past.survey[,c("x", "y", "cell", "FIAdensity", "INVYRcd")])

msk.fia.surveys$INVYRcd <- factor(msk.fia.surveys$INVYRcd, c("1980s", "1990s", "2000s"))
# D: Histogram colored by species cluster:
# make a histogram of denisty betwen -2.5 and 1 colored by species cluster:


library(ggpubr)
inset <- ggboxplot( data= msk.fia.surveys, x = 'INVYRcd',y = 'FIAdensity', merge=TRUE,width = 0.5, fill = "INVYRcd",  palette =c("grey28", "grey40", "grey60"), outlier.size = 0.0005)+ylim(0,600) +theme_bw()+theme(axis.title = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(), legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1))

# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(inset)

# Place box plots inside the histogram plot
msk.fia.surveys$INV_place <- ifelse(msk.fia.surveys$INVYRcd %in% "2000s", 1000, 
                                     ifelse(msk.fia.surveys$INVYRcd %in% "1990s", 900,
                                            ifelse(msk.fia.surveys$INVYRcd %in% "1980s", 800, NA )))

# figure 2 new again:
inset2 <- ggboxplot( data= msk.fia.surveys, x = 'INVYRcd',y = 'FIAdensity', merge=TRUE,width = 0.5, fill = "INVYRcd",  palette =c("grey28", "grey40", "grey60"), outlier.size = 0.0005)+ylim(0,600) +theme_bw(base_size = 8)+theme(axis.title = element_blank(),axis.ticks.y= element_blank(), axis.text.y= element_blank(),legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob2 <- ggplotGrob(inset2)
clust.hist.fia.full <- clust.hist.fia.full+ylim(0,6100)

f.clust.hist.inset.msk <- clust.hist.fia.full.msk+ scale_y_continuous(breaks=c(0,250,500,750))+theme(plot.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ annotation_custom(grob = xbp_grob2, ymin = 700, ymax = 1100, xmin = -140, xmax = 648)#+ annotation_custom(grob = xbp_grob, ymin = 750, ymax = 1100, xmin = -141, xmax = 648)
f.clust.hist.inset.msk <- clust.hist.fia.full.msk+ scale_y_continuous(breaks=c(0,1000,2000,3000, 4000), limits = c(0,6100))+theme(plot.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ annotation_custom(grob = xbp_grob2, ymin = 4500, ymax = 6600, xmin = -140, xmax = 648)#+ annotation_custom(grob = xbp_grob, ymin = 750, ymax = 1100, xmin = -141, xmax = 648)


png(height = 8.4, width = 4, units = 'in', res = 300, "outputs/paper_figs_unc/fig1_6panel_trans_msk_fia.png")
plot_grid(pls.map.alt.color + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "A", size = 3), 
          FIA.map.alt.color.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "D", size = 3),
          pls.clust+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "B", size = 3),
          fia.clust.msk+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "E", size = 3), 
          clust.hist.full + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5))+ annotate("text", x=600, y=20,label= "C", size = 3),
          #hist.inset+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)) + annotate("text", x=600, y=20,label= "F", size = 3), 
          
          f.clust.hist.inset.msk+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)) + annotate("text", x=600, y=20,label= "F", size = 3), 
          ncol = 2, align = "h", axis="tb", scale = 1) 
dev.off()


# --------------------------Plot figure 1, but with Both FIA and PLS values masked out----------------


png(height = 8.4, width = 4, units = 'in', res = 300, "outputs/paper_figs_unc/fig1_6panel_trans_msk_fia_pls.png")
plot_grid(pls.map.alt.color.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "A", size = 3), 
          FIA.map.alt.color.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "D", size = 3),
          pls.clust.msk+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "B", size = 3),
          fia.clust.msk+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA)) + annotate("text", x=-90000, y=1486000,label= "E", size = 3), 
          clust.hist.full.msk + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5))+ annotate("text", x=600, y=20,label= "C", size = 3),
          #hist.inset+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)) + annotate("text", x=600, y=20,label= "F", size = 3), 
          
          f.clust.hist.inset.msk+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.background=element_rect(fill=NA, colour=NA), axis.text = element_text(size = 5), axis.title =  element_text(size = 5)) + annotate("text", x=600, y=20,label= "F", size = 3), 
          ncol = 2, align = "h", axis="tb", scale = 1) 
dev.off()


# <<<<<<<<<<<<<<< map bimodality evaluated on samples from  esimates: >>>>>>>>>>>>>>>>>>>>>>>>>>>>

# map bimodal based on PC1:
# out.df <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_PC1fia_stat.rds")
# pvalues <- out.df  %>% group_by(PC1fia_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
#                                                                median.p = median(pvalue, na.rm = TRUE),
#                                                                ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
#                                                                ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
#                                                                mean.d = mean(dip, na.rm = TRUE),
#                                                                median.d = median(dip, na.rm = TRUE),
#                                                                ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
#                                                                ci.high.d = quantile(dip, 0.975, na.rm = TRUE)) 
#                                                                
# 
# 
# dens$PC1fia_bins <- cut(dens$PC1fia, breaks=seq(-5.5, 4.5, by = 0.25))
# 
# # also make density bins:
# FIA.df <- dens
# 
# ordered.cuts <- data.frame(PC1fia_bins = unique(cut(FIA.df[order(FIA.df$PC1fia),]$PC1fia, breaks=seq(-5.5, 4.5, by = 0.25))),
#                            mids = seq(-5.375, 4.5, by = 0.25))
# pvalues <- left_join(ordered.cuts, pvalues, by = "PC1fia_bins")
# bimod.pc.FIA <- left_join(dens, pvalues, by = c("PC1fia_bins"))
# 
# 
# # if there are too few grid cells (<50), then we won't evaluature the bimodality
# bin.counts <- bimod.pc.FIA %>% group_by(PC1fia_bins) %>% dplyr::summarise(ncells_f_pc1 = length(mean.p))
# bimod.pc.FIA <- merge(bimod.pc.FIA, bin.counts, by = "PC1fia_bins")
# 
# # merge with the envt + pc data
# bimod.pc.FIA$bimclass_lowsamp <- ifelse(bimod.pc.FIA$ncells_f_pc1 <= 50, "low-sample", "okay")
# bimod.pc.FIA$bimclass <- ifelse(bimod.pc.FIA$mean.p <= 0.05 & bimod.pc.FIA$bimclass_lowsamp %in% "okay", "bimodal", "unimodal")
# 
# bimod.pc.FIA.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
#   geom_raster(data=bimod.pc.FIA, aes(x=x, y=y, fill = bimclass))+
#   geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
#   labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal"='#d73027',"unimodal" = '#4575b4', "low sample" = "tan"
#   )) +
#   coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
#                                               panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")
# 
# 
# 
# pvalues <- merge(pvalues, bin.counts, by = "PC1fia_bins")
# pvalues$lowsamp <- ifelse(pvalues$ncells_f_pc1 <= 50, "low-sample", "okay")
# 
# write.csv(pvalues,"outputs/n_cells_in_pc1_fia_bins.csv", row.names = FALSE)# save bin.counts to use in predicting the future:
# 
# 
# pc1.dip.FIA <- ggplot(pvalues[ pvalues$lowsamp %in% "okay",], aes(mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("FIA PC1")+ylab("DIP value")+xlim(-6.4, 4.5)
# pc1.pval.FIA <- ggplot(pvalues[ pvalues$lowsamp %in% "okay",], aes(mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("FIA PC1")+ylab("P value")+xlim(-6.4, 4.5)
# 
# 
# png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/FIA_dip_pvalues_unc_pc1.png")
# plot_grid(pc1.dip.FIA, pc1.pval.FIA)
# dev.off()
# 
# # pased on P-PET:
# 
# out.df <- readRDS("outputs/bimodal_bins_p_value_dipP_FIA_PPET_stat.rds")
# pvalues <- out.df  %>% group_by(ppet_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
#                                                                 median.p = median(pvalue, na.rm = TRUE),
#                                                                 ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
#                                                                 ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
#                                                                 mean.d = mean(dip, na.rm = TRUE),
#                                                                 median.d = median(dip, na.rm = TRUE),
#                                                                 ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
#                                                                 ci.high.d = quantile(dip, 0.975, na.rm = TRUE))
# 
# 
# dens$ppet_binsfia <- cut(dens$GS_ppet_mod, breaks=seq(-170, 310, by = 15))
# 
# ordered.cuts <- data.frame(ppet_bins = levels(cut(FIA.df[order(FIA.df$GS_ppet_mod),]$GS_ppet, breaks=seq(-170, 310, by = 15))),
#                            ppet_mids = seq(-167.5, 310, by = 15))
# pvalues <- left_join(ordered.cuts, pvalues, by = "ppet_bins")
# 
# bimod.ppet.FIA <- merge(dens, pvalues, by.x = c("ppet_binsfia"), by.y = c("ppet_bins"))
# # if there are too few grid cells (<50), then we won't evaluature the bimodality
# bin.counts <- bimod.ppet.FIA %>% group_by(ppet_binsfia) %>% dplyr::summarise(ncells_f_ppet = length(mean.p))
# bimod.ppet.FIA <- merge(bimod.ppet.FIA, bin.counts, by = "ppet_binsfia")
# 
# # merge with the envt + pc data
# bimod.ppet.FIA$bimclass_ppet_lowsamp <- ifelse(bimod.ppet.FIA$ncells_f_ppet <= 50, "low-sample", "okay")
# bimod.ppet.FIA$bimclass_ppet <- ifelse(bimod.ppet.FIA$mean.p <= 0.05 &bimod.ppet.FIA$bimclass_ppet_lowsamp %in% "okay", "bimodal", "unimodal")
# 
# 
# # merge with the envt + pc data
# 
# bimod.ppet.FIA.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
#   geom_raster(data=bimod.ppet.FIA, aes(x=x, y=y, fill = bimclass_ppet))+
#   geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
#   labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal" = '#d73027', "unimodal" = '#4575b4', "low sample" = "tan"
#   )) +
#   coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
#                                               panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")
# 
# 
# pvalues <- merge(pvalues, bin.counts, by.x = "ppet_bins", by.y = "ppet_binsfia")
# pvalues$lowsamp <- ifelse(pvalues$ncells_f_ppet <= 50, "low-sample", "okay")
# 
# write.csv(pvalues,"outputs/n_cells_in_ppet_fia_bins.csv", row.names = FALSE)# save bin.counts to use in predicting the future:
# 
# ppet.dip.FIA <- ggplot(pvalues[ pvalues$lowsamp %in% "okay",], aes(ppet_mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("FIA P-PET")+ylab("DIP value")+xlim(-200, 310)
# ppet.pval.FIA <- ggplot(pvalues[ pvalues$lowsamp %in% "okay",], aes(ppet_mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("FIA P-PET")+ylab("P value")+xlim(-200, 310)
# 
# 
# png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/FIA_dip_pvalues_unc_ppet.png")
# plot_grid(ppet.dip.FIA, ppet.pval.FIA)
# dev.off()
# 
# # based on soil moisture estimates:
# out.df <- readRDS("outputs/bimodal_bins_p_value_dipP_FIA_soil_15bins_kde_stat.rds")
# 
# pvalues <- out.df  %>% group_by(soil_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
#                                                                 median.p = median(pvalue, na.rm = TRUE),
#                                                                 ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
#                                                                 ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
#                                                                 mean.d = mean(dip, na.rm = TRUE),
#                                                                 median.d = median(dip, na.rm = TRUE),
#                                                                 ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
#                                                                 ci.high.d = quantile(dip, 0.975, na.rm = TRUE))
# 
# 
# dens$soil_binsfia <- cut(dens$mean_GS_soil_m, breaks=seq(0, 1.8, by = 0.05))
# 
# ordered.cuts <- data.frame(soil_bins = levels(unique(cut(FIA.df[order(FIA.df$mean_GS_soil_m),]$mean_GS_soil_m, breaks=seq(0, 1.8, by = 0.05)))),
#                            mids = seq(0.025, 1.8, by = 0.05))
# pvalues <- left_join(ordered.cuts, pvalues, by = "soil_bins")
# 
# bimod.sm.FIA <- merge(dens, pvalues, by.x = "soil_binsfia", by.y = c("soil_bins"), all.x = TRUE)
# 
# # if there are too few grid cells (<50), then we won't evaluature the bimodality
# bin.counts <- bimod.sm.FIA %>% group_by(soil_binsfia) %>% dplyr::summarise(ncells_f_soil = length(mean.p))
# bimod.sm.FIA <- merge(bimod.sm.FIA, bin.counts, by.x = "soil_binsfia")
# 
# # merge with the envt + pc data
# bimod.sm.FIA$bimclass_soil_lowsamp <- ifelse(bimod.sm.FIA$ncells_f_soil <= 200, "low-sample", "okay")
# bimod.sm.FIA$bimclass_soil_f <- ifelse(bimod.sm.FIA$mean.p <= 0.05 & bimod.sm.FIA$bimclass_soil_lowsamp %in% "okay", "bimodal", "unimodal")
# 
# ggplot(bimod.sm.FIA, aes(x,y, fill = soil_binsfia))+geom_raster()
# 
# # merge with the envt + pc data
# 
# 
# bimod.sm.FIA.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
#   geom_raster(data=bimod.sm.FIA, aes(x=x, y=y, fill = bimclass_soil_f))+
#   geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
#   labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal" = '#4575b4', "low sample" = "tan"
#   )) +
#   coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
#                                               panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")
# 
# 
# # omit stats for low sample areas"
# pvalues <- merge(pvalues, bin.counts, by.x = "soil_bins", by.y = "soil_binsfia")
# pvalues$lowsamp <- ifelse(pvalues$ncells_f_soil <= 50, "low-sample", "okay")
# 
# write.csv(pvalues,"outputs/n_cells_in_soil_fia_bins.csv", row.names = FALSE)# save bin.counts to use in predicting the future:
# 
# soil.dip.FIA <- ggplot(pvalues[ pvalues$lowsamp %in% "okay",], aes(mids, mean.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("FIA Soil moisture")+ylab("DIP value")
# soil.pval.FIA <- ggplot(pvalues[ pvalues$lowsamp %in% "okay",], aes(mids, mean.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("FIA Soil moisture")+ylab("P value")
# 
# 
# png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/FIA_dip_pvalues_unc_ppet.png")
# plot_grid(soil.dip.FIA, soil.pval.FIA)
# dev.off()
# 
# 
# png(height = 10, width = 10, units = "in", res = 300, "outputs/paper_figs_unc/bimodal_maps_with_pvalues_FIA.png")
# plot_grid(bimod.pc.FIA.map, bimod.ppet.FIA.map, bimod.sm.FIA.map,
#           pc1_unc_fia, ppet_unc_fia, soil_unc_fia,
#           pc1.dip.FIA + ylim(0,0.1), ppet.dip.FIA+ylim(0,0.1), soil.dip.FIA+ylim(0,0.1),
#           pc1.pval.FIA, ppet.pval.FIA, soil.pval.FIA, 
#           ncol = 3, rel_heights = c(1,1,0.5, 0.5))
# dev.off()
# 
# 


# -------------------------Make FIA & PLS histogram plots + uncertainty-------------------
# png("outputs/paper_figs_unc/mean_density_histogram_pls_fia.png")
# ggplot()+geom_histogram(data = dens, aes(mean_dens_fia), bw = 25, fill = "red", alpha = 0.75)+geom_histogram(data = dens, aes(mean_dens), bw = 25,fill = "blue", alpha = 0.75)+xlim(0, 600)+xlab("Density (trees/ha)")
# dev.off()
# 
# 
# # want to plot the total number of grid cells in each density class
# # then want to plot the minimum number and maximum number of grid cells in each density class
# # need to define the breaks for mean_density, ci.low, and ci.high
# 
# 
# 
# # cut den into pls density bins:
# # also make density bins:
# dens$dens.bins <- cut(dens$mean_dens, breaks=seq(0, 775, by = 25))
# dens$dens.bins.low <- cut(dens$ci.low_dens, breaks=seq(0, 775, by = 25))
# dens$dens.bins.high <- cut(dens$ci.high_dens, breaks=seq(0, 775, by = 25))
# 
# pls <- dens %>% count(dens.bins)
# colnames(pls) <- c("bins", "pls")
# 
# pls.low <- dens %>% count(dens.bins.low)
# colnames(pls.low) <- c("bins", "pls.low")
# 
# pls.high <- dens %>% count(dens.bins.high)
# colnames(pls.high) <- c("bins", "pls.high")
# count.ci <- merge(pls, pls.low, by = "bins")
# count.ci <- merge(count.ci, pls.high, by = "bins")
# 
# ordered.bins <- data.frame(bins = levels(cut(dens[order(dens$mean_dens),]$mean_dens, breaks=seq(0, 775, by = 25))),
#                            mids = seq(13.5, 775, by = 25))
# 
# count.ci <- merge(count.ci, ordered.bins, by = "bins")
# ggplot(count.ci, aes(bins, pls))+geom_bar(stat = "identity")+geom_errorbar(data = count.ci, aes(ymin=pls.low, ymax=pls.high),width=1)
# 
# ggplot()+geom_ribbon(data = count.ci, aes(x=mids, ymin=pls.low, ymax=pls.high),fill="darkgrey", alpha=0.9)
# 

# >>>>>>>>>>>>>>>>>>>>>>>.new figures with 2d density plots: <<<<<<<<<<<<<<<<<<<<<<<
pls.df <- dens
library(ks)
library(ggplotify)
# for PC1:
H <- Hpi.diag(x=na.omit(cbind(pls.df$PC1, pls.df$mean_dens)) )
fhat <- kde(x=na.omit(cbind(pls.df$PC1, pls.df$mean_dens)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550))
#points(na.omit(cbind(pls.df$PC1, pls.df$mean_dens)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)


pc.pls.mix <- read.csv( "outputs/mixture_model/pls_pc1_mixture_mode_estimates.csv")
mid.summary.pc1 <- pc.pls.mix  %>% group_by(mode, pc1_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                  ci.low = quantile(mean_dens,0.025),
                                                                                                  ci.high = quantile(mean_dens, 0.975), 
                                                                                       ncell = length(mean_dens))

mid.summary.pc1.quants <- pc.pls.mix  %>% group_by(mode, pc1_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                       ci.low = quantile(mean_dens,0.025),
                                                                                       ci.high = quantile(mean_dens, 0.975),
                                                                                       ci.80 = quantile(mean_dens, 0.8),
                                                                                       ci.20 = quantile(mean_dens, 0.2),
                                                                                       ci.70 = quantile(mean_dens, 0.7),
                                                                                       ci.30 = quantile(mean_dens, 0.3),
                                                                                       ci.60 = quantile(mean_dens, 0.6),
                                                                                       ci.40 = quantile(mean_dens, 0.4),
                                                                                       ncell = length(mean_dens))



ncell <- mid.summary.pc1 %>% dplyr::select(mode, mids, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)











low_ci <- mid.summary.pc1 %>%dplyr::select(mode, mids, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.pc1 %>%dplyr::select(mode, mids,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c("high_Forest", "high_Savanna")


merged.pc1 <- merge(low_ci, high_ci, by = c("pc1_bins", "mids"))
merged.pc1 <- merge(merged.pc1, ncell, by = c("pc1_bins", "mids"))

merged.pc1$bimodal <- ifelse(merged.pc1$low_Forest > merged.pc1$high_Savanna & merged.pc1$`Savanna` > 50 & merged.pc1$`Forest` > 50,"bimodal", "NS")

merged.pc1[is.na(merged.pc1$bimodal), ]$bimodal <- "One mode"
merged.pc1$y <- -41


mid.summary.lowprob <- pc.pls.mix %>% group_by(prob >= 0.49 &  prob <= 0.51, pc1_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                                                ci.low = quantile(mean_dens,0.025),
                                                                                                                                ci.high = quantile(mean_dens, 0.975))


hysteresis.pc1.pls.quants <- ggplot(data = data.frame(mid.summary.pc1), aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.pc1, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  geom_ribbon(data = mid.summary.pc1.quants, aes(ymin = ci.20, ymax = ci.80, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  geom_ribbon(data = mid.summary.pc1.quants, aes(ymin = ci.30, ymax = ci.70, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  geom_ribbon(data = mid.summary.pc1.quants, aes(ymin = ci.40, ymax = ci.60, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  theme_bw()+scale_fill_manual(values = c('#005a32', '#8c510a'))+scale_color_manual(values = c('#005a32', '#8c510a'))+geom_smooth(data = mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 &  prob <= 0.51` %in% T,], aes(mids, mean), color = "black", linetype = "dashed",se = FALSE)+
  ylab("Mean Tree Density (stems/ha)")+xlab("growing season P-PET")+theme(panel.grid.major = element_blank())

# get bimod.pc.pls from previous code
pc1.bim.line <- bimod.pc.pls[,c("pc1_bins", "mids","ncells_pc1", "mean.p", "mean.d", "bimclass_lowsamp", "bimclass")]
#pc1.bim.line <- bimod.ppet.pls[,c("ppet_bins", "mids","ncells_ppet", "mean.p", "mean.d", "bimclass_lowsamp", "bimclass_ppet")]
pc1.bim.line <- pc1.bim.line[!duplicated(pc1.bim.line),]
pc1.bim.line$y <- -41


library(base2grob)

smoothingSpline.Forest = smooth.spline(mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mean, spar=.75)
plot(mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Savanna = smooth.spline(mid.summary.pc1[mid.summary.pc1$mode %in% "Savanna", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Savanna", ]$mean, spar=.75)
plot(mid.summary.pc1[mid.summary.pc1$mode %in% "Savanna", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Savanna", ]$mean)
lines(smoothingSpline.Savanna, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)


pls.kde.plot.pc1.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7 ) + points(pc1.bim.line[pc1.bim.line$bimclass %in% "bimodal",]$mids , pc1.bim.line[pc1.bim.line$bimclass %in% "bimodal",]$y, col = "darkblue", pch = 15, cex = 1)+
                                    text(-5, 500, "A"))

pls.kde.plot.pc1.gg.lines <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7 ) + points(merged.pc1[merged.pc1$bimodal %in% "bimodal",]$mids , merged.pc1[merged.pc1$bimodal %in% "bimodal",]$y, col = "darkblue", pch = 15, cex = 1)+
                                         lines(smoothingSpline.Savanna, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen")+  text(-5, 500, "A"))

pls.kde.plot.pc1.gg.lines

# plot with the equal spline on it
pls.kde.plot.pc1.gg.lines.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), xlim = c(-6, 5.5), cex.axis = 0.7) + points(data = merged.pc1[merged.pc1$bimodal %in% "bimodal",], y~mids, cex = 1,  pch = 15,col = "darkblue")+
                                              lines(smoothingSpline.Savanna, lwd = 2, col = "black")+  lines(smoothingSpline.Forest, lwd = 2, col = "black") +lines(smoothingSpline.Equal, lwd = 2, col = "grey", lty = 2)+  text(-5, 500, "A"))
pls.kde.plot.pc1.gg.lines.hys



# ----------------for P-PET:
H <- Hpi.diag(x=na.omit(cbind(pls.df$GS_ppet, pls.df$mean_dens)) )
fhat <- kde(x=na.omit(cbind(pls.df$GS_ppet, pls.df$mean_dens)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,75,80,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(0,550))
#points(na.omit(cbind(pls.df$PC1, pls.df$mean_dens)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
pls.kde.plot.ppet <- recordPlot()
pls.kde.plot.ppet.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(0,550), xlim = c(-200, 300)))


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)


# get bimod.pc.pls from previous code
ppet.bim.line <- bimod.ppet.pls[,c("ppet_bins", "mids","ncells_ppet", "mean.p", "mean.d", "bimclass_lowsamp", "bimclass_ppet")]
ppet.bim.line <- ppet.bim.line[!duplicated(ppet.bim.line),]
ppet.bim.line$y <- -41

# ggplotify the kde plots here:
pls.kde.plot.ppet.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim = c(-200, 300), cex.axis = 0.7) + points(data = ppet.bim.line[ppet.bim.line$bimclass_ppet %in% "bimodal",], y~mids, cex = 1,  pch = 15,col = "darkblue") + text(-170,500, "B"))
pls.kde.plot.ppet.gg


ppet.pls.mix <- read.csv( "outputs/mixture_model/pls_ppet_mixture_mode_estimates.csv")
mid.summary.ppet <- ppet.pls.mix %>% group_by(mode, GS_ppet_bins, mids_ppet) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                      ci.low = quantile(mean_dens,0.025),
                                                                                      ci.high = quantile(mean_dens, 0.975), 
                                                                                      ncell = length(mean_dens))
mid.summary.ppet.quants <- ppet.pls.mix  %>% group_by(mode, GS_ppet_bins, mids_ppet) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                              ci.low = quantile(mean_dens,0.025),
                                                                                              ci.high = quantile(mean_dens, 0.975),
                                                                                              ci.80 = quantile(mean_dens, 0.8),
                                                                                              ci.20 = quantile(mean_dens, 0.2),
                                                                                              ci.70 = quantile(mean_dens, 0.7),
                                                                                              ci.30 = quantile(mean_dens, 0.3),
                                                                                              ci.60 = quantile(mean_dens, 0.6),
                                                                                              ci.40 = quantile(mean_dens, 0.4),
                                                                                              ncell = length(mean_dens))

ncell <- mid.summary.ppet %>%dplyr::select(mode, mids_ppet, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)







hysteresis.ppet.pls.quants <- ggplot(data = data.frame(mid.summary.ppet), aes(mids_ppet, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.ppet, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  geom_ribbon(data = mid.summary.ppet.quants, aes(ymin = ci.20, ymax = ci.80, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  geom_ribbon(data = mid.summary.ppet.quants, aes(ymin = ci.30, ymax = ci.70, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  geom_ribbon(data = mid.summary.ppet.quants, aes(ymin = ci.40, ymax = ci.60, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  theme_bw()+scale_fill_manual(values = c('#005a32', '#8c510a'))+scale_color_manual(values = c('#005a32', '#8c510a'))+geom_smooth(data = mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 &  prob <= 0.51` %in% T,], aes(mids_ppet, mean), color = "black", linetype = "dashed",se = FALSE)+
  ylab("Mean Tree Density (stems/ha)")+xlab("growing season P-PET")+theme(panel.grid.major = element_blank())

low_ci <- mid.summary.ppet %>%dplyr::select(mode,mids_ppet, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.ppet %>%dplyr::select(mode, mids_ppet,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c("high_Forest", "high_Savanna")

merged.ppet <- merge(low_ci, high_ci, by = c("GS_ppet_bins", "mids_ppet"))
merged.ppet <- merge(merged.ppet, ncell, by = c("GS_ppet_bins", "mids_ppet"))

merged.ppet$bimodal <- ifelse(merged.ppet$low_Forest > merged.ppet$high_Savanna & merged.ppet$`Savanna` > 50 & merged.ppet$`Forest` > 50,"bimodal", "NS")

merged.ppet[is.na(merged.ppet$bimodal), ]$bimodal <- "One mode"
merged.ppet$y <- -41

# get summary to make lines
mid.summary.lowprob <- ppet.pls.mix %>% group_by(prob_ppet >= 0.499 & prob_ppet <= 0.509, GS_ppet_bins, mids_ppet) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                                  ci.low = quantile(mean_dens,0.025),
                                                                                                                  ci.high = quantile(mean_dens, 0.975))




smoothingSpline.Forest = smooth.spline(mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mids_ppet, mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mean, spar=.75)
plot(mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mids_ppet, mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Savanna = smooth.spline(mid.summary.ppet[mid.summary.ppet$mode %in% "Savanna", ]$mids_ppet, mid.summary.ppet[mid.summary.ppet$mode %in% "Savanna", ]$mean, spar=.75)
plot(mid.summary.ppet[mid.summary.ppet$mode %in% "Savanna", ]$mids_ppet, mid.summary.ppet[mid.summary.ppet$mode %in% "Savanna", ]$mean)
lines(smoothingSpline.Savanna, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob_ppet >= 0.499 & prob_ppet <= 0.509` %in% T & mid.summary.lowprob$ci.high < 400, ]$mids_ppet, mid.summary.lowprob[mid.summary.lowprob$`prob_ppet >= 0.499 & prob_ppet <= 0.509` %in% T & mid.summary.lowprob$ci.high < 400,, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob_ppet >= 0.49 &  prob_ppet <= 0.51` %in% T, ]$mids_ppet, mid.summary.lowprob[mid.summary.lowprob$`prob_ppet >= 0.49 &  prob_ppet <= 0.51` %in% T, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)


pls.kde.plot.ppet.gg.lines <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim = c(-200, 300), cex.axis = 0.7) + points(data = merged.ppet[merged.ppet$bimodal %in% "bimodal",], y~mids_ppet, cex = 1,  pch = 15,col = "darkblue")+
                                          lines(smoothingSpline.Savanna, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen")+ text(-170,500, "B"))
pls.kde.plot.ppet.gg.lines

# plot with the equal spline on it
pls.kde.plot.ppet.gg.lines.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim = c(-200, 300), cex.axis = 0.7) + points(data = merged.ppet[merged.ppet$bimodal %in% "bimodal",], y~mids_ppet, cex = 1,  pch = 15,col = "darkblue")+
                                          lines(smoothingSpline.Savanna, lwd = 2, col = "black")+  lines(smoothingSpline.Forest, lwd = 2, col = "black") +lines(smoothingSpline.Equal, lwd = 2, col = "grey", lty = 2)+ text(-170,500, "B"))
pls.kde.plot.ppet.gg.lines.hys

#------- for soil moisture/bucket model

H <- Hpi.diag(x=na.omit(cbind(pls.df$mean_GS_soil, pls.df$mean_dens)) )
fhat <- kde(x=na.omit(cbind(pls.df$mean_GS_soil, pls.df$mean_dens)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,65,75,80,85,95), xlab = "Growing season soil", ylab = "Tree density", ylim = c(-40,550))
#points(na.omit(cbind(pls.df$PC1, pls.df$mean_dens)), cex=0.3, pch=16)

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)

# get bimod.pc.pls from previous code
soil.bim.line <- bimod.sm.pls[,c("soil_bins", "mids","ncells_soil", "mean.p", "mean.d", "bimclass_lowsamp", "bimclass_soil")]
soil.bim.line <- soil.bim.line[!duplicated(soil.bim.line),]
soil.bim.line$y <- -41
sm.bim.line <- soil.bim.line

# ggplotify the kde plots here:
pls.kde.plot.sm.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7) + points(data = sm.bim.line[sm.bim.line$bimclass_soil %in% "bimodal",], y~mids, cex = 1,  pch = 15,col = "darkblue") + text(0.1,500, "C"))
pls.kde.plot.sm.gg


soil.pls.mix <- read.csv( "outputs/mixture_model/pls_soil_mixture_mode_estimates.csv")
mid.summary.soil <- soil.pls.mix %>% group_by(mode, mean_GS_soil_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                  ci.low = quantile(mean_dens,0.025),
                                                                                                  ci.high = quantile(mean_dens, 0.975), 
                                                                                                  ncell = length(mean_dens) )

mid.summary.soil.quants <- soil.pls.mix  %>% group_by(mode, mean_GS_soil_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                          ci.low = quantile(mean_dens,0.025),
                                                                                                          ci.high = quantile(mean_dens, 0.975),
                                                                                                          ci.80 = quantile(mean_dens, 0.8),
                                                                                                          ci.20 = quantile(mean_dens, 0.2),
                                                                                                          ci.70 = quantile(mean_dens, 0.7),
                                                                                                          ci.30 = quantile(mean_dens, 0.3),
                                                                                                          ci.60 = quantile(mean_dens, 0.6),
                                                                                                          ci.40 = quantile(mean_dens, 0.4),
                                                                                                          ncell = length(mean_dens))


ncell <- mid.summary.soil %>%dplyr::select(mode, mids, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)





hysteresis.soil.pls.quants <- ggplot(data = data.frame(mid.summary.soil), aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.soil, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  geom_ribbon(data = mid.summary.soil.quants, aes(ymin = ci.20, ymax = ci.80, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  geom_ribbon(data = mid.summary.soil.quants, aes(ymin = ci.30, ymax = ci.70, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  geom_ribbon(data = mid.summary.soil.quants, aes(ymin = ci.40, ymax = ci.60, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  theme_bw()+scale_fill_manual(values = c('#005a32', '#8c510a'))+scale_color_manual(values = c('#005a32', '#8c510a'))+#geom_smooth(data = mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 &  prob <= 0.51` %in% T,], aes(mids, mean), color = "black", linetype = "dashed",se = FALSE)+
  ylab("Mean Tree Density (stems/ha)")+xlab("growing season P-PET")+theme(panel.grid.major = element_blank())


low_ci <- mid.summary.soil %>%dplyr::select(mode, mids, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.soil %>%dplyr::select(mode, mids,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c("high_Forest", "high_Savanna")

merged.soil <- merge(low_ci, high_ci, by = c("mean_GS_soil_bins", "mids"))
merged.soil <- merge(merged.soil, ncell, by = c("mean_GS_soil_bins", "mids"))

merged.soil$bimodal <- ifelse(merged.soil$low_Forest > merged.soil$high_Savanna & merged.soil$`Savanna` > 50 & merged.soil$`Forest` > 50,"bimodal", "NS")
merged.soil[is.na(merged.soil$bimodal), ]$bimodal <- "One mode"
merged.soil$y <- -41

mid.summary.lowprob <- soil.pls.mix %>% group_by(prob_soil >= 0.49 &  prob_soil <= 0.51, mean_GS_soil_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                                                       ci.low = quantile(mean_dens,0.025),
                                                                                                                                       ci.high = quantile(mean_dens, 0.975))




smoothingSpline.Forest = smooth.spline(mid.summary.soil[mid.summary.soil$mode %in% "Forest" , ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Forest" , ]$mean, spar=.75)
plot(mid.summary.soil[mid.summary.soil$mode %in% "Forest" , ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Forest" ,]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Savanna = smooth.spline(mid.summary.soil[mid.summary.soil$mode %in% "Savanna" , ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Savanna" , ]$mean, spar=.75)
plot(mid.summary.soil[mid.summary.soil$mode %in% "Savanna" ,]$mids,mid.summary.soil[mid.summary.soil$mode %in% "Forest" ,]$mean)
lines(smoothingSpline.Savanna, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 & prob_soil <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400 & mid.summary.lowprob$mean_GS_soil_bins %in% merged.soil[merged.soil$bimodal %in% "bimodal",]$mean_GS_soil_bins, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 & prob_soil <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400 & mid.summary.lowprob$mean_GS_soil_bins %in% merged.soil[merged.soil$bimodal %in% "bimodal",]$mean_GS_soil_bins, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 & prob_soil <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400 & mid.summary.lowprob$mean_GS_soil_bins %in% merged.soil[merged.soil$bimodal %in% "bimodal",]$mean_GS_soil_bins, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 & prob_soil <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400 & mid.summary.lowprob$mean_GS_soil_bins %in% merged.soil[merged.soil$bimodal %in% "bimodal",]$mean_GS_soil_bins, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)


pls.kde.plot.sm.gg.lines <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7) + points(data = merged.soil[merged.soil$bimodal %in% "bimodal",], y~mids, cex = 1,  pch = 15,col = "darkblue") +
                                      lines(smoothingSpline.Savanna, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen") + text(0.1,500, "C"))
pls.kde.plot.sm.gg.lines

# plot with the equal spline on it
pls.kde.plot.sm.gg.lines.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7) + points(data = merged.soil[merged.soil$bimodal %in% "bimodal",], y~mids, cex = 1,  pch = 15,col = "darkblue") +
                                              lines(smoothingSpline.Savanna, lwd = 2, col = "black")+  lines(smoothingSpline.Forest, lwd = 2, col = "black") +lines(smoothingSpline.Equal, lwd = 2, col = "grey", lty = 2) + text(0.1,500, "C"))
pls.kde.plot.sm.gg.lines.hys

# >>>>>>>>>>>>>>>>>>>>>>> FIA figures with 2d density plots: <<<<<<<<<<<<<<<<<<<<<<<
pls.df <- dens
library(ks)

# for PC1:
H <- Hpi.diag(x=na.omit(cbind(pls.df$PC1fia, pls.df$mean_dens_fia)) )
fhat <- kde(x=na.omit(cbind(pls.df$PC1fia, pls.df$mean_dens_fia)), H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density")
#points(na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["16%"])[[1]])
contour_95 <- data.frame(contour_95)



# get bimodal classifications from previos DF
pc1.f.bim.line <- bimod.pc.FIA[, c("PC1fia_bins","mids","ncells_f_pc1", "mean.p", "mean.d", "bimclass_lowsamp", "bimclass") ]
pc1.f.bim.line <- pc1.f.bim.line[!duplicated(pc1.f.bim.line),]
pc1.f.bim.line$y <- -41


pc1.fia.mix <- read.csv( "outputs/mixture_model/fia_pc1_mixture_mode_estimates.csv")
mid.summary.pc1 <- pc1.fia.mix %>% group_by(mode, pc1_bins_fia, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                  ci.low = quantile(mean_dens_fia,0.025),
                                                                                                  ci.high = quantile(mean_dens_fia, 0.975), 
                                                                                           ncell = length(mean_dens_fia))


ncell <- mid.summary.pc1 %>%dplyr::select(mode, mids, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)


low_ci <- mid.summary.pc1 %>%dplyr::select(mode, mids, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.pc1 %>%dplyr::select(mode, mids,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c("high_Forest", "high_Savanna")

merged.fia.pc1 <- merge(low_ci, high_ci, by = c("pc1_bins_fia", "mids"))
merged.fia.pc1 <- merge(merged.fia.pc1, ncell, by = c("pc1_bins_fia", "mids"))

merged.fia.pc1$bimodal <- ifelse(merged.fia.pc1$low_Forest > merged.fia.pc1$high_Savanna & merged.fia.pc1$`Low Density Forest` > 50 & merged.fia.pc1$`Forest` > 50,"bimodal", "One mode")
merged.fia.pc1[is.na(merged.fia.pc1$bimodal), ]$bimodal <- "One mode"
merged.fia.pc1$y <- -41







mid.summary.lowprob <- pc1.fia.mix %>% group_by(prob >= 0.49 &  prob <= 0.51, pc1_bins_fia, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                                                       ci.low = quantile(mean_dens_fia,0.025),
                                                                                                                                       ci.high = quantile(mean_dens_fia, 0.975))




smoothingSpline.Forest = smooth.spline(mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mean, spar=.75)
plot(mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Low = smooth.spline(mid.summary.pc1[mid.summary.pc1$mode %in% "Low Density Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Low Density Forest", ]$mean, spar=.75)
plot(mid.summary.pc1[mid.summary.pc1$mode %in% "Low Density Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Low Density Forest", ]$mean)
lines(smoothingSpline.Low, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids_soil, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)


fia.kde.plot.pc1.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550),  yaxt="n", cex.axis=0.7) + points(data = pc1.f.bim.line[pc1.f.bim.line$bimclass %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+ text(-5.5,500, "E"))+ xlab("P-PET")
fia.kde.plot.pc1.gg + xlab("PC1")

fia.kde.plot.pc1.gg.lines <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550),  yaxt="n", cex.axis=0.7) + points(data = merged.fia.pc1[merged.fia.pc1$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                         lines(smoothingSpline.Low, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen")+text(-5.5,500, "E"))+ xlab("P-PET")
fia.kde.plot.pc1.gg.lines + xlab("PC1")

# alternate figure where if the CI of modes overlap, we only draw one line:

# for fia if there is just one mode everywhere, just plot 1 line + ci
unique(merged.fia.pc1$bimodal)
mid.summary.ppet.one.mode <- pc1.fia.mix %>% group_by(pc1_bins_fia, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                ci.low = quantile(mean_dens_fia,0.025),
                                                                                                ci.high = quantile(mean_dens_fia, 0.975),
                                                                                                ncell = length(mean_dens),
                                                                                                mode = "Forest")



smoothingSpline.one = smooth.spline(mid.summary.ppet.one.mode$mids, mid.summary.ppet.one.mode$mean, spar=.75)
plot(mid.summary.ppet.one.mode$mids, mid.summary.ppet.one.mode$mean)
lines(smoothingSpline.one, col = "brown")



fia.kde.plot.pc1.gg.lines.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550),  yaxt="n", cex.axis=0.7) + points(data = merged.fia.pc1[merged.fia.pc1$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                         lines(smoothingSpline.one, lwd = 2, col = "black")+text(-5.5,500, "E"))+ xlab("P-PET")
fia.kde.plot.pc1.gg.lines.hys + xlab("PC1")


ggplot(mid.summary.pc1, aes(mids, mean, color = mode))+geom_point()+geom_errorbar(aes(ymin = ci.low, ymax = ci.high, width = 0, alpha = 0.8), size = 2)+ylim(0, 600)

# --------------------for P-PET:
H <- Hpi.diag(x=na.omit(cbind(pls.df$GS_ppet_mod, pls.df$mean_dens_fia)) )
fhat <- kde(x=na.omit(cbind(pls.df$GS_ppet_mod, pls.df$mean_dens_fia)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,75,80,85,95), xlab = "P-PET", ylab = "Tree density")
#points(na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
fia.kde.plot.ppet <- recordPlot()
fia.kde.plot.ppet.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density"))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)


# get bimodal classifications from previos DF
ppet.f.bim.line <- bimod.ppet.FIA[, c("ppet_binsfia","ppet_mids","ncells_f_ppet", "mean.p", "mean.d", "bimclass_ppet_lowsamp", "bimclass_ppet") ]
ppet.f.bim.line <- ppet.f.bim.line[!duplicated(ppet.f.bim.line),]
ppet.f.bim.line$y <- -41

ppet.fia.mix <- read.csv( "outputs/mixture_model/fia_ppet_mixture_mode_estimates.csv")
mid.summary.ppet <- ppet.fia.mix %>% group_by(mode, GS_ppet_mod_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                           ci.low = quantile(mean_dens_fia,0.025),
                                                                                           ci.high = quantile(mean_dens_fia, 0.975), 
                                                                                           ncell = length(mean_dens_fia))




low_ci <- mid.summary.ppet %>%dplyr::select(mode, mids, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.ppet %>%dplyr::select(mode, mids,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c("high_Forest", "high_Savanna")


ncell <- mid.summary.ppet %>%dplyr::select(mode, mids, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)




merged.fia.ppet <- merge(low_ci, high_ci, by = c("GS_ppet_mod_bins", "mids"))
merged.fia.ppet <- merge(merged.fia.ppet, ncell, by = c("GS_ppet_mod_bins", "mids"))
merged.fia.ppet$bimodal <- ifelse(merged.fia.ppet$low_Forest > merged.fia.ppet$high_Savanna & merged.fia.ppet$`Low Density Forest` > 50 & merged.fia.ppet$`Forest` > 50,"bimodal", "NS")
merged.fia.ppet[is.na(merged.fia.ppet$bimodal), ]$bimodal <- "One mode"
merged.fia.ppet$y <- -41

mid.summary.lowprob <- ppet.fia.mix %>% group_by(prob_ppet >= 0.49 &  prob_ppet <= 0.51, GS_ppet_mod_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                                       ci.low = quantile(mean_dens_fia,0.025),
                                                                                                                       ci.high = quantile(mean_dens_fia, 0.975))




smoothingSpline.Forest = smooth.spline(mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mids, mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mean, spar=.75)
plot(mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mids, mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Low = smooth.spline(mid.summary.ppet[mid.summary.ppet$mode %in% "Low Density Forest", ]$mids, mid.summary.ppet[mid.summary.ppet$mode %in% "Low Density Forest", ]$mean, spar=.75)
plot(mid.summary.ppet[mid.summary.ppet$mode %in% "Low Density Forest", ]$mids, mid.summary.ppet[mid.summary.ppet$mode %in% "Low Density Forest", ]$mean)
lines(smoothingSpline.Low, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids_soil, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)



fia.kde.plot.ppet.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab="P-PET",ylab=NA,  ylim = c(-40,550), xlim = c(-200, 300),  yaxt="n" , cex.axis=0.7) + points(data = ppet.f.bim.line[ppet.f.bim.line$bimclass %in% "bimodal",], y~ppet_mids, cex = 0.9,  pch = 15,col = "red")+ text(-170,500, "F"))+xlab("P-PET")
fia.kde.plot.ppet.gg +xlab("P-PET")


fia.kde.plot.ppet.gg.lines <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550), xlim = c(-200, 300), yaxt="n", cex.axis=0.7) + points(data = merged.fia.ppet[merged.fia.ppet$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                         lines(smoothingSpline.Low, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen")+text(-175,500, "F"))+ xlab("P-PET")
fia.kde.plot.ppet.gg.lines 

# alternate figure where if the CI of modes overlap, we only draw one line:

# for fia if there is just one mode everywhere, just plot 1 line + ci
unique(merged.fia.ppet$bimodal)
mid.summary.ppet.one.mode <- ppet.fia.mix %>% group_by(GS_ppet_mod_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                               ci.low = quantile(mean_dens_fia,0.025),
                                                                                               ci.high = quantile(mean_dens_fia, 0.975),
                                                                                               ncell = length(mean_dens),
                                                                                               mode = "Forest")



smoothingSpline.one = smooth.spline(mid.summary.ppet.one.mode$mids, mid.summary.ppet.one.mode$mean, spar=.75)
plot(mid.summary.ppet.one.mode$mids, mid.summary.ppet.one.mode$mean)
lines(smoothingSpline.one, col = "brown")




fia.kde.plot.ppet.gg.lines.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "P-PET",  ylim = c(-40,550), xlim = c(-200, 300), yaxt="n", cex.axis=0.7) + points(data = merged.fia.ppet[merged.fia.ppet$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                          lines(smoothingSpline.one, lwd = 2, col = "black")+text(-175,500, "F"))+ xlab("P-PET")

fia.kde.plot.ppet.gg.lines.hys 







# for soil moisture/bucket model

H <- Hpi.diag(x=na.omit(cbind(pls.df$mean_GS_soil_m, pls.df$mean_dens_fia)) )
fhat <- kde(x=na.omit(cbind(pls.df$mean_GS_soil_m, pls.df$mean_dens_fia)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,65,75,80,85,95), xlab = "Growing season soil", ylab = "Tree density")
#points(na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
fia.kde.plot.sm <- recordPlot()
fia.kde.plot.sm.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "soil moisture", ylab = "Tree density"))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)


# get bimodal classifications from previos DF
sm.f.bim.line <- bimod.sm.FIA[, c("soil_binsfia","mids","ncells_f_soil", "mean.p", "mean.d", "bimclass_soil_lowsamp", "bimclass_soil_f") ]
sm.f.bim.line <- sm.f.bim.line[!duplicated(sm.f.bim.line),]
sm.f.bim.line$y <- -41

soil.fia.mix <- read.csv( "outputs/mixture_model/fia_soil_mixture_mode_estimates.csv")
mid.summary.soil <- soil.fia.mix %>% group_by(mode, mean_GS_soil_m_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                 ci.low = quantile(mean_dens_fia,0.025),
                                                                                                 ci.high = quantile(mean_dens_fia, 0.975),
                                                                                                 ncell = length(mean_dens_fia))




low_ci <- mid.summary.soil %>%dplyr::select(mode, mids, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.soil %>%dplyr::select(mode, mids,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c( "high_Forest", "high_Savanna")


ncell <- mid.summary.soil %>%dplyr::select(mode, mids, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)

merged.fia.soil <- merge(low_ci, high_ci, by = c("mean_GS_soil_m_bins", "mids"))
merged.fia.soil <- merge(merged.fia.soil, ncell, by = c("mean_GS_soil_m_bins", "mids"))
merged.fia.soil$bimodal <- ifelse(merged.fia.soil$low_Forest > merged.fia.soil$high_Savanna & merged.fia.soil$`High Density Mode` > 50 & merged.fia.soil$`Low Density Mode` > 50,"bimodal", "One mode")
merged.fia.soil[is.na(merged.fia.soil$bimodal), ]$bimodal <- "One mode"
merged.fia.soil$y <- -41

mid.summary.lowprob <- soil.fia.mix %>% group_by(prob_soil >= 0.49 &  prob_soil <= 0.51, mean_GS_soil_m_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                                                      ci.low = quantile(mean_dens_fia,0.025),
                                                                                                                                      ci.high = quantile(mean_dens_fia, 0.975))




smoothingSpline.Forest = smooth.spline(mid.summary.soil[mid.summary.soil$mode %in% "High Density Mode", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "High Density Mode", ]$mean, spar=.75)
plot(mid.summary.soil[mid.summary.soil$mode %in% "High Density Mode", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "High Density Mode", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Low = smooth.spline(mid.summary.soil[mid.summary.soil$mode %in% "Low Density Mode", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Low Density Mode", ]$mean, spar=.75)
plot(mid.summary.soil[mid.summary.soil$mode %in% "Low Density Mode", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Low Density Mode", ]$mean)
lines(smoothingSpline.Low, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids_soil, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)


# make the plot with GGPLOT:
fia.kde.plot.sm.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = NA, ylim = c(-40,550), yaxt="n",  cex.axis=0.9) + points(data = sm.f.bim.line[sm.f.bim.line$bimclass_soil_f %in% "bimodal",], y~mids, cex = 0.8,  pch = 15,col = "red")+ text(0.1,500, "G"))
fia.kde.plot.sm.gg 

fia.kde.plot.sm.gg.lines <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550),  yaxt="n", cex.axis=0.7) + points(data = merged.fia.soil[merged.fia.soil$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                          lines(smoothingSpline.Low, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen")+text(0.1,500, "G"))+ xlab("Soil Moisture")
fia.kde.plot.sm.gg.lines + xlab("Soil Moisture")


# alternate figure where if the CI of modes overlap, we only draw one line:

# for fia if there is just one mode everywhere, just plot 1 line + ci
unique(merged.fia.soil$bimodal)
mid.summary.soil.one.mode <- soil.fia.mix %>% group_by(mean_GS_soil_m_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                    ci.low = quantile(mean_dens_fia,0.025),
                                                                                                    ci.high = quantile(mean_dens_fia, 0.975),
                                                                                                    ncell = length(mean_dens),
                                                                                                    mode = "Forest")



smoothingSpline.one = smooth.spline(mid.summary.soil.one.mode$mids, mid.summary.soil.one.mode$mean, spar=.75)
plot(mid.summary.soil.one.mode$mids, mid.summary.soil.one.mode$mean)
lines(smoothingSpline.one, col = "brown")





fia.kde.plot.sm.gg.lines.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550),  yaxt="n", cex.axis=0.7) + points(data = merged.fia.soil[merged.fia.soil$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                            lines(smoothingSpline.one, lwd = 2, col = "black")+text(0.1,500, "G"))+ xlab("Soil Moisture")
fia.kde.plot.sm.gg.lines.hys + xlab("Soil Moisture")








# need to merge together all of the bimodal/unimodal tags
library(ggplotify)

pls.df$pc1_bins <- cut(pls.df$PC1, breaks=seq(-5.5, 4.5, by = 0.25))
kde.surf.pc1.pls.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass"], merged.pc1[,!colnames( merged.pc1) %in% "y"], by = "pc1_bins")

pls.df$GS_ppet_bins <- cut(pls.df$GS_ppet, breaks=seq(-170, 310, by = 15))
kde.surf.ppet.pls.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass_ppet"], merged.ppet[,!colnames(merged.ppet) %in% "y"], by = "GS_ppet_bins")

pls.df$mean_GS_soil_bins <- cut(pls.df$mean_GS_soil, breaks=seq(0, 1.8, by = 0.05))
kde.surf.soil.pls.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass_soil"], merged.soil[,!colnames(merged.soil) %in% "y"], by = "mean_GS_soil_bins")


# now merge all of these together to make a map of 1, 2, 3, bimodal metrics:
bim.class.m <- merge(kde.surf.pc1.pls.df[,c("x", "y", "bimodal")],kde.surf.soil.pls.df [,c("x", "y", "bimodal")], by = c("x", "y"))
bim.class.m <- merge(bim.class.m, kde.surf.ppet.pls.df[,c("x", "y", "bimodal", "mean_dens")])

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


one.bimpct <- round((length(bim.class.m[bim.class.m$nbimod %in% c("1","2", "3"),]$nbimod)/length(bim.class.m$nbimod))*100, digits = 2)
two.bimpct <- round((length(bim.class.m[bim.class.m$nbimod %in% c("2","3"),]$nbimod)/length(bim.class.m$nbimod))*100, digits = 2)
three.bimpct <- round((length(bim.class.m[bim.class.m$nbimod %in% "3",]$nbimod)/length(bim.class.m$nbimod))*100, digits = 2)




# merge for fia
pls.df$pc1_bins_fia <- cut(pls.df$PC1fia, breaks=seq(-5.5, 4.5, by = 0.25))
kde.surf.pc1.fia.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass_f"], merged.fia.pc1[,!colnames( merged.fia.pc1) %in% "y"], by = "pc1_bins_fia")

pls.df$GS_ppet_mod_bins_fia <- cut(pls.df$GS_ppet_mod, breaks=seq(-170, 310, by = 15))
kde.surf.ppet.fia.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass_ppet_f"], merged.fia.ppet[,!colnames(merged.fia.ppet) %in% "y"], by.x = "GS_ppet_mod_bins_fia",by.y = "GS_ppet_mod_bins")

pls.df$soil_bins_f <- cut(pls.df$mean_GS_soil_m, breaks=seq(0, 1.8, by = 0.05))
kde.surf.soil.fia.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass_soil_f"], merged.fia.soil[,!colnames(merged.fia.soil) %in% "y"], by.x = "soil_bins_f", by.y = "mean_GS_soil_m_bins")


# FIA three color maps
# now merge all of these together to make a map of 1, 2, 3, bimodal metrics:
bim.class.m.f <- merge(kde.surf.pc1.fia.df[,c("x", "y", "bimodal")], kde.surf.soil.fia.df [,c("x", "y", "bimodal")], by = c("x", "y"), all.x = TRUE)
bim.class.m.f <- merge(bim.class.m.f, kde.surf.ppet.fia.df[,c("x", "y", "bimodal", "mean_dens_fia")], all.x = TRUE)

bim.class.m.f$nbimod <- as.character(rowSums(bim.class.m.f[,3:5] == "bimodal", na.rm = TRUE))
# define nbimod as a category:
bim.class.m.f$nbimod <- factor(bim.class.m.f$nbimod, levels = c("0","1", "2", "3", "No data"))
#[!is.na(bim.class.m.f$mean_dens),]
three.color.bimodal.plots.fia <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bim.class.m.f, aes(x=x, y=y, fill = nbimod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red","darkgrey"
  ), labels = c("0","1", "2", "3", "No data"), drop = F) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.23),legend.background = element_rect(fill=alpha('transparent', 0 )),legend.key = element_rect(colour = 'black',  size = 0.5, linetype='solid'),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "H", size = 4)


one.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "1",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
two.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "2",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
three.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "3",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)

one.bimpct <- one.bimpct + two.bimpct + three.bimpct
two.bimpct <-  two.bimpct + three.bimpct
three.bimpct <-  three.bimpct





# merge fia and pls bimodal dfs together:
kde.surf.pc1.df <- merge(kde.surf.pc1.pls.df[,c("x", "y",  "mean_dens",  "bimodal","pc1_bins", "mids")], kde.surf.pc1.fia.df[,c("x", "y",  "mean_dens_fia","bimodal","pc1_bins", "mids")], by = c("x", "y"), all = TRUE)
# rename:
colnames(kde.surf.pc1.df) <- c("x" , "y" , "mean_dens",  "bimclass", "pc1_bins", "mids" , "mean_dens_fia", "bimclass_f", "pc1_bins_f",  "mids_f" )     

kde.surf.ppet.df <- merge(kde.surf.ppet.pls.df[,c("x", "y",  "mean_dens",  "bimodal","GS_ppet_bins", "mids_ppet")], kde.surf.ppet.fia.df[,c("x", "y",  "mean_dens_fia","bimodal", "GS_ppet_mod_bins_fia","mids")], by = c("x", "y"), all = TRUE)
colnames(kde.surf.ppet.df) <- c("x" , "y" , "mean_dens",  "bimclass_ppet", "ppet_bins", "mids" , "mean_dens_fia", "bimclass_ppet_f", "ppet_bins_f",  "mids_f" )     

kde.surf.soilm.df <- merge(kde.surf.soil.pls.df[,c("x", "y",  "mean_dens",  "bimodal", "mean_GS_soil_bins", "mids")], kde.surf.soil.fia.df[,c("x", "y",  "mean_dens_fia","bimodal", "soil_bins_f","mids")], by = c("x", "y"), all = TRUE)
colnames(kde.surf.soilm.df) <- c("x" , "y" , "mean_dens",  "bimclass_soil", "soil_bins", "mids" , "mean_dens_fia", "bimclass_soil_f", "soil_bins_f",  "mids_f" )     




flipped.pc1.hist <- ggplot(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",], aes(mean_dens))+geom_density(color = "blue")+
  geom_density(data = kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",], aes(mean_dens_fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

flipped.ppet.hist <- ggplot(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",], aes(mean_dens))+geom_density(color = "blue")+
  geom_density(data = kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",], aes(mean_dens_fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

flipped.soilm.hist <- ggplot(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",], aes(mean_dens))+geom_density(color = "blue")+
  geom_density(data = kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",], aes(mean_dens_fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)


# alternative: get density lines then ggplotify them to align:
pls.soilm.density.df <- data.frame(y = density(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$mean_dens)$y, 
           x = density(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$mean_dens)$x)

fia.soilm.density.df <- data.frame(y = density(na.omit(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$mean_dens_fia))$y, 
                                 x = density(na.omit(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$mean_dens_fia))$x)
flipped.soilm.hist.gg <- as.ggplot(~plot(fia.soilm.density.df[fia.soilm.density.df$x < 550,] , type = "l", col = "red", ylim = c(-40, 550), yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.soilm.density.df[pls.soilm.density.df$x < 550 & pls.soilm.density.df$x > -41,] , type = "l", col = "blue"))



pls.ppet.density.df <- data.frame(y = density(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$mean_dens)$y, 
                                 x = density(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$mean_dens)$x)

fia.ppet.density.df <- data.frame(y = density(na.omit(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$mean_dens_fia))$y, 
                                 x = density(na.omit(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$mean_dens_fia))$x)
#flipped.ppet.hist.gg <- as.ggplot(~plot( fia.ppet.density.df[fia.ppet.density.df$x < 550,] , type = "l", col = "red", ylim = c(-40, 550),yaxt="n", ylab = NA, xlab = NA, xaxt = "n",xlim = c(0, 0.005)) + lines(pls.ppet.density.df[pls.ppet.density.df$x < 550 & pls.ppet.density.df$x > -41,] , type = "l", col = "blue"))

flipped.ppet.hist.gg <- as.ggplot(~plot(fia.ppet.density.df[fia.ppet.density.df$x < 550,],type = "l", col = "red", ylim = c(-40, 550),yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.ppet.density.df[pls.ppet.density.df$x < 550 & pls.ppet.density.df$x > -41,] , type = "l", col = "blue"))

pls.pc1.density.df <- data.frame(y = density(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$mean_dens)$y, 
                                  x = density(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$mean_dens)$x)

fia.pc1.density.df <- data.frame(y = density(na.omit(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$mean_dens_fia))$y, 
                                  x = density(na.omit(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$mean_dens_fia))$x)
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
#png(height = 9, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/new_figure_3.png")
grid.arrange(arrangeGrob(g1,g2,g3, ncol=3, nrow=1, widths = c(1,1,0.2)), 
             arrangeGrob(g4, g5, g6, ncol = 3, nrow = 1, widths = c(1,1,0.2)) ,
             arrangeGrob(g7,g8, g9, ncol = 3, nrow = 1, widths = c(1,1,0.2)), 
             arrangeGrob(g10, g11, ncol = 3, nrow = 1, widths = c(1,1, 0)))
dev.off()


png(height = 10, width = 6, units = "in", res = 500, "outputs/paper_figs_unc/new_figure_3_kde_plot_with_hist_uncertainty.png")
fig3 <- grid.arrange(g, grow2, grow3,grow4, ncol = 1)
fig3
dev.off()



library(gtable)
g1 <- ggplotGrob(pls.kde.plot.pc1.gg.lines+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-0.1), "cm")))
g2 <- ggplotGrob(fia.kde.plot.pc1.gg.lines+theme(plot.margin=unit(c(-0.7,-1,-0.5,-1), "cm")))
g3 <- ggplotGrob(flipped.pc1.hist.gg+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g4 <- ggplotGrob(pls.kde.plot.ppet.gg.lines+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g5 <- ggplotGrob(fia.kde.plot.ppet.gg.lines+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g6 <- ggplotGrob(flipped.ppet.hist.gg+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g7 <- ggplotGrob(pls.kde.plot.sm.gg.lines+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g8 <- ggplotGrob(fia.kde.plot.sm.gg.lines+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
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
#png(height = 9, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/new_figure_3.png")
grid.arrange(arrangeGrob(g1,g2,g3, ncol=3, nrow=1, widths = c(1,1,0.2)), 
             arrangeGrob(g4, g5, g6, ncol = 3, nrow = 1, widths = c(1,1,0.2)) ,
             arrangeGrob(g7,g8, g9, ncol = 3, nrow = 1, widths = c(1,1,0.2)), 
             arrangeGrob(g10, g11, ncol = 3, nrow = 1, widths = c(1,1, 0)))
dev.off()


png(height = 10, width = 6, units = "in", res = 500, "outputs/paper_figs_unc/new_figure_3_kde_plot_with_lines.png")
fig3 <- grid.arrange(g, grow2, grow3,grow4, ncol = 1)
fig3
dev.off()


# plot the hysteresis figures with dashed lines & onely one line for FIA:

library(gtable)
g1 <- ggplotGrob(pls.kde.plot.pc1.gg.lines.hys+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-0.1), "cm")))
g2 <- ggplotGrob(fia.kde.plot.pc1.gg.lines.hys+theme(plot.margin=unit(c(-0.7,-1,-0.5,-1), "cm")))
g3 <- ggplotGrob(flipped.pc1.hist.gg+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g4 <- ggplotGrob(pls.kde.plot.ppet.gg.lines.hys+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g5 <- ggplotGrob(fia.kde.plot.ppet.gg.lines.hys+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g6 <- ggplotGrob(flipped.ppet.hist.gg+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g7 <- ggplotGrob(pls.kde.plot.sm.gg.lines+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g8 <- ggplotGrob(fia.kde.plot.sm.gg.lines.hys+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
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
#png(height = 9, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/new_figure_3.png")
grid.arrange(arrangeGrob(g1,g2,g3, ncol=3, nrow=1, widths = c(1,1,0.2)), 
             arrangeGrob(g4, g5, g6, ncol = 3, nrow = 1, widths = c(1,1,0.2)) ,
             arrangeGrob(g7,g8, g9, ncol = 3, nrow = 1, widths = c(1,1,0.2)), 
             arrangeGrob(g10, g11, ncol = 3, nrow = 1, widths = c(1,1, 0)))
dev.off()


png(height = 10, width = 6, units = "in", res = 500, "outputs/paper_figs_unc/new_figure_3_kde_plot_with_lines_hys.png")
fig3 <- grid.arrange(g, grow2, grow3,grow4, ncol = 1)
fig3
dev.off()


# >>>>>>>>>>>>>>>>>>>>>>> ALL STAT DRAWS 2d density plots: <<<<<<<<<<<<<<<<<<<<<<<
total.pls <- read.csv("data/extracted_total_PLS_density_draws.csv")
total.fia <- read.csv("data/extracted_total_FIA_density_draws.csv")

colnames(total.pls) <- c("x",  "y","sample_pls" ,"pls" )
colnames(total.fia) <-c("x",  "y","sample_fia" ,"fia" )

total.pls <- total.pls[!is.na(total.pls$pls),]
total.fia <- total.fia[!is.na(total.fia$fia),]

pls.dens <- left_join(total.pls, dens.pr, by=c("x", "y"))
fia.dens <- left_join(total.fia, dens.pr,  by=c("x", "y"))



# for PC1:
H <- Hpi.diag(x=na.omit(cbind(pls.dens$PC1, pls.dens$pls)) )
fhat <- kde(x=na.omit(cbind(pls.dens$PC1, pls.dens$pls)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550))
#points(na.omit(cbind(pls.dens$PC1, pls.dens$pls)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)

pc.pls.mix <- read.csv( "outputs/mixture_model/pls_pc1_mixture_mode_estimates.csv")
mid.summary.pc1 <- pc.pls.mix  %>% group_by(mode, pc1_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                       ci.low = quantile(mean_dens,0.025),
                                                                                       ci.high = quantile(mean_dens, 0.975), 
                                                                                       ncell = length(mean_dens))

ncell <- mid.summary.pc1 %>% dplyr::select(mode, mids, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)










#merged.ppet$bimodal <- ifelse(merged.ppet$low_Forest > merged.ppet$high_Savanna & merged.ppet$`Savanna` > 50 & merged.ppet$`Forest` > 50,"bimodal", "NS")

low_ci <- mid.summary.pc1 %>%dplyr::select(mode, mids, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.pc1 %>%dplyr::select(mode, mids,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c("high_Forest", "high_Savanna")


merged.pc1 <- merge(low_ci, high_ci, by = c("pc1_bins", "mids"))
merged.pc1 <- merge(merged.pc1, ncell, by = c("pc1_bins", "mids"))

merged.pc1$bimodal <- ifelse(merged.pc1$low_Forest > merged.pc1$high_Savanna & merged.pc1$`Savanna` > 50 & merged.pc1$`Forest` > 50,"bimodal", "NS")

merged.pc1[is.na(merged.pc1$bimodal), ]$bimodal <- "One mode"
merged.pc1$y <- -41


mid.summary.lowprob <- pc.pls.mix %>% group_by(prob >= 0.49 &  prob <= 0.51, pc1_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                                  ci.low = quantile(mean_dens,0.025),
                                                                                                                  ci.high = quantile(mean_dens, 0.975))


hysteresis.pc1.pls <- ggplot(data = data.frame(mid.summary.pc1), aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.pc1, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  theme_bw()+scale_fill_manual(values = c('#005a32', '#8c510a'))+scale_color_manual(values = c('#005a32', '#8c510a'))+geom_smooth(data = mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 &  prob <= 0.51` %in% T,], aes(mids, mean), color = "black", linetype = "dashed",se = FALSE)+
  ylab("Mean Tree Density (stems/ha)")+xlab("growing season P-PET")+theme(panel.grid.major = element_blank())

# get bimod.pc.pls from previous code
pc1.bim.line <- bimod.pc.pls[,c("pc1_bins", "mids","ncells_pc1", "mean.p", "mean.d", "bimclass_lowsamp", "bimclass")]
#pc1.bim.line <- bimod.ppet.pls[,c("ppet_bins", "mids","ncells_ppet", "mean.p", "mean.d", "bimclass_lowsamp", "bimclass_ppet")]
pc1.bim.line <- pc1.bim.line[!duplicated(pc1.bim.line),]
pc1.bim.line$y <- -41


library(base2grob)

smoothingSpline.Forest = smooth.spline(mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mean, spar=.75)
plot(mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Savanna = smooth.spline(mid.summary.pc1[mid.summary.pc1$mode %in% "Savanna", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Savanna", ]$mean, spar=.75)
plot(mid.summary.pc1[mid.summary.pc1$mode %in% "Savanna", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Savanna", ]$mean)
lines(smoothingSpline.Savanna, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)


pls.kde.plot.pc1.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), xlim = c(-6, 5),cex.axis = 0.7 ) + points(pc1.bim.line[pc1.bim.line$bimclass %in% "bimodal",]$mids , pc1.bim.line[pc1.bim.line$bimclass %in% "bimodal",]$y, col = "darkblue", pch = 15, cex = 1)+
                                   text(-5, 500, "A"))

pls.kde.plot.pc1.gg.lines.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), xlim = c(-6, 5),cex.axis = 0.7 ) + points(merged.pc1[merged.pc1$bimodal %in% "bimodal",]$mids , merged.pc1[merged.pc1$bimodal %in% "bimodal",]$y, col = "darkblue", pch = 15, cex = 1)+
                                         lines(smoothingSpline.Savanna, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen")+  text(-5, 500, "A"))

pls.kde.plot.pc1.gg.lines.full


# plot with the equal spline on it
pls.kde.plot.pc1.gg.lines.full.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), xlim = c(-6, 5.5), cex.axis = 0.7) + points(data = merged.pc1[merged.pc1$bimodal %in% "bimodal",], y~mids, cex = 1,  pch = 15,col = "darkblue")+
                                             lines(smoothingSpline.Savanna, lwd = 2, col = "black")+  lines(smoothingSpline.Forest, lwd = 2, col = "black") +lines(smoothingSpline.Equal, lwd = 2, col = "grey", lty = 2)+  text(-5, 500, "A"))
pls.kde.plot.pc1.gg.lines.full.hys

# ------------------------PLS for P-PET:
H <- Hpi.diag(x=na.omit(cbind(pls.dens$GS_ppet, pls.dens$pls)) )
fhat <- kde(x=na.omit(cbind(pls.dens$GS_ppet, pls.dens$pls)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,75,80,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(0,550))
#points(na.omit(cbind(pls.dens$PC1, pls.dens$pls)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
pls.kde.plot.ppet <- recordPlot()
pls.kde.plot.ppet.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(0,550), xlim = c(-180, 300)))


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)



# read in statistical summaries
ppet.pls.mix <- read.csv( "outputs/mixture_model/pls_ppet_mixture_mode_estimates.csv")
mid.summary.ppet <- ppet.pls.mix %>% group_by(mode, GS_ppet_bins, mids_ppet) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                  ci.low = quantile(mean_dens,0.025),
                                                                                                  ci.high = quantile(mean_dens, 0.975), 
                                                                                                  ncell = length(mean_dens))

ncell <- mid.summary.ppet %>%dplyr::select(mode, mids_ppet, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)








low_ci <- mid.summary.ppet %>%dplyr::select(mode,mids_ppet, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.ppet %>%dplyr::select(mode, mids_ppet,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c("high_Forest", "high_Savanna")

merged.ppet <- merge(low_ci, high_ci, by = c("GS_ppet_bins", "mids_ppet"))
merged.ppet <- merge(merged.ppet, ncell, by = c("GS_ppet_bins", "mids_ppet"))

merged.ppet$bimodal <- ifelse(merged.ppet$low_Forest > merged.ppet$high_Savanna & merged.ppet$`Savanna` > 50 & merged.ppet$`Forest` > 50,"bimodal", "NS")

merged.ppet[is.na(merged.ppet$bimodal), ]$bimodal <- "One mode"
merged.ppet$y <- -41

# get summary to make lines
mid.summary.lowprob <- ppet.pls.mix %>% group_by(prob_ppet >= 0.49 &  prob_ppet <= 0.51, GS_ppet_bins, mids_ppet) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                                                       ci.low = quantile(mean_dens,0.025),
                                                                                                                                       ci.high = quantile(mean_dens, 0.975))




smoothingSpline.Forest = smooth.spline(mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mids_ppet, mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mean, spar=.75)
plot(mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mids_ppet, mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Savanna = smooth.spline(mid.summary.ppet[mid.summary.ppet$mode %in% "Savanna", ]$mids_ppet, mid.summary.ppet[mid.summary.ppet$mode %in% "Savanna", ]$mean, spar=.75)
plot(mid.summary.ppet[mid.summary.ppet$mode %in% "Savanna", ]$mids_ppet, mid.summary.ppet[mid.summary.ppet$mode %in% "Savanna", ]$mean)
lines(smoothingSpline.Savanna, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob_ppet >= 0.49 & prob_ppet <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mids_ppet, mid.summary.lowprob[mid.summary.lowprob$`prob_ppet >= 0.49 & prob_ppet <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400,, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob_ppet >= 0.49 & prob_ppet <= 0.51` %in% T, ]$mids_ppet, mid.summary.lowprob[mid.summary.lowprob$`prob_ppet >= 0.49 & prob_ppet <= 0.51` %in% T, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)


pls.kde.plot.ppet.gg.lines.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim = c(-200, 300), cex.axis = 0.7) + points(data = merged.ppet[merged.ppet$bimodal %in% "bimodal",], y~mids_ppet, cex = 1,  pch = 15,col = "darkblue")+
                                          lines(smoothingSpline.Savanna, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen")+ text(-170,500, "B"))
pls.kde.plot.ppet.gg.lines.full



# ggplotify the kde plots here:
pls.kde.plot.ppet.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim = c(-200, 300), cex.axis = 0.7) + points(data = merged.ppet[merged.ppet$bimodal %in% "bimodal",], y~mids_ppet, cex = 1,  pch = 15,col = "darkblue") + text(-170,500, "B"))
pls.kde.plot.ppet.gg.full

# plot with the equal spline on it
pls.kde.plot.ppet.gg.lines.full.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim = c(-200, 300), cex.axis = 0.7) + points(data = merged.ppet[merged.ppet$bimodal %in% "bimodal",], y~mids_ppet, cex = 1,  pch = 15,col = "darkblue")+
                                              lines(smoothingSpline.Savanna, lwd = 2, col = "black")+  lines(smoothingSpline.Forest, lwd = 2, col = "black") +lines(smoothingSpline.Equal, lwd = 2, col = "grey", lty = 2)+ text(-170,500, "B"))
pls.kde.plot.ppet.gg.lines.full.hys


#-------------------------- for soil moisture/bucket model

H <- Hpi.diag(x=na.omit(cbind(pls.dens$mean_GS_soil, pls.dens$pls)) )
fhat <- kde(x=na.omit(cbind(pls.dens$mean_GS_soil, pls.dens$pls)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,65,75,80,85,95), xlab = "Growing season soil", ylab = "Tree density", ylim = c(-40,550))

plot(fhat, display="slice", cont=c(85), add = TRUE)
pls.kde.plot.sm <- recordPlot()
pls.kde.plot.sm.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "soil moisture", ylab = "Tree density", ylim = c(0,550), xlim=c(0.5, 1.5)))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)




soil.pls.mix <- read.csv( "outputs/mixture_model/pls_soil_mixture_mode_estimates.csv")
mid.summary.soil <- soil.pls.mix %>% group_by(mode, mean_GS_soil_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                  ci.low = quantile(mean_dens,0.025),
                                                                                                  ci.high = quantile(mean_dens, 0.975), 
                                                                                                  ncell = length(mean_dens) )

ncell <- mid.summary.soil %>%dplyr::select(mode, mids, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)




low_ci <- mid.summary.soil %>%dplyr::select(mode, mids, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.soil %>%dplyr::select(mode, mids,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c("high_Forest", "high_Savanna")

merged.soil <- merge(low_ci, high_ci, by = c("mean_GS_soil_bins", "mids"))
merged.soil <- merge(merged.soil, ncell, by = c("mean_GS_soil_bins", "mids"))

merged.soil$bimodal <- ifelse(merged.soil$low_Forest > merged.soil$high_Savanna & merged.soil$`Savanna` > 50 & merged.soil$`Forest` > 50,"bimodal", "NS")
merged.soil[is.na(merged.soil$bimodal), ]$bimodal <- "One mode"
merged.soil$y <- -41

mid.summary.lowprob <- soil.pls.mix %>% group_by(prob_soil >= 0.49 &  prob_soil <= 0.51, mean_GS_soil_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                                                       ci.low = quantile(mean_dens,0.025),
                                                                                                                                       ci.high = quantile(mean_dens, 0.975))




smoothingSpline.Forest = smooth.spline(mid.summary.soil[mid.summary.soil$mode %in% "Forest", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Forest", ]$mean, spar=.75)
plot(mid.summary.soil[mid.summary.soil$mode %in% "Forest", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Forest", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Savanna = smooth.spline(mid.summary.soil[mid.summary.soil$mode %in% "Savanna", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Savanna", ]$mean, spar=.75)
plot(mid.summary.soil[mid.summary.soil$mode %in% "Savanna", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Savanna", ]$mean)
lines(smoothingSpline.Savanna, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 & prob_soil <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400 & mid.summary.lowprob$mean_GS_soil_bins %in% merged.soil[merged.soil$bimodal %in% "bimodal",]$mean_GS_soil_bins, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 & prob_soil <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400 & mid.summary.lowprob$mean_GS_soil_bins %in% merged.soil[merged.soil$bimodal %in% "bimodal",]$mean_GS_soil_bins, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 & prob_soil <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400 & mid.summary.lowprob$mean_GS_soil_bins %in% merged.soil[merged.soil$bimodal %in% "bimodal",]$mean_GS_soil_bins, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 & prob_soil <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400 & mid.summary.lowprob$mean_GS_soil_bins %in% merged.soil[merged.soil$bimodal %in% "bimodal",]$mean_GS_soil_bins, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)


pls.kde.plot.sm.gg.lines.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = "Tree density", ylim = c(-40,550), xlim = c(0, 1.6),cex.axis = 0.7) + points(data = merged.soil[merged.soil$bimodal %in% "bimodal",], y~mids, cex = 1,  pch = 15,col = "darkblue") +
                                        lines(smoothingSpline.Savanna, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen") + text(0.1,500, "C"))
pls.kde.plot.sm.gg.lines.full
# ggplotify the kde plots here:
pls.kde.plot.sm.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = "Tree density", ylim = c(-40,550), xlim = c(0, 1.6), cex.axis = 0.7) + points(data = merged.soil[merged.soil$bimodal %in% "bimodal",], y~mids, cex = 1,  pch = 15,col = "darkblue")+ text(0.15,500, "C"))
pls.kde.plot.sm.gg.full

# plot with the equal spline on it
pls.kde.plot.sm.gg.lines.full.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = "Tree density", ylim = c(-40,550), xlim=c(0,1.7), cex.axis = 0.7) + points(data = merged.soil[merged.soil$bimodal %in% "bimodal",], y~mids, cex = 1,  pch = 15,col = "darkblue") +
                                            lines(smoothingSpline.Savanna, lwd = 2, col = "black")+  lines(smoothingSpline.Forest, lwd = 2, col = "black") +lines(smoothingSpline.Equal, lwd = 2, col = "grey", lty = 2) + text(0.1,500, "C"))
pls.kde.plot.sm.gg.lines.full.hys



# >>>>>>>>>>>>>>>>>>>>>>> FIA full sample distn with 2d density plots: <<<<<<<<<<<<<<<<<<<<<<<

# for PC1:
H <- Hpi.diag(x=na.omit(cbind(fia.dens$PC1fia, fia.dens$fia)) )
fhat <- kde(x=na.omit(cbind(fia.dens$PC1fia, fia.dens$fia)), H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95,99), xlab = "PC1", ylab = "Tree density")
#points(na.omit(cbind(fia.dens$PC1, fia.dens$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["16%"])[[1]])
contour_95 <- data.frame(contour_95)






pc1.fia.mix <- read.csv( "outputs/mixture_model/fia_pc1_mixture_mode_estimates.csv")
mid.summary.pc1 <- pc1.fia.mix %>% group_by(mode, pc1_bins_fia, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                           ci.low = quantile(mean_dens_fia,0.025),
                                                                                           ci.high = quantile(mean_dens_fia, 0.975), 
                                                                                           ncell = length(mean_dens_fia))


ncell <- mid.summary.pc1 %>%dplyr::select(mode, mids, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)


low_ci <- mid.summary.pc1 %>%dplyr::select(mode, mids, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.pc1 %>%dplyr::select(mode, mids,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c("high_Forest", "high_Savanna")

merged.fia.pc1 <- merge(low_ci, high_ci, by = c("pc1_bins_fia", "mids"))
merged.fia.pc1 <- merge(merged.fia.pc1, ncell, by = c("pc1_bins_fia", "mids"))

merged.fia.pc1$bimodal <- ifelse(merged.fia.pc1$low_Forest > merged.fia.pc1$high_Savanna & merged.fia.pc1$`Low Density Forest` > 50 & merged.fia.pc1$`Forest` > 50,"bimodal", "One mode")
merged.fia.pc1[is.na(merged.fia.pc1$bimodal), ]$bimodal <- "One mode"
merged.fia.pc1$y <- -41







mid.summary.lowprob <- pc1.fia.mix %>% group_by(prob >= 0.49 &  prob <= 0.51, pc1_bins_fia, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                                       ci.low = quantile(mean_dens_fia,0.025),
                                                                                                                       ci.high = quantile(mean_dens_fia, 0.975))




smoothingSpline.Forest = smooth.spline(mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mean, spar=.75)
plot(mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Low = smooth.spline(mid.summary.pc1[mid.summary.pc1$mode %in% "Low Density Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Low Density Forest", ]$mean, spar=.75)
plot(mid.summary.pc1[mid.summary.pc1$mode %in% "Low Density Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Low Density Forest", ]$mean)
lines(smoothingSpline.Low, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids_soil, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)


#fia.kde.plot.pc1.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550),  yaxt="n", cex.axis=0.7) + points(data = pc1.f.bim.line[pc1.f.bim.line$bimclass %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+ text(-5.5,500, "E"))+ xlab("P-PET")
#fia.kde.plot.pc1.gg + xlab("PC1")

fia.kde.plot.pc1.gg.full.lines <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550), xlim = c(-6, 5), yaxt="n", cex.axis=0.7) + points(data = merged.fia.pc1[merged.fia.pc1$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                         lines(smoothingSpline.Low, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen")+text(-5.5,500, "E"))+ xlab("P-PET")
fia.kde.plot.pc1.gg.full.lines + xlab("PC1")

fia.kde.plot.pc1.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,99), ylab = " ",xlab = "PC1",  ylim = c(-40,550),  xlim = c(-6, 5),yaxt="n", cex.axis=0.7) + points(data = merged.fia.pc1[merged.fia.pc1$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+ text(-5,500, "E"))+ xlab("PC1")
fia.kde.plot.pc1.gg.full + xlab("PC1")

# alternate figure where if the CI of modes overlap, we only draw one line:

# for fia if there is just one mode everywhere, just plot 1 line + ci
unique(merged.fia.pc1$bimodal)
mid.summary.ppet.one.mode <- pc1.fia.mix %>% group_by(pc1_bins_fia, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                               ci.low = quantile(mean_dens_fia,0.025),
                                                                                               ci.high = quantile(mean_dens_fia, 0.975),
                                                                                               ncell = length(mean_dens),
                                                                                               mode = "Forest")



smoothingSpline.one = smooth.spline(mid.summary.ppet.one.mode$mids, mid.summary.ppet.one.mode$mean, spar=.75)
plot(mid.summary.ppet.one.mode$mids, mid.summary.ppet.one.mode$mean)
lines(smoothingSpline.one, col = "brown")



fia.kde.plot.pc1.gg.lines.full.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550),   xlim = c(-6, 5.5),yaxt="n", cex.axis=0.7) + points(data = merged.fia.pc1[merged.fia.pc1$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                             lines(smoothingSpline.one, lwd = 2, col = "black")+text(-5.5,500, "E"))+ xlab("P-PET")
fia.kde.plot.pc1.gg.lines.full.hys + xlab("PC1")



# --------------------for P-PET:
H <- Hpi.diag(x=na.omit(cbind(fia.dens$GS_ppet_mod, fia.dens$fia)) )
fhat <- kde(x=na.omit(cbind(fia.dens$GS_ppet_mod, fia.dens$fia)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,75,80,85,95), xlab = "P-PET", ylab = "Tree density")
#points(na.omit(cbind(fia.dens$PC1, fia.dens$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
fia.kde.plot.ppet <- recordPlot()
fia.kde.plot.ppet.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density"))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)





ppet.fia.mix <- read.csv( "outputs/mixture_model/fia_ppet_mixture_mode_estimates.csv")
mid.summary.ppet <- ppet.fia.mix %>% group_by(mode, GS_ppet_mod_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                 ci.low = quantile(mean_dens_fia,0.025),
                                                                                                 ci.high = quantile(mean_dens_fia, 0.975), 
                                                                                                 ncell = length(mean_dens_fia))




low_ci <- mid.summary.ppet %>%dplyr::select(mode, mids, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.ppet %>%dplyr::select(mode, mids,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c("high_Forest", "high_Savanna")


ncell <- mid.summary.ppet %>%dplyr::select(mode, mids, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)




merged.fia.ppet <- merge(low_ci, high_ci, by = c("GS_ppet_mod_bins", "mids"))
merged.fia.ppet <- merge(merged.fia.ppet, ncell, by = c("GS_ppet_mod_bins", "mids"))
merged.fia.ppet$bimodal <- ifelse(merged.fia.ppet$low_Forest > merged.fia.ppet$high_Savanna & merged.fia.ppet$`Low Density Forest` > 50 & merged.fia.ppet$`Forest` > 50,"bimodal", "One mode")
merged.fia.ppet[is.na(merged.fia.ppet$bimodal), ]$bimodal <- "One mode"
merged.fia.ppet$y <- -41

mid.summary.lowprob <- ppet.fia.mix %>% group_by(prob_ppet >= 0.49 &  prob_ppet <= 0.51, GS_ppet_mod_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                                                      ci.low = quantile(mean_dens_fia,0.025),
                                                                                                                                      ci.high = quantile(mean_dens_fia, 0.975))




smoothingSpline.Forest = smooth.spline(mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mids, mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mean, spar=.75)
plot(mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mids, mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Low = smooth.spline(mid.summary.ppet[mid.summary.ppet$mode %in% "Low Density Forest", ]$mids, mid.summary.ppet[mid.summary.ppet$mode %in% "Low Density Forest", ]$mean, spar=.75)
plot(mid.summary.ppet[mid.summary.ppet$mode %in% "Low Density Forest", ]$mids, mid.summary.ppet[mid.summary.ppet$mode %in% "Low Density Forest", ]$mean)
lines(smoothingSpline.Low, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)





fia.kde.plot.ppet.gg.lines.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550), xlim = c(-200, 300), yaxt="n", cex.axis=0.7) + # points(data = merged.fia.ppet[merged.fia.ppet$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                          lines(smoothingSpline.Low, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen")+text(-175,500, "F"))+ xlab("P-PET")
fia.kde.plot.ppet.gg.lines.full 

fia.kde.plot.ppet.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab="P-PET",ylab=NA,  ylim = c(-40,550), xlim = c(-200, 300),  yaxt="n" , cex.axis=0.7)) #+ points(data = ppet.f.bim.line[ppet.f.bim.line$bimclass %in% "bimodal",], y~ppet_mids, cex = 0.9,  pch = 15,col = "red")+ text(-170,500, "F"))+xlab("P-PET")
fia.kde.plot.ppet.gg.full +xlab("P-PET")

# alternate figure where if the CI of modes overlap, we only draw one line:

# for fia if there is just one mode everywhere, just plot 1 line + ci
unique(merged.fia.ppet$bimodal)
mid.summary.ppet.one.mode <- ppet.fia.mix %>% group_by(GS_ppet_mod_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                    ci.low = quantile(mean_dens_fia,0.025),
                                                                                                    ci.high = quantile(mean_dens_fia, 0.975),
                                                                                                    ncell = length(mean_dens),
                                                                                                    mode = "Forest")



smoothingSpline.one = smooth.spline(mid.summary.ppet.one.mode$mids, mid.summary.ppet.one.mode$mean, spar=.75)
plot(mid.summary.ppet.one.mode$mids, mid.summary.ppet.one.mode$mean)
lines(smoothingSpline.one, col = "brown")




fia.kde.plot.ppet.gg.lines.full.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "P-PET",  ylim = c(-40,550), xlim = c(-200, 300), yaxt="n", cex.axis=0.7) + points(data = merged.fia.ppet[merged.fia.ppet$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                              lines(smoothingSpline.one, lwd = 2, col = "black")+text(-175,500, "F"))+ xlab("P-PET")

fia.kde.plot.ppet.gg.lines.full.hys 






# for soil moisture/bucket model

H <- Hpi.diag(x=na.omit(cbind(fia.dens$mean_GS_soil_m, fia.dens$fia)) )
fhat <- kde(x=na.omit(cbind(fia.dens$mean_GS_soil_m, fia.dens$fia)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,65,75,80,85,95), xlab = "Growing season soil", ylab = "Tree density")
#points(na.omit(cbind(fia.dens$PC1, fia.dens$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
fia.kde.plot.sm <- recordPlot()
fia.kde.plot.sm.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "soil moisture", ylab = "Tree density"))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)






soil.fia.mix <- read.csv( "outputs/mixture_model/fia_soil_mixture_mode_estimates.csv")
mid.summary.soil <- soil.fia.mix %>% group_by(mode, mean_GS_soil_m_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                    ci.low = quantile(mean_dens_fia,0.025),
                                                                                                    ci.high = quantile(mean_dens_fia, 0.975),
                                                                                                    ncell = length(mean_dens_fia))




low_ci <- mid.summary.soil %>%dplyr::select(mode, mids, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.soil %>%dplyr::select(mode, mids,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c( "high_Forest", "high_Savanna")


ncell <- mid.summary.soil %>%dplyr::select(mode, mids, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)

merged.fia.soil <- merge(low_ci, high_ci, by = c("mean_GS_soil_m_bins", "mids"))
merged.fia.soil <- merge(merged.fia.soil, ncell, by = c("mean_GS_soil_m_bins", "mids"))
merged.fia.soil$bimodal <- ifelse(merged.fia.soil$low_Forest > merged.fia.soil$high_Savanna & merged.fia.soil$`High Density Mode` > 50 & merged.fia.soil$`Low Density Mode` > 50,"bimodal", "One mode")
merged.fia.soil[is.na(merged.fia.soil$bimodal), ]$bimodal <- "One mode"
merged.fia.soil$y <- -41

mid.summary.lowprob <- soil.fia.mix %>% group_by(prob_soil >= 0.49 &  prob_soil <= 0.51, mean_GS_soil_m_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                                                         ci.low = quantile(mean_dens_fia,0.025),
                                                                                                                                         ci.high = quantile(mean_dens_fia, 0.975))




smoothingSpline.Forest = smooth.spline(mid.summary.soil[mid.summary.soil$mode %in% "High Density Mode", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "High Density Mode", ]$mean, spar=.75)
plot(mid.summary.soil[mid.summary.soil$mode %in% "High Density Mode", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "High Density Mode", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Low = smooth.spline(mid.summary.soil[mid.summary.soil$mode %in% "Low Density Mode", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Low Density Mode", ]$mean, spar=.75)
plot(mid.summary.soil[mid.summary.soil$mode %in% "Low Density Mode", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Low Density Mode", ]$mean)
lines(smoothingSpline.Low, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids_soil, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)


# make the plot with GGPLOT:
fia.kde.plot.sm.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = NA, ylim = c(-40,550), xlim = c(0, 1.6), yaxt="n",  cex.axis=0.9) + text(0.1,500, "G"))
fia.kde.plot.sm.gg.full 

fia.kde.plot.sm.gg.lines.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550), xlim = c(0, 1.6), yaxt="n", cex.axis=0.7) + points(data = merged.fia.soil[merged.fia.soil$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                        lines(smoothingSpline.Low, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen")+text(0.15,500, "G"))+ xlab("Soil Moisture")
fia.kde.plot.sm.gg.lines.full + xlab("Soil Moisture")

# alternate figure where if the CI of modes overlap, we only draw one line:

# for fia if there is just one mode everywhere, just plot 1 line + ci
unique(merged.fia.soil$bimodal)
mid.summary.soil.one.mode <- soil.fia.mix %>% group_by(mean_GS_soil_m_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                       ci.low = quantile(mean_dens_fia,0.025),
                                                                                                       ci.high = quantile(mean_dens_fia, 0.975),
                                                                                                       ncell = length(mean_dens),
                                                                                                       mode = "Forest")



smoothingSpline.one = smooth.spline(mid.summary.soil.one.mode$mids, mid.summary.soil.one.mode$mean, spar=.75)
plot(mid.summary.soil.one.mode$mids, mid.summary.soil.one.mode$mean)
lines(smoothingSpline.one, col = "brown")





fia.kde.plot.sm.gg.lines.full.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "Soil Moisture",  ylim = c(-40,550), xlim=c(0,1.7),  yaxt="n", cex.axis=0.7) + points(data = merged.fia.soil[merged.fia.soil$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                            lines(smoothingSpline.one, lwd = 2, col = "black")+text(0.1,500, "G"))+ xlab("Soil Moisture")
fia.kde.plot.sm.gg.lines.full.hys + xlab("Soil Moisture")




# make ggplot figures of cluster density

# need to merge together all of the bimodal/unimodal tags
library(ggplotify)



pls.dens$pc1_bins <- cut(pls.dens$PC1, breaks=seq(-5.5, 4.5, by = 0.25))
kde.surf.pc1.pls.dens <- left_join(pls.dens, merged.pc1[,!colnames( merged.pc1) %in% "y"], by = "pc1_bins")

pls.dens$GS_ppet_bins <- cut(pls.dens$GS_ppet, breaks=seq(-170, 310, by = 15))
kde.surf.ppet.pls.dens <- left_join(pls.dens, merged.ppet[,!colnames(merged.ppet) %in% "y"], by = "GS_ppet_bins")

pls.dens$mean_GS_soil_bins <- cut(pls.dens$mean_GS_soil, breaks=seq(0, 1.8, by = 0.05))
kde.surf.soil.pls.dens <- left_join( pls.dens, merged.soil[,!colnames(merged.soil) %in% "y"], by = "mean_GS_soil_bins")



# merge together the data and the merged summaries of what is bimodal by statistical mixture model for fia
fia.dens$pc1_bins_fia <- cut(fia.dens$PC1fia, breaks=seq(-5.5, 4.5, by = 0.25))
kde.surf.pc1.fia.dens <- left_join(fia.dens, merged.fia.pc1[,!colnames( merged.fia.pc1) %in% "y"], by = "pc1_bins_fia")

fia.dens$GS_ppet_mod_bins <- cut(fia.dens$GS_ppet_mod, breaks=seq(-170, 310, by = 15))
kde.surf.ppet.fia.dens <- left_join(fia.dens, merged.fia.ppet[,!colnames(merged.fia.ppet) %in% "y"], by = "GS_ppet_mod_bins") #,by.y = "GS_ppet_mod_bins")

fia.dens$mean_GS_soil_m_bins <- cut(fia.dens$mean_GS_soil_m, breaks=seq(0, 1.8, by = 0.05))
kde.surf.soil.fia.dens <- left_join(fia.dens, merged.fia.soil[,!colnames(merged.fia.soil) %in% "y"], by= "mean_GS_soil_m_bins")



# need to rename to left join by sample number (otherwise merge/left_join freaks out)
colnames(kde.surf.pc1.pls.dens)[3] <- "sample" 
colnames(kde.surf.pc1.fia.dens)[3] <- "sample" 

colnames(kde.surf.ppet.pls.dens)[3] <- "sample" 
colnames(kde.surf.ppet.fia.dens)[3] <- "sample" 

colnames(kde.surf.soil.pls.dens)[3] <- "sample" 
colnames(kde.surf.soil.fia.dens)[3] <- "sample" 

kde.surf.pc1.dens <- left_join(kde.surf.pc1.pls.dens[,c("x", "y", "sample", "pls",  "bimodal","pc1_bins", "mids")], kde.surf.pc1.fia.dens[,c("x", "y", "sample", "fia","bimodal","pc1_bins_fia", "mids")], by = c("x", "y", "sample"))
# rename:
colnames(kde.surf.pc1.dens) <- c("x" , "y" ,"sample", "mean_dens",  "bimclass", "pc1_bins", "mids" , "mean_dens_fia", "bimclass_f", "pc1_bins_f",  "mids_f" )     

kde.surf.ppet.dens <- left_join(kde.surf.ppet.pls.dens[,c("x", "y", "sample", "pls",  "bimodal","GS_ppet_bins", "mids_ppet")], kde.surf.ppet.fia.dens[,c("x", "y",  "sample","fia","bimodal", "GS_ppet_mod_bins","mids")], by = c("x", "y", "sample"))
colnames(kde.surf.ppet.dens) <- c("x" , "y" , "sample", "mean_dens", "bimclass_ppet", "ppet_bins", "mids" , "mean_dens_fia", "bimclass_ppet_f", "ppet_bins_f",  "mids_f" )     

kde.surf.soilm.dens <- left_join(kde.surf.soil.pls.dens[,c("x", "y", "sample", "pls",  "bimodal", "mean_GS_soil_bins", "mids")], kde.surf.soil.fia.dens[,c("x", "y", "sample", "fia","bimodal", "mean_GS_soil_m_bins","mids")], by = c("x", "y", "sample"))
colnames(kde.surf.soilm.dens) <- c("x" , "y" , "sample","mean_dens",  "bimclass_soil", "soil_bins", "mids" , "mean_dens_fia", "bimclass_soil_f", "soil_bins_f",  "mids_f" )     



# make marginal histograms for figure 2 based on the pls data that is significantly bimodal & the climate space that matches that in the fia
bimodal.pc1.bins <- unique(kde.surf.pc1.dens[kde.surf.pc1.dens$bimclass %in% "bimodal",]$pc1_bins)

flipped.pc1.hist.full <- ggplot(kde.surf.pc1.dens[kde.surf.pc1.dens$bimclass %in% "bimodal",], aes(mean_dens))+geom_density(color = "blue")+
  geom_density(data = kde.surf.pc1.dens[kde.surf.pc1.dens$pc1_bins_f %in% bimodal.pc1.bins,], aes(mean_dens_fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

bimodal.ppet.bins <- unique(kde.surf.ppet.dens[kde.surf.ppet.dens$bimclass_ppet %in% "bimodal",]$ppet_bins)
flipped.ppet.hist.full <- ggplot(kde.surf.ppet.dens[kde.surf.ppet.dens$bimclass_ppet %in% "bimodal",], aes(mean_dens))+geom_density(color = "blue")+
  geom_density(data = kde.surf.ppet.dens[kde.surf.ppet.dens$ppet_bins_f %in% bimodal.ppet.bins,], aes(mean_dens_fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

bimodal.soil.bins <- unique(kde.surf.soilm.dens[kde.surf.soilm.dens$bimclass_soil %in% "bimodal",]$soil_bins)
flipped.soilm.hist.full <- ggplot(kde.surf.soilm.dens[kde.surf.soilm.dens$bimclass_soil %in% "bimodal",], aes(mean_dens))+geom_density(color = "blue")+
  geom_density(data = kde.surf.soilm.dens[kde.surf.soilm.dens$soil_bins_f %in% bimodal.soil.bins,], aes(mean_dens_fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)


# alternative: get density lines then ggplotify them to align:

# for soil moisture
pls.soilm.density.dens <- data.frame(y = density(kde.surf.soilm.dens[kde.surf.soilm.dens$bimclass_soil %in% "bimodal",]$mean_dens)$y, 
                                   x = density(kde.surf.soilm.dens[kde.surf.soilm.dens$bimclass_soil %in% "bimodal",]$mean_dens)$x)

fia.soilm.density.dens <- data.frame(y = density(na.omit(kde.surf.soilm.dens[kde.surf.soilm.dens$soil_bins_f %in% bimodal.soil.bins,]$mean_dens_fia))$y, 
                                   x = density(na.omit(kde.surf.soilm.dens[kde.surf.soilm.dens$soil_bins_f %in% bimodal.soil.bins,]$mean_dens_fia))$x)
flipped.soilm.hist.gg.full <- as.ggplot(~plot(fia.soilm.density.dens[fia.soilm.density.dens$x < 550,] , type = "l", col = "red", ylim = c(-40, 550), yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.soilm.density.dens[pls.soilm.density.dens$x < 550 & pls.soilm.density.dens$x > -41,] , type = "l", col = "blue"))


# for ppet
pls.ppet.density.dens <- data.frame(y = density(kde.surf.ppet.dens[kde.surf.ppet.dens$bimclass_ppet %in% "bimodal",]$mean_dens)$y, 
                                  x = density(kde.surf.ppet.dens[kde.surf.ppet.dens$bimclass_ppet %in% "bimodal",]$mean_dens)$x)

fia.ppet.density.dens <- data.frame(y = density(na.omit(kde.surf.ppet.dens[kde.surf.ppet.dens$ppet_bins_f %in% bimodal.ppet.bins,]$mean_dens_fia))$y, 
                                  x = density(na.omit(kde.surf.ppet.dens[kde.surf.ppet.dens$ppet_bins_f %in% bimodal.ppet.bins,]$mean_dens_fia))$x)

flipped.ppet.hist.gg.full <- as.ggplot(~plot(fia.ppet.density.dens[fia.ppet.density.dens$x < 550,],type = "l", col = "red", ylim = c(-40, 550),yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.ppet.density.dens[pls.ppet.density.dens$x < 550 & pls.ppet.density.dens$x > -41,] , type = "l", col = "blue"))

# for pc1
pls.pc1.density.dens <- data.frame(y = density(kde.surf.pc1.dens[kde.surf.pc1.dens$bimclass %in% "bimodal",]$mean_dens)$y, 
                                 x = density(kde.surf.pc1.dens[kde.surf.pc1.dens$bimclass %in% "bimodal",]$mean_dens)$x)

fia.pc1.density.dens <- data.frame(y = density(na.omit(kde.surf.pc1.dens[kde.surf.pc1.dens$pc1_bins_f %in% bimodal.pc1.bins,]$mean_dens_fia))$y, 
                                 x = density(na.omit(kde.surf.pc1.dens[kde.surf.pc1.dens$pc1_bins_f %in% bimodal.pc1.bins,]$mean_dens_fia))$x)
flipped.pc1.hist.gg.full <- as.ggplot(~plot(fia.pc1.density.dens[fia.pc1.density.dens$x < 550 ,], type = "l", col = "red", ylim = c(-41, 550), yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.pc1.density.dens[pls.pc1.density.dens$x < 550,], type = "l", col = "blue"))



# grobs for aligning all these plots into figure 2 (no lines on the figure)

library(gtable)
g1 <- ggplotGrob(pls.kde.plot.pc1.gg.full+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-0.1), "cm")))
g2 <- ggplotGrob(fia.kde.plot.pc1.gg.full+theme(plot.margin=unit(c(-0.7,-1,-0.5,-1), "cm")))
g3 <- ggplotGrob(flipped.pc1.hist.gg.full+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g4 <- ggplotGrob(pls.kde.plot.ppet.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g5 <- ggplotGrob(fia.kde.plot.ppet.gg.full+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g6 <- ggplotGrob(flipped.ppet.hist.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g7 <- ggplotGrob(pls.kde.plot.sm.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g8 <- ggplotGrob(fia.kde.plot.sm.gg.full+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g9 <- ggplotGrob(flipped.soilm.hist.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))
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
#png(height = 9, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/new_figure_3.png")
grid.arrange(arrangeGrob(g1,g2,g3, ncol=3, nrow=1, widths = c(1,1,0.2)), 
             arrangeGrob(g4, g5, g6, ncol = 3, nrow = 1, widths = c(1,1,0.2)) ,
             arrangeGrob(g7,g8, g9, ncol = 3, nrow = 1, widths = c(1,1,0.2)), 
             arrangeGrob(g10, g11, ncol = 3, nrow = 1, widths = c(1,1, 0)))
#dev.off()


png(height = 10, width = 6, units = "in", res = 500, "outputs/paper_figs_unc/new_figure_3_kde_plot_with_hist_uncertainty_full_stats.png")
fig3 <- grid.arrange(g, grow2, grow3, grow4, ncol = 1)
fig3
dev.off()


# grobs for aligning all these plots into figure 2 (with lines of mean mode estimated from stat model)

library(gtable)
g1 <- ggplotGrob(pls.kde.plot.pc1.gg.lines.full+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-0.1), "cm")))
g2 <- ggplotGrob(fia.kde.plot.pc1.gg.full.lines+theme(plot.margin=unit(c(-0.7,-1,-0.5,-1), "cm")))
g3 <- ggplotGrob(flipped.pc1.hist.gg.full+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g4 <- ggplotGrob(pls.kde.plot.ppet.gg.lines.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g5 <- ggplotGrob(fia.kde.plot.ppet.gg.lines.full+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g6 <- ggplotGrob(flipped.ppet.hist.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g7 <- ggplotGrob(pls.kde.plot.sm.gg.lines.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g8 <- ggplotGrob(fia.kde.plot.sm.gg.lines.full+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g9 <- ggplotGrob(flipped.soilm.hist.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))
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
#png(height = 9, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/new_figure_3.png")
grid.arrange(arrangeGrob(g1,g2,g3, ncol=3, nrow=1, widths = c(1,1,0.2)), 
             arrangeGrob(g4, g5, g6, ncol = 3, nrow = 1, widths = c(1,1,0.2)) ,
             arrangeGrob(g7,g8, g9, ncol = 3, nrow = 1, widths = c(1,1,0.2)), 
             arrangeGrob(g10, g11, ncol = 3, nrow = 1, widths = c(1,1, 0)))
dev.off()


png(height = 10, width = 6, units = "in", res = 500, "outputs/paper_figs_unc/new_figure_3_kde_plot_with_hist_uncertainty_full_stats_lines.png")
fig3 <- grid.arrange(g, grow2, grow3, grow4, ncol = 1)
fig3
dev.off()


# grobs for aligning all these plots into figure 2 (with lines of mean mode estimated from stat model & the 0.5 mode)

library(gtable)
g1 <- ggplotGrob(pls.kde.plot.pc1.gg.lines.full.hys+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-0.1), "cm")))
g2 <- ggplotGrob(fia.kde.plot.pc1.gg.lines.full.hys+theme(plot.margin=unit(c(-0.7,-1,-0.5,-1), "cm")))
g3 <- ggplotGrob(flipped.pc1.hist.gg.full+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g4 <- ggplotGrob(pls.kde.plot.ppet.gg.lines.full.hys+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g5 <- ggplotGrob(fia.kde.plot.ppet.gg.lines.full.hys+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g6 <- ggplotGrob(flipped.ppet.hist.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g7 <- ggplotGrob(pls.kde.plot.sm.gg.lines.full.hys+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g8 <- ggplotGrob(fia.kde.plot.sm.gg.lines.full.hys+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g9 <- ggplotGrob(flipped.soilm.hist.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))
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
#png(height = 9, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/new_figure_3.png")
grid.arrange(arrangeGrob(g1,g2,g3, ncol=3, nrow=1, widths = c(1,1,0.2)), 
             arrangeGrob(g4, g5, g6, ncol = 3, nrow = 1, widths = c(1,1,0.2)) ,
             arrangeGrob(g7,g8, g9, ncol = 3, nrow = 1, widths = c(1,1,0.2)), 
             arrangeGrob(g10, g11, ncol = 3, nrow = 1, widths = c(1,1, 0)))
dev.off()


png(height = 10, width = 6, units = "in", res = 500, "outputs/paper_figs_unc/new_figure_3_kde_plot_with_hist_uncertainty_full_stats_lines_hys.png")
fig3 <- grid.arrange(g, grow2, grow3, grow4, ncol = 1)
fig3
dev.off()



# >>>>>>>>>>>>>>>>>>>>> Plot figure 3 with grid cells without FIA plots masked out of FIA <<<<<<<<<<<<<<<<<<<<<<<<<
# for PC1:
total.fia <- read.csv("data/extracted_total_FIA_density_draws.csv")
colnames(total.fia) <-c("x",  "y","sample_fia" ,"fia" )
total.fia <- total.fia[!is.na(total.fia$fia),]
fia.dens.omit <- left_join(total.fia, dens.pr,  by=c("x", "y"))

fia.dens.omit <- fia.dens.omit[!is.na(fia.dens.omit$FIAdensity),]

fia.check.full <- fia.dens %>% group_by(x,y) %>% dplyr::summarize(mean=mean(fia, na.rm=TRUE), 
                                                           PC1fia = mean(PC1fia, na.rm=TRUE))
ggplot(fia.check.full, aes(x, y, fill = PC1fia))+geom_raster()

fia.check <- fia.dens.omit %>% group_by(x,y) %>% dplyr::summarize(mean=mean(fia, na.rm=TRUE))
ggplot(fia.check, aes(x, y, fill = mean))+geom_raster()

# do the same for PLS:

pls.dens.omit <- pls.dens[!is.na(pls.dens$FIAdensity), ]

#pls.dens.omit <- pls.dens.omit[!is.na(pls.dens.omit$FIAdensity),]

pls.check <- pls.dens.omit %>% group_by(x,y) %>% dplyr::summarize(mean=mean(pls, na.rm=TRUE))
ggplot(pls.check, aes(x, y, fill = mean))+geom_raster()


# need to update this code to reflect masked dataframes:
# for PC1:
H <- Hpi.diag(x=na.omit(cbind(pls.dens.omit$PC1, pls.dens.omit$pls)) )
fhat <- kde(x=na.omit(cbind(pls.dens.omit$PC1, pls.dens.omit$pls)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550))
#points(na.omit(cbind(pls.dens.omit$PC1, pls.dens.omit$pls)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)

pc.pls.mix <- read.csv( "outputs/mixture_model_msk/pls_pc1_mixture_mode_estimates.csv")
mid.summary.pc1 <- pc.pls.mix  %>% group_by(mode, pc1_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                       ci.low = quantile(mean_dens,0.025),
                                                                                       ci.high = quantile(mean_dens, 0.975), 
                                                                                       ncell = length(mean_dens))

ncell <- mid.summary.pc1 %>%dplyr::select(mode, mids, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)










#merged.ppet$bimodal <- ifelse(merged.ppet$low_Forest > merged.ppet$high_Savanna & merged.ppet$`Savanna` > 50 & merged.ppet$`Forest` > 50,"bimodal", "NS")

low_ci <- mid.summary.pc1 %>%dplyr::select(mode, mids, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.pc1 %>%dplyr::select(mode, mids,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c("high_Forest", "high_Savanna")


merged.pc1 <- merge(low_ci, high_ci, by = c("pc1_bins", "mids"))
merged.pc1 <- merge(merged.pc1, ncell, by = c("pc1_bins", "mids"))

merged.pc1$bimodal <- ifelse(merged.pc1$low_Forest > merged.pc1$high_Savanna & merged.pc1$`Savanna` > 50 & merged.pc1$`Forest` > 50,"bimodal", "NS")

merged.pc1[is.na(merged.pc1$bimodal), ]$bimodal <- "One mode"
merged.pc1$y <- -41


mid.summary.lowprob <- pc.pls.mix %>% group_by(prob >= 0.49 &  prob <= 0.51, pc1_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                                  ci.low = quantile(mean_dens,0.025),
                                                                                                                  ci.high = quantile(mean_dens, 0.975))


hysteresis.pc1.pls <- ggplot(data = data.frame(mid.summary.pc1), aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.pc1, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  theme_bw()+scale_fill_manual(values = c('#005a32', '#8c510a'))+scale_color_manual(values = c('#005a32', '#8c510a'))+geom_smooth(data = mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 &  prob <= 0.51` %in% T,], aes(mids, mean), color = "black", linetype = "dashed",se = FALSE)+
  ylab("Mean Tree Density (stems/ha)")+xlab("growing season P-PET")+theme(panel.grid.major = element_blank())

# get bimod.pc.pls from previous code
pc1.bim.line <- bimod.pc.pls[,c("pc1_bins", "mids","ncells_pc1", "mean.p", "mean.d", "bimclass_lowsamp", "bimclass")]
#pc1.bim.line <- bimod.ppet.pls[,c("ppet_bins", "mids","ncells_ppet", "mean.p", "mean.d", "bimclass_lowsamp", "bimclass_ppet")]
pc1.bim.line <- pc1.bim.line[!duplicated(pc1.bim.line),]
pc1.bim.line$y <- -41


library(base2grob)

smoothingSpline.Forest = smooth.spline(mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mean, spar=.75)
plot(mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Savanna = smooth.spline(mid.summary.pc1[mid.summary.pc1$mode %in% "Savanna", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Savanna", ]$mean, spar=.75)
plot(mid.summary.pc1[mid.summary.pc1$mode %in% "Savanna", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Savanna", ]$mean)
lines(smoothingSpline.Savanna, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob >= 0.49 & prob <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)


pls.kde.plot.pc1.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), xlim = c(-6, 5),cex.axis = 0.7 ) + points(pc1.bim.line[pc1.bim.line$bimclass %in% "bimodal",]$mids , pc1.bim.line[pc1.bim.line$bimclass %in% "bimodal",]$y, col = "darkblue", pch = 15, cex = 1)+
                                        text(-5, 500, "A"))

pls.kde.plot.pc1.gg.lines.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), xlim = c(-6, 5),cex.axis = 0.7 ) + points(merged.pc1[merged.pc1$bimodal %in% "bimodal",]$mids , merged.pc1[merged.pc1$bimodal %in% "bimodal",]$y, col = "darkblue", pch = 15, cex = 1)+
                                              lines(smoothingSpline.Savanna, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen")+  text(-5, 500, "A"))

pls.kde.plot.pc1.gg.lines.full


# plot with the equal spline on it
pls.kde.plot.pc1.gg.lines.full.msk.full.msk.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), xlim = c(-6, 5.5), cex.axis = 0.7) + points(data = merged.pc1[merged.pc1$bimodal %in% "bimodal",], y~mids, cex = 1,  pch = 15,col = "darkblue")+
                                             lines(smoothingSpline.Savanna, lwd = 2, col = "black")+  lines(smoothingSpline.Forest, lwd = 2, col = "black") +lines(smoothingSpline.Equal, lwd = 2, col = "grey", lty = 2)+  text(-5, 500, "A"))
pls.kde.plot.pc1.gg.lines.full.msk.full.msk.hys



# ------------------------PLS for P-PET:
H <- Hpi.diag(x=na.omit(cbind(pls.dens.omit$GS_ppet, pls.dens.omit$pls)) )
fhat <- kde(x=na.omit(cbind(pls.dens.omit$GS_ppet, pls.dens.omit$pls)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,75,80,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(0,550))
#points(na.omit(cbind(pls.dens.omit$PC1, pls.dens.omit$pls)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
pls.kde.plot.ppet <- recordPlot()
pls.kde.plot.ppet.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(0,550), xlim = c(-180, 300)))


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)



# read in statistical summaries
ppet.pls.mix <- read.csv( "outputs/mixture_model_msk/pls_ppet_mixture_mode_estimates.csv")
mid.summary.ppet <- ppet.pls.mix %>% group_by(mode, GS_ppet_bins, mids_ppet) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                  ci.low = quantile(mean_dens,0.025),
                                                                                                  ci.high = quantile(mean_dens, 0.975), 
                                                                                                  ncell = length(mean_dens))

ncell <- mid.summary.ppet %>%dplyr::select(mode, mids_ppet, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)








low_ci <- mid.summary.ppet %>%dplyr::select(mode,mids_ppet, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.ppet %>%dplyr::select(mode, mids_ppet,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c("high_Forest", "high_Savanna")

merged.ppet <- merge(low_ci, high_ci, by = c("GS_ppet_bins", "mids_ppet"))
merged.ppet <- merge(merged.ppet, ncell, by = c("GS_ppet_bins", "mids_ppet"))

merged.ppet$bimodal <- ifelse(merged.ppet$low_Forest > merged.ppet$high_Savanna & merged.ppet$`Savanna` > 50 & merged.ppet$`Forest` > 50,"bimodal", "NS")

merged.ppet[is.na(merged.ppet$bimodal), ]$bimodal <- "One mode"
merged.ppet$y <- -41

# get summary to make lines
mid.summary.lowprob <- ppet.pls.mix %>% group_by(prob_ppet >= 0.499 & prob_ppet <= 0.509, GS_ppet_bins, mids_ppet) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                                                        ci.low = quantile(mean_dens,0.025),
                                                                                                                                        ci.high = quantile(mean_dens, 0.975))




smoothingSpline.Forest = smooth.spline(mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mids_ppet, mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mean, spar=.75)
plot(mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mids_ppet, mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Savanna = smooth.spline(mid.summary.ppet[mid.summary.ppet$mode %in% "Savanna", ]$mids_ppet, mid.summary.ppet[mid.summary.ppet$mode %in% "Savanna", ]$mean, spar=.75)
plot(mid.summary.ppet[mid.summary.ppet$mode %in% "Savanna", ]$mids_ppet, mid.summary.ppet[mid.summary.ppet$mode %in% "Savanna", ]$mean)
lines(smoothingSpline.Savanna, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob_ppet >= 0.499 & prob_ppet <= 0.509` %in% T & mid.summary.lowprob$ci.high < 400, ]$mids_ppet, mid.summary.lowprob[mid.summary.lowprob$`prob_ppet >= 0.499 & prob_ppet <= 0.509` %in% T & mid.summary.lowprob$ci.high < 400,, ]$mean, spar=.5)
#plot(mid.summary.lowprob[mid.summary.lowprob$`prob_ppet >= 0.49 &  prob_ppet <= 0.51` %in% T, ]$mids_ppet, mid.summary.lowprob[mid.summary.lowprob$`prob_ppet >= 0.49 &  prob_ppet <= 0.51` %in% T, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)


pls.kde.plot.ppet.gg.lines.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim = c(-200, 300), cex.axis = 0.7) + points(data = merged.ppet[merged.ppet$bimodal %in% "bimodal",], y~mids_ppet, cex = 1,  pch = 15,col = "darkblue")+
                                               lines(smoothingSpline.Savanna, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen")+ text(-170,500, "B"))
pls.kde.plot.ppet.gg.lines.full



# ggplotify the kde plots here:
pls.kde.plot.ppet.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim = c(-200, 300), cex.axis = 0.7) + points(data = merged.ppet[merged.ppet$bimodal %in% "bimodal",], y~mids_ppet, cex = 1,  pch = 15,col = "darkblue") + text(-170,500, "B"))
pls.kde.plot.ppet.gg.full

# plot with the equal spline on it
pls.kde.plot.ppet.gg.lines.full.msk.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim = c(-200, 300), cex.axis = 0.7) + points(data = merged.ppet[merged.ppet$bimodal %in% "bimodal",], y~mids_ppet, cex = 1,  pch = 15,col = "darkblue")+
                                              lines(smoothingSpline.Savanna, lwd = 2, col = "black")+  lines(smoothingSpline.Forest, lwd = 2, col = "black") +lines(smoothingSpline.Equal, lwd = 2, col = "grey", lty = 2)+ text(-170,500, "B"))
pls.kde.plot.ppet.gg.lines.full.msk.hys


#-------------------------- for soil moisture/bucket model

H <- Hpi.diag(x=na.omit(cbind(pls.dens.omit$mean_GS_soil, pls.dens.omit$pls)) )
fhat <- kde(x=na.omit(cbind(pls.dens.omit$mean_GS_soil, pls.dens.omit$pls)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,65,75,80,85,95), xlab = "Growing season soil", ylab = "Tree density", ylim = c(-40,550))

plot(fhat, display="slice", cont=c(85), add = TRUE)
pls.kde.plot.sm <- recordPlot()
pls.kde.plot.sm.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "soil moisture", ylab = "Tree density", ylim = c(0,550), xlim=c(0.5, 1.5)))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)




soil.pls.mix <- read.csv( "outputs/mixture_model_msk//pls_soil_mixture_mode_estimates.csv")
mid.summary.soil <- soil.pls.mix %>% group_by(mode, mean_GS_soil_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                  ci.low = quantile(mean_dens,0.025),
                                                                                                  ci.high = quantile(mean_dens, 0.975), 
                                                                                                  ncell = length(mean_dens) )

ncell <- mid.summary.soil %>%dplyr::select(mode, mids, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)




low_ci <- mid.summary.soil %>%dplyr::select(mode, mids, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.soil %>%dplyr::select(mode, mids,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c( "low_Savanna","low_Forest")
colnames(high_ci)[3:4] <- c( "high_Savanna", "high_Forest")

merged.soil <- merge(low_ci, high_ci, by = c("mean_GS_soil_bins", "mids"))
merged.soil <- merge(merged.soil, ncell, by = c("mean_GS_soil_bins", "mids"))

merged.soil$bimodal <- ifelse(merged.soil$low_Forest > merged.soil$high_Savanna & merged.soil$`Savanna` > 50 & merged.soil$`Forest` > 50,"bimodal", "NS")
merged.soil[is.na(merged.soil$bimodal), ]$bimodal <- "One mode"
merged.soil$y <- -41

mid.summary.lowprob <- soil.pls.mix %>% group_by(prob_soil >= 0.49 &  prob_soil <= 0.51, mean_GS_soil_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                                                       ci.low = quantile(mean_dens,0.025),
                                                                                                                                       ci.high = quantile(mean_dens, 0.975))




smoothingSpline.Forest = smooth.spline(mid.summary.soil[mid.summary.soil$mode %in% "Forest", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Forest", ]$mean, spar=.75)
plot(mid.summary.soil[mid.summary.soil$mode %in% "Forest", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Forest", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Savanna = smooth.spline(mid.summary.soil[mid.summary.soil$mode %in% "Savanna", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Savanna", ]$mean, spar=.75)
plot(mid.summary.soil[mid.summary.soil$mode %in% "Savanna", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Savanna", ]$mean)
lines(smoothingSpline.Savanna, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 & prob_soil <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400 , ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 & prob_soil <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400 , ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 & prob_soil <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400 & mid.summary.lowprob$mean_GS_soil_bins %in% merged.soil[merged.soil$bimodal %in% "bimodal",]$mean_GS_soil_bins, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 & prob_soil <= 0.51` %in% T & mid.summary.lowprob$ci.high < 400 & mid.summary.lowprob$mean_GS_soil_bins %in% merged.soil[merged.soil$bimodal %in% "bimodal",]$mean_GS_soil_bins, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)



pls.kde.plot.sm.gg.lines.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = "Tree density", ylim = c(-40,550), xlim = c(0, 1.6),cex.axis = 0.7) + points(data = merged.soil[merged.soil$bimodal %in% "bimodal",], y~mids, cex = 1,  pch = 15,col = "darkblue") +
                                             lines(smoothingSpline.Savanna, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen") + text(0.1,500, "C"))
pls.kde.plot.sm.gg.lines.full
# ggplotify the kde plots here:
pls.kde.plot.sm.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = "Tree density", ylim = c(-40,550), xlim = c(0, 1.6), cex.axis = 0.7) + points(data = merged.soil[merged.soil$bimodal %in% "bimodal",], y~mids, cex = 1,  pch = 15,col = "darkblue")+ text(0.15,500, "C"))
pls.kde.plot.sm.gg.full

# plot with the equal spline on it
pls.kde.plot.sm.gg.lines.full.msk.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = "Tree density", ylim = c(-40,550), xlim= c(0, 1.6), cex.axis = 0.7) + points(data = merged.soil[merged.soil$bimodal %in% "bimodal",], y~mids, cex = 1,  pch = 15,col = "darkblue") +
                                            lines(smoothingSpline.Savanna, lwd = 2, col = "black")+  lines(smoothingSpline.Forest, lwd = 2, col = "black") +lines(smoothingSpline.Equal, lwd = 2, col = "grey", lty = 2) + text(0.1,500, "C"))
pls.kde.plot.sm.gg.lines.full.msk.hys



# >>>>>>>>>>>>>>>>>>>>>>> FIA full sample distn with 2d density plots: <<<<<<<<<<<<<<<<<<<<<<<
fia.dens.omit <- fia.dens[!is.na(fia.dens$FIAdensity),]
# for PC1:
H <- Hpi.diag(x=na.omit(cbind(fia.dens.omit$PC1fia, fia.dens.omit$fia)) )
fhat <- kde(x=na.omit(cbind(fia.dens.omit$PC1fia, fia.dens.omit$fia)), H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95,99), xlab = "PC1", ylab = "Tree density")
#points(na.omit(cbind(fia.dens.omit$PC1, fia.dens.omit$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["16%"])[[1]])
contour_95 <- data.frame(contour_95)






pc1.fia.mix <- read.csv( "outputs/mixture_model_msk/fia_pc1_mixture_mode_estimates.csv")
mid.summary.pc1 <- pc1.fia.mix %>% group_by(mode, pc1_bins_fia, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                           ci.low = quantile(mean_dens_fia,0.025),
                                                                                           ci.high = quantile(mean_dens_fia, 0.975), 
                                                                                           ncell = length(mean_dens_fia))


ncell <- mid.summary.pc1 %>%dplyr::select(mode, mids, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)


low_ci <- mid.summary.pc1 %>%dplyr::select(mode, mids, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.pc1 %>%dplyr::select(mode, mids,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c("high_Forest", "high_Savanna")

merged.fia.pc1 <- merge(low_ci, high_ci, by = c("pc1_bins_fia", "mids"))
merged.fia.pc1 <- merge(merged.fia.pc1, ncell, by = c("pc1_bins_fia", "mids"))

merged.fia.pc1$bimodal <- ifelse(merged.fia.pc1$low_Forest > merged.fia.pc1$high_Savanna & merged.fia.pc1$`Low Density Forest` > 50 & merged.fia.pc1$`Forest` > 50,"bimodal", "One mode")
merged.fia.pc1[is.na(merged.fia.pc1$bimodal), ]$bimodal <- "One mode"
merged.fia.pc1$y <- -41







mid.summary.lowprob <- pc1.fia.mix %>% group_by(prob >= 0.49 &  prob <= 0.51, pc1_bins_fia, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                                       ci.low = quantile(mean_dens_fia,0.025),
                                                                                                                       ci.high = quantile(mean_dens_fia, 0.975))




smoothingSpline.Forest = smooth.spline(mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mean, spar=.75)
plot(mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Forest", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Low = smooth.spline(mid.summary.pc1[mid.summary.pc1$mode %in% "Low Density Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Low Density Forest", ]$mean, spar=.75)
plot(mid.summary.pc1[mid.summary.pc1$mode %in% "Low Density Forest", ]$mids, mid.summary.pc1[mid.summary.pc1$mode %in% "Low Density Forest", ]$mean)
lines(smoothingSpline.Low, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids_soil, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)


#fia.kde.plot.pc1.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550),  yaxt="n", cex.axis=0.7) + points(data = pc1.f.bim.line[pc1.f.bim.line$bimclass %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+ text(-5.5,500, "E"))+ xlab("P-PET")
#fia.kde.plot.pc1.gg + xlab("PC1")

fia.kde.plot.pc1.gg.full.lines <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550), xlim = c(-6, 5), yaxt="n", cex.axis=0.7) + points(data = merged.fia.pc1[merged.fia.pc1$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                              lines(smoothingSpline.Low, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen")+text(-5.5,500, "E"))+ xlab("P-PET")
fia.kde.plot.pc1.gg.full.lines + xlab("PC1")

fia.kde.plot.pc1.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,99), ylab = " ",xlab = "PC1",  ylim = c(-40,550),  xlim = c(-6, 5),yaxt="n", cex.axis=0.7) + points(data = merged.fia.pc1[merged.fia.pc1$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+ text(-5,500, "E"))+ xlab("PC1")
fia.kde.plot.pc1.gg.full + xlab("PC1")


# for fia if there is just one mode everywhere, just plot 1 line + ci
unique(merged.fia.pc1$bimodal)
mid.summary.ppet.one.mode <- pc1.fia.mix %>% group_by(pc1_bins_fia, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                               ci.low = quantile(mean_dens_fia,0.025),
                                                                                               ci.high = quantile(mean_dens_fia, 0.975),
                                                                                               ncell = length(mean_dens),
                                                                                               mode = "Forest")


smoothingSpline.one = smooth.spline(mid.summary.ppet.one.mode$mids, mid.summary.ppet.one.mode$mean, spar=.75)
plot(mid.summary.ppet.one.mode$mids, mid.summary.ppet.one.mode$mean)
lines(smoothingSpline.one, col = "brown")



fia.kde.plot.pc1.gg.lines.full.msk.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550),xlim= c(-6, 5),  yaxt="n", cex.axis=0.7) + points(data = merged.fia.pc1[merged.fia.pc1$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                             lines(smoothingSpline.one, lwd = 2, col = "black")+text(-5.5,500, "E"))+ xlab("P-PET")
fia.kde.plot.pc1.gg.lines.full.msk.hys + xlab("PC1")


# --------------------for P-PET:
H <- Hpi.diag(x=na.omit(cbind(fia.dens.omit$GS_ppet_mod, fia.dens.omit$fia)) )
fhat <- kde(x=na.omit(cbind(fia.dens.omit$GS_ppet_mod, fia.dens.omit$fia)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,75,80,85,95), xlab = "P-PET", ylab = "Tree density")
#points(na.omit(cbind(fia.dens.omit$PC1, fia.dens.omit$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
fia.kde.plot.ppet <- recordPlot()
fia.kde.plot.ppet.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density"))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)





ppet.fia.mix <- read.csv( "outputs/mixture_model_msk/fia_ppet_mixture_mode_estimates.csv")
mid.summary.ppet <- ppet.fia.mix %>% group_by(mode, GS_ppet_mod_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                 ci.low = quantile(mean_dens_fia,0.025),
                                                                                                 ci.high = quantile(mean_dens_fia, 0.975), 
                                                                                                 ncell = length(mean_dens_fia))




low_ci <- mid.summary.ppet %>%dplyr::select(mode, mids, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.ppet %>%dplyr::select(mode, mids,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c("high_Forest", "high_Savanna")


ncell <- mid.summary.ppet %>%dplyr::select(mode, mids, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)




merged.fia.ppet <- merge(low_ci, high_ci, by = c("GS_ppet_mod_bins", "mids"))
merged.fia.ppet <- merge(merged.fia.ppet, ncell, by = c("GS_ppet_mod_bins", "mids"))
merged.fia.ppet$bimodal <- ifelse(merged.fia.ppet$low_Forest > merged.fia.ppet$high_Savanna & merged.fia.ppet$`Low Density Forest` > 50 & merged.fia.ppet$`Forest` > 50,"bimodal", "One mode")
merged.fia.ppet[is.na(merged.fia.ppet$bimodal), ]$bimodal <- "One mode"
merged.fia.ppet$y <- -41

mid.summary.lowprob <- ppet.fia.mix %>% group_by(prob_ppet >= 0.49 &  prob_ppet <= 0.51, GS_ppet_mod_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                                                      ci.low = quantile(mean_dens_fia,0.025),
                                                                                                                                      ci.high = quantile(mean_dens_fia, 0.975))




smoothingSpline.Forest = smooth.spline(mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mids, mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mean, spar=.75)
plot(mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mids, mid.summary.ppet[mid.summary.ppet$mode %in% "Forest", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Low = smooth.spline(mid.summary.ppet[mid.summary.ppet$mode %in% "Low Density Forest", ]$mids, mid.summary.ppet[mid.summary.ppet$mode %in% "Low Density Forest", ]$mean, spar=.75)
plot(mid.summary.ppet[mid.summary.ppet$mode %in% "Low Density Forest", ]$mids, mid.summary.ppet[mid.summary.ppet$mode %in% "Low Density Forest", ]$mean)
lines(smoothingSpline.Low, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids_soil, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)





fia.kde.plot.ppet.gg.lines.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550), xlim = c(-200, 300), yaxt="n", cex.axis=0.7) + points(data = merged.fia.ppet[merged.fia.ppet$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                               lines(smoothingSpline.Low, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen")+text(-175,500, "F"))+ xlab("P-PET")
fia.kde.plot.ppet.gg.lines.full 

fia.kde.plot.ppet.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab="P-PET",ylab=NA,  ylim = c(-40,550), xlim = c(-200, 300),  yaxt="n" , cex.axis=0.7) + points(data = ppet.f.bim.line[ppet.f.bim.line$bimclass %in% "bimodal",], y~ppet_mids, cex = 0.9,  pch = 15,col = "red")+ text(-170,500, "F"))+xlab("P-PET")
fia.kde.plot.ppet.gg.full +xlab("P-PET")

# alternate figure where if the CI of modes overlap, we only draw one line:

# for fia if there is just one mode everywhere, just plot 1 line + ci
unique(merged.fia.ppet$bimodal)
mid.summary.ppet.one.mode <- ppet.fia.mix %>% group_by(GS_ppet_mod_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                    ci.low = quantile(mean_dens_fia,0.025),
                                                                                                    ci.high = quantile(mean_dens_fia, 0.975),
                                                                                                    ncell = length(mean_dens),
                                                                                                    mode = "Forest")



smoothingSpline.one = smooth.spline(mid.summary.ppet.one.mode$mids, mid.summary.ppet.one.mode$mean, spar=.75)
plot(mid.summary.ppet.one.mode$mids, mid.summary.ppet.one.mode$mean)
lines(smoothingSpline.one, col = "brown")




fia.kde.plot.ppet.gg.lines.full.msk.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "P-PET",  ylim = c(-40,550), xlim = c(-200, 300), yaxt="n", cex.axis=0.7) + points(data = merged.fia.ppet[merged.fia.ppet$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                              lines(smoothingSpline.one, lwd = 2, col = "black")+text(-175,500, "F"))+ xlab("P-PET")

fia.kde.plot.ppet.gg.lines.full.msk.hys 




# ------------for soil moisture/bucket model

H <- Hpi.diag(x=na.omit(cbind(fia.dens.omit$mean_GS_soil_m, fia.dens.omit$fia)) )
fhat <- kde(x=na.omit(cbind(fia.dens.omit$mean_GS_soil_m, fia.dens.omit$fia)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,65,75,80,85,95), xlab = "Growing season soil", ylab = "Tree density")
#points(na.omit(cbind(fia.dens.omit$PC1, fia.dens.omit$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
fia.kde.plot.sm <- recordPlot()
fia.kde.plot.sm.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "soil moisture", ylab = "Tree density"))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)






soil.fia.mix <- read.csv( "outputs/mixture_model_msk/fia_soil_mixture_mode_estimates.csv")
mid.summary.soil <- soil.fia.mix %>% group_by(mode, mean_GS_soil_m_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                    ci.low = quantile(mean_dens_fia,0.025),
                                                                                                    ci.high = quantile(mean_dens_fia, 0.975),
                                                                                                    ncell = length(mean_dens_fia))




low_ci <- mid.summary.soil %>%dplyr::select(mode, mids, ci.low) %>% spread(key = mode, value = ci.low, drop = TRUE)
high_ci <- mid.summary.soil %>%dplyr::select(mode, mids,ci.high) %>% spread(key = mode, value = ci.high, drop = TRUE)
colnames(low_ci)[3:4] <- c("low_Forest", "low_Savanna")
colnames(high_ci)[3:4] <- c( "high_Forest", "high_Savanna")


ncell <- mid.summary.soil %>%dplyr::select(mode, mids, ncell) %>% spread(key = mode, value = ncell, drop = TRUE)

merged.fia.soil <- merge(low_ci, high_ci, by = c("mean_GS_soil_m_bins", "mids"))
merged.fia.soil <- merge(merged.fia.soil, ncell, by = c("mean_GS_soil_m_bins", "mids"))
merged.fia.soil$bimodal <- ifelse(merged.fia.soil$low_Forest > merged.fia.soil$high_Savanna & merged.fia.soil$`High Density Mode` > 50 & merged.fia.soil$`Low Density Mode` > 50,"bimodal", "One mode")
merged.fia.soil[is.na(merged.fia.soil$bimodal), ]$bimodal <- "One mode"
merged.fia.soil$y <- -41

mid.summary.lowprob <- soil.fia.mix %>% group_by(prob_soil >= 0.49 &  prob_soil <= 0.51, mean_GS_soil_m_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                                                         ci.low = quantile(mean_dens_fia,0.025),
                                                                                                                                         ci.high = quantile(mean_dens_fia, 0.975))




smoothingSpline.Forest = smooth.spline(mid.summary.soil[mid.summary.soil$mode %in% "High Density Mode", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "High Density Mode", ]$mean, spar=.75)
plot(mid.summary.soil[mid.summary.soil$mode %in% "High Density Mode", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "High Density Mode", ]$mean)
lines(smoothingSpline.Forest)

smoothingSpline.Low = smooth.spline(mid.summary.soil[mid.summary.soil$mode %in% "Low Density Mode", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Low Density Mode", ]$mean, spar=.75)
plot(mid.summary.soil[mid.summary.soil$mode %in% "Low Density Mode", ]$mids, mid.summary.soil[mid.summary.soil$mode %in% "Low Density Mode", ]$mean)
lines(smoothingSpline.Low, col = "brown")

smoothingSpline.Equal = smooth.spline(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean, spar=.5)
plot(mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mids_soil, mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.49 &  prob_soil <= 0.51` %in% T, ]$mean)
lines(smoothingSpline.Equal, col = "brown", lty = 2)


# make the plot with GGPLOT:
fia.kde.plot.sm.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = NA, ylim = c(-40,550), xlim = c(0, 1.6), yaxt="n",  cex.axis=0.9) + points(data = sm.f.bim.line[sm.f.bim.line$bimclass_soil_f %in% "bimodal",], y~mids, cex = 0.8,  pch = 15,col = "red")+ text(0.1,500, "G"))
fia.kde.plot.sm.gg.full 

fia.kde.plot.sm.gg.lines.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550), xlim = c(0, 1.6), yaxt="n", cex.axis=0.7) + points(data = merged.fia.soil[merged.fia.soil$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                             lines(smoothingSpline.Low, lwd = 2, col = "brown")+  lines(smoothingSpline.Forest, lwd = 2, col = "forestgreen")+text(0.15,500, "G"))+ xlab("Soil Moisture")
fia.kde.plot.sm.gg.lines.full + xlab("Soil Moisture")


unique(merged.fia.soil$bimodal)
mid.summary.soil.one.mode <- soil.fia.mix %>% group_by(mean_GS_soil_m_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                       ci.low = quantile(mean_dens_fia,0.025),
                                                                                                       ci.high = quantile(mean_dens_fia, 0.975),
                                                                                                       ncell = length(mean_dens),
                                                                                                       mode = "Forest")



smoothingSpline.one = smooth.spline(mid.summary.soil.one.mode$mids, mid.summary.soil.one.mode$mean, spar=.75)
plot(mid.summary.soil.one.mode$mids, mid.summary.soil.one.mode$mean)
lines(smoothingSpline.one, col = "brown")

fia.kde.plot.sm.gg.lines.full.msk.hys <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "Soil Moisture",  ylim = c(-40,550), xlim= c(0, 1.6),  yaxt="n", cex.axis=0.7) + points(data = merged.fia.soil[merged.fia.soil$bimodal %in% "bimodal",], y~mids, cex = 0.9,  pch = 15,col = "red")+
                                            lines(smoothingSpline.one, lwd = 2, col = "black")+text(0.1,500, "G"))+xlab("Soil Moisture")
fia.kde.plot.sm.gg.lines.full.msk.hys + xlab("Soil Moisture")

###KH: below nees to be fixed!

# ------------------------make ggplot figures of cluster density

# need to merge together all of the bimodal/unimodal tags
library(ggplotify)



pls.dens.omit$pc1_bins <- cut(pls.dens.omit$PC1, breaks=seq(-5.5, 4.5, by = 0.25))
kde.surf.pc1.pls.dens.omit <- left_join(pls.dens.omit, merged.pc1[,!colnames( merged.pc1) %in% "y"], by = "pc1_bins")



pls.dens.omit$GS_ppet_bins <- cut(pls.dens.omit$GS_ppet, breaks=seq(-170, 310, by = 15))
kde.surf.ppet.pls.dens.omit <- left_join(pls.dens.omit, merged.ppet[,!colnames(merged.ppet) %in% "y"], by = "GS_ppet_bins")

pls.dens.omit$mean_GS_soil_bins <- cut(pls.dens.omit$mean_GS_soil, breaks=seq(0, 1.8, by = 0.05))
kde.surf.soil.pls.dens.omit <- left_join( pls.dens.omit, merged.soil[,!colnames(merged.soil) %in% "y"], by = "mean_GS_soil_bins")



# merge together the data and the merged summaries of what is bimodal by statistical mixture model for fia
fia.dens.omit$pc1_bins_fia <- cut(fia.dens.omit$PC1fia, breaks=seq(-5.5, 4.5, by = 0.25))
kde.surf.pc1.fia.dens.omit <- left_join(fia.dens.omit, merged.fia.pc1[,!colnames( merged.fia.pc1) %in% "y"], by = "pc1_bins_fia")

fia.dens.omit$GS_ppet_mod_bins <- cut(fia.dens.omit$GS_ppet_mod, breaks=seq(-170, 310, by = 15))
kde.surf.ppet.fia.dens.omit <- left_join(fia.dens.omit, merged.fia.ppet[,!colnames(merged.fia.ppet) %in% "y"], by = "GS_ppet_mod_bins") #,by.y = "GS_ppet_mod_bins")

fia.dens.omit$mean_GS_soil_m_bins <- cut(fia.dens.omit$mean_GS_soil_m, breaks=seq(0, 1.8, by = 0.05))
kde.surf.soil.fia.dens.omit <- left_join(fia.dens.omit, merged.fia.soil[,!colnames(merged.fia.soil) %in% "y"], by= "mean_GS_soil_m_bins")



# need to rename to left join by sample number (otherwise merge/left_join freaks out)
colnames(kde.surf.pc1.pls.dens.omit)[3] <- "sample" 
colnames(kde.surf.pc1.fia.dens.omit)[3] <- "sample" 

colnames(kde.surf.ppet.pls.dens.omit)[3] <- "sample" 
colnames(kde.surf.ppet.fia.dens.omit)[3] <- "sample" 

colnames(kde.surf.soil.pls.dens.omit)[3] <- "sample" 
colnames(kde.surf.soil.fia.dens.omit)[3] <- "sample" 

kde.surf.pc1.dens.omit <- left_join(kde.surf.pc1.pls.dens.omit[,c("x", "y", "sample", "pls",  "bimodal","pc1_bins", "mids")], kde.surf.pc1.fia.dens.omit[,c("x", "y", "sample", "fia","bimodal","pc1_bins_fia", "mids")], by = c("x", "y", "sample"))
# rename:
colnames(kde.surf.pc1.dens.omit) <- c("x" , "y" ,"sample", "mean_dens",  "bimclass", "pc1_bins", "mids" , "mean_dens_fia", "bimclass_f", "pc1_bins_f",  "mids_f" )     

kde.surf.ppet.dens.omit <- left_join(kde.surf.ppet.pls.dens.omit[,c("x", "y", "sample", "pls",  "bimodal","GS_ppet_bins", "mids_ppet")], kde.surf.ppet.fia.dens.omit[,c("x", "y",  "sample","fia","bimodal", "GS_ppet_mod_bins","mids")], by = c("x", "y", "sample"))
colnames(kde.surf.ppet.dens.omit) <- c("x" , "y" , "sample", "mean_dens", "bimclass_ppet", "ppet_bins", "mids" , "mean_dens_fia", "bimclass_ppet_f", "ppet_bins_f",  "mids_f" )     

kde.surf.soilm.dens.omit <- left_join(kde.surf.soil.pls.dens.omit[,c("x", "y", "sample", "pls",  "bimodal", "mean_GS_soil_bins", "mids")], kde.surf.soil.fia.dens.omit[,c("x", "y", "sample", "fia","bimodal", "mean_GS_soil_m_bins","mids")], by = c("x", "y", "sample"))
colnames(kde.surf.soilm.dens.omit) <- c("x" , "y" , "sample","mean_dens",  "bimclass_soil", "soil_bins", "mids" , "mean_dens_fia", "bimclass_soil_f", "soil_bins_f",  "mids_f" )     



# make marginal histograms for figure 2 based on the pls data that is significantly bimodal & the climate space that matches that in the fia
bimodal.pc1.bins <- unique(kde.surf.pc1.dens.omit[kde.surf.pc1.dens.omit$bimclass %in% "bimodal",]$pc1_bins)

flipped.pc1.hist.full <- ggplot(kde.surf.pc1.dens.omit[kde.surf.pc1.dens.omit$bimclass %in% "bimodal",], aes(mean_dens))+geom_density(color = "blue")+
  geom_density(data = kde.surf.pc1.dens.omit[kde.surf.pc1.dens.omit$pc1_bins_f %in% bimodal.pc1.bins,], aes(mean_dens_fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

bimodal.ppet.bins <- unique(kde.surf.ppet.dens.omit[kde.surf.ppet.dens.omit$bimclass_ppet %in% "bimodal",]$ppet_bins)
flipped.ppet.hist.full <- ggplot(kde.surf.ppet.dens.omit[kde.surf.ppet.dens.omit$bimclass_ppet %in% "bimodal",], aes(mean_dens))+geom_density(color = "blue")+
  geom_density(data = kde.surf.ppet.dens.omit[kde.surf.ppet.dens.omit$ppet_bins_f %in% bimodal.ppet.bins,], aes(mean_dens_fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

bimodal.soil.bins <- unique(kde.surf.soilm.dens.omit[kde.surf.soilm.dens.omit$bimclass_soil %in% "bimodal",]$soil_bins)
flipped.soilm.hist.full <- ggplot(kde.surf.soilm.dens.omit[kde.surf.soilm.dens.omit$bimclass_soil %in% "bimodal",], aes(mean_dens))+geom_density(color = "blue")+
  geom_density(data = kde.surf.soilm.dens.omit[kde.surf.soilm.dens.omit$soil_bins_f %in% bimodal.soil.bins,], aes(mean_dens_fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)


# alternative: get density lines then ggplotify them to align:

# for soil moisture
pls.soilm.density.dens <- data.frame(y = density(kde.surf.soilm.dens.omit[kde.surf.soilm.dens.omit$bimclass_soil %in% "bimodal",]$mean_dens)$y, 
                                     x = density(kde.surf.soilm.dens.omit[kde.surf.soilm.dens.omit$bimclass_soil %in% "bimodal",]$mean_dens)$x)

fia.soilm.density.dens <- data.frame(y = density(na.omit(kde.surf.soilm.dens.omit[kde.surf.soilm.dens.omit$soil_bins_f %in% bimodal.soil.bins,]$mean_dens_fia))$y, 
                                     x = density(na.omit(kde.surf.soilm.dens.omit[kde.surf.soilm.dens.omit$soil_bins_f %in% bimodal.soil.bins,]$mean_dens_fia))$x)
flipped.soilm.hist.gg.full <- as.ggplot(~plot(fia.soilm.density.dens[fia.soilm.density.dens$x < 550,] , type = "l", col = "red", ylim = c(-40, 550), yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.soilm.density.dens[pls.soilm.density.dens$x < 550 & pls.soilm.density.dens$x > -41,] , type = "l", col = "blue"))


# for ppet
pls.ppet.density.dens <- data.frame(y = density(kde.surf.ppet.dens.omit[kde.surf.ppet.dens.omit$bimclass_ppet %in% "bimodal",]$mean_dens)$y, 
                                    x = density(kde.surf.ppet.dens.omit[kde.surf.ppet.dens.omit$bimclass_ppet %in% "bimodal",]$mean_dens)$x)

fia.ppet.density.dens <- data.frame(y = density(na.omit(kde.surf.ppet.dens.omit[kde.surf.ppet.dens.omit$ppet_bins_f %in% bimodal.ppet.bins,]$mean_dens_fia))$y, 
                                    x = density(na.omit(kde.surf.ppet.dens.omit[kde.surf.ppet.dens.omit$ppet_bins_f %in% bimodal.ppet.bins,]$mean_dens_fia))$x)

flipped.ppet.hist.gg.full <- as.ggplot(~plot(fia.ppet.density.dens[fia.ppet.density.dens$x < 550,],type = "l", col = "red", ylim = c(-40, 550),yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.ppet.density.dens[pls.ppet.density.dens$x < 550 & pls.ppet.density.dens$x > -41,] , type = "l", col = "blue"))

# for pc1
pls.pc1.density.dens <- data.frame(y = density(kde.surf.pc1.dens.omit[kde.surf.pc1.dens.omit$bimclass %in% "bimodal",]$mean_dens)$y, 
                                   x = density(kde.surf.pc1.dens.omit[kde.surf.pc1.dens.omit$bimclass %in% "bimodal",]$mean_dens)$x)

fia.pc1.density.dens <- data.frame(y = density(na.omit(kde.surf.pc1.dens.omit[kde.surf.pc1.dens.omit$pc1_bins_f %in% bimodal.pc1.bins,]$mean_dens_fia))$y, 
                                   x = density(na.omit(kde.surf.pc1.dens.omit[kde.surf.pc1.dens.omit$pc1_bins_f %in% bimodal.pc1.bins,]$mean_dens_fia))$x)
flipped.pc1.hist.gg.full <- as.ggplot(~plot(fia.pc1.density.dens[fia.pc1.density.dens$x < 550 ,], type = "l", col = "red", ylim = c(-41, 550), yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.pc1.density.dens[pls.pc1.density.dens$x < 550,], type = "l", col = "blue"))


# -----------------make msked versions of the three color bimodal plots:
# now merge all of these together to make a map of 1, 2, 3, bimodal metrics:

# get only the unique x y values and bimodal
pc1.full.bimodal.pls <- unique(kde.surf.pc1.pls.dens.omit[,c("x", "y", "bimodal")])
soil.full.bimodal.pls  <- unique(kde.surf.soil.pls.dens.omit[,c("x", "y", "bimodal")])
ppet.full.bimodal.pls <- unique(kde.surf.ppet.pls.dens.omit[,c("x", "y", "bimodal")])


pc1.full.bimodal.fia <- unique(kde.surf.pc1.fia.dens.omit[,c("x", "y", "bimodal")])
soil.full.bimodal.fia <- unique(kde.surf.soil.fia.dens.omit[,c("x", "y", "bimodal")])
ppet.full.bimodal.fia <- unique(kde.surf.ppet.fia.dens.omit[,c("x", "y", "bimodal")])




bim.class.m.msk <- left_join(pc1.full.bimodal.pls , pc1.full.bimodal.pls , by = c("x", "y"))
bim.class.m.msk  <- left_join(bim.class.m.msk , ppet.full.bimodal.pls[,c("x", "y", "bimodal")])

bim.class.m.msk $nbimod <- as.character(rowSums(bim.class.m.msk [,3:5] == "bimodal", na.rm = TRUE))
# define nbimod as a category:
bim.class.m.msk $nbimod <- factor(bim.class.m.msk $nbimod, levels = c("0","1", "2", "3", "No data"))

three.color.bimodal.plots.msk <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bim.class.m.msk , aes(x=x, y=y, fill = nbimod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red","darkgrey"
  ), labels = c("0","1", "2", "3", "No data"), drop = F) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.23),legend.background = element_rect(fill=alpha('transparent', 0 )),legend.key = element_rect(colour = 'black',  size = 0.5, linetype='solid'),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "D", size = 4)




# make 3 color msked plots for fia:
bim.class.m.f.msk <- left_join(pc1.full.bimodal.fia, soil.full.bimodal.fia, by = c("x", "y"))
bim.class.m.f.msk <- left_join(bim.class.m.f.msk, ppet.full.bimodal.fia, all.x = TRUE)

bim.class.m.f.msk$nbimod <- as.character(rowSums(bim.class.m.f.msk[,3:5] == "bimodal", na.rm = TRUE))
# define nbimod as a category:
bim.class.m.f.msk$nbimod <- factor(bim.class.m.f.msk$nbimod, levels = c("0","1", "2", "3", "No data"))

three.color.bimodal.plots.fia.msk <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bim.class.m.f.msk, aes(x=x, y=y, fill = nbimod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red","darkgrey"
  ), labels = c("0","1", "2", "3", "No data"), drop = F) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.23),legend.background = element_rect(fill=alpha('transparent', 0 )),legend.key = element_rect(colour = 'black',  size = 0.5, linetype='solid'),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "H", size = 4)


one.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "1",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
two.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "2",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
three.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "3",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)

one.bimpct <- one.bimpct + two.bimpct + three.bimpct
two.bimpct <-  two.bimpct + three.bimpct
three.bimpct <-  three.bimpct




# grobs for aligning all these plots into figure 2 (no lines on the figure)

library(gtable)
g1 <- ggplotGrob(pls.kde.plot.pc1.gg.full+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-0.1), "cm")))
g2 <- ggplotGrob(fia.kde.plot.pc1.gg.full+theme(plot.margin=unit(c(-0.7,-1,-0.5,-1), "cm")))
g3 <- ggplotGrob(flipped.pc1.hist.gg.full+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g4 <- ggplotGrob(pls.kde.plot.ppet.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g5 <- ggplotGrob(fia.kde.plot.ppet.gg.full+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g6 <- ggplotGrob(flipped.ppet.hist.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g7 <- ggplotGrob(pls.kde.plot.sm.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g8 <- ggplotGrob(fia.kde.plot.sm.gg.full+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g9 <- ggplotGrob(flipped.soilm.hist.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))
g10 <- ggplotGrob(three.color.bimodal.plots.msk)
g11 <- ggplotGrob(three.color.bimodal.plots.fia.msk)

g <- cbind(g1 ,g2, g3, size = "first")
g$heights <-unit.pmax(g1$heights, g2$heights, g3$heights)#, 

grow2 <- cbind(g4 ,g5, g6, size = "first")
grow2$heights <-unit.pmax(g4$heights, g5$heights, g6$heights)#, 

grow3 <- cbind(g7 ,g8, g9, size = "first")
grow3$heights <-unit.pmax(g7$heights, g8$heights, g9$heights)#, 
grow4 <- cbind(g10, g11, size = "first")
grow4$heights <-unit.pmax(g10$heights, g11$heights)
#png(height = 9, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/new_figure_3.png")
grid.arrange(arrangeGrob(g1,g2,g3, ncol=3, nrow=1, widths = c(1,1,0.2)), 
             arrangeGrob(g4, g5, g6, ncol = 3, nrow = 1, widths = c(1,1,0.2)) ,
             arrangeGrob(g7,g8, g9, ncol = 3, nrow = 1, widths = c(1,1,0.2)), 
             arrangeGrob(g10, g11, ncol = 3, nrow = 1, widths = c(1,1, 0)))
#dev.off()


png(height = 10, width = 6, units = "in", res = 500, "outputs/paper_figs_unc/new_figure_3_kde_plot_with_hist_uncertainty_full_stats_msk.png")
fig3 <- grid.arrange(g, grow2, grow3, grow4, ncol = 1)
fig3
dev.off()


# grobs for aligning all these plots into figure 2 (with lines of mean mode estimated from stat model)

library(gtable)
g1 <- ggplotGrob(pls.kde.plot.pc1.gg.lines.full+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-0.1), "cm")))
g2 <- ggplotGrob(fia.kde.plot.pc1.gg.full.lines+theme(plot.margin=unit(c(-0.7,-1,-0.5,-1), "cm")))
g3 <- ggplotGrob(flipped.pc1.hist.gg.full+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g4 <- ggplotGrob(pls.kde.plot.ppet.gg.lines.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g5 <- ggplotGrob(fia.kde.plot.ppet.gg.lines.full+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g6 <- ggplotGrob(flipped.ppet.hist.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g7 <- ggplotGrob(pls.kde.plot.sm.gg.lines.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g8 <- ggplotGrob(fia.kde.plot.sm.gg.lines.full+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g9 <- ggplotGrob(flipped.soilm.hist.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))
g10 <- ggplotGrob(three.color.bimodal.plots.msk)
g11 <- ggplotGrob(three.color.bimodal.plots.fia.msk)

g <- cbind(g1 ,g2, g3, size = "first")
g$heights <-unit.pmax(g1$heights, g2$heights, g3$heights)#, 

grow2 <- cbind(g4 ,g5, g6, size = "first")
grow2$heights <-unit.pmax(g4$heights, g5$heights, g6$heights)#, 

grow3 <- cbind(g7 ,g8, g9, size = "first")
grow3$heights <-unit.pmax(g7$heights, g8$heights, g9$heights)#, 
grow4 <- cbind(g10, g11, size = "first")
grow4$heights <-unit.pmax(g10$heights, g11$heights)
#png(height = 9, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/new_figure_3.png")
grid.arrange(arrangeGrob(g1,g2,g3, ncol=3, nrow=1, widths = c(1,1,0.2)), 
             arrangeGrob(g4, g5, g6, ncol = 3, nrow = 1, widths = c(1,1,0.2)) ,
             arrangeGrob(g7,g8, g9, ncol = 3, nrow = 1, widths = c(1,1,0.2)), 
             arrangeGrob(g10, g11, ncol = 3, nrow = 1, widths = c(1,1, 0)))
dev.off()


png(height = 10, width = 6, units = "in", res = 500, "outputs/paper_figs_unc/new_figure_3_kde_plot_with_hist_uncertainty_full_stats_lines_msk.png")
fig3 <- grid.arrange(g, grow2, grow3, grow4, ncol = 1)
fig3
dev.off()


# plot the hysteresis figures with dashed lines & onely one line for FIA:

library(gtable)
g1 <- ggplotGrob(pls.kde.plot.pc1.gg.lines.full.msk.full.msk.hys+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-0.1), "cm")))
g2 <- ggplotGrob(fia.kde.plot.pc1.gg.lines.full.msk.hys+theme(plot.margin=unit(c(-0.7,-1,-0.5,-1), "cm")))
g3 <- ggplotGrob(flipped.pc1.hist.gg.full+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g4 <- ggplotGrob(pls.kde.plot.ppet.gg.lines.full.msk.hys+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g5 <- ggplotGrob(fia.kde.plot.ppet.gg.lines.full.msk.hys+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g6 <- ggplotGrob(flipped.ppet.hist.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g7 <- ggplotGrob(pls.kde.plot.sm.gg.lines.full.msk.hys+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g8 <- ggplotGrob(fia.kde.plot.sm.gg.lines.full.msk.hys+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g9 <- ggplotGrob(flipped.soilm.hist.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))
g10 <- ggplotGrob(three.color.bimodal.plots.msk)
g11 <- ggplotGrob(three.color.bimodal.plots.fia.msk)

g <- cbind(g1 ,g2, g3, size = "first")
g$heights <-unit.pmax(g1$heights, g2$heights, g3$heights)#, 

grow2 <- cbind(g4 ,g5, g6, size = "first")
grow2$heights <-unit.pmax(g4$heights, g5$heights, g6$heights)#, 

grow3 <- cbind(g7 ,g8, g9, size = "first")
grow3$heights <-unit.pmax(g7$heights, g8$heights, g9$heights)#, 
grow4 <- cbind(g10, g11, size = "first")
grow4$heights <-unit.pmax(g10$heights, g11$heights)
#png(height = 9, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/new_figure_3.png")
grid.arrange(arrangeGrob(g1,g2,g3, ncol=3, nrow=1, widths = c(1,1,0.2)), 
             arrangeGrob(g4, g5, g6, ncol = 3, nrow = 1, widths = c(1,1,0.2)) ,
             arrangeGrob(g7,g8, g9, ncol = 3, nrow = 1, widths = c(1,1,0.2)), 
             arrangeGrob(g10, g11, ncol = 3, nrow = 1, widths = c(1,1, 0)))
dev.off()


png(height = 10, width = 6, units = "in", res = 500, "outputs/paper_figs_unc/new_figure_3_kde_plot_with_hist_uncertainty_full_stats_lines_msk_hys.png")
fig3 <- grid.arrange(g, grow2, grow3,grow4, ncol = 1)
fig3
dev.off()



