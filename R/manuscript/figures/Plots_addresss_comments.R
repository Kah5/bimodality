library(ggplot2)
library(cowplot)
library(tidyr)
library(dplyr)

dens <- read.csv("outputs/density_full_FIA_PLS_unc.csv")

pls.fia.scatter.sm <- ggplot(dens, aes(mean_dens, mean_dens_fia))+geom_point()+ylim(0, 700)+xlim(0,700)+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ylab("FIA mean density (statistically smoothed)")+xlab("PLS mean density (statistically smooothed)")+theme_bw(base_size = 12)


pls.fia.scatter <- ggplot(dens, aes(PLSdensity, FIAdensity))+geom_point()+ylim(0, 700)+xlim(0,700)+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ylab("FIA density")+xlab("PLS density")+theme_bw(base_size = 12)



png(height = 4, width = 8, units = "in", res = 200, "outputs/FIA_PLS_scatterplots.png")
plot_grid(pls.fia.scatter.sm, pls.fia.scatter)
dev.off()


# read in the FIA composition clusters
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
                                                          "Pine/Maple/Poplar/Oak/Ash-FIA" = "Pine", 
                                                          "Poplar/Pine/Tamarack/Fir-PLS" = "Boreal/Sub-boreal", 
                                                          "Beech/Maple/Pine-PLS" = "Beech-Maple",
                                                          "Maple/Cedar/Poplar-FIA" = "Maple Mixed Forest",
                                                          "Oak/Maple/Other/Hickory-FIA" = "Oak-Mixed"))

clust_10$orderedforesttype <- factor(clust_10$foresttype, c("Oak", "Pine", "Aspen", "N. Mixed Forest", "Boreal/Sub-boreal","Oak/Maple/Ash", "Oak-Hickory", "Beech-Maple", "Maple Mixed Forest", "Oak-Mixed"))
#clust_10$orderedforesttype <- factor(dens.clust$foresttype, levels = rev(c("Aspen", "Maple Mixed Forest", "Oak-Mixed","Oak/Maple/Ash","Oak-Hickory","Pine","Oak",  "Boreal/Sub-boreal", "N. Mixed Forest",  "Beech-Maple")))

# create a stable coloring scheme:
compColors <- c('#386cb0',"#f0027f",'#ff7f00',"#ffff99","#7fc97f", "#beaed4",'#a6cee3',"#b15928",  "#004529",  '#fdc086')
names(compColors) <- levels(clust_10$orderedforesttype)


# make the same scatter plots but colored by density
pls.fia.scatter.sm.sp <- ggplot(clust_10, aes(mean_dens, mean_dens_fia, color = orderedforesttype))+geom_point(size = 0.5)+scale_color_manual(values = compColors)+ylim(0, 700)+xlim(0,700)+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ylab("FIA mean density (statistically smoothed)")+xlab("PLS mean density (statistically smooothed)")+theme_bw()


pls.fia.scatter.sp <- ggplot(clust_10, aes(PLSdensity, FIAdensity, color = orderedforesttype))+geom_point(size = 0.5)+scale_color_manual(values = compColors)+ylim(0, 700)+xlim(0,700)+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ylab("FIA density")+xlab("PLS density")+theme_bw()



png(height = 4, width = 15, units = "in", res = 200, "outputs/FIA_PLS_scatterplots_sp.png")
plot_grid(pls.fia.scatter.sm.sp, pls.fia.scatter.sp)
dev.off()

# by forest type for good measure
png(height = 10, width = 10, units = "in", res = 200, "outputs/FIA_PLS_scatterplots_sp_stat_facet.png")
pls.fia.scatter.sm.sp.facet <- ggplot(clust_10, aes(mean_dens, mean_dens_fia, color = orderedforesttype))+geom_point(size = 0.5)+scale_color_manual(values = compColors)+ylim(0, 700)+xlim(0,700)+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ylab("FIA mean density (statistically smoothed)")+xlab("PLS mean density (statistically smooothed)")+theme_bw()+facet_wrap(~orderedforesttype)
pls.fia.scatter.sm.sp.facet
dev.off()

png(height = 10, width = 10, units = "in", res = 200, "outputs/FIA_PLS_scatterplots_sp_raw_facet.png")
pls.fia.scatter.sp.facet <- ggplot(clust_10, aes(PLSdensity, FIAdensity, color = orderedforesttype))+geom_point(size = 0.5)+scale_color_manual(values = compColors)+ylim(0, 700)+xlim(0,700)+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ylab("FIA density")+xlab("PLS density")+theme_bw()+facet_wrap(~orderedforesttype)
pls.fia.scatter.sp.facet
dev.off()


# Calculate change in tree density over time of mean statistical estimates:
clust_10$diff.sm <- clust_10$mean_dens_fia - clust_10$mean_dens
clust_10$diff.raw <- clust_10$FIAdensity - clust_10$PLSdensity

summary.by.clust.type <- clust_10 %>% group_by(orderedforesttype) %>% summarise(differences = mean(diff.sm), 
                                                       sd.diff = sd(diff.sm),
                                                       mean.FIA = mean(mean_dens_fia), 
                                                       sd.FIA = sd(mean_dens_fia),
                                                       mean.PLS = mean(mean_dens), 
                                                       sd.PLS = sd(mean_dens))


ggplot(summary.by.clust.type, aes(mean.PLS, differences, color = orderedforesttype))+geom_point(size = 1.5)+scale_color_manual(values = compColors)

summary.by.clust.type$orderedforesttype.dens <- factor(summary.by.clust.type$orderedforesttype, levels = summary.by.clust.type[order(summary.by.clust.type$mean.PLS),]$orderedforesttype)

bar.density.pls.type <-  ggplot(summary.by.clust.type, aes(orderedforesttype.dens, mean.PLS, fill = orderedforesttype))+
  geom_bar(stat = "identity")+scale_fill_manual(values = compColors)+ylab("Average Tree Density")+theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())

bar.difference.pls.type <- ggplot(summary.by.clust.type, aes(orderedforesttype.dens, differences, fill = orderedforesttype))+
  geom_bar(stat = "identity")+scale_fill_manual(values = compColors)+ylab("Mean difference \n between modern and past \n tree density")+theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())

png(height = 6, width = 5, units = "in", res = 300, "outputs/paper_figs_unc/density_by_pls_group_and_change_over_time.png")
plot_grid(bar.density.pls.type, bar.difference.pls.type, nrow = 2, align = "hv",labels = "AUTO")
dev.off()



# differences of all the grid cells 
sm.difference.plot <- ggplot(clust_10, aes(mean_dens, diff.sm, color = orderedforesttype))+geom_point(size = 0.5)+scale_color_manual(values = compColors)+
  geom_hline(color = "grey",linetype = "dashed", yintercept = 0)+ylab("Modern - Past Tree Density (stems/ha)")+xlab("Past Tree Density")+theme_bw(base_size = 12)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  legend.title = element_blank())+ylim(-500, 200)+xlim(0, 700)


raw.difference.plot <-ggplot(clust_10[clust_10$diff.raw > -478.05,], aes(mean_dens, diff.sm, color = orderedforesttype))+geom_point(size = 0.5)+scale_color_manual(values = compColors)+
  geom_hline(color = "grey",linetype = "dashed", yintercept = 0)+ylab("Modern - Past Tree Density (stems/ha)")+xlab("Past Tree Density")+theme_bw(base_size = 12)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  legend.title = element_blank())+ylim(-500, 200)+xlim(0, 700)


library(scales)
library(sp)
library(rgdal)
library(raster)

# need to set up state outlines:
all_states <- map_data("state")
states <- subset(all_states, region %in% c('minnesota', 'wisconsin', 'michigan', "illinois",'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata.sp<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata.sp)


# 
map.diffs.sm <- ggplot(clust_10, aes(x,y, fill = diff.sm))+geom_raster()+scale_fill_gradient2(
  low = scales::muted("#7b3294"), mid = "white", high = scales::muted("#008837"),
  space = "Lab",
  guide = "colourbar",
  limits = c(-478.05, 200))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  coord_equal()+
  theme_bw(base_size = 12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.title = element_blank())

# ggplot(clust_10, aes(x,y, fill = diff.sm))+geom_raster()+ scale_fill_gradient2(
#   low = "#7b3294", mid = "white", high = "#008837",
#   space = "Lab",
#   guide = "colourbar",
#   limits = c(-478.05, 195.52)
# )+theme_bw(base_size = 12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.title = element_blank())
cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")

map.raw.pls <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=clust_10, aes(x=x, y=y, fill = PLSdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,500), name ="Tree \n Density", na.value = 'darkgrey') +
  theme_bw(base_size = 8)+ theme(legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()

map.raw.fia <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=clust_10, aes(x=x, y=y, fill = FIAdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,500), name ="Tree \n Density", na.value = 'darkgrey') +
  theme_bw(base_size = 8)+ theme(legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()



map.diffs.raw <- ggplot()+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = "grey")+
  geom_raster(data = clust_10[clust_10$diff.raw > -478.05,], aes(x = x,y = y, fill = diff.raw))+ scale_fill_gradient2(
  low = scales::muted("#7b3294"), mid = "white", high = scales::muted("#008837"),
  space = "Lab",
  guide = "colourbar",
  limits = c(-478.05, 200))+
  coord_equal()+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw(base_size = 12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.title = element_blank())


png(height = 8, width = 10, units = "in", res = 300, "outputs/paper_figs_unc/pls.fia.difference.plots.maps.png")
plot_grid(raw.difference.plot, sm.difference.plot, 
          map.diffs.raw, map.diffs.sm, ncol = 2, labels = c("A","B", "C", "D"))
dev.off()

png(height = 11, width = 10, units = "in", res = 300, "outputs/paper_figs_unc/pls.fia.difference.plots.maps.with.density.png")
plot_grid(map.raw.pls, map.raw.fia, 
          map.diffs.raw, map.diffs.sm, raw.difference.plot, sm.difference.plot, ncol = 2, labels = c("A","B", "C", "D", "E", "F"), align = "hv")
dev.off()

#ggplot(clust_10, aes(PLSdensity, diff.raw, color = orderedforesttype))+geom_point(size = 0.5)+scale_color_manual(values = compColors)+
 # geom_hline(color = "grey",linetype = "dashed", yintercept = 0)+ylab("Raw Modern - Past Tree Density (stems/ha)")+xlab("Past Tree Density")+theme_bw(base_size = 12)+xlim(0,1250)+ylim(-1000, 500)

# read simons estimates
simon <- read.csv("density.basal.biomass_alb_v0.9-10.csv")

ggplot(simon, aes(x,y, fill = stem.dens))+geom_raster()

ggplot(simon, aes(stem.dens))+geom_histogram()




library(sp)
library(rgdal)
library(raster)

stats.shp <- readOGR("us_alb/us_alb.shp")
plot(stats.shp)
plot(stats.shp[stats.shp$STATE_NAME %in% "Minnesota"])

coordinates(dens) <- ~ x + y
gridded(dens) <- TRUE
dens.rast <- raster::stack(dens)

dens <- as.data.frame(dens)
UMW <- raster::extract(x = dens.rast,
                       y = stats.shp,
                       df = TRUE)

UMW <- raster::extract(x = dens.rast,
                       y = stats.shp,
                       df = TRUE)

state.ids <- data.frame(state = stats.shp$STATE_NAME, 
           ID = 1:length(stats.shp$STATE_NAME))

UMW <- left_join(state.ids, UMW, by = "ID")
UMW.xy <- left_join(UMW, dens[, c("X", "x", "y")], by = "X")

ggplot(UMW.xy, aes(PLSdensity, fill = state))+geom_histogram()+facet_wrap(~state)
ggplot(UMW.xy, aes(x,y, fill = state))+geom_raster()

ggplot(UMW.xy[UMW.xy$state %in% c("Wisconsin", "Minnesota", "Michigan", "Illinois", "Indiana"),], aes(PLSdensity, FIAdensity, color = state))+geom_point()

#pls.fia.scatter.sm <-


  
ggplot(UMW.xy[UMW.xy$state %in% c("Wisconsin", "Minnesota", "Michigan", "Illinois", "Indiana"),], aes(mean_dens, mean_dens_fia, color = state))+geom_point(size = 0.5)+ylim(0, 700)+xlim(0,700)+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ylab("FIA mean density (statistically smoothed)")+xlab("PLS mean density (statistically smooothed)")

ggplot(UMW.xy[UMW.xy$state %in% c("Wisconsin", "Minnesota", "Michigan", "Illinois", "Indiana"),], aes(PLSdensity, FIAdensity, color = state))+geom_point(size = 0.5)+ylim(0, 700)+xlim(0,700)+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ylab("FIA density")+xlab("PLS density")


ggplot(UMW.xy[UMW.xy$state %in% c("Wisconsin", "Minnesota", "Michigan", "Illinois", "Indiana"),], aes(PLSdensity, FIAdensity, color = state))+geom_point(size = 0.5)+ylim(0, 700)+xlim(0,700)+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ylab("FIA density")+xlab("PLS density")


ggplot(UMW.xy[UMW.xy$state %in% c("Wisconsin", "Minnesota", "Michigan", "Illinois", "Indiana"),], aes(PLSdensity, FIAdensity, color = state))+geom_point(size = 0.5)+ylim(0, 700)+xlim(0,700)+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ylab("FIA density")+xlab("PLS density")+facet_wrap(~state)

ggplot(UMW.xy[UMW.xy$state %in% c("Wisconsin", "Minnesota", "Michigan"),], aes(PLSdensity, FIAdensity, color = state))+geom_point(size = 0.5)+ylim(0, 700)+xlim(0,700)+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ylab("FIA density")+xlab("PLS density")

ggplot(UMW.xy[UMW.xy$state %in% c("Wisconsin", "Minnesota", "Michigan"),], aes(PLSdensity, FIAdensity, color = state))+geom_point(size = 0.5)+ylim(0, 700)+xlim(0,700)+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+ylab("FIA density")+xlab("PLS density")


UMW.states <- UMW.xy[UMW.xy$state %in% c("Wisconsin", "Minnesota", "Michigan", "Illinois", "Indiana"),]

UMW.xy.simon <- merge(UMW.states, simon, by = c("x", "y"))

ggplot(UMW.xy.simon, aes(x, y, fill = PLSdensity))+geom_raster()

ggplot(UMW.xy.simon, aes(PLSdensity, stem.dens))+geom_point()+geom_abline(intercept = 0, slope = 1, color = "red")+ylim(0,900)+xlim(0,900)

# pls densities mostly match, with noise
ggplot(UMW.xy.simon, aes(mean_dens, stem.dens))+geom_point()+geom_abline(intercept = 0, slope = 1, color = "red")+ylim(0,900)+xlim(0,900)
ggplot(UMW.xy.simon, aes(mean_dens, PLSdensity))+geom_point()+geom_abline(intercept = 0, slope = 1, color = "red")+ylim(0,900)+xlim(0,900)


ggplot(UMW.xy.simon[!is.na(UMW.xy.simon$FIAdensity),], aes(mean_dens, mean_dens_fia))+geom_point()+geom_abline(intercept = 0, slope = 1, color = "red")+ylim(0,900)+xlim(0,900)
ggplot(UMW.xy.simon, aes(PLSdensity, FIAdensity))+geom_point()+geom_abline(intercept = 0, slope = 1, color = "red")+ylim(0,900)+xlim(0,900)


# for the upper midwest we don't see evidence of higher stem density overall.....
UMW.xy.simon[!is.na(UMW.xy.simon$FIAdensity),] %>% summarise(FIA.density.mean = mean(FIAdensity),
                                                             PLS.density.mean = mean(PLSdensity), 
                                                             goring2016.pls.mean = mean(stem.dens),
                                                             FIA.stat.mean = mean(mean_dens_fia),
                                                             PLS.stat.mean = mean(mean_dens))


# okay lets see about on a grid cell by grid cell basis if we have increased or decreased:
UMW.xy.simon$diff.FIA.PLS <- UMW.xy.simon$FIAdensity - UMW.xy.simon$PLSdensity
UMW.xy.simon$diff.FIAstat.PLSstat <- UMW.xy.simon$mean_dens_fia - UMW.xy.simon$mean_dens
UMW.xy.simon$diff.FIA.simon <- UMW.xy.simon$FIAdensity - UMW.xy.simon$stem.dens
UMW.xy.simon$diff.PLS.simon <- UMW.xy.simon$PLSdensity - UMW.xy.simon$stem.dens


UMW.xy.simon[!is.na(UMW.xy.simon$FIAdensity),] %>% summarise(raw.diff = mean(diff.FIA.simon),
                                                             stat.diff = mean(diff.FIAstat.PLSstat), 
                                                             FIA.simon.diff = mean(diff.FIA.simon),
                                                             PLS.simon.diff = mean(diff.PLS.simon))



ggplot(UMW.xy.simon, aes(PLSdensity,diff.FIA.PLS))+geom_point()+xlim(0, 1000)+ylim(-1000, 1000)
ggplot(UMW.xy.simon[!is.na(UMW.xy.simon$FIAdensity),], aes(PLSdensity,diff.FIA.PLS))+geom_point()+xlim(0, 1000)+ylim(-1000, 1000)


ggplot(UMW.xy.simon[!is.na(UMW.xy.simon$FIAdensity),], aes(mean_dens, diff.FIAstat.PLSstat))+geom_point()+xlim(0, 1000)+ylim(-1000, 1000)


# maps of overall soils and climate space for the supplement:
temppalette <- c("#d53e4f",
  "#fc8d59",
  "#fee08b",
  "#ffffbf",
  "#e6f598",
  "#99d594",
  "#3288bd")
mappalette <- c("#8c510a",
  "#d8b365",
  "#f6e8c3",
  "#c7eae5",
  "#5ab4ac",
  "#01665e")


soilpalette <- c("#ffffe5",
  "#fff7bc",
  "#fee391",
  "#fec44f",
  "#fe9929",
  "#ec7014",
  "#cc4c02",
  "#8c2d04")


# get state outlines:
# need to set up state outlines:
all_states <- map_data("state")
states <- subset(all_states, region %in% c('minnesota', 'wisconsin', 'michigan', "illinois",'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata.sp<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata.sp)




# maps of precipitation
pls.map.map <- ggplot(dens, aes(x,y, fill = MAP1910))+geom_raster()+theme_bw()+
  scale_fill_gradientn(colours = mappalette, limits = c(480, 1260), name ="Annual Precipitation", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 8),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()





fia.map.map <-ggplot(dens, aes(x,y, fill = MAP2011))+geom_raster()+theme_bw()+
  scale_fill_gradientn(colours = mappalette, limits = c(480, 1260), name ="Annual \n Precipitation", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  theme_bw(base_size = 8)+ theme(legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 8),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()



# maps of temperature (modern)
fia.temp.map <-ggplot(dens, aes(x,y, fill = modtmean))+geom_raster()+theme_bw()+
  scale_fill_gradientn(colours = temppalette, name ="Mean Annual \n Temparature \n (degC)", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  theme_bw(base_size = 8)+ theme(legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 8),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


# map of PC1

pls.PC1.map <-ggplot(dens, aes(x,y, fill = PC1))+geom_raster()+theme_bw()+
  scale_fill_gradientn(colours = temppalette, limits = c(-5.5, 4.1), name ="PC1", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  theme_bw(base_size = 8)+ theme(legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 8),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()

fia.pc1.map <- ggplot(dens, aes(x,y, fill = PC1fia))+geom_raster()+theme_bw()+
  scale_fill_gradientn(colours = temppalette, limits = c(-5.5, 4.1), name ="PC1 \n modern", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  theme_bw(base_size = 8)+ theme(legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 8),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


pls.soilmoist.map <- ggplot(dens, aes(x,y, fill = mean_GS_soil))+geom_raster()+theme_bw()+
  scale_fill_gradientn(colours = mappalette, name ="Soil \n Moisture", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  theme_bw(base_size = 8)+ theme(legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 8),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


pls.ppet.map <-ggplot(dens, aes(x,y, fill = GS_ppet))+geom_raster()+theme_bw()+
  scale_fill_gradientn(colours = mappalette, limits = c(-167,300), name ="P-PET", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  theme_bw(base_size = 8)+ theme(legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 8),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()

fia.ppet.map <-ggplot(dens, aes(x,y, fill = GS_ppet_mod))+geom_raster()+theme_bw()+
  scale_fill_gradientn(colours = mappalette, limits = c(-167,300),name ="P-PET \n modern", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  theme_bw(base_size = 8)+ theme(legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 8),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


sand.map <- ggplot(dens, aes(x,y, fill = sandpct))+geom_raster()+theme_bw()+
  scale_fill_gradientn(colours = soilpalette, name ="% sand", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  theme_bw(base_size = 8)+ theme(,legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 8),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


awc.map <- ggplot(dens, aes(x,y, fill = awc))+geom_raster()+theme_bw()+
  scale_fill_gradientn(colours = soilpalette, name ="available \n water \n capacity", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  theme_bw(base_size = 8)+ theme(legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 8),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()



cec.map <- ggplot(dens, aes(x,y, fill = CEC))+geom_raster()+theme_bw()+
  scale_fill_gradientn(colours = soilpalette, name ="Cation \n Exchange \n Capacity", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  theme_bw(base_size = 8)+ theme(legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 8),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


caco3.map <- ggplot(dens, aes(x,y, fill = CaCO3))+geom_raster()+theme_bw()+
  scale_fill_gradientn(colours = soilpalette, name ="Calcium \n Carbonate", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  theme_bw(base_size = 8)+ theme(legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 8),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


#legend.position=c(0.2, 0.25),
png(height = 4, width = 6, units = "in", res = 300, "outputs/maps_of_climate_and_soils.png")
plot_grid(fia.map.map+coord_equal(), fia.temp.map+coord_equal(), 
          fia.ppet.map+coord_equal(), fia.pc1.map+coord_equal(), 
          pls.soilmoist.map+coord_equal(), sand.map+coord_equal(), 
          awc.map+coord_equal(), caco3.map+coord_equal(), 
          cec.map+coord_equal(), ncol = 3, labels = "AUTO", align = "hv")
dev.off()

