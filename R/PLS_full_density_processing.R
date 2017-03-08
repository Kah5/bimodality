# PLS_full_density_processing.R
# This script performs many of the bimodality funcitons that FIA_density_processing.R does, but 
# this script includes all of the PLS data, not just the data that overlaps with FIA
version <- "1.6-5"
setwd( "C:/Users/JMac/Documents/Kelly/biomodality")
library(data.table)
library(reshape2)
library(dtplyr)
library(ggplot2)
library(hexbin)
library(grid)
library(gridExtra)
library(sp)
library(raster)
library(rgdal)


pls.inil <- read.csv(paste0('outputs/biomass_no_na_pointwise.ests_v',version, '.csv'))
pls.inil <- dcast(pls.inil, x + y + cell ~., mean, na.rm = TRUE, value.var = 'density')

colnames(pls.inil) <- c('x', 'y', 'cell','PLSdensity')
hist(pls.inil$PLSdensity, xlim = c(0, 400),breaks = 50)


#can aggregate by species
#pls.spec<- read.csv(paste0('outputs/density_tables.csv'))
pls.spec <- read.csv(paste0('outputs/biomass_no_na_pointwise.ests_v',version, '.csv'))
pls.spec <- dcast(pls.spec, x + y + cell ~spec, mean, na.rm = TRUE, value.var = 'density')
pls.spec$total <- rowSums(pls.spec[4:36], na.rm=TRUE)
hist(pls.spec$total)
pls.new <- pls.spec[,c('x', 'y', 'cell', 'total')]
colnames(pls.new) <- c('x', 'y', 'cell','PLSdensity')

umdw <- read.csv('data/plss_density_alb_v0.9-10.csv')
#umdw.mean <- dcast(umdw, x + y + cell ~., mean, na.rm = TRUE, value.var = 'density')
umdw$total <- rowSums(umdw[,5:32], na.rm= TRUE)
umdw.new <- umdw[,c('x', 'y', 'cell', 'total')]


colnames(umdw.new) <- c('x', 'y', 'cell', 'PLSdensity')
umdw.n <- umdw.new[,c('x', 'y', 'PLSdensity')]
coordinates(umdw.n) <- ~x+y
gridded(umdw.n) <- TRUE
umdw.rast <- raster(umdw.n)
plot(umdw.rast)
proj4string(umdw.rast) <- '+init=epsg:3175'

writeRaster(umdw.rast, "data/upper_midwest.ascii", overwrite = TRUE)

hist(umdw.new$PLSdensity, breaks = 25)

densitys <- rbind(pls.inil, umdw.new)
#coordinates(pls.inil)<- ~x+y

# read in lower mi data
#note that for some reason, 1 grid cell is duplicated
nodups <- densitys[!duplicated(densitys$cell),] # remove dups
dup <- densitys[duplicated(densitys$cell),] # what is the duplicated row?
#test <- rbind(nodups, dup)
densitys <- nodups

hist(densitys$PLSdensity, breaks = 50)
#map out density:
ggplot(densitys, aes(x,y,color = PLSdensity))+geom_point()


#keep only the 99th percentile of densitys---this is also what simon does
nine.nine.pct <- quantile(densitys[,4], probs = 0.995, na.rm=TRUE )
densitys$PLSdensity[densitys$PLSdensity > nine.nine.pct['99.5%']] <- nine.nine.pct['99.5%']

summary(densitys)

hist(densitys$PLSdensity, breaks = 50)
hist(densitys[densitys$PLSdensity > 0.5,]$PLSdensity, breaks = 50)

#map out 
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)
#################################
#plot maps of tree density
#################################
#dens.pr<- data.frame(dens.pr)


sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=densitys, aes(x=x, y=y, fill = PLSdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw()
png(paste0("outputs/v",version,"/PLS__full_tree_density_map.png"))
pls.map
dev.off()


write.csv(densitys, paste0("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_alb", version,".csv"))

################################################################
#comparison of FIA and PLS datasets to climate
###############################################################

past.precip.mo <- read.csv('outputs/pr_monthly_Prism_1895-1925_full.csv')

past.precip.mo$deltaP <- past.precip.mo$SI

mod.precip.mo <- read.csv('outputs/pr_monthly_Prism_30yrnorms_full.csv')

#read in modern precipitation seasonality:
#mod.precip.mo$moddeltaP <- rowSums(abs(mod.precip.mo[,2:13]-(mod.precip.mo[,14]/12)))/mod.precip.mo[,14]
mod.precip.mo <- mod.precip.mo[complete.cases(mod.precip.mo),]

#read in mean annual precipitaiton for modern and past
mod.precip <- read.csv('data/spec_table_30yr_prism_full.csv')
past.precip <- read.csv('outputs/pr_monthly_Prism_1895-1925_full.csv')

#read in mean annual temperature for modern and the past:
mod.tmean <- read.csv('outputs/tmean_30yr_prism.csv')
past.tmean <- read.csv('outputs/tmean_yr_Prism_1895-1925_full.csv')


mod.tmean.mo <- read.csv('outputs/tmean_monthly_Prism_30yrnorms_full.csv')
#rename temperature seasonality:
past.tmean$deltaT <- past.tmean$cv/100

mod.tmean.mo$moddeltaT <- mod.tmean.mo$cv/100 
#dens.pr <- merge(densitys, past.precip[,c('x', 'y', 'extract.avg.alb..dens.table...c..x....y....')], by =c('x', 'y'))
#dens.pr <- merge(densitys, past.precip[,c('x', 'y', 'total_.')], by =c('x', 'y'))
dens.pr <- merge(densitys, past.precip[,c('x', 'y', 'total')], by =c('x', 'y'))
dens.pr <- merge(dens.pr, mod.precip[,c('x', 'y', 'pr30yr')], by = c('x', 'y'))
colnames(dens.pr)[5:6] <- c('MAP1910', "MAP2011")

#now add the precipitation seasonality to the dataframe
dens.pr <- merge(dens.pr, mod.precip.mo[,c('x', 'y', 'SI')], by = c('x', 'y') )
dens.pr <- merge(dens.pr, past.precip.mo[,c('x', 'y', 'deltaP')], by = c('x', 'y') )
colnames(dens.pr)[7:8]<- c('moderndeltaP', 'pastdeltaP')
nodups <- dens.pr[!duplicated(dens.pr$cell),] #

dens.pr <- nodups

#now add the mean temperature to the dataframe
dens.pr <- merge(dens.pr, mod.tmean[,c('x', 'y', 'modtmean')], by = c('x', 'y') )
dens.pr <- merge(dens.pr, past.tmean[,c('x', 'y', 'Mean', 'deltaT')], by = c('x', 'y') )
colnames(dens.pr)[9:11] <- c('modtmean','pasttmean', 'deltaT')
dens.pr <- merge(dens.pr, mod.tmean.mo[,c('x', 'y', 'moddeltaT')], by = c('x', 'y') )


nodups <- dens.pr[!duplicated(dens.pr$cell),] #

dens.pr <- nodups
hist(nodups$PLSdensity, breaks =50)


write.csv(nodups, paste0("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_pr_alb",version,".csv"))

#nine.five.pct<- quantile(dens.pr$PLSdensity, probs = .95, na.rm=TRUE)
#dens.pr[dens.pr$PLSdensity>nine.five.pct,]$PLSdensity <- nine.five.pct #patch fix the overestimates of density

#plot histograms
hist(dens.pr$PLSdensity, breaks = 50, xlim = c(0,550), xlab = 'PLS density (stems/ha)', main = 'PLS Midwest Density')
#hist(dens.pr$FIAdensity, breaks = 50, xlim = c(0,550), xlab = 'FIA density(stems/ha)',main = 'FIA Midwest Density')

#plot raw data
plot(dens.pr$MAP1910,dens.pr$PLSdensity, xlab = 'Past MAP', ylab = 'PLS density')
#plot(dens.pr$MAP2011,dens.pr$FIAdensity, xlab = 'Modern MAP', ylab = 'Modern density')
plot(dens.pr$pastdeltaP, dens.pr$PLSdensity, xlab = "Past P seasonality", ylab = "PLS density")
#plot(dens.pr$moderndeltaP, dens.pr$FIAdensity, xlab = "Modern P seasonality", ylab = "FIA density")
plot(dens.pr$pasttmean, dens.pr$PLSdensity, xlab = 'Past Tmean', ylab = "PLSdensity")
#plot(dens.pr$MAP2011, dens.pr$PLSdensity, xlab = 'Modern MAP', ylab = 'PLS density')
#plot(dens.pr$MAP1910, dens.pr$FIAdensity, xlab = 'Past MAP', ylab = 'Modern density')
plot(dens.pr$pasttmean, dens.pr$MAP1910, xlab = 'Past Tmean', ylab = "Past Precip")
##########################
#read in soils data
sand8km <- raster("data/8km_UMW_sand1.tif")
plot(sand8km)

#sand1km <- raster("data/1km_UMW_sand1.tif")


# need to project sand to great lakes albers coordinate system
sand8km.alb <- projectRaster(sand8km, crs ='+init=epsg:3175')
#sand1km.alb <- projectRaster(sand1km, crs = '+init=epsg:3175')


#awc
awc8km <- raster("C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_awc1.tif")
#awc1km <- raster ("C:/Users/JMac/Box Sync/GSSURGOtifs/1km_UMW_awc1.tif")

awc8km.alb <- projectRaster(awc8km, crs ='+init=epsg:3175')
#awc1km.alb <- projectRaster(awc1km, crs = '+init=epsg:3175')


ksat8km <- raster("C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_ksat1.tif")
#ksat1km <- raster ("C:/Users/JMac/Box Sync/GSSURGOtifs/1km_UMW_ksat1.tif")

ksat8km.alb <- projectRaster(ksat8km, crs ='+init=epsg:3175')
#ksat1km.alb <- projectRaster(ksat1km, crs = '+init=epsg:3175')

#write albers rasters to files:
writeRaster(awc8km.alb, "C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_awcalb.tif", overwrite = TRUE)
writeRaster(sand8km.alb, "C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_sandalb.tif", overwrite = TRUE)
writeRaster(ksat8km.alb, "C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_ksatalb.tif", overwrite = TRUE)

#extract soils data using FIA and ps points
dens.pr$sandpct <- extract(sand8km.alb, dens.pr[,c('x', 'y')], method = 'bilinear')
dens.pr$awc <- extract(awc8km.alb, dens.pr[,c('x', 'y')])
dens.pr$ksat <- extract(ksat8km.alb, dens.pr[,c('x', 'y')])



summary(rowSums(dens.pr != 0, na.rm=TRUE))
#dens.pr$diff <- dens.pr$FIAdensity - dens.pr$PLSdensity

library(ggExtra)
library(ggplot2)

png(paste0('outputs/v',version,'/full/PLS_full_precip_hist_prism.png'))
#X11(width = 5)
p <- ggplot(dens.pr, aes(MAP1910, PLSdensity)) + geom_point() + theme_classic() + xlab('Mean Annual Precipitation (mm)') + ylab('Pre-Settlement \n Tree Density \n (Trees/hectare)')+
  xlim(450, 1200) + ylim(0, 800)+theme_bw()+
  theme(text = element_text(size = 20))
ggExtra::ggMarginal(p, type = "histogram",size = 3, colour = 'black', fill = 'red')
dev.off()

png(paste0('outputs/v',version,'/full/PLS__full_delta_precip_hist_prism.png'))
#X11(width = 5)
p <- ggplot(dens.pr, aes(pastdeltaP, PLSdensity)) + geom_point() + theme_classic() + xlab('Precipitation seasonality') + ylab('Pre-Settlement \n Tree Density \n (Trees/hectare)')+
  xlim(0,1) + ylim(0, 800)+theme_bw()+
  theme(text = element_text(size = 20))
ggExtra::ggMarginal(p, type = "histogram",size = 3, colour = 'black', fill = 'red')
dev.off()

sandpls <- ggplot(dens.pr, aes(sandpct, PLSdensity)) + geom_point() + theme_classic()+ xlab('% sand 1-30cm') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  theme_bw()+theme(text = element_text(size = 20))
awcpls <- ggplot(dens.pr, aes(awc, PLSdensity)) + geom_point() + theme_classic()+ xlab('AWC 1-30cm') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  theme_bw()+theme(text = element_text(size = 20))
ksatpls <- ggplot(dens.pr, aes(ksat, PLSdensity)) + geom_point() + theme_classic()+ xlab('ksat 1-30cm') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  theme_bw()+theme(text = element_text(size = 20))

png(paste0('outputs/v',version,'/full/PLS_full_sand.png'))
sandpls
dev.off()
png(paste0('outputs/v',version,'/full/PLS_full_awc.png'))
awcpls
dev.off()
png(paste0('outputs/v',version,'/full/PLS_full_ksat.png'))
ksatpls
dev.off()

##########################################################
# assign density classificaitons of savanna, forests, etc#
##########################################################

#Using the Rheumtella Classification scheme:
# prairie (<0.5 trees/ha)
# savanna (0.5-47 trees/ha)
# forest cover (>47 trees/ha)

dens.pr$ecotype<- 'test'
dens.pr[dens.pr$PLSdensity >= 47, ]$ecotype <-  "Forest"
dens.pr[dens.pr$PLSdensity < 47, ]$ecotype <-  "Savanna" 
dens.pr[dens.pr$PLSdensity == 0, ]$ecotype <-  "prairie"

ggplot(data = dens.pr, aes(x = x, y = y, color = ecotype)) + geom_point()

write.csv(dens.pr, paste0("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_pr_alb",version,".csv"))
# we could also do kmeans clustering, but this results in some overlap between clusters
fit.km <- kmeans(dens.pr$PLSdensity, 4, nstart=25)
dens.pr$kmeans <- fit.km$cluster
plot(fit.km$cluster, dens.pr$PLSdensity)

ggplot(dens.full, aes(x = kmeans, y = PLSdensity, group = kmeans))+geom_boxplot()

####################################################
#PCA analysis
#####################################################
dens.rm <- na.exclude(dens.pr)
dens.rm <- data.frame(dens.rm)
scale.dens <- scale(dens.rm[, c('MAP1910', "MAP2011", "moderndeltaP", 
                                "pastdeltaP", "modtmean", "pasttmean",
                                "moddeltaT", "deltaT", "sandpct", "awc")]) #PC all but ksat and diff
dens.dens <- dens.rm[, c('PLSdensity')] # pls density

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
dens.pca <- princomp(scale.dens[,c('MAP1910',   
                                   "pastdeltaP", "pasttmean",
                                    "deltaT", "sandpct", "awc")],
                     na.rm=TRUE) 

plot(dens.pca)
dens.pca$loadings
scores <- data.frame(dens.pca$scores[,1:2])
scores$PLS <- dens.dens
scores$ecotype <- dens.rm$ecotype

dens.rm$PC1 <- scores[,1]
dens.rm$PC2 <- scores[,2]
dens.rm <- data.frame(dens.rm)
PC <- dens.pca
#plot scores by tree density in trees per hectare
png(paste0("outputs/v", version,"/full/pca_no_loadings.png"))
ggplot(scores, aes(x = Comp.1, y = Comp.2, color = PLS)) +geom_point()+
  scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +theme_bw()
dev.off()

data <- data.frame(obsnames=row.names(PC$scores[1]), PC$Comp.1)
ggbiplot(dens.pca)

plot(dens.pca, type = "l")
print(dens.pca)
summary(dens.pca)
biplot(dens.pca)

# using biplot
library(ggbiplot)
g <- ggbiplot(dens.pca, obs.scale = 1, var.scale = 1, labels.size
              = 20,alpha = 0)
# layer the points from pls underneath the pca biplot
# using a clever trick to manipulate the layers
g$layers <- c(geom_point(data = scores, aes(x = Comp.1, y = Comp.2, color = PLS)), g$layers)
g <- g + scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +theme_bw(base_size = 15) 

#write to png
png(width = 800, height = 400,paste0("outputs/v",version,"/full/pca_biplot.png"))
g + ggtitle('PCA biplot with PLS tree density')
dev.off()

#now plot biplot with the classification colors
g2 <- ggbiplot(dens.pca, obs.scale = 1, var.scale = 1, labels.size
               = 20,alpha = 0)
# layer the points from pls underneath the pca biplot
# using a clever trick to manipulate the layers
g2$layers <- c(geom_point(data = scores, aes(x = Comp.1, y = Comp.2, color = ecotype)), g2$layers)

png(width = 800, height = 400,paste0("outputs/v",version,"/full/pca_biplot_class.png"))
g2 + ggtitle('PCA biplot with Rheumtell density classification')
dev.off()

# add the scores from pca to the dens.pr data frame


test1 <- merge(dens.pr,unique(dens.rm[,c('x','y', 'PC1', 'PC2')]),  by = c('x','y'), all.x= T)
#convert dens.rm to the new dens.pr---we only lose ~150 grid cells
dens.pr <- test1

##################################################################
# PCA on FIA dataset
##################################################################
#dens.fia <- dens.rm[, c('FIAdensity')] # pls density

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
dens.pca <- princomp(scale.dens[,c("MAP2011", "moderndeltaP", 
                                   "modtmean", "moddeltaT", 
                                   "sandpct", "awc")],
                     na.rm=TRUE) 

plot(dens.pca)
dens.pca$loadings
scores <- data.frame(dens.pca$scores[,1:2])
#scores$FIA <- dens.fia
#scores$ecotype <- dens.rm$fiaecotype

dens.rm$PC1fia <- scores[,1]
dens.rm$PC2fia <- scores[,2]
dens.rm <- data.frame(dens.rm)
PC <- dens.pca
#plot scores by tree density in trees per hectare

data <- data.frame(obsnames=row.names(PC$scores[1]), PC$Comp.1)

# using biplot
library(ggbiplot)
g <- ggbiplot(dens.pca, obs.scale = 1, var.scale = 1, labels.size
              = 20,alpha = 0)
# layer the points from pls underneath the pca biplot
# using a clever trick to manipulate the layers
g$layers <- c(geom_point(data = scores, aes(x = Comp.1, y = Comp.2, color = "black")), g$layers)
g <- g #+ scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +theme_bw(base_size = 15) 

#write to png
png(width = 800, height = 400,"outputs/v1.6/pca_fia_biplot.png")
g + ggtitle('PCA biplot with PLS tree density')
dev.off()



# add the scores from pca to the dens.pr data frame
#this merge is not working
test1 <- merge(dens.pr, unique(dens.rm[,c('x','y','cell', 'PC1fia', 'PC2fia')]),  by = c('x','y','cell'), all.x = T)

#convert dens.rm to the new dens.pr---we only lose ~150 grid cells
dens.pr <- test1
write.csv(dens.pr, "data/dens_pr_FULL_PLS_FIA_with_cov.csv")

##############################################
# Read in CMIP 4 projections
#############################################
ccesm <- read.csv("outputs/CCSM4pr_t_2070_full.csv")



dens.pr <- merge(dens.pr, ccesm, by = c("x", "y"))


# for each rcp, we need to determine the places outside of the range of PLS climate:
# function to find climate space outside of PLS range:

find.noanalog<- function(dens.pr,rcp){
 dens.pr[,c(paste0("rcp",rcp,"NA"))] <- "Within Range"
 dens.pr[,c(paste0('rcp',rcp,"NAclim"))] <- "Within Range" # column labeling the reason climate is out of range
 #dens.pr[,c(paste0('rcp',rcp,"NAclimhigh"))] <- "Within Range" # column labeling the reason climate is out of range
 
 # rcp 4.5
prange <- range(dens.pr$MAP1910)
trange <- range(dens.pr$pasttmean)
pcvrange <- range(dens.pr$pastdeltaP)
tcvrange <- range(dens.pr$deltaT)

# identify climate space outside of the modern/pls range
precip.out <- dens.pr[dens.pr[,c(paste0("pr.",rcp))] < prange[1] | dens.pr[,c(paste0("pr.",rcp))] > prange[2], ]
pcv.out <- dens.pr[dens.pr[,c(paste0("pr.",rcp,"SI"))] < pcvrange[1] | dens.pr[,c(paste0("pr.",rcp,"SI"))]> pcvrange[2], ]
temp.out <- dens.pr[dens.pr[,c(paste0("tn.",rcp))] < trange[1] | dens.pr[,c(paste0("tn.",rcp))]> trange[2], ]
tcv.out <- dens.pr[dens.pr[,c(paste0("tn.",rcp,"cv"))] < tcvrange[1] | dens.pr[,c(paste0("tn.",rcp,"cv"))]> tcvrange[2], ]

# identify grid cells with climate space lower than modern/pls
pcv.low <- dens.pr[dens.pr[,c(paste0("pr.",rcp,"SI"))] < pcvrange[1],  ]
tcv.low <- dens.pr[dens.pr[,c(paste0("tn.",rcp,"cv"))] < tcvrange[1] , ]
precip.low <- dens.pr[dens.pr[,c(paste0("pr.",rcp))] < prange[1], ]
temp.low <- dens.pr[dens.pr[,c(paste0("tn.",rcp))] < trange[1] , ]

dens.pr[dens.pr$cell %in% precip.low$cell, c(paste0("rcp",rcp,"NAclim"))] <- "Low precip"
dens.pr[dens.pr$cell %in% temp.low$cell, c(paste0("rcp",rcp,"NAclim"))] <- "Low temp"
dens.pr[dens.pr$cell %in% tcv.low$cell, c(paste0("rcp",rcp,"NAclim"))] <- "Low temp CV"
dens.pr[dens.pr$cell %in% pcv.low$cell, c(paste0("rcp",rcp,"NAclim"))] <- "Low precip SI"

# identify the gird cells that are higher than modern/pls
pcv.high <- dens.pr[ dens.pr[,c(paste0("pr.",rcp,"SI"))]> pcvrange[2], ]
tcv.high <- dens.pr[ dens.pr[,c(paste0("tn.",rcp,"cv"))]> tcvrange[2], ]
precip.high <- dens.pr[ dens.pr[,c(paste0("pr.",rcp))] > prange[2], ]
temp.high <- dens.pr[ dens.pr[,c(paste0("tn.",rcp))]> trange[2], ]

dens.pr[dens.pr$cell %in% precip.high$cell, c(paste0("rcp",rcp,"NAclim"))] <- "High precip"
dens.pr[dens.pr$cell %in% temp.high$cell, c(paste0("rcp",rcp,"NAclim"))] <- "High temp"
dens.pr[dens.pr$cell %in% tcv.high$cell, c(paste0("rcp",rcp,"NAclim"))] <- "High temp CV"
dens.pr[dens.pr$cell %in% pcv.high$cell, c(paste0("rcp",rcp,"NAclim"))] <- "High precip SI"


dens.pr[dens.pr$cell %in% precip.out$cell, c(paste0("rcp",rcp,"NA"))] <- "no-analog"
dens.pr[dens.pr$cell %in% temp.out$cell, c(paste0("rcp",rcp,"NA"))] <- "no-analog"
dens.pr[dens.pr$cell %in% tcv.out$cell, c(paste0("rcp",rcp,"NA"))] <- "no-analog"
dens.pr[dens.pr$cell %in% pcv.out$cell, c(paste0("rcp",rcp,"NA"))] <- "no-analog"

dens.pr[,c("x","y", "cell", paste0("rcp",rcp,"NA"), paste0('rcp', rcp,"NAclim"))]

}

# find noanalog climates, label high or low
NArcp26 <- find.noanalog(dens.pr=dens.pr, rcp = "26")
NArcp45 <- find.noanalog(dens.pr=dens.pr, rcp = "45")
NArcp85 <- find.noanalog(dens.pr=dens.pr, rcp = "85")
NArcp60 <- find.noanalog(dens.pr = dens.pr, rcp = "60")

dens.pr <- merge(dens.pr, NArcp85, by = c("x", "y", "cell"))
dens.pr <- merge(dens.pr, NArcp60, by = c("x", "y", "cell"))
dens.pr <- merge(dens.pr, NArcp45, by = c("x", "y", "cell"))
dens.pr <- merge(dens.pr, NArcp26, by = c("x", "y", "cell"))

# maps for places where rcps predict no-analog climates for 2070:
a <- ggplot(dens.pr, aes(x,y, color = rcp26NA))+geom_point()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw()+coord_equal()+theme_bw() + ggtitle("RCP 2.6")
b <- ggplot(dens.pr, aes(x,y, color = rcp45NA))+geom_point()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw() + ggtitle("RCP 4.5")
c <- ggplot(dens.pr, aes(x,y, color = rcp60NA))+geom_point()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw() + ggtitle("RCP 6.0")
d <- ggplot(dens.pr, aes(x,y, color = rcp85NA))+geom_point()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw() + ggtitle("RCP 8.5")
source("R/grid_arrange_shared_legend.R")

png(height = 4, width = 12, units = "in", res = 300, "outputs/v1.6-5/full/no-analog-ccsm4-climates.png")
grid_arrange_shared_legend(a,b,c,d, nrow = 1, ncol = 4)
dev.off()

# plot reasons for no-analog climate:

a <- ggplot(dens.pr, aes(x,y, color = rcp26NAclim))+geom_point()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw()+coord_equal()+theme_bw() + ggtitle("RCP 2.6")
b <- ggplot(dens.pr, aes(x,y, color = rcp45NAclim))+geom_point()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw() + ggtitle("RCP 4.5")
c <- ggplot(dens.pr, aes(x,y, color = rcp60NAclim))+geom_point()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw() + ggtitle("RCP 6.0")
d <- ggplot(dens.pr, aes(x,y, color = rcp85NAclim))+geom_point()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw() + ggtitle("RCP 8.5")
source("R/grid_arrange_shared_legend.R")

png(height = 4, width = 12, units = "in", res = 300, "outputs/v1.6-5/full/no-analog-ccsm4-climates-reason.png")
grid_arrange_shared_legend(a,b,c,d, nrow = 1, ncol = 4)
dev.off()

# predict PCA with the diffrent projections:

res<-princomp(scale.dens[,c('MAP1910',   
                            "pastdeltaP", "pasttmean",
                            "deltaT", "sandpct", "awc")])

#created a function to predict the PC scores for the different RCP's using the PCA from PLS
predict.PCA<- function(rcp){
cc <- scale(dens.pr[,c(paste0("pr.",rcp), 
                       paste0("pr.",rcp,"SI"), 
                         paste0("tn.",rcp),
                           paste0("tn.",rcp, "cv"),
                                               "sandpct","awc")])
colnames(cc) <- c('MAP1910',   
                  "pastdeltaP", "pasttmean",
                  "deltaT", "sandpct", "awc")

newscores <- predict(res,newdata=cc) # predict new scores based on the prevous 

dens.pr[,paste0('PC1_cc',rcp)] <- newscores[,1]
dens.pr[,paste0('PC2_cc',rcp)]  <- newscores[,2]
dens.pr
}

dens.pr <- predict.PCA("26")
dens.pr <- predict.PCA("45")
dens.pr <- predict.PCA("60")
dens.pr <- predict.PCA("85")

#################################################################################################
#separate density values by precipitation bins, sand bins, and soil bins for bimodality analysis#
#################################################################################################

dens.pr$plsprbins <- cut(dens.pr$MAP1910, #labels = c('350-400mm', '400-450mm', '450-500mm', '550-600mm', '600-650mm','650-700mm','700-750mm','750-800mm','800-850mm',  '850-900mm','900-950mm','950-1000mm','1000-1050mm','1050-1100mm', '1100-1150mm','1150-1200mm', '1200-1250mm', '1250-1300mm'),
                         breaks=c(200,250,300,400,500,600, 700,800,900, 1000,1100,1200, 1400))

# create labeling function that takes the beginning of var range, end of var range, and the value to split by:
label.breaks <- function(beg, end, splitby){
  labels.test <- data.frame(first = seq(beg, end, by = splitby), second = seq((beg + splitby), (end + splitby), by = splitby))
  labels.test <- paste (labels.test$first, '-' , labels.test$second)
  labels.test
}
# use the label.breaks function and cut to cut environmental data up into different bins
dens.pr$plsprbins50 <- cut(dens.pr$MAP1910, breaks = seq(250, 1350, by = 50), labels = label.breaks(250, 1300, 50))
dens.pr$plsprbins100 <- cut(dens.pr$MAP1910, breaks = seq(250, 1350, by = 100), labels = label.breaks(250, 1250, 100))
dens.pr$plsprbins75 <- cut(dens.pr$MAP1910, breaks = seq(250, 1350, by = 75), labels = label.breaks(250, 1275, 75))
dens.pr$plsprbins150 <- cut(dens.pr$MAP1910, breaks = seq(250, 1350, by = 150), labels = label.breaks(250, 1250, 150))
dens.pr$plsprbins25 <- cut(dens.pr$MAP1910, breaks = seq(250, 1350, by = 25), labels = label.breaks(250, 1325,  25))
dens.pr$sandbins <- cut(dens.pr$sandpct, breaks = seq(0, 100, by = 10), labels = label.breaks(0,90, 10))
dens.pr$ksatbins <- cut(dens.pr$ksat, breaks = seq(0,300, by = 10), labels = label.breaks(0,290, 10))
dens.pr$pastdeltPbins <- cut(dens.pr$pastdeltaP, breaks = seq(0,1, by = .10), labels = label.breaks(0,0.9, 0.1))
dens.pr$pasttmeanbins <- cut(dens.pr$pasttmean, breaks = seq(0,15, by = 1.5), labels = label.breaks(0,14, 1.5))
dens.pr$PC1bins <- cut(dens.pr$PC1, breaks = seq(-5,5.5, by = 1), labels = label.breaks(-5,4.5, 1))
dens.pr$PC2bins <- cut(dens.pr$PC2, breaks = seq(-4,3, by = 0.5), labels = label.breaks(-4,2.5, 0.5))
dens.pr$PC1fiabins <- cut(dens.pr$PC1fia, breaks = seq(-5,5.5, by = 1), labels = label.breaks(-5,4.5, 1))
dens.pr$PC2fiabins <- cut(dens.pr$PC2fia, breaks = seq(-3,4, by = 0.5), labels = label.breaks(-3,3.5, 0.5))
dens.pr$PC1_cc26bins <- cut(dens.pr$PC1_cc26,  breaks = seq(-5,5.5, by = 1), labels = label.breaks(-5,4.5, 1))
dens.pr$PC2_cc26bins <- cut(dens.pr$PC2_cc26, breaks = seq(-3,4, by = 0.5), labels = label.breaks(-3,3.5, 0.5))
dens.pr$PC1_cc45bins <- cut(dens.pr$PC1_cc45,  breaks = seq(-5,5.5, by = 1), labels = label.breaks(-5,4.5, 1))
dens.pr$PC2_cc45bins <- cut(dens.pr$PC2_cc45, breaks = seq(-3,4, by = 0.5), labels = label.breaks(-3,3.5, 0.5))
dens.pr$PC1_cc60bins <- cut(dens.pr$PC1_cc60,  breaks = seq(-5,5.5, by = 1), labels = label.breaks(-5,4.5, 1))
dens.pr$PC2_cc60bins <- cut(dens.pr$PC2_cc60, breaks = seq(-3,4, by = 0.5), labels = label.breaks(-3,3.5, 0.5))
dens.pr$PC1_cc85bins <- cut(dens.pr$PC1_cc85, breaks = seq(-5,5.5, by = 1), labels = label.breaks(-5,4.5, 1))
dens.pr$PC2_cc85bins <- cut(dens.pr$PC2_cc85, breaks = seq(-3,4, by = 0.5), labels = label.breaks(-3,3.5, 0.5))


test<- dens.pr[!is.na(dens.pr),]

write.csv(dens.pr, "data/PLS_full_dens_pr_with_bins.csv")

melted <- melt(test, id.vars = c("x", 'y', 'cell', 'plsprbins',  'plsprbins50','plsprbins75', 
                                 'plsprbins100','plsprbins150', 'plsprbins25', 
                                 'MAP1910',  
                                  'sandpct', 'awc', 'ksat', 'sandbins', 'ksatbins', 
                                 'pastdeltaP','deltaT',  'pastdeltPbins', 'pasttmeanbins',
                                 'pasttmean', "PC1", "PC2",'PC1bins', 'PC2bins', 
                                 "PC1fiabins", "PC2fiabins","PC1_cc2.6bins",'PC2_cc2.6bins','ecotype')) 

#load map data for future maps
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)

#pdf("outputs/binned_histograms_pr_AGU_12_6_16_large_bins.pdf")
png(paste0('outputs/v',version,'/full/PLS_full_density_histogram.png'))#
ggplot(dens.pr, aes(PLSdensity)) +geom_histogram(fill= "#D55E00",color = "black") +xlim(0, 700)+ xlab("PLS tree density (stems/ha)")+ ylab('# grid cells')+ 
  theme_bw(base_size = 25)#+ facet_wrap(~plsprbins)
dev.off()

#####################################################
# CREATE HEXBIN PLOTS of Density vs. envtl variables#
#####################################################

library(lattice)

# for precipitation
png(paste0('outputs/v',version,'/full/PLS_precipitation_hexbin.png'))
ggplot(dens.pr, aes(MAP1910, PLSdensity)) +geom_hex()+ylim(0,600)+ xlim(400,1400) + 
  theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits = c(1,130))+
  xlab(' Mean Annual Precipitation (mm) \n PRISM 1900-1910') +ylab(" Past Tree Density (stems/ha)")
dev.off()

#for tmean
png(paste0('outputs/v',version,'/full/PLS_tmean_hexbin.png'))
ggplot(dens.pr, aes(pasttmean, PLSdensity)) +geom_hex()+ylim(0,600)+ xlim(0,15) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits=c(0,90))+
  xlab(' Mean Annual Temperature (degC)\n PRISM 1900-1910') +ylab(" Past Tree Density (stems/ha)") 
dev.off()

#for PC1
png(paste0('outputs/v',version,'/full/PLS_PC1_hexbin.png'))
ggplot(dens.pr, aes(PC1, PLSdensity)) +geom_hex()+ylim(0,600)+ xlim(-5,9) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits=c(0,90))+
  xlab(' Principal component 1') +ylab(" Past Tree Density (stems/ha)") 
dev.off()

#for PC2
png(paste0('outputs/v',version,'/full/PLS_PC2_hexbin.png'))
ggplot(dens.pr, aes(PC2, PLSdensity)) +geom_hex()+ylim(0,600)+ xlim(-5,5) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits=c(0,90))+
  xlab(' Principal component 2') +ylab(" Past Tree Density (stems/ha)") 
dev.off()

rbpalette <- c('red', "blue")
ggplot(melted, aes(value, fill = variable)) +geom_density(alpha = 0.3)  +xlim(0, 400)+ facet_grid(plsprbins~., scales = 'free_y')+scale_fill_brewer(palette = "Set1")

##################################################
#plot out denisty distriburions binned by envt#
##################################################

png(paste0('outputs/v',version,'/full/precipitation_full_by_bins.png'))
ggplot(melted, aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)  +xlim(0, 400)+ facet_wrap(~plsprbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()

#plot by sandiness
png(paste0('outputs/v',version,'/full/sand_full_by_bins.png'))
ggplot(melted, aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)  +xlim(0, 400)+ facet_wrap(~sandbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()

ggplot(melted, aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)  +xlim(0, 400)+ facet_wrap(~pasttmeanbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')


#plot out climate space:
png(paste0('outputs/v',version,'/full/precip_vs_temp_full_pls.png'))
ggplot(dens.pr, aes(x = MAP1910, y = pasttmean, colour = PLSdensity))+geom_point()+
  scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +theme_bw()
dev.off()

#plot by ksat
png(paste0('outputs/v',version,'/full/ksat_full_by_bins.png'))
ggplot(melted, aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)  +xlim(0, 400)+ facet_wrap(~ksatbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()

#plot by past deltaPbins 
png(paste0('outputs/v',version,'/full/pastDeltaP_full_by_bins.png'))
ggplot(melted, aes(value, colour = variable))+ geom_density(size = 2, alpha = 0.1) +xlim(0, 400)+ facet_wrap(~pastdeltPbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()

#plot by mod deltaPbins
png(paste0('outputs/v',version,'/full/moddeltaP_full_by_bins.png'))
ggplot(melted, aes(value, colour = variable))+ geom_density(size = 2, alpha = 0.1) +xlim(0, 400)+ facet_wrap(~moddeltPbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()

###################################
#calculate bimodality coefficients#
####################################
library(modes)

#this function uses the bimodality_coefficient funcition in the modes library to calculate the 
#bimodality coefficient of the density (FIA or PLS) within a given set of bins (climate, sand, etc)

calc.BC <- function(data, binby, density){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 1)
  for (i in 1:length(bins)){
    coeffs[i]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
  }
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$V1 <- as.numeric(as.character(coef.bins$V1))
  #coef.bins
  coef.bins <- coef.bins[order(as.numeric(as.character(coef.bins$bins))),]
  coef.bins$bins <- factor(coef.bins$bins, levels = coef.bins$bins[order(as.numeric(as.character(coef.bins$bins)))])# reorder so it plots well
    a <- ggplot(coef.bins, aes(x = bins, y = V1))+geom_point()+
    geom_hline( yintercept = 5/9)+ylim(0,1)+theme_bw()+
    theme(axis.text = element_text(angle = 90))+
    xlab('Mean annual Precipitaiton range (mm/yr)') + ylab('Bimodality Coefficient')+
    ggtitle(paste0('Bimodality coefficients for ', binby))
  a
 
}

pdf(paste0('outputs/v',version,'/bimodality_coefficient_binplots.pdf'))
calc.BC(data = dens.pr, binby = 'plsprbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'plsprbins100', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'plsprbins75', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'plsprbins25', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'sandbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'ksatbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'pastdeltPbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'pasttmeanbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'PC1bins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'PC2bins', density = "PLSdensity")
dev.off()

png(height = 400, width = 400, paste0('outputs/v',version,'/full/PLS_PC1_PC2_BC_bins.png'))
pushViewport(viewport(layout = grid.layout(2, 1)))
print(calc.BC(data = dens.pr, binby = 'PC1bins', density = "PLSdensity")+ ggtitle('BC for PC1 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(calc.BC(data = dens.pr, binby = 'PC2bins', density = "PLSdensity")+ ggtitle('BC for PC2  PLS'),   vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
dev.off()

#calc.BC(data = dens.pr, binby = 'fiaprbins25', density = "FIAdensity")

#calc.BC(data = dens.pr, binby = 'fiaprbins', density = "PLSdensity")

calc.BC(data = dens.pr, binby = 'sandbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'ksatbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'pastdeltPbins', density = "PLSdensity")
#dev.off()


#this function maps out the region that is bimodal & uses the ecotypes to classify this
map.bimodal <- function(data, binby, density){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 2)
  for (i in 1:length(bins)){
    coeffs[i,1]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
    coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
  merged <- merge(coef.bins, dens.pr, by.x = "bins",by.y = binby)
  #define bimodality
  merged$bimodal <- "Stable"
  merged[merged$BC >= 0.55 & merged$dipP <= 0.05,]$bimodal <- "Bimodal"
  
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  if(density == "PLSdensity"){
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$ecotype)
  }else{
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$fiaecotype)
    
  }
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = classification))+ scale_fill_manual(values = c(
      '#1a9641', # dark green
      '#fdae61', # light orange
      '#a6d96a', # light green
      '#d7191c', # red
      '#fee08b', # tan
      'black'), limits = c("Stable Forest" , 'Stable Savanna', 'Bimodal Forest', "Bimodal Savanna", 'Bimodal prairie', 'Stable prairie') )+
    theme_bw()+
    xlab("easting") + ylab("northing") +coord_equal()+
    ggtitle(paste0(binby, ' for ',density))
  
}


#map out bimodalities--note the region varies by bin size
pdf(paste0('outputs/v',version,'/full/bimodal_maps.pdf'))
map.bimodal(data = dens.pr, binby = 'plsprbins50', density = "PLSdensity")
#map.bimodal(data = dens.pr, binby = 'fiaprbins', density = "FIAdensity")
map.bimodal(data = dens.pr, binby = 'plsprbins100', density = "PLSdensity")
#map.bimodal(data = dens.pr, binby = 'fiaprbins100', density = "FIAdensity")
map.bimodal(data = dens.pr, binby = 'plsprbins75', density = "PLSdensity")
#map.bimodal(data = dens.pr, binby = 'fiaprbins75', density = "FIAdensity")
map.bimodal(data = dens.pr, binby = 'plsprbins25', density = "PLSdensity")
#map.bimodal(data = dens.pr, binby = 'fiaprbins25', density = "FIAdensity")
#map.bimodal(data = dens.pr, binby = 'fiaprbins', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'sandbins', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'ksatbins', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'pastdeltPbins', density = "PLSdensity")
dev.off()

png(height = 6, width = 10, units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_PC1_PC2_map.png'))
pushViewport(viewport(layout = grid.layout(1, 2)))
print(map.bimodal(data = dens.pr, binby = 'PC1bins', density = "PLSdensity")+ ggtitle('Bimodal Regions for PC1 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal(data = dens.pr, binby = 'PC2bins', density = "PLSdensity") + ggtitle('Bimodal Regions for PC2 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

png(height = 6, width = 15, units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_Precip_25_50_75_map.png'))
pushViewport(viewport(layout = grid.layout(1, 3)))
print(map.bimodal(data = dens.pr, binby = 'plsprbins25', density = "PLSdensity")+ ggtitle('Bimodal Regions for Precip25 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal(data = dens.pr, binby = 'plsprbins50', density = "PLSdensity") + ggtitle('Bimodal Regions for  Precip50 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(map.bimodal(data = dens.pr, binby = 'plsprbins75', density = "PLSdensity") + ggtitle('Bimodal Regions for  Precip75 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
dev.off()


png(height = 6, width = 15, units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_tmean_delta_p_map.png'))
pushViewport(viewport(layout = grid.layout(1, 3)))
print(map.bimodal(data = dens.pr, binby = 'pastdeltPbins', density = "PLSdensity")+ ggtitle('Bimodal Regions for deltaP PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal(data = dens.pr, binby = 'pasttmeanbins', density = "PLSdensity") + ggtitle('Bimodal Regions for  tmean PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(map.bimodal(data = dens.pr, binby = 'sandbins', density = "PLSdensity") + ggtitle('Bimodal Regions for sand PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
dev.off()



#make alternative maps that only plot prairie as one type of prairie:
map.bimodal.5c <- function(data, binby, density){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 2)
  for (i in 1:length(bins)){
    coeffs[i,1]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)])) # should we also do BC on the denisty estimated function?
    coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
  merged <- merge(coef.bins, dens.pr, by.x = "bins",by.y = binby)
  #define bimodality
  merged$bimodal <- "Stable"
  merged[merged$BC >= 0.55 & merged$dipP <= 0.05,]$bimodal <- "Bimodal"
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  if(density == "PLSdensity"){
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$ecotype)
    merged[merged$classification %in% 'Bimodal prairie',]$classification <- "Prairie"
    merged[merged$classification %in% 'Stable prairie',]$classification <- "Prairie"
    
    }else{
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$fiaecotype)
    
  }
  
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = classification))+ scale_fill_manual(values = c(
      '#01665e', # light green
      '#5ab4ac', # dark teal
      '#8c510a', # red
      '#d8b365', # light tan
      '#fee08b', # tan
      'black'), limits = c('Bimodal Forest',"Stable Forest" ,   "Bimodal Savanna", 'Stable Savanna','Prairie') )+
    theme_bw()+
    xlab("easting") + ylab("northing") +coord_equal()+
    ggtitle(paste0(binby, ' for ',density))
  
}

pdf(paste0('outputs/v',version,'/full/bimodal_maps_5col.pdf'))
map.bimodal.5c(data = dens.pr, binby = 'plsprbins50', density = "PLSdensity")
#map.bimodal.5c(data = dens.pr, binby = 'fiaprbins', density = "FIAdensity")
map.bimodal.5c(data = dens.pr, binby = 'pasttmeanbins', density = "PLSdensity")
map.bimodal.5c(data = dens.pr, binby = 'plsprbins100', density = "PLSdensity")
#map.bimodal.5c(data = dens.pr, binby = 'fiaprbins100', density = "FIAdensity")
map.bimodal.5c(data = dens.pr, binby = 'plsprbins75', density = "PLSdensity")
#map.bimodal.5c(data = dens.pr, binby = 'fiaprbins75', density = "FIAdensity")
map.bimodal.5c(data = dens.pr, binby = 'plsprbins25', density = "PLSdensity")
#map.bimodal.5c(data = dens.pr, binby = 'fiaprbins25', density = "FIAdensity")
#map.bimodal.5c(data = dens.pr, binby = 'fiaprbins', density = "PLSdensity")
map.bimodal.5c(data = dens.pr, binby = 'sandbins', density = "PLSdensity")
map.bimodal.5c(data = dens.pr, binby = 'ksatbins', density = "PLSdensity")
map.bimodal.5c(data = dens.pr, binby = 'pastdeltPbins', density = "PLSdensity")
dev.off()


png(height = 6, width =5 , units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_PC1_map_5col.png'))
map.bimodal.5c(data = dens.pr, binby = 'PC1bins', density = "PLSdensity")
dev.off()

png(height = 6, width =5 , units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_PC1fiaclimate_map_5col.png'))
map.bimodal.5c(data = dens.pr, binby = 'PC1fiabins', density = "PLSdensity")
dev.off()

png(height = 6, width = 10, units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_PC1_PC2_map_5col.png'))
pushViewport(viewport(layout = grid.layout(1, 2)))
print(map.bimodal.5c(data = dens.pr, binby = 'PC1bins', density = "PLSdensity")+ ggtitle(' PC1 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal.5c(data = dens.pr, binby = 'PC2bins', density = "PLSdensity") + ggtitle('PC2 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

png(height = 6, width = 15, units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_Precip_25_50_75_map_5col.png'))
pushViewport(viewport(layout = grid.layout(1, 3)))
print(map.bimodal.5c(data = dens.pr, binby = 'plsprbins25', density = "PLSdensity")+ ggtitle(' Precip25 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal.5c(data = dens.pr, binby = 'plsprbins50', density = "PLSdensity") + ggtitle('Precip50 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(map.bimodal.5c(data = dens.pr, binby = 'plsprbins75', density = "PLSdensity") + ggtitle('Precip75 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
dev.off()


png(height = 6, width = 15, units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_tmean_delta_p_map_5col.png'))
pushViewport(viewport(layout = grid.layout(1, 3)))
print(map.bimodal.5c(data = dens.pr, binby = 'pastdeltPbins', density = "PLSdensity")+ ggtitle('deltaP PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal.5c(data = dens.pr, binby = 'pasttmeanbins', density = "PLSdensity") + ggtitle('tmean PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(map.bimodal.5c(data = dens.pr, binby = 'sandbins', density = "PLSdensity") + ggtitle('sand PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
dev.off()

#function for plotting where bimodality would be under future climate (assuming pls relationship with cliamte)
library(splitstackshape)
library(modes)

bimodal.future <- function(data, binby, density, binby2){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 2)
  for (i in 1:length(bins)){
    coeffs[i,1]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
    coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
  coef.new <- strsplit(as.character(coef.bins$bins), " - ")
  library(plyr)
  coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
  colnames(coef.new) <- c("low", "high")
  coef.bins <- cbind(coef.bins, coef.new)
  
  #merge bins iwth the second binby -> here is is future climate
  merged <- merge(coef.bins, dens.pr, by.x = "bins", by.y = binby2)
  
  
  #define bimodality
  merged$bimodal <- "Stable"
  #criteria for bimodality
  merged[merged$BC >= 0.55 & merged$dipP <= 0.05,]$bimodal <- "Bimodal"
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  if(density == "PLSdensity"){
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$ecotype)
    merged[merged$classification %in% 'Bimodal prairie',]$classification <- "Prairie"
    merged[merged$classification %in% 'Stable prairie',]$classification <- "Prairie"
    merged[merged$classification %in% 'Bimodal Savanna',]$classification <- "Bimodal"
    merged[merged$classification %in% 'Bimodal Forest',]$classification <- "Bimodal"
  }else{
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$fiaecotype)
    
  }
  
  #merged
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = classification))+ scale_fill_manual(values = c(
      '#1a9641',
     '#fdae61',
     '#d7191c',
     '#ffffbf'
      ), limits = c('Stable Forest',"Stable Savanna",'Bimodal', "Prairie") )+
    theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank())+
    xlab("easting") + ylab("northing") +coord_equal() + ggtitle(binby2)
  
}

source("R/grid_arrange_shared_legend.R")

a <- bimodal.future(data = dens.pr, binby = 'PC1bins', density = "PLSdensity", binby2 ='PC1bins' ) + ggtitle ("PLS PC1")
b <- bimodal.future(data = dens.pr, binby = 'PC1_cc26bins', density = "PLSdensity", binby2 ='PC1_cc26bins' ) + ggtitle("RCP 2.6 PC1")
c <- bimodal.future(data = dens.pr, binby = 'PC1_cc26bins', density = "PLSdensity", binby2 ='PC1_cc45bins' )+ ggtitle("RCP 4.5 PC1")
d <- bimodal.future(data = dens.pr, binby = 'PC1_cc26bins', density = "PLSdensity", binby2 ='PC1_cc85bins' )+ ggtitle("RCP 8.5 PC1")


e<-bimodal.future(data = dens.pr, binby = 'PC1bins', density = "PLSdensity", binby2 ='PC1fiabins' ) + ggtitle ("PLS PC1")

png(height = 4, width = 12, units = "in",res = 300, filename = paste0('outputs/v1.6-5/full/RCP_scenario_PC1_maps.png'))
grid_arrange_shared_legend(a,b,c,d, nrow = 1, ncol=4, position = c("bottom"))
dev.off()

#######################################
#plot the projecitons into the future, 
#but highlight the no-analog climates:
#######################################
bimodal.future.NA <- function(data, binby, density, binby2, rcp){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 2)
  for (i in 1:length(bins)){
    coeffs[i,1]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
    coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
  coef.new <- strsplit(as.character(coef.bins$bins), " - ")
  library(plyr)
  coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
  colnames(coef.new) <- c("low", "high")
  coef.bins <- cbind(coef.bins, coef.new)
  
  #merge bins iwth the second binby -> here is is future climate
  merged <- merge(coef.bins, dens.pr, by.x = "bins", by.y = binby2)
  
  
  #define bimodality
  merged$bimodal <- "Stable"
  #criteria for bimodality
  merged[merged$BC >= 0.55 & merged$dipP <= 0.05,]$bimodal <- "Bimodal"
  merged[merged[,c(paste0("rcp",rcp,"NA"))] %in% 'no-analog',]$bimodal <- "no-analog"
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  
  
  #merged
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = bimodal))+ scale_fill_manual(values = c(
      '#1a9641',
      'black',
      '#d7191c'
    ), limits = c('Stable',"no-analog",'Bimodal') )+
    theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank())+
    xlab("easting") + ylab("northing") +coord_equal() + ggtitle(binby2)
  
}
a <- bimodal.future.NA(data = dens.pr, binby = 'PC1_cc26bins', density = "PLSdensity", binby2 ='PC1_cc60bins', rcp = "60" ) + ggtitle ("RCP 6.0 PC1")
b <- bimodal.future.NA(data = dens.pr, binby = 'PC1_cc26bins', density = "PLSdensity", binby2 ='PC1_cc26bins',  rcp = "26") + ggtitle("RCP 2.6 PC1")
c <- bimodal.future.NA(data = dens.pr, binby = 'PC1_cc26bins', density = "PLSdensity", binby2 ='PC1_cc45bins', rcp = '45' )+ ggtitle("RCP 4.5 PC1")
d <- bimodal.future.NA(data = dens.pr, binby = 'PC1_cc26bins', density = "PLSdensity", binby2 ='PC1_cc85bins', rcp = '85' )+ ggtitle("RCP 8.5 PC1")

png(height = 4, width = 12, units = "in",res = 300, filename = paste0('outputs/v1.6-5/full/RCP_scenario_PC1_noanalog_maps.png'))
grid_arrange_shared_legend(b,c,a,d, nrow = 1, ncol=4, position = c("bottom"))
dev.off()

###################################################################
# bimodal.df function outputs the dataframe of bimodal/not bimodal 

bimodal.df <- function(data, binby, density, binby2){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 2)
  for (i in 1:length(bins)){
    coeffs[i,1]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
    coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
  coef.new <- strsplit(as.character(coef.bins$bins), " - ")
  library(plyr)
  coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
  colnames(coef.new) <- c("low", "high")
  coef.bins <- cbind(coef.bins, coef.new)
  
  #merge bins iwth the second binby -> here is is future climate
  merged <- merge(coef.bins, dens.pr, by.x = "bins", by.y = binby2)
  
  
  #define bimodality
  merged$bimodal <- "Stable"
  #criteria for bimodality
  merged[merged$BC >= 0.55 & merged$dipP <= 0.05,]$bimodal <- "Bimodal"
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  if(density == "PLSdensity"){
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$ecotype)
    merged[merged$classification %in% 'Bimodal prairie',]$classification <- "Prairie"
    merged[merged$classification %in% 'Stable prairie',]$classification <- "Prairie"
    
  }else{
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$fiaecotype)
    
  }
  
merged
}

df.new <- bimodal.df(data = dens.pr, binby = 'PC1bins', density = "PLSdensity", binby2 = 'PC1bins')
df.mod <- bimodal.df(data = dens.pr, binby = 'PC1bins', density = "PLSdensity", binby2 = 'PC1fiabins')
df.8.5 <- bimodal.df(data = dens.pr, binby = 'PC1bins', density = "PLSdensity", binby2 = "PC1_cc85bins")
df.4.5 <- bimodal.df(data = dens.pr, binby = 'PC1bins', density = "PLSdensity", binby2 = "PC1_cc45bins")
df.2.6 <- bimodal.df(data = dens.pr, binby = 'PC1bins', density = "PLSdensity", binby2 = "PC1_cc26bins")

# calculate the % that should be bimodal in the modern landscape
a <- nrow(df.mod[df.mod$bimodal == "Bimodal",])/nrow(df.mod)
b <- nrow(df.new[df.new$bimodal == "Bimodal",])/nrow(df.new)
c <- nrow(df.8.5[df.8.5$bimodal == "Bimodal",])/nrow(df.8.5)
d <- nrow(df.4.5[df.4.5$bimodal == "Bimodal",])/nrow(df.4.5)
e <- nrow(df.2.6[df.2.6$bimodal == "Bimodal",])/nrow(df.2.6)


ggplot(df.new, aes(x = MAP1910, y = PLSdensity, color = classification))+geom_point()+ 
  stat_density2d(data = df.new, aes(colour = bimodal),fill = "transparent",geom="polygon") +
  theme_bw()

dens.pr<- read.csv("data/PLS_full_dens_pr_with_bins.csv")
write.csv(df.new, "outputs/PLS_full_dens_pr_bins_with_bimodality_for_PC1.csv")
# plot out climate space that is bimodal
png(height = 4, width = 6, units = "in", res = 300, filename = "outputs/v1.6-5/MAP_TEMP_bimodal_space.png")
ggplot(df.new, aes(x = MAP1910, y = pasttmean))+ geom_point()+ 
  stat_density2d(data = df.new, aes(colour = bimodal),fill = "transparent",geom="polygon") +
  theme_bw()+ ylab ("Mean Temperature (degC), 1895-1925") + xlab("Mean Annual Precipitation (mm/yr), 1895-1925")
dev.off()

png(height = 4, width = 6, units = "in", res = 300, filename = "outputs/v1.6-5/MAP_deltaTEMP_bimodal_space.png")
ggplot(df.new, aes(x = MAP1910, y = deltaT))+ geom_point()+ 
  stat_density2d(data = df.new, aes(colour = bimodal),fill = "transparent",geom="polygon") +
  theme_bw()+ ylab ("Mean Temperature (degC), 1895-1925") + xlab("Mean Annual Precipitation (mm/yr), 1895-1925")
dev.off()

png(height = 4, width = 6, units = "in", res = 300, filename = "outputs/v1.6-5/MAP_deltaTEMP_bimodal_space.png")
ggplot(df.new, aes(x = MAP1910, y = deltaT))+ geom_point()+ 
  stat_density2d(data = df.new, aes(colour = bimodal),fill = "transparent",geom="polygon") +
  theme_bw()+ ylab ("Tempearature Seasonality (degC), 1895-1925") + xlab("Mean Annual Precipitation (mm/yr), 1895-1925")
dev.off()




ggplot(df.new, aes(x = pastdeltaP, y = deltaT, color = classification)) + stat_density2d()+ #+ geom_point()
scale_color_manual(values = c(
  '#01665e', # light green
  '#5ab4ac', # dark teal
  '#8c510a', # red
  '#d8b365', # light tan
  '#fee08b', # tan
  'black'), limits = c('Bimodal Forest',"Stable Forest" ,   "Bimodal Savanna", 'Stable Savanna','Prairie') )+
  theme_bw() 

library(rgl)
plot3d(x = df.new$MAP1910, y = df.new$PLSdensity, z = df.new$BC, groups = df.new$bimodal,
          surface=FALSE, ellipsoid = TRUE)

source_url("https://raw.github.com/low-decarie/FAAV/master/r/stat-ellipse.R")    
qplot(data=df, x=x, y=y, colour=colour)+stat_ellipse()
library(scatterplot3d)
scatterplot3d(df.new$pasttmean, df.new$MAP1910, df.new$sandpct,color = as.character(df.new$bimodal), angle=20) 
df.new$color <- 1
df.new[df.new$bimodal %in% c("Stable"),]$color <- 2

#3d plot of climate space that is bimodal
png(height = 3, width = 4, units = 'in',res = 300, filename = "outputs/v1.6-5/full/3d-plot-pr_t_sand.png")
s3d <- with(df.new, scatterplot3d( MAP1910, pasttmean, sandpct,color = color, pch = 19,xlab ="MAP(mm/year)", ylab="temp. (degC)"),zlab = "% sand", angle = -180 )
legend("topleft", inset=.01,      # location and inset
                    bty="n", cex=1,              # suppress legend box, shrink text 50%
                    title="Climate space",
                    c("Bimodal", "Stable"), fill=c("red", "black"))
dev.off()

#another dimension of climate
png(height = 3, width = 4, units = 'in', res = 300, filename = "outputs/v1.6-5/full/3d-plot-pr_dt_sand.png")
s3d <- with(df.new, scatterplot3d( MAP1910, deltaT,sandpct, color = color, pch = 19), angle = -180)
legend("topleft", inset=.01,      # location and inset
       bty="n", cex=1,              # suppress legend box, shrink text 50%
       title="Climate space",
       c("Bimodal", "Stable"), fill=c("red", "black"))
dev.off()

plot3d(df.new$pasttmean, df.new$MAP1910, df.new$sandpct, col = df.new$color)
write.csv(df.new , "outputs/v1.6-5/full/dens_pr_dataframe_full_w_bimodal.csv")
write.csv(dens.pr, "outputs/v1.6-5/full/dens_pr_dataframe_full.csv")

#rolling BC
rollBC_r = function(x,y,xout,width) {
  out = numeric(length(xout))
  for( i in seq_along(xout) ) {
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i] = bimodality_coefficient( y[window ] ) # what is the BC for places with less than 300 trees per hectare
  }
  ggplot()+geom_point(aes(x = xout, y = out))+
    geom_hline( yintercept = 5/9)+ylim(0,1)+theme_bw()+
    xlab('interval center') + ylab('Bimodality Coefficient') +ggtitle(paste0( 'Bimodality coefficient for binwidth = ', width))
  
}

#need to order the 
ordered <- dens.pr[order(dens.pr$MAP1910),]
ordered$rownum <- 1:length(ordered$MAP1910)

pdf(paste0('outputs/v',version,'/full/rolling_BC_plots_500_cutoff.pdf'))

png(paste0('outputs/v',version,'/full/bimodality_coefficient_roll_pls_25bins.png'))

rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 150)
dev.off()

rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 200)
rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 300)
rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 250)
rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 100)
rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 75)
rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 50)
rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 25)
rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 10)
dev.off()

#this version of roll_BC_by10 takes the BC every 10mm of preciptiation
rollBC_by_10_r = function(x,y,xout,width) {
  out = 1:length(seq(200, 1350, by = 10) )
  for( i in 1:length(seq(200, 1350, by = 10))) {
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i] = bimodality_coefficient( y[window] )
  }
  ggplot()+geom_point(aes(x = xout, y = out))+
    geom_hline( yintercept = 5/9)+ylim(0,1)+theme_bw()+
    xlab('interval center') + ylab('Bimodality Coefficient') +ggtitle(paste0( 'Bimodality coefficient for binwidth = ', width))
}   
rollBC_by_10_r(ordered$MAP1910, ordered$PLSdensity, seq(200, 1350, by = 10)  , 100)


rollBC_by_10 = function(x,y,xout,width) {
  out = 1:length(seq(200, 1350, by = 10) )
  for( i in 1:length(seq(200, 1350, by = 10))) {
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i] = bimodality_coefficient( y[window] )
  }
  df <- data.frame(mid = xout, max = xout + width,min = xout - width,BC = out)
  
}

---
  