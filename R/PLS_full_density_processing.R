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
hist(umdw.new$PLSdensity)

densitys <- rbind(pls.inil, umdw.new)
#coordinates(pls.inil)<- ~x+y

#test.ex<- extract(density.FIA.table, extent(pls.inil))
#write.csv(pls.inil,C:/Users/JMac/Documents/Kelly/biomodality/outputs )
#plot raw data
#map out density:
ggplot(densitys, aes(x,y,color = PLSdensity))+geom_point()


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

#create variable for precipitation seasonality
past.precip.mo <- read.csv(paste0('outputs/pr_monthly_Prism_1900_1909_full.csv'))
past.precip.mo$max <- apply(past.precip.mo[ , 2:13], 1, max)
past.precip.mo$min <- apply(past.precip.mo[ , 2:13], 1, min) 
past.precip.mo$deltaP <- (past.precip.mo$max-past.precip.mo$min)/(past.precip.mo$max+past.precip.mo$min)


#read in mean annual precipitaiton for modern and past

past.precip <- read.csv('outputs/pr_monthly_Prism_1900_1909_full.csv')

past.tmean <- read.csv('outputs/tmean_yr_Prism_1900-1910_full.csv')

#calculate seasonality from tmean:
past.tmean$max <- apply(past.tmean[ , 2:13], 1, max) + 273.15 # convert to kelvin
past.tmean$min <- apply(past.tmean[ , 2:13], 1, min) + 273.15 # convert to kelvin
past.tmean$deltaT <- ((past.tmean$max-past.tmean$min)/(past.tmean$max+past.tmean$min))*100


dens.pr <- merge(densitys, past.precip.mo[,c('x', 'y', 'total', 'deltaP')], by =c('x', 'y'), all.x = F)
#dens.pr <- merge(dens.pr, mod.precip[,c('x', 'y', 'pr30yr')], by = c('x', 'y'))
colnames(dens.pr)[5:6] <- c('MAP1910', 'pastdeltaP')

#now add the precipitation seasonality to the dataframe
#dens.pr <- merge(dens.pr, mod.precip.mo[,c('x', 'y', 'deltaP')], by = c('x', 'y') )
#dens.pr <- merge(dens.pr, past.precip.mo[,c('x', 'y', 'deltaP')], by = c('x', 'y'), all.x = TRUE)
#colnames(dens.pr)[6]<- c( )

#now add the mean temperature to the dataframe
#dens.pr <- merge(dens.pr, mod.tmean[,c('x', 'y', 'prism30yr')], by = c('x', 'y') )
dens.pr <- merge(dens.pr, past.tmean[,c('x', 'y', 'Mean', "deltaT")], by = c('x', 'y'), all.x = F)
colnames(dens.pr)[7:8] <- c('pasttmean', "pastdeltaT")

write.csv(dens.pr, paste0("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_pr_alb",version,".csv"))

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
#plot(sand1km)

# need to project sand to great lakes albers coordinate system
sand8km.alb <- projectRaster(sand8km, crs ='+init=epsg:3175')
#sand1km.alb <- projectRaster(sand1km, crs = '+init=epsg:3175')


#awc
awc8km <- raster("C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_awc1.tif")
#awc1km <- raster ("C:/Users/JMac/Box Sync/GSSURGOtifs/1km_UMW_awc1.tif")

awc8km.alb <- projectRaster(awc8km, crs ='+init=epsg:3175')
#awc1km.alb <- projectRaster(awc1km, crs = '+init=epsg:3175')

#ksat

ksat8km <- raster("C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_ksat1.tif")
#ksat1km <- raster ("C:/Users/JMac/Box Sync/GSSURGOtifs/1km_UMW_ksat1.tif")

ksat8km.alb <- projectRaster(ksat8km, crs ='+init=epsg:3175')
#ksat1km.alb <- projectRaster(ksat1km, crs = '+init=epsg:3175')

#extract soils data using FIA and ps points
dens.pr$sandpct <- extract(sand8km.alb, dens.pr[,c('x', 'y')], method = 'bilinear')
dens.pr$awc <- extract(awc8km.alb, dens.pr[,c('x', 'y')])
dens.pr$ksat <- extract(ksat8km.alb, dens.pr[,c('x', 'y')])



summary(rowSums(dens.pr != 0, na.rm=TRUE))
#dens.pr$diff <- dens.pr$FIAdensity - dens.pr$PLSdensity



PLS.lm<- lm(dens.pr$PLSdensity ~dens.pr$MAP1910)


summary(PLS.lm)

library(mgcv)
#make gams 
PLS.gam <- gam(dens.pr$PLSdensity ~ dens.pr$MAP1910  +dens.pr$pasttmean +dens.pr$sandpct + dens.pr$awc, method = "ML")
summary(PLS.gam) # explains 41% of deviance

PLS.gam1 <- gam(dens.pr$PLSdensity ~ dens.pr$MAP1910 +dens.pr$pasttmean+ dens.pr$sandpct, method = "ML")
summary(PLS.gam1) #explains 39% deviance

PLS.gam3 <- gam(dens.pr$PLSdensity ~ dens.pr$MAP1910 +dens.pr$pasttmean +dens.pr$awc , method = "ML")
summary(PLS.gam3) #explains 41.3% of deviance

PLS.gam4 <- gam(dens.pr$PLSdensity ~ dens.pr$awc , method = "ML")
summary(PLS.gam4) #explains 12.5% of deviance

PLS.gam5 <- gam(dens.pr$PLSdensity ~ dens.pr$awc +dens.pr$sandpct , method = "ML")
summary(PLS.gam5) #explains 14.9% of deviance

PLS.gam2 <- gam(dens.pr$PLSdensity ~ dens.pr$MAP1910 , method = "ML")
summary(PLS.gam2) #explains 0.004% deviance

PLS.gam6 <- gam(dens.pr$PLSdensity ~ dens.pr$pastdeltaP , method = "ML")
summary(PLS.gam6) #explains 3.02% deviance

PLSgam7 <- gam(PLSdensity ~ pasttmean , method = "ML", data = dens.pr)
summary(PLSgam7) #explains 15.8% deviance
plot(PLS.gam7, residuals = TRUE)



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

####################################################
#PCA analysis
#####################################################
dens.rm <- na.exclude(dens.pr)
dens.rm <- data.frame(dens.rm)
scale.dens <- scale(dens.rm[, 5:11]) #PC all but ksat and diff
dens.dens <- dens.rm[, c('PLSdensity')] # pls density

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
dens.pca <- princomp(scale.dens,
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
coordinates(dens.rm) ~x + y
gridded(dens.rm) <- TRUE
proj4string(dens.rm) <- '+init=epsg:3175'
pca <- raster(dens.rm)

test1 <- merge(dens.pr,unique(dens.rm[,c('x','y', 'PC1', 'PC2')]),  by = c('x','y'), all.x= T)
#convert dens.rm to the new dens.pr---we only lose ~150 grid cells
dens.pr <- test1

ggplot(dens.pr, aes(x = x, y = y, color= ecotype))+geom_point()
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
dens.pr$pasttmeanbins <- cut(dens.pr$pasttmean, breaks = seq(0,14, by = 1), labels = label.breaks(0,13, 1))
dens.pr$PC1bins <- cut(dens.pr$PC1, breaks = seq(-9,5, by = 1), labels = label.breaks(-9,4, 1))
dens.pr$PC2bins <- cut(dens.pr$PC2, breaks = seq(-4,3, by = 0.5), labels = label.breaks(-4,2.5, 0.5))


test<- dens.pr[!is.na(dens.pr),]
melted <- melt(test, id.vars = c("x", 'y', 'cell', 'plsprbins',  'plsprbins50','plsprbins75', 
                                 'plsprbins100','plsprbins150', 'plsprbins25', 
                                 'MAP1910',  
                                  'sandpct', 'awc', 'ksat', 'sandbins', 'ksatbins', 
                                 'pastdeltaP','pastdeltaT',  'pastdeltPbins', 'pasttmeanbins','pasttmean', "PC1", "PC2",'PC1bins', 'PC2bins', 'ecotype')) 

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
  coeffs <- matrix(NA, length(bins), 1)
  for (i in 1:length(bins)){
    coeffs[i]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  merged <- merge(coef.bins, dens.pr, by.x = "bins",by.y = binby)
  #define bimodality
  merged$bimodal <- "Stable"
  merged[merged$BC >= 0.5,]$bimodal <- "Bimodal"
  
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
  coeffs <- matrix(NA, length(bins), 1)
  for (i in 1:length(bins)){
    coeffs[i]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  merged <- merge(coef.bins, dens.pr, by.x = "bins",by.y = binby)
  #define bimodality
  merged$bimodal <- "Stable"
  merged[merged$BC >= 0.5,]$bimodal <- "Bimodal"
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  if(density == "PLSdensity"){
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$ecotype)
  }else{
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$fiaecotype)
    
  }
  
  merged[merged$classification %in% 'Bimodal prairie',]$classification <- "Prairie"
  merged[merged$classification %in% 'Stable prairie',]$classification <- "Prairie"
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = classification))+ scale_fill_manual(values = c(
      '#1a9641', # dark green
      '#fdae61', # light orange
      '#a6d96a', # light green
      '#d7191c', # red
      '#fee08b', # tan
      'black'), limits = c("Stable Forest" , 'Stable Savanna', 'Bimodal Forest', "Bimodal Savanna", 'Prairie') )+
    theme_bw()+
    xlab("easting") + ylab("northing") +coord_equal()+
    ggtitle(paste0(binby, ' for ',density))
  
}

pdf(paste0('outputs/v',version,'/full/bimodal_maps_5col.pdf'))
map.bimodal.5c(data = dens.pr, binby = 'plsprbins50', density = "PLSdensity")
#map.bimodal.5c(data = dens.pr, binby = 'fiaprbins', density = "FIAdensity")
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

