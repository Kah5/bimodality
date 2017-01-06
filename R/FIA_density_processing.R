#read in FIA from Sean's repository
# set pls version to use
version <- "1.5-2"
setwd( "C:/Users/JMac/Documents/Kelly/biomodality")
library(data.table)
library(reshape2)
library(dtplyr)
library(ggplot2)

FIA <- read.csv('data/FIA_species_plot_parameters_paleongrid.csv')
speciesconversion <- read.csv('data/FIA_conversion-SGD_remove_dups.csv')

FIA.pal <- merge(FIA, speciesconversion, by = 'spcd' )
density.FIA.table <- dcast(FIA.pal, plt_cn ~ PalEON, sum, na.rm=TRUE, value.var = 'density') #sum all species in common taxa in FIA grid cells
density.FIA.table <- dcast(FIA.pal, x + y + cell ~ PalEON, mean, na.rm=TRUE, value.var = 'density') #average density of plots in grid cell
density.FIA.table$FIAdensity <- rowSums(density.FIA.table[,5:24], na.rm = TRUE)
summary(density.FIA.table$FIAdensity)
hist(density.FIA.table$FIAdensity, breaks = 100)
#coordinates(density.FIA.table) <- ~x +y

#if you want to do a quick plot
#coordinates(density.FIA.table) <- ~x + y
#gridded(density.FIA.table) <- TRUE
#spplot(density.FIA.table, 'total')

#merge inil pls and inil FIA

#read in tree level data
#pls.inil<- read.csv('outputs/density_tables.csv')
pls.inil <- read.csv(paste0('outputs/biomass_no_na_pointwise.ests_v',version, '.csv'))
pls.inil <- dcast(pls.inil, x + y + cell ~., mean, na.rm = TRUE, value.var = 'density')

colnames(pls.inil) <- c('x', 'y', 'cell','PLSdensity')
hist(pls.inil$PLSdensity, xlim = c(0, 400),breaks = 100)


#can aggregate by species
#pls.spec<- read.csv(paste0('outputs/density_tables.csv'))
pls.spec <- read.csv(paste0('outputs/biomass_no_na_pointwise.ests_v',version, '.csv'))
pls.spec <- dcast(pls.spec, x + y + cell ~spec, mean, na.rm = TRUE, value.var = 'density')
pls.spec$total <- rowSums(pls.spec[4:36], na.rm=TRUE)
hist(pls.spec$total)
pls.new <- pls.spec[,c('x', 'y', 'cell', 'total')]
colnames(pls.new) <- c('x', 'y', 'cell','PLSdensity')

umdw <- read.csv('data/plss_density_alb_v0.9-6.csv')
#umdw.mean <- dcast(umdw, x + y + cell ~., mean, na.rm = TRUE, value.var = 'density')
umdw$total <- rowSums(umdw[,5:33], na.rm= TRUE)
umdw.new <- umdw[,c('x', 'y', 'cell', 'total')]


colnames(umdw.new) <- c('x', 'y', 'cell', 'PLSdensity')
hist(umdw.new$PLSdensity)

pls.inil <- rbind(pls.inil, umdw.new)
#coordinates(pls.inil)<- ~x+y

#test.ex<- extract(density.FIA.table, extent(pls.inil))
#write.csv(pls.inil,C:/Users/JMac/Documents/Kelly/biomodality/outputs )
#plot raw data


#merge inil pls and inilFIA
densitys <- merge(pls.inil[,c('x', 'y', 'cell', 'PLSdensity')], density.FIA.table[,c('x', 'y', 'cell', 'FIAdensity')],
                  by = c('x', 'y', 'cell'), all.x = TRUE)

#map out density:
ggplot(densitys, aes(x,y,color = PLSdensity))+geom_point()
ggplot(densitys, aes(x,y,color = FIAdensity))+geom_point()




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
library(ggplot2)

sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=densitys, aes(x=x, y=y, fill = PLSdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw()
png("outputs/PLS_tree_density_map.png")
pls.map
dev.off()

fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=densitys, aes(x=x, y=y, fill = FIAdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="FIA tree density") + 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw()
png("outputs/FIA_tree_density_map.png")
fia.map
dev.off()

#densitys <- merge(pls.inil[,c('x', 'y', 'cell', 'PLSdensity')], density.FIA.table[,c('x', 'y', 'cell', 'FIAdensity')],
 #                 by = c('x', 'y', 'cell'))
#densitys <- densitys[densitys$PLSdensity > 14.87, ]
write.csv(densitys, paste0("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_alb", version,".csv"))
#this merge yields only 457 grid cells across indiana and illinois where we have both PLS and FIA data

#this is to join the species tables
pls.names<- colnames(pls.spec)
umdw.names<- colnames(umdw)
colnames(pls.spec)[6] <- 'Bald.cypress'
colnames(pls.spec)[10] <- 'Black.gum'
colnames(pls.spec)[13] <- 'Cedar.juniper'
colnames(pls.spec)[25] <- 'Other.hardwood'
colnames(pls.spec)[32] <- 'Tulip.poplar'

pls.names<- colnames(pls.spec)
umdw.names<- colnames(umdw)

#create name vectors for species columns missing in the upper and lower midewst
to.add.umdw <-pls.names[!pls.names %in% umdw.names]
to.add.pls <- umdw.names[!umdw.names %in% pls.names]

#add these species columns to the respective dataframes, but with 0 for data values
pls.spec[,to.add.pls] <- NA
umdw[,to.add.umdw] <- NA


#now I need to add columns to FIA dataframe for species not seen in FIA, but seen in PLS:





#reorder the columns so the pls.spec and umdw dataframes match
pls.spec<- pls.spec[ , order(names(pls.spec))]
umdw <- umdw[,order(names(umdw))]

full.spec <- rbind(pls.spec, umdw)

#move around the columns
require(dplyr)
full.spec<- full.spec %>%
  dplyr::select(cell, everything())

full.spec<- full.spec %>%
  dplyr::select(y, everything())

full.spec<- full.spec %>%
  dplyr::select(x, everything())

full.spec<- full.spec %>%
  dplyr:: select(X, everything())

full.spec<- full.spec %>%
  dplyr:: select(-total, everything())

#now add totals to the 'total columns
full.spec$total <- rowSums(full.spec[,5:41], na.rm = TRUE)
summary(full.spec$total)
hist(full.spec$total, breaks = 1000, xlim = c(0,600))

colnames(full.spec)[42] <- 'PLSdensity'

#now I need to add columns to FIA dataframe for species not seen in FIA, but seen in PLS:

pls.full.n<- colnames(full.spec)
fia.names<- colnames(density.FIA.table)
fia.names[!fia.names %in% pls.full.n]
pls.full.n[!pls.full.n %in% fia.names]

colnames(density.FIA.table)[11] <- 'Cedar.juniper'
colnames(density.FIA.table)[19] <- 'Other.hardwood'
fia.names[!fia.names %in% pls.full.n]
pls.full.n[!pls.full.n %in% fia.names]

density.FIA.table<- density.FIA.table[,-4] # remove Var.4
density.FIA.table<- density.FIA.table[,-5]# remove atlantic white cedar
density.FIA.table<- density.FIA.table[,-23] #remove total

pls.names<- colnames(full.spec)
fia.names<- colnames(density.FIA.table)

#create name vectors for species columns missing in the upper and lower midewst
to.add.fia <-pls.names[!pls.names %in% fia.names]
#to.add.pls <- fia.names[!fia.names %in% pls.names]

#add these species columns to the respective dataframes, but with 0 for data values
#pls.spec[,to.add.pls] <- NA
density.FIA.table[,to.add.fia] <- NA
density.FIA.table<- density.FIA.table[,-42] #remove plsdensity columns

FIA.full <- density.FIA.table[,order(names(density.FIA.table))]

FIA.full<- FIA.full %>%
  dplyr:: select(cell, everything())

FIA.full<- FIA.full %>%
  dplyr:: select(y, everything())

FIA.full<- FIA.full %>%
  dplyr::select(x, everything())

FIA.full<- FIA.full %>%
  dplyr::select(X, everything())

#FIA.full<- FIA.full %>%
 # dplyr::select(-total, everything())

#now add totals to the 'total columns
FIA.full$FIAdensity <- rowSums(FIA.full[,5:41], na.rm = TRUE)
summary(FIA.full$FIAdensity)
hist(FIA.full$FIAdensity, breaks = 50, xlim = c(0,600))




################################################################
#comparison of FIA and PLS datasets to climate
###############################################################
past.precip.mo <- read.csv('outputs/pr_monthly_Prism_1895_1905.csv')
past.precip.mo$max <- apply(past.precip.mo[ , 2:13], 1, max)
past.precip.mo$min <- apply(past.precip.mo[ , 2:13], 1, min) 
past.precip.mo$deltaP <- (past.precip.mo$max-past.precip.mo$min)/(past.precip.mo$max+past.precip.mo$min)

mod.precip.mo <- read.csv('outputs/pr_monthly_Prism_30yrnorms.csv')
mod.precip.mo$max <- apply(mod.precip.mo[ , 3:14], 1, max)
mod.precip.mo$min <- apply(mod.precip.mo[ , 3:14], 1, min) 
mod.precip.mo$deltaP <- (mod.precip.mo$max-mod.precip.mo$min)/(mod.precip.mo$max+mod.precip.mo$min)
mod.precip.mo <- mod.precip.mo[complete.cases(mod.precip.mo),]

mod.precip <- read.csv('data/spec_table_30yr_prism_full.csv')
past.precip <- read.csv('outputs/pr_monthly_Prism_1900_1910.csv')
mod.tmean <- read.csv('outputs/tmean_Prism_30yr.csv')
past.tmean <- read.csv('outputs/tmean_yr_Prism_1900-1910.csv')
#past.precip <- read.csv('data/PLSpoints_pr_alb_full1900_1950_GHCN.csv') #climate for indiana and il
#mod.precip <- read.csv('data/spec_table_30yr_prism.csv') #climate for indiana and il


#dens.pr <- merge(densitys, past.precip[,c('x', 'y', 'extract.avg.alb..dens.table...c..x....y....')], by =c('x', 'y'))
#dens.pr <- merge(densitys, past.precip[,c('x', 'y', 'total_.')], by =c('x', 'y'))
dens.pr <- merge(densitys, past.precip[,c('x', 'y', '.')], by =c('x', 'y'))
dens.pr <- merge(dens.pr, mod.precip[,c('x', 'y', 'pr30yr')], by = c('x', 'y'))
colnames(dens.pr)[6:7] <- c('MAP1910', "MAP2011")

#now add the precipitation seasonality to the dataframe
dens.pr <- merge(dens.pr, mod.precip.mo[,c('x', 'y', 'deltaP')], by = c('x', 'y') )
dens.pr <- merge(dens.pr, past.precip.mo[,c('x', 'y', 'deltaP')], by = c('x', 'y') )
colnames(dens.pr)[8:9]<- c('moderndeltaP', 'pastdeltaP')

#now add the mean temperature to the dataframe
#dens.pr <- merge(dens.pr, mod.tmean[,c('x', 'y', 'prism30yr')], by = c('x', 'y') )
dens.pr <- merge(dens.pr, past.tmean[,c('x', 'y', '.')], by = c('x', 'y') )
colnames(dens.pr)[10] <- c('pasttmean')
dens.pr$pasttmean<- dens.pr$pasttmean/10 # convert from C*10 to Celcius
write.csv(dens.pr, paste0("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_density_pr_alb",version,".csv"))

#nine.five.pct<- quantile(dens.pr$PLSdensity, probs = .95, na.rm=TRUE)
#dens.pr[dens.pr$PLSdensity>nine.five.pct,]$PLSdensity <- nine.five.pct #patch fix the overestimates of density

#plot histograms
hist(dens.pr$PLSdensity, breaks = 50, xlim = c(0,550), xlab = 'PLS density (stems/ha)', main = 'PLS Midwest Density')
hist(dens.pr$FIAdensity, breaks = 50, xlim = c(0,550), xlab = 'FIA density(stems/ha)',main = 'FIA Midwest Density')

#plot raw data
plot(dens.pr$MAP1910,dens.pr$PLSdensity, xlab = 'Past MAP', ylab = 'PLS density')
plot(dens.pr$MAP2011,dens.pr$FIAdensity, xlab = 'Modern MAP', ylab = 'Modern density')
plot(dens.pr$pastdeltaP, dens.pr$PLSdensity, xlab = "Past P seasonality", ylab = "PLS density")
plot(dens.pr$moderndeltaP, dens.pr$FIAdensity, xlab = "Modern P seasonality", ylab = "FIA density")
plot(dens.pr$pasttmean, dens.pr$PLSdensity, xlab = 'Past Tmean', ylab = "PLSdensity")
#plot(dens.pr$MAP2011, dens.pr$PLSdensity, xlab = 'Modern MAP', ylab = 'PLS density')
#plot(dens.pr$MAP1910, dens.pr$FIAdensity, xlab = 'Past MAP', ylab = 'Modern density')
plot(dens.pr$pasttmean, dens.pr$MAP1910, xlab = 'Past Tmean', ylab = "Past Precip")
##########################
#read in soils data
sand8km <- raster("data/8km_UMW_sand1.tif")
plot(sand8km)

sand1km <- raster("data/1km_UMW_sand1.tif")
plot(sand1km)

# need to project sand to great lakes albers coordinate system
sand8km.alb <- projectRaster(sand8km, crs ='+init=epsg:3175')
sand1km.alb <- projectRaster(sand1km, crs = '+init=epsg:3175')


#awc
awc8km <- raster("C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_awc1.tif")
awc1km <- raster ("C:/Users/JMac/Box Sync/GSSURGOtifs/1km_UMW_awc1.tif")

awc8km.alb <- projectRaster(awc8km, crs ='+init=epsg:3175')
awc1km.alb <- projectRaster(awc1km, crs = '+init=epsg:3175')

#ksat

ksat8km <- raster("C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_ksat1.tif")
ksat1km <- raster ("C:/Users/JMac/Box Sync/GSSURGOtifs/1km_UMW_ksat1.tif")

ksat8km.alb <- projectRaster(ksat8km, crs ='+init=epsg:3175')
ksat1km.alb <- projectRaster(ksat1km, crs = '+init=epsg:3175')

#extract soils data using FIA and ps points
dens.pr$sandpct <- extract(sand8km.alb, dens.pr[,c('x', 'y')], method = 'bilinear')
dens.pr$awc <- extract(awc8km.alb, dens.pr[,c('x', 'y')])
dens.pr$ksat <- extract(ksat8km.alb, dens.pr[,c('x', 'y')])



summary(rowSums(dens.pr != 0, na.rm=TRUE))
dens.pr$diff <- dens.pr$FIAdensity - dens.pr$PLSdensity


  
PLS.lm<- lm(dens.pr$PLSdensity ~dens.pr$MAP1910)
FIA.lm<- lm(dens.pr$FIAdensity ~dens.pr$MAP2011)
PLS_mod.lm<- lm(dens.pr$PLSdensity ~dens.pr$MAP2011)
FIA_pas.lm <- lm(dens.pr$FIAdensity~dens.pr$MAP1910)
diff.lm <- lm(dens.pr$diff ~dens.pr$PLSdensity)

summary(PLS.lm)
summary(FIA.lm)
summary(PLS_mod.lm)
summary(FIA_pas.lm)
summary(diff.lm)
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

FIA.gam <- gam(dens.pr$FIAdensity ~ dens.pr$MAP2011 + dens.pr$sandpct + dens.pr$awc, method = "ML")
summary(FIA.gam) # explains 4% of deviance


library(ggExtra)
library(ggplot2)

png('outputs/PLS_precip_hist_prism.png')
#X11(width = 5)
p <- ggplot(dens.pr, aes(MAP1910, PLSdensity)) + geom_point() + theme_classic() + xlab('Mean Annual Precipitation (mm)') + ylab('Pre-Settlement \n Tree Density \n (Trees/hectare)')+
  xlim(450, 1200) + ylim(0, 800)+theme_bw()+
  theme(text = element_text(size = 20))
ggExtra::ggMarginal(p, type = "histogram",size = 3, colour = 'black', fill = 'red')
dev.off()

png('outputs/FIA_precip_hist_prism.png')
p <- ggplot(dens.pr, aes(MAP2011, FIAdensity)) + geom_point() + theme_classic()+ xlab('Mean Annual Precipitation (mm)') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  xlim(450, 1200) + ylim(0, 800)+theme_bw()+theme(text = element_text(size = 20))
ggExtra::ggMarginal(p, type = "histogram", size = 3, colour = 'black', fill = "#0072B2")

dev.off()

#make plots for precipitation seasonality
png('outputs/PLS_delta_precip_hist_prism.png')
#X11(width = 5)
p <- ggplot(dens.pr, aes(pastdeltaP, PLSdensity)) + geom_point() + theme_classic() + xlab('Precipitation seasonality') + ylab('Pre-Settlement \n Tree Density \n (Trees/hectare)')+
  xlim(0,1) + ylim(0, 800)+theme_bw()+
  theme(text = element_text(size = 20))
ggExtra::ggMarginal(p, type = "histogram",size = 3, colour = 'black', fill = 'red')
dev.off()

png('outputs/FIA_delta_precip_hist_prism.png')
p <- ggplot(dens.pr, aes(moderndeltaP, FIAdensity)) + geom_point() + theme_classic()+ xlab('Precipitation seasonality') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  xlim(0,1) + ylim(0, 800)+theme_bw()+theme(text = element_text(size = 20))
ggExtra::ggMarginal(p, type = "histogram", size = 3, colour = 'black', fill = "#0072B2")

dev.off()

sandfia <- ggplot(dens.pr, aes(sandpct, FIAdensity)) + geom_point() + theme_classic()+ xlab('% sand 1-30cm') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  theme_bw()+theme(text = element_text(size = 20))
awcfia <- ggplot(dens.pr, aes(awc, FIAdensity)) + geom_point() + theme_classic()+ xlab('awc 1-30cm') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  theme_bw()+theme(text = element_text(size = 20))
ksatfia <- ggplot(dens.pr, aes(ksat, FIAdensity)) + geom_point() + theme_classic()+ xlab('ksat 1-30cm') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  theme_bw()+theme(text = element_text(size = 20))
png('outputs/FIA_sand.png')
sandfia
dev.off()
png('outputs/FIA_awc.png')
awcfia
dev.off()
png('outputs/FIA_ksat.png')
ksatfia
dev.off()
sandpls <- ggplot(dens.pr, aes(sandpct, PLSdensity)) + geom_point() + theme_classic()+ xlab('% sand 1-30cm') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  theme_bw()+theme(text = element_text(size = 20))
awcpls <- ggplot(dens.pr, aes(awc, PLSdensity)) + geom_point() + theme_classic()+ xlab('AWC 1-30cm') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  theme_bw()+theme(text = element_text(size = 20))
ksatpls <- ggplot(dens.pr, aes(ksat, PLSdensity)) + geom_point() + theme_classic()+ xlab('ksat 1-30cm') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  theme_bw()+theme(text = element_text(size = 20))

png('outputs/PLS_sand.png')
sandpls
dev.off()
png('outputs/PLS_awc.png')
awcpls
dev.off()
png('outputs/PLS_ksat.png')
ksatpls
dev.off()

plot(dens.pr$PLSdensity, dens.pr$diff, xlab='PLS tree density (trees/ha)', ylab='increase in density since PLS (trees/ha)')

#linear regression model for sand
sand.lm <- lm(dens.pr$sandpct ~dens.pr$PLSdensity)
summary(sand.lm)
sand.fia.lm <- lm(dens.pr$sandpct~dens.pr$FIAdensity)
summary(sand.fia.lm)

#linear model for ksat
ksat.lm<- lm(dens.pr$ksat~dens.pr$PLSdensity)
summary(ksat.lm)

ksat.fia.lm <- lm(dens.pr$ksat~dens.pr$FIAdensity)
summary(ksat.fia.lm)

#linear regression model for awc
awc.lm <- lm(dens.pr$awc~ dens.pr$PLSdensity)
summary(awc.lm)

awc.fia.lm <- lm(dens.pr$awc ~ dens.pr$FIAdensity)
summary(awc.fia.lm)

library(MASS)  # in case it is not already loaded 
set.seed(101)
n <- 1000
X <- mvrnorm(n, mu=c(.5,2.5), Sigma=matrix(c(1,.6,.6,1), ncol=2))

## some pretty colors
library(RColorBrewer)
k <- 11
my.cols <- rev(brewer.pal(k, "RdYlBu"))

## compute 2D kernel density, see MASS book, pp. 130-131
z <- kde2d(dens.pr$PLSdensity, dens.pr$diff, n=50)

plot(dens.pr$PLSdensity, dens.pr$diff, xlab='PLS tree density (trees/ha)', xlim= c(0,800),ylim = c(-1000, 1000),ylab='increase in density since PLS (trees/ha)', pch=19, cex=.4)
contour(z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE)
abline(a = 0, b = 0, col = 'red')
legend("topleft", paste("R=", round(cor(dens.pr$PLSdensity, dens.pr$diff),2)), bty="n")




library(hexbin)
#### 
#plot denisity histograms binned by precipitation amount
#100mm precipitation bins
#dens.pr$plsprbins <- cut(dens.pr$MAP1910, #labels = c('350-400mm', '400-450mm', '450-500mm', '550-600mm', '600-650mm','650-700mm','700-750mm','750-800mm','800-850mm',  '850-900mm','900-950mm','950-1000mm','1000-1050mm','1050-1100mm', '1100-1150mm','1150-1200mm', '1200-1250mm', '1250-1300mm'),
                      #   breaks=c(200,250,300,400,500,600, 700,800,900, 1000,1100,1200, 1400))
#dens.pr$fiaprbins <- cut(dens.pr$MAP2011, #labels = c('350-400mm', '500-650mm', '650-700mm', '700-850mm', '850-1000mm', '1000-1150mm', '1150-1300mm'),
                        # breaks=c( 200,250,300,400,500,600, 700,800,900, 1000,1100,1200, 1400))
#make cuts for sliding window plots
#dens.pr$plsprbins <- cut(dens.pr$MAP1910, labels = c('200-400mm', '400-550mm', '550-600mm', '600-850mm', '850-1000mm','1000-1150mm','1150-1300mm','1300-1450mm'),
 #                        breaks=c(200,400,550,700,850, 1000,1150,1300, 1450))
#dens.pr$fiaprbins <- cut(dens.pr$MAP2011, labels = c('200-400mm', '400-550mm', '550-600mm', '600-850mm', '850-1000mm','1000-1150mm','1150-1300mm','1300-1450mm'),
 #                        breaks=c(200,400,550,700,850, 1000,1150,1300, 1450))
#create multiple sets of bins for precipitation:

dens.pr$plsprbins <- cut(dens.pr$MAP1910, breaks = seq(250, 1350, by = 50), labels = seq(250, 1300, by = 50))
dens.pr$fiaprbins <- cut(dens.pr$MAP2011, breaks = seq(250, 1350, by = 50), labels = seq(250, 1300, by = 50))
dens.pr$plsprbins100 <- cut(dens.pr$MAP1910, breaks = seq(250, 1350, by = 100), labels = seq(250, 1250, by = 100))
dens.pr$fiaprbins100 <- cut(dens.pr$MAP2011, breaks = seq(250, 1350, by = 100), labels = seq(250, 1250, by = 100))
dens.pr$plsprbins75 <- cut(dens.pr$MAP1910, breaks = seq(250, 1350, by = 75), labels = seq(250, 1275, by = 75))
dens.pr$fiaprbins75 <- cut(dens.pr$MAP2011, breaks = seq(250, 1350, by = 75), labels = seq(250, 1275, by = 75))
dens.pr$plsprbins150 <- cut(dens.pr$MAP1910, breaks = seq(250, 1350, by = 150), labels = seq(250, 1250, by = 150))
dens.pr$fiaprbins150 <- cut(dens.pr$MAP2011, breaks = seq(250, 1350, by = 150), labels = seq(250, 1250, by = 150))
dens.pr$plsprbins25 <- cut(dens.pr$MAP1910, breaks = seq(250, 1350, by = 25), labels = seq(250, 1325, by = 25))

dens.pr$fiaprbins25 <- cut(dens.pr$MAP2011, breaks = seq(250, 1350, by = 25), labels = seq(250, 1325, by = 25))

dens.pr$sandbins <- cut(dens.pr$sandpct, breaks = seq(0, 100, by = 10))
dens.pr$ksatbins <- cut(dens.pr$ksat, breaks = seq(0,300, by = 10))
dens.pr$moddeltPbins <- cut(dens.pr$moderndeltaP, breaks = seq(0,1, by = .10))
dens.pr$pastdeltPbins <- cut(dens.pr$pastdeltaP, breaks = seq(0,1, by = .10))

test<- dens.pr[!is.na(dens.pr),]
melted <- melt(test, id.vars = c("x", 'y', 'cell', 'plsprbins', 'fiaprbins', 'plsprbins50', 'fiaprbins50','plsprbins75', 'fiaprbins75',
                                 'plsprbins100', 'fiaprbins100','plsprbins150', 'fiaprbins150','plsprbins25', 'fiaprbins25',
                                 'MAP1910', "MAP2011", 
                                 'diff', 'sandpct', 'awc', 'ksat', 'sandbins', 'ksatbins', 'moderndeltaP', 
                                 'pastdeltaP', 'moddeltPbins', 'pastdeltPbins')) 

#map out 
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)

#pdf("outputs/binned_histograms_pr_AGU_12_6_16_large_bins.pdf")
png('outputs/PLS_density_histogrom.png')#
ggplot(dens.pr, aes(PLSdensity)) +geom_histogram(fill= "#D55E00",color = "black") +xlim(0, 700)+ xlab("PLS tree density (stems/ha)")+ ylab('# grid cells')+ 
  theme_bw(base_size = 25)#+ facet_wrap(~plsprbins)
dev.off()

png('outputs/FIA_density_histogram.png')#,
ggplot(dens.pr, aes(FIAdensity)) +geom_histogram(binwidth = 30,fill ="#0072B2",  color = 'black') +xlim(0, 700)+xlab('Modern Tree density (stems/ha)')+ylab("# grid cells")+
  theme_bw(base_size = 25)#+ facet_wrap(~fiaprbins)
dev.off()

library(lattice)
#hexbin plots to show the density of points in precipitatoins
hexbinplot(dens.pr$FIAdensity~ dens.pr$MAP2011, aspect = 1, bins=50, 
           xlab = expression(alpha), ylab = expression(test), 
           style = "nested.lattice",
           panel = function(...) {
             panel.hexbinplot(...)
             panel.abline(h=0)
           })

hbin <- hexbin(dens.pr$MAP2011, dens.pr$FIAdensity, xbins = 100)
plot(hbin)
png('outputs/fia_precipitation_hexbin.png')
ggplot(dens.pr, aes(MAP2011,FIAdensity))+geom_bin2d(bins = 75) +ylim(0,600)+ xlim(400,1400)+
  scale_fill_gradient(low='blue', high='black')+theme_bw(base_size = 20)+
  xlab(' Mean Annual Precipitation (mm) \n PRISM 1900-1910') +ylab(" Modern Tree Density (stems/ha)")
dev.off()


#hbin <- hexbin(dens.pr$MAP1910, dens.pr$PLSdensity, xbins = 100)
#plot(hbin)


png('outputs/PLS_precipitation_hexbin.png')
ggplot(dens.pr, aes(MAP1910,PLSdensity))+geom_bin2d(bins = 75) +ylim(0,600) + xlim(400, 1400)+
 scale_fill_gradient(low='red', high='black')+theme_bw(base_size = 20)+
  xlab('Mean Annual Precipitation (mm) \n PRISM 1900-1910') + ylab("PLS Tree Density (stems/ha)")
dev.off()

rbpalette <- c('red', "blue")
ggplot(melted, aes(value, fill = variable)) +geom_density(alpha = 0.3)  +xlim(0, 400)+ facet_grid(plsprbins~., scales = 'free_y')+scale_fill_brewer(palette = "Set1")

png('outputs/precipitation_by_bins.png')
ggplot(melted, aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)  +xlim(0, 400)+ facet_wrap(~plsprbins, scales = 'free_y')+
 scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()
ggplot(melted, aes(value, fill = variable)) +geom_histogram(binwidth = 35, alpha = 0.3)  +xlim(0, 600)+ facet_wrap(~plsprbins)+scale_fill_brewer(palette = "Set1")

#plot by sandiness
png('outputs/sand_by_bins.png')
ggplot(melted, aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)  +xlim(0, 400)+ facet_wrap(~sandbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()

#plot out climate space:
ggplot(dens.pr, aes(x = MAP1910, y = pasttmean, colour = PLSdensity))+geom_point()+
  scale_fill_gradientn(colours = cbpalette, limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey')

#plot by ksat
png('outputs/ksat_by_bins.png')
ggplot(melted, aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)  +xlim(0, 400)+ facet_wrap(~ksatbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()

#plot by past deltaPbins 
png('outputs/pastDeltaP_by_bins.png')
ggplot(melted, aes(value, colour = variable))+ geom_density(size = 2, alpha = 0.1) +xlim(0, 400)+ facet_wrap(~pastdeltPbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()

#plot by mod deltaPbins
png('outputs/moddeltaP_by_bins.png')
ggplot(melted, aes(value, colour = variable))+ geom_density(size = 2, alpha = 0.1) +xlim(0, 400)+ facet_wrap(~moddeltPbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()

#calculate bimodality coefficients
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
ggplot(coef.bins, aes(x = bins, y = V1))+geom_point()+
 geom_hline( yintercept = 5/9)+ylim(0,1)+theme_bw()+
  theme(axis.text = element_text(angle = 90))+
  xlab('bins') + ylab('Bimodality Coefficient')+
  ggtitle(paste0('Bimodality coefficients for ', binby))
}

pdf('outputs/bimodality_coefficient_binplots.pdf')
calc.BC(data = dens.pr, binby = 'plsprbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'fiaprbins', density = "FIAdensity")
calc.BC(data = dens.pr, binby = 'plsprbins100', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'fiaprbins100', density = "FIAdensity")
calc.BC(data = dens.pr, binby = 'plsprbins75', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'fiaprbins75', density = "FIAdensity")
calc.BC(data = dens.pr, binby = 'plsprbins25', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'fiaprbins25', density = "FIAdensity")

calc.BC(data = dens.pr, binby = 'fiaprbins', density = "PLSdensity")

calc.BC(data = dens.pr, binby = 'sandbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'ksatbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'pastdeltPbins', density = "PLSdensity")
dev.off()




#rolling BC
rollBC_r = function(x,y,xout,width) {
  out = numeric(length(xout))
  for( i in seq_along(xout) ) {
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i] = bimodality_coefficient( y[window & y < 500] ) # what is the BC for places with less than 300 trees per hectare
  }
  ggplot()+geom_point(aes(x = xout, y = out))+
    geom_hline( yintercept = 5/9)+ylim(0,1)+theme_bw()+
    xlab('interval center') + ylab('Bimodality Coefficient') +ggtitle(paste0( 'Bimodality coefficient for binwidth = ', width))
  
}

ordered <- dens.pr[order(dens.pr$MAP1910),]
ordered$rownum <- 1:length(ordered$MAP1910)

pdf('outputs/rolling_BC_plots_500_cutoff.pdf')
rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 150)
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
rollBC_by_10(ordered$MAP1910, ordered$PLSdensity, seq(200, 1350, by = 10)  , 100)


rollBC_by_10 = function(x,y,xout,width) {
  out = 1:length(seq(200, 1350, by = 10) )
  for( i in 1:length(seq(200, 1350, by = 10))) {
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i] = bimodality_coefficient( y[window] )
  }
  df <- data.frame(mid = xout, max = xout + width,min = xout - width,BC = out)
  
}

BC_vals <- rollBC_by_10(ordered$MAP1910, ordered$PLSdensity, seq(200, 1350, by = 10)  , 100)

bc

# merge ordered with the bimodality coefficients
rollBC_merge_r = function(x,y,xout,width) {
  out = numeric(length(xout))
  for( i in seq_along(xout) ) {
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i] = bimodality_coefficient( y[window ] ) # what is the BC for places with less than 300 trees per hectare
  }
  ordered$BC <- out
  ordered
}

dens_BC <- rollBC_merge_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 25)
dens_BC <- dens_BC[!is.na(dens_BC$BC),]
dens_BC$bimodal <- "Not Bimodal" #create a non-bimodal group
dens_BC[dens_BC$BC >=  0.55, ]$bimodal <- "Bimodal" # assign bimodality to grid cells that have high BC
#dens_BC[dens_BC$MAP1910 >= 500 & dens_BC$MAP1910 <= 1000, ]$bimodal <- "Bimodal" 
summary(dens_BC)
plot(dens_BC$BC, dens_BC$PLSdensity, xlim=c(0,1))

#plot relationship:
ggplot(dens.pr, aes(x = MAP1910, y = PLSdensity)) + geom_point()+ylim(0,400)+
  stat_smooth()+facet_wrap(~plsprbins, scales = 'free_x')

library(mgcv)
glm1 = gam(PLSdensity~s(MAP1910),family=gaussian,data=dens.pr)
plot.new()
plot(dens.pr$MAP1910,fitted(glm1),col="springgreen",lwd=2)
summary(glm1)

#Create gam funciton over each of these bins to see if sand and precip can explain bimodality

#map out where bimodality occurs on the modern landscape
p<- ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'grey')+
    geom_raster(data= dens_BC, aes(x = x, y= y, fill = bimodal))+ scale_fill_manual(values = c('purple', 'forestgreen'))+theme_bw()+
    xlab("easting") + ylab("northing") +coord_equal()
#png('outputs/pls_bimodal_climate.png')
p
#dev.off()

f <- ggplot() + geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'grey') + 
  geom_raster(data = test.f, aes(x = x,y = y, fill = bimodal))+ scale_fill_manual(values = c('purple', 'forestgreen'))+theme_bw()+
  xlab("easting") + ylab("northing") +coord_equal()
png('outputs/fia_bimodal_climate.png')
f
dev.off()

fp <- ggplot() + geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'grey')+
  geom_raster(data = test.f1, aes(x=x,y=y, fill = bimodal)) + scale_fill_manual(values = c('purple', 'forestgreen')) + theme_bw()+
  xlab('easting') + ylab('northing') + coord_equal()
png('outputs/modern_bimodal_climate.png')
fp
dev.off()


#using library MClust and Bimodality Index defined in Wang et al.
#apparently BI>1.1 is bimodal...this says PLS data is not
library(mclust)
x.gmm2 = Mclust(dens.pr$PLSdensity, G = 2) # bimodal dist
summary(x.gmm2)
x.gmm1 = Mclust(dens.pr$PLSdensity, G = 1) # unimodal dist
summary(x.gmm1)
means<- x.gmm$parameters$mean
sig <- (means[2] - means[1])/sd(dens.pr$PLSdensity)
BI <- sqrt(x.gmm$parameters$pro[1]*(1-x.gmm$parameters$pro[1]))*sig
BI <- sqrt(x.gmm$parameters$pro[1]*(1-x.gmm$parameters$pro[1]))*sig
BI

library(diptest)
#################################################
#using diptest statistics--Not sure how great this is:
coeffs <- matrix(NA, 22, 2)
bins <- as.character(unique(dens.pr$plsprbins))
for (i in 1:22){
  a <- dip.test(dens.pr[dens.pr$plsprbins %in% bins[i],]$PLSdensity)
  coeffs[i,1] <- a$statistic
  coeffs[i,2] <- a$p.value
}
coef.bins<- cbind(coeffs, bins)

coeffsfia <- matrix(NA, 11, 2)
binsfia <- as.character(unique(dens.pr$plsprbins))
for (i in 1:11){
  a<- dip.test(dens.pr[dens.pr$fiaprbins %in% binsfia[i],]$FIAdensity)
  coeffsfia[i,1] <- a$statistic
  coeffsfia[i,2] <- a$p.value
  }
coef.bins.fia <- cbind(coeffsfia, binsfia)

#merge together with dens.pr
test <- merge(dens.pr, coef.bins, by.x = "plsprbins", by.y = 'bins')
test$V1 <- as.numeric(as.character(test$V1))
test$V2 <- as.numeric(as.character(test$V2))
test$bimodal <- "Not Bimodal"
test[test$V2 < 0.01& test$MAP1910 < 1000, ]$bimodal <- "Bimodal"

#for FIA
test.f <- merge(dens.pr, coef.bins.fia, by.x = "fiaprbins", by.y = 'binsfia')
test.f$V1 <- as.numeric(as.character(test.f$V1))
test.f$V2 <- as.numeric(as.character(test.f$V2))
test.f[is.na(test.f$V1),]$V1 <- 1

test.f$bimodal <- "Not Bimodal"
test.f[test.f$V2 <  0.01 & test.f$MAP2011 < 1000, ]$bimodal <- "Bimodal"

#for FIA using pls climate that is bimodal
test.f1 <- merge(dens.pr, coef.bins, by.x = "fiaprbins", by.y = 'bins')
test.f1$V1 <- as.numeric(as.character(test.f1$V1))
test.f1$V2 <- as.numeric(as.character(test.f1$V2))


test.f1$bimodal <- "Not Bimodal"
test.f1[test.f1$V2 <  0.01 & test.f1$MAP1910 < 1000, ]$bimodal <- "Bimodal"

ggplot(test, aes(x,y, fill = bimodal))+geom_raster()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat, colour= 'black'), fill = NA)
ggplot(test.f, aes(x,y, fill = bimodal))+geom_raster()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat, colour= 'black'), fill = NA)
ggplot(test.f1, aes(x,y, fill = bimodal))+geom_raster()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat, colour= 'black'), fill = NA)


ggplot(dens.pr, aes(x,y, fill = plsprbins))+geom_raster()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat, colour= 'black'), fill = NA)
ggplot(dens.pr, aes(x,y, fill = fiaprbins))+geom_raster()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat, colour= 'black'), fill = NA)

#need to do this with species, dens.pr is now just densitys
dev.off()

###read in soil covariates from STATSGO (working on ssurgo)
covariates <- read.csv('C:/Users/JMac/Documents/Kelly/BIOMASS/biomass_UW/IL_IN_covariates/datacovariates_full_extract_v2.csv')

#merge covariates with density data
dens.soils <- merge(dens.pr, covariates, by = c('x', 'y', 'cell'))
library(stargazer)


#basic linear models to get an idea of what is important
#PLS.silt<- lm(dens.soils$PLSdensity ~dens.soils$silt)
#plot(PLSdensity ~ silt, data=dens.soils)
#lines(lowess(dens.soils$silt ~ dens.soils$PLSdensity), col="red")
#abline(PLS.silt)

PLS.sand<- lm(dens.soils$PLSdensity ~dens.soils$sandpct)
#PLS.clay<- lm(dens.soils$PLSdensity ~dens.soils$clay)
PLS.awc<- lm(dens.soils$PLSdensity ~dens.soils$awc)
#PLS.DEM<- lm(dens.soils$PLSdensity ~dens.soils$DEM)
PLS.ksat<- lm(dens.soils$PLSdensity ~dens.soils$ksat)
#summary(PLS.DEM)
#summary(PLS.silt)
summary(PLS.sand)
#summary(PLS.clay)
summary(PLS.awc)
summary(PLS.ksat)
stargazer(PLS.DEM, PLS.silt, PLS.sand, PLS.clay, PLS.awc, PLS.ksat, type="html",
          dep.var.labels=c("PLS Tree Density"),  
          covariate.labels=c("DEM","silt","sand",
                             "clay","AWC", "ksat"), flip = TRUE, out="PLSmodels.htm")

#FIA
FIA.silt<- lm(dens.soils$FIAdensity ~dens.soils$silt)
FIA.sand<- lm(dens.soils$FIAdensity ~dens.soils$sand)
FIA.clay<- lm(dens.soils$FIAdensity ~dens.soils$clay)
FIA.awc<- lm(dens.soils$FIAdensity ~dens.soils$awc)
FIA.DEM<- lm(dens.soils$FIAdensity ~dens.soils$DEM)
FIA.ksat<- lm(dens.soils$FIAdensity ~dens.soils$ksat)

summary(FIA.DEM)
summary(FIA.silt)
summary(FIA.sand)
summary(FIA.clay)
summary(FIA.awc)
summary(FIA.ksat)


model.summary = rbind(coef(summary(FIA.silt))[,1:4],coef(summary(FIA.DEM))[, 1:4])
stargazer(model.summary, flip=TRUE, out = 'FIAmodel.htm')
stargazer(FIA.DEM, FIA.silt, FIA.sand, FIA.clay, FIA.awc, FIA.ksat, type="html",
          dep.var.labels=c("FIA Tree Density"), column.separate = c(1), 
          covariate.labels=c("DEM","silt","sand",
                             "clay","AWC", "ksat"), align  = TRUE, out="FIAmodels.htm")


pdf("outputs/soil_density_plots.pdf")
p.dem <- ggplot(dens.soils, aes(DEM, PLSdensity)) + geom_point() + ylim(0, 800)+ theme_classic() + xlab('elevation (m)') + ylab('Pre-Settlement \n Tree Density \n (Trees/hectare)')+
   theme_bw()+
  theme(text = element_text(size = 20))+geom_smooth(method = 'lm')
p.dem

p.silt <- ggplot(dens.soils, aes(silt, PLSdensity)) + geom_point() + ylim(0, 800)+ theme_classic() + xlab('% silt') + ylab('Pre-Settlement \n Tree Density \n (Trees/hectare)')+
  theme_bw()+
  theme(text = element_text(size = 20))+geom_smooth(method = 'lm')
p.silt

p.sand <- ggplot(dens.soils, aes(sand, PLSdensity)) + geom_point() + ylim(0, 800)+ theme_classic() + xlab('% sand') + ylab('Pre-Settlement \n Tree Density \n (Trees/hectare)')+
  theme_bw()+
  theme(text = element_text(size = 20))+geom_smooth(method = 'lm')
p.sand

p.clay <- ggplot(dens.soils, aes(clay, PLSdensity)) + geom_point() + ylim(0, 800)+ theme_classic() + xlab('% clay') + ylab('Pre-Settlement \n Tree Density \n (Trees/hectare)')+
  theme_bw()+
  theme(text = element_text(size = 20))+geom_smooth(method = 'lm')
p.clay

p.awc <- ggplot(dens.soils, aes(awc, PLSdensity)) + geom_point() + ylim(0, 800)+ theme_classic() + xlab('awc)') + ylab('Pre-Settlement \n Tree Density \n (Trees/hectare)')+
  theme_bw()+
  theme(text = element_text(size = 20))+geom_smooth(method = 'lm')
p.awc


f.DEM <- ggplot(dens.soils, aes(DEM, FIAdensity)) + geom_point()  + ylim(0, 800) + theme_classic() + xlab('elevation (m)') + ylab('Modern \n Tree Density \n (Trees/hectare)')+
  theme_bw()+
  theme(text = element_text(size = 20)) +geom_smooth(method = 'lm')
f.DEM

f.silt <- ggplot(dens.soils, aes(silt, FIAdensity)) + geom_point() + ylim(0, 800)+ theme_classic() + xlab('% silt') + ylab('Modern \n Tree Density \n (Trees/hectare)')+
  theme_bw()+
  theme(text = element_text(size = 20))+geom_smooth(method = 'lm')
f.silt

f.sand <- ggplot(dens.soils, aes(sand, FIAdensity)) + geom_point() + ylim(0, 800)+ theme_classic() + xlab('% sand') + ylab('Modern \n Tree Density \n (Trees/hectare)')+
  theme_bw()+
  theme(text = element_text(size = 20))+geom_smooth(method = 'lm')
f.sand

f.clay <- ggplot(dens.soils, aes(clay, FIAdensity)) + geom_point() + ylim(0, 800)+ theme_classic() + xlab('% clay') + ylab('Modern \n Tree Density \n (Trees/hectare)')+
  theme_bw()+
  theme(text = element_text(size = 20))+geom_smooth(method = 'lm')
f.clay

f.awc <- ggplot(dens.soils, aes(awc, FIAdensity)) + geom_point() + ylim(0, 800)+ theme_classic() + xlab('awc)') + ylab('Modern \n Tree Density \n (Trees/hectare)')+
  theme_bw()+
  theme(text = element_text(size = 20))+geom_smooth(method = 'lm')
f.awc

#so soils don't seem to explain much of the tree density overall...
#what about in the places of intermediate precipitation?

ggplot(dens.soils, aes(DEM, PLSdensity)) +geom_point() + ylim(0, 800)+ facet_wrap(~plsprbins)
ggplot(dens.soils, aes(silt, PLSdensity)) +geom_point() + ylim(0, 800)+ facet_wrap(~plsprbins)
ggplot(dens.soils, aes(sand, PLSdensity)) +geom_point() + ylim(0, 800)+ facet_wrap(~plsprbins)
ggplot(dens.soils, aes(clay, PLSdensity)) +geom_point() + ylim(0, 800)+ facet_wrap(~plsprbins)
ggplot(dens.soils, aes(awc, PLSdensity)) +geom_point() + ylim(0, 800)+ facet_wrap(~plsprbins)
ggplot(dens.soils, aes(ksat, PLSdensity)) +geom_point() + ylim(0, 800)+ facet_wrap(~plsprbins)

ggplot(dens.soils, aes(DEM, FIAdensity)) +geom_point() + ylim(0, 800)+ facet_wrap(~plsprbins)
ggplot(dens.soils, aes(silt, FIAdensity)) +geom_point() + ylim(0, 800)+ facet_wrap(~plsprbins)
ggplot(dens.soils, aes(sand, FIAdensity)) +geom_point() + ylim(0, 800)+ facet_wrap(~plsprbins)
ggplot(dens.soils, aes(clay, FIAdensity)) +geom_point() + ylim(0, 800)+ facet_wrap(~plsprbins)
ggplot(dens.soils, aes(awc, FIAdensity)) +geom_point() + ylim(0, 800)+ facet_wrap(~plsprbins)
ggplot(dens.soils, aes(ksat, FIAdensity)) +geom_point() + ylim(0, 800)+ facet_wrap(~plsprbins)

#DEM seems to be the only one where the data isn't really messed up. I am going to average the density by bins for soils
by_silt<- summarise (group_by(dens.soils, silt), mean = mean(PLSdensity), sd = sd(PLSdensity))
by_sand<- summarise (group_by(dens.soils, sand), mean =mean(PLSdensity), sd = sd(PLSdensity))
by_clay<- summarise (group_by(dens.soils, clay), mean =mean(PLSdensity), sd = sd(PLSdensity))
by_ksat<- summarise (group_by(dens.soils, ksat), mean =mean(PLSdensity), sd = sd(PLSdensity))
by_awc<- summarise (group_by(dens.soils, awc), mean =mean(PLSdensity), sd = sd(PLSdensity))
by_DEM <- summarise (group_by(dens.soils, DEM), mean =mean(PLSdensity), sd = sd(PLSdensity))

#now plot average PLS density by % soil characteristics
ggplot(by_silt, aes(silt, mean))+geom_point()+geom_errorbar(width=.1, aes(ymin=mean-sd, ymax=mean+sd), color = 'grey')+theme_bw()
ggplot(by_sand, aes(sand, mean))+geom_point()+geom_errorbar(width=.1, aes(ymin=mean-sd, ymax=mean+sd), color = 'grey')+theme_bw()
ggplot(by_clay, aes(clay, mean))+geom_point()+geom_errorbar(width=.1, aes(ymin=mean-sd, ymax=mean+sd), color = 'grey')+theme_bw()
ggplot(by_awc, aes(awc, mean))+geom_point()+geom_errorbar(width=.1, aes(ymin=mean-sd, ymax=mean+sd), color = 'grey')+theme_bw()
ggplot(by_ksat, aes(ksat, mean))+geom_point()+geom_errorbar(width=.1, aes(ymin=mean-sd, ymax=mean+sd), color = 'grey')+theme_bw()
#ggplot(by_DEM, aes(DEM, mean))+geom_point()+geom_errorbar(width=.1, aes(ymin=mean-sd, ymax=mean+sd), color = 'grey')+theme_bw()

#Do the same for FIA density
by_silt.f<- summarise (group_by(dens.soils, silt), mean = mean(FIAdensity), sd = sd(FIAdensity))
by_sand.f<- summarise (group_by(dens.soils, sand), mean =mean(FIAdensity), sd = sd(FIAdensity))
by_clay.f<- summarise (group_by(dens.soils, clay), mean =mean(FIAdensity), sd = sd(FIAdensity))
by_ksat.f<- summarise (group_by(dens.soils, ksat), mean =mean(FIAdensity), sd = sd(FIAdensity))
by_awc.f<- summarise (group_by(dens.soils, awc), mean =mean(FIAdensity), sd = sd(FIAdensity))
by_DEM.f <- summarise (group_by(dens.soils, DEM), mean =mean(FIAdensity), sd = sd(FIAdensity))

#now plot average PLS density by % soil characteristics
ggplot(by_silt.f, aes(silt, mean))+geom_point()+geom_errorbar(width=.1, aes(ymin=mean-sd, ymax=mean+sd), color = 'grey')+theme_bw()
ggplot(by_sand.f, aes(sand, mean))+geom_point()+geom_errorbar(width=.1, aes(ymin=mean-sd, ymax=mean+sd), color = 'grey')+theme_bw()
ggplot(by_clay.f, aes(clay, mean))+geom_point()+geom_errorbar(width=.1, aes(ymin=mean-sd, ymax=mean+sd), color = 'grey')+theme_bw()
ggplot(by_awc.f, aes(awc, mean))+geom_point()+geom_errorbar(width=.1, aes(ymin=mean-sd, ymax=mean+sd), color = 'grey')+theme_bw()
ggplot(by_ksat.f, aes(ksat, mean))+geom_point()+geom_errorbar(width=.1, aes(ymin=mean-sd, ymax=mean+sd), color = 'grey')+theme_bw()
#ggplot(by_DEM, aes(DEM, mean))+geom_point()+geom_errorbar(width=.1, aes(ymin=mean-sd, ymax=mean+sd), color = 'grey')+theme_bw()
dev.off()

###########################################################
#does temperature explain the distribution in density?####
##########################################################
t.mod <- read.csv('data/air_temp_alb_2000_2011_GHCN.csv')
t.past <- read.csv('data/air_temp_alb_1900_1910_GHCN.csv')


##calculate species richness: number of species in each grid cell
dens.pr$spec.rich <- rowSums(dens.pr[,5:41] != 0, na.rm=TRUE)
fia.dens.pr$spec.rich <- rowSums(fia.dens.pr[,5:41] != 0, na.rm=TRUE)

#species richness is way lower in FIA compared to PLS

#calculate frequency of species i---pi = # trees of species i/total number of trees
#H = - SUM(pi *ln(pi))
pi.pls <- dens.pr[,5:41]/dens.pr$PLSdensity

pi.fia <- fia.dens.pr[,5:41]/fia.dens.pr$FIAdensity

dens.pr$H.pls <- -1*rowSums((pi.pls)*log(pi.pls), na.rm=TRUE)
fia.dens.pr$H.fia <- -1*rowSums((pi.fia)*log(pi.fia), na.rm=TRUE)

#calculate simpsons index (lambda) = sum (#trees species*(# trees of species i-1))/(total density*(total density -1))
#simpsons gives probability that any two random tress will be different species
#this gives some negative probabilities so double check
lambda.pls <- rowSums((dens.pr[,5:41]*(dens.pr[,5:41]-1))/(dens.pr$PLSdensity*(dens.pr$PLSdensity-1)), na.rm = TRUE)
lambda.fia <- rowSums((fia.dens.pr[,5:41]*(fia.dens.pr[,5:41]-1))/(fia.dens.pr$PLSdensity*(fia.dens.pr$FIAdensity-1)), na.rm = TRUE)

#similarity index to compare pls & FIA
#sim = 2*sum(n*c)

dens.only <- dens.pr[,5:41]
dens.only[is.na(dens.only)] <- 0
shan.div <- diversity(dens.only, index = "shannon")


bci.mds<-metaMDS(dens.only, distance = "bray", k = 2, trymax = 20, autotransform =TRUE, noshare = 0.1, expand = TRUE, trace = 1, plot = FALSE) #makes the object bci.mds using Bray-Curtis ordination

plot(bci.mds, choices = c(1, 2), type="n") #plots the ordination axes
points(bci.mds, display = c("sites", "species"))#displays both sites and species on the same plot.  Try choosing just “sites” to reduce clutter
text(bci.mds, display = c("sites", "species"))




######################################################
#read in us ecoregions shapefile to overlay in ggplot#
######################################################
library(sp)
library(rgdal)
library(plyr)
library(ggplot2)
library("rgdal")
library("rgeos")
library("dplyr")

ecoregions <- readOGR(dsn = 'data/eco-us-shp/eco_us.shp', layer = 'eco_us')
#has all ecoregions, only want the Laurentian Mixed Forest Province, Prairie Parkland (Temperate) Province

Prairie <- c('Prairie Parkland (Temperate) Province',                                                      
              'Great Plains Steppe and Shrub Province')
Forest <- c('Laurentian Mixed Forest Province','Ozark Broadleaf Forest - Meadow Province','Eastern Broadleaf Forest (Continental) Province')

ecoregions.P <- ecoregions[ecoregions$PROVINCE %in% Prairie|ecoregions$PROVINCE %in% Forest, ]
#ecoregions.F <- ecoregions[ecoregions$PROVINCE %in% Forest]
ecoregions <- ecoregions[ecoregions$PROVINCE %in% Prairie & ecoregions$PROVINCE %in% Forest, ]
#ecoregions$PF <- NA
#ecoregions@data[ecoregions$PROVINCE %in% Forest, ]$PF <- 1
#ecoregions@data[ecoregions$PROVINCE %in% Prairie, ]$PF <- 0
#X11(width = 14)





#plot(ecoregions)
P.data <- spTransform(ecoregions.P, CRS('+init=epsg:3175'))
eco.data <-spTransform(ecoregions, CRS('+init=epsg:3175'))
P.data_df <- rename(P.data_df, ECO_US_ID = id)
P.data_df <- left_join(P.data_df, P.data@data, by = 'ECO_US_ID')

#P.data@data$id <- rownames(P.data@data)
F.data <- spTransform(ecoregions.F, CRS('+init=epsg:3175'))
P.data_df <- fortify(P.data) #fortify to plot in ggplot
#ecoreg.df     <- join(P.data_df,P.data@data, by="ECO_US_ID")
F.data_df <- fortify(F.data)
ecoregionmap<- ggplot()+
  geom_polygon(data = P.data_df,aes(x = long, y = lat, group = group, colours = group)) +
  geom_polygon(data = F.data_df,aes(x = long, y = lat, group = group), fill = 'tan')+
  geom_raster(data=fia.dens.pr, aes(x=x, y=y, fill = FIAdensity))
  #coord_map()
  coord_equal()
  #geom_path(color = 'red') +theme_bw() 
ecoregionmap


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
library(ggplot2)
melt.test <- melt(dens.pr[,1:41], id = c('x', 'y', 'cell', 'X'))
pls.2map <- ggplot()+ geom_raster(data=melt.test, aes(x=x, y=y, fill = value))+
  labs(x="easting", y="northing", title="Tree PLS density") + 
  scale_fill_gradientn(colours = rev(terrain.colors(4)), name ="Tree Dens. \n (stems/ha)") + facet_wrap(~variable)
pls.2map

X11(width = 18)

pdf('outputs/maps_CHW_talk.pdf')
pls.map

sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

pls.map <- ggplot()+ geom_raster(data=dens.pr, aes(x=x, y=y, fill = PLSdensity))+
  labs(x="easting", y="northing", title="PLS tree density") + 
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat, colour= 'black'), fill = NA)+
  scale_fill_gradientn(colours = cbpalette, limits = c(0,700), name ="Tree \n Density \n (trees/hectare)") +
  coord_equal()
pls.map

FIA.map <- ggplot()+ geom_raster(data=dens.pr, aes(x=x, y=y, fill = FIAdensity))+
  labs(x="easting", y="northing", title="Tree FIA density") + 
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat, colour= 'black'), fill = NA)+
  scale_fill_gradientn(colours = rev(terrain.colors(4)), limits = c(0,700), name= "Tree density") +
  coord_equal()
FIA.map

pls.map <- ggplot()+ geom_raster(data=dens.pr, aes(x=x, y=y, fill = PLSdensity))+
  labs(x="easting", y="northing", title="Tree PLS density") + 
  scale_fill_gradientn(colours = rev(terrain.colors(4)), limits = c(0,1000), name ="Tree Dens. \n (stems/ha)") 
  

pls.map

FIA.map <- ggplot()+ geom_raster(data=fia.dens.pr, aes(x=x, y=y, fill = FIAdensity))+
  labs(x="easting", y="northing", title="Tree FIA density") + 
  scale_fill_gradientn(colours = rev(terrain.colors(4)), limits = c(0,1000),name ="Tree Dens. \n (stems/ha)") 
FIA.map

diff.map <- ggplot()+ geom_raster(data=dens.pr, aes(x=x, y=y, fill = diff))+
  labs(x="easting", y="northing", title="FIA-PLS")  + 
  scale_fill_gradientn(colours = rainbow(4), name ="Difference in \n Tree Dens. \n (stems/ha)")
diff.map

pr1901.map <- ggplot()+ geom_raster(data=dens.pr, aes(x=x, y=y, fill = MAP1910))+
  labs(x="easting", y="northing", title="1895-1935 MAP")  + 
  scale_fill_gradientn(colours = rainbow(4), name ="Precip. \n (mm/year)",limits = c(0,1300))
pr1901.map

pr2011.map <- ggplot()+ geom_raster(data=dens.pr, aes(x=x, y=y, fill = MAP2011))+
  labs(x="easting", y="northing", title="1975-2011 MAP")  + 
  scale_fill_gradientn(colours = rainbow(4), name ="TPrecip. \n (mm/year)",limits = c(0,1300))
pr2011.map

dev.off()
#use ggplot to plot data and regression line
pls.pr <- ggplot()+ geom_point(data=dens.pr, aes(x=MAP1910, y=PLSdensity))+
  geom_smooth(data=dens.pr, aes(x=MAP1910, y=PLSdensity),method=lm) 
pls.pr


pls.dens.rich <- ggplot()+ geom_point(data=dens.pr, aes(x=H.pls, y=PLSdensity))+
  geom_smooth(data=dens.pr, aes(x=H.pls, y=PLSdensity),method=lm) 
pls.dens.rich

fia.pr <- ggplot()+ geom_point(data=dens.pr, aes(x=MAP1910, y=FIAdensity))+
  geom_smooth(data=dens.pr, aes(x=MAP1910, y=FIAdensity),method=lm) 
fia.pr

dif.pr <- ggplot()+ geom_point(data=dens.pr, aes(x=MAP2011, y=diff))+
  geom_smooth(data=dens.pr, aes(x=MAP2011, y=diff),method=lm) 
dif.pr

dif <- ggplot()+ geom_point(data=dens.pr, aes(x=PLSdensity, y=FIAdensity))+
  geom_smooth(data=dens.pr, aes(x=PLSdensity, y=FIAdensity),method=lm) 
dif





##################################################################################
#find grid cells that have FIA data in them, but no PLS data in indiana & illinois
##################################################################################
#map out 
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))


#find grid cells not in pls dataset (this includes the eastern states as well)
test2<- density.FIA.table[!density.FIA.table$cell %in% pls.inil$cell,]

#rasterize test2
coordinates(test2) <- ~x+y
gridded(test2)<-TRUE
notinpls <- stack(test2)

#crop by the outline of indiana & illinois state borders
test<- crop(notinpls, extent(mapdata))
plot(test[['FIAdensity']])
test.df <- as.data.frame(test, xy = TRUE)

head(test.df)
#avg.alb <- as.data.frame(avg.alb, xy = TRUE)

needed.pls <- test.df[,c('x', 'y', 'cell', 'FIAdensity')]
needed.pls<- na.omit(needed.pls)

summary(needed.pls)
write.csv(needed.pls, 'outputs/fia_grid_cells_with_no_pls.csv')

#create a raster
coordinates(needed.pls) <- ~x+y
gridded(needed.pls) <- TRUE
needed.pls.rast <- stack(needed.pls)

writeRaster(needed.pls.rast, 'outputs/fia_grid_with_no_pls.tif', overwrite = TRUE)

