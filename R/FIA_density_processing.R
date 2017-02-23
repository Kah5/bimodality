#read in FIA from Sean's repository
# set pls version to use
version <- "1.6-5"
setwd( "C:/Users/JMac/Documents/Kelly/biomodality")
library(data.table)
library(reshape2)
library(dtplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(modes)

FIA <- read.csv('data/FIA_species_plot_parameters_paleongrid.csv')
speciesconversion <- read.csv('data/FIA_conversion-SGD_remove_dups.csv')

FIA.pal <- merge(FIA, speciesconversion, by = 'spcd' )
FIA.by.paleon <- dcast(FIA.pal, x + y+ cell+ plt_cn ~ PalEON, sum, na.rm=TRUE, value.var = 'density') #sum all species in common taxa in FIA grid cells
FIA.by.paleon$FIAdensity <- rowSums(FIA.by.paleon[,6:25], na.rm = TRUE) # sum the total density in each plot
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y', 'cell', 'plt_cn', 'Var.5')) # melt the dataframe
fia.by.cell <- dcast(fia.melt, x + y+ cell ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell

density.FIA.table <- fia.by.cell

lowdens <- FIA.by.paleon[FIA.by.paleon$FIAdensity < 47,]
summary(lowdens) # note there seems to be alot of density grid cells at 44.6...
low.densm <- melt(lowdens, id.vars = c("x", "y", "cell", "plt_cn", "Var.5"))

ggplot(low.densm, aes(value))+geom_histogram()+facet_wrap(~variable)
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

umdw <- read.csv('data/plss_density_alb_v0.9-10.csv')
#umdw.mean <- dcast(umdw, x + y + cell ~., mean, na.rm = TRUE, value.var = 'density')
umdw$total <- rowSums(umdw[,4:32], na.rm= TRUE)
umdw.new <- umdw[,c('x', 'y', 'cell', 'total')]


colnames(umdw.new) <- c('x', 'y', 'cell', 'PLSdensity')
hist(umdw.new$PLSdensity, breaks = 25)

pls.inil <- rbind(pls.inil, umdw.new)
#coordinates(pls.inil)<- ~x+y

#test.ex<- extract(density.FIA.table, extent(pls.inil))
#write.csv(pls.inil,C:/Users/JMac/Documents/Kelly/biomodality/outputs )
#plot raw data


#merge inil pls and inilFIA
densitys <- merge(pls.inil[,c('x', 'y', 'cell', 'PLSdensity')], density.FIA.table[,c('x', 'y', 'cell', 'FIAdensity')],
                  by = c('x', 'y', 'cell'))

#note that for some reason, 1 grid cell is duplicated
nodups <- densitys[!duplicated(densitys$cell),] # remove dups
dup <- densitys[duplicated(densitys$cell),] # what is the duplicated row?
#test <- rbind(nodups, dup)
densitys <- nodups

hist(densitys$PLSdensity, breaks = 50)
#map out density:
ggplot(densitys, aes(x,y,color = PLSdensity))+geom_point()
ggplot(densitys, aes(x,y,color = FIAdensity))+geom_point()

#keep only the 99th percentile of densitys---this is also what simon does
nine.nine.pct <- apply(densitys[,4:ncol(densitys)], 2, quantile, probs = 0.995, na.rm=TRUE)
densitys$PLSdensity[densitys$PLSdensity > nine.nine.pct['PLSdensity']] <- nine.nine.pct['PLSdensity']
densitys$FIAdensity[densitys$FIAdensity > nine.nine.pct['FIAdensity']] <- nine.nine.pct['FIAdensity']

summary(densitys)

hist(densitys$PLSdensity, breaks = 25)
hist(densitys$FIAdensity, breaks = 25)
#print out the histograms here
png(height=4, width=8, units = 'in',res = 300, paste0("outputs/v",version,"/FIA_PLS_hists.png"), type="cairo")
pushViewport(viewport(layout = grid.layout(1, 2)))
print(ggplot(densitys, aes(PLSdensity)) +geom_histogram(fill= "#D55E00",color = "black") +xlim(0, 700)+ xlab("PLS tree density (stems/ha)")+ ylab('# grid cells')+ 
        theme_bw(base_size = 10), vp = viewport(layout.pos.row = 1, layout.pos.col = 1))#+ facet_wrap(~plsprbins)
print(ggplot(densitys, aes(FIAdensity)) +geom_histogram(binwidth = 30,fill ="#0072B2",  color = 'black') +xlim(0, 700)+xlab('Modern Tree density (stems/ha)')+ylab("# grid cells")+
        theme_bw(base_size = 10),vp = viewport(layout.pos.row = 1, layout.pos.col = 2))#+ facet_wrap(~fiaprbins)

dev.off()

#make difference plot with ggplot:
dens <- densitys
dens$diff <- dens$FIAdensity - dens$PLSdensity
png(width = 4, height = 3,units = 'in', res = 300, paste0('outputs/v', version, '/density_difference_plot.png'))
ggplot(dens, aes(x = PLSdensity, y = diff))+ geom_point()+geom_density_2d() +geom_smooth(method = 'lm', color = 'red')+xlim(0,600)+
  theme_bw(base_size = 15)+ ylab('increase in density since PLS (trees/ha)') + xlab('PLS tree density (trees/ha)') + annotate("text", x=400, y=900,label= paste("R-squared =", round(cor(dens$PLSdensity, dens$diff),2)), size = 5)
dev.off()

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
png(paste0('outputs/v',version,'/PLS_tree_density_map.png'))
pls.map
dev.off()

fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=densitys, aes(x=x, y=y, fill = FIAdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="FIA tree density") + 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw()
png(paste0('outputs/v',version,'/FIA_tree_density_map.png'))
fia.map
dev.off()

#map PLS and fia side by side
png(height=400, width=800,paste0('outputs/v', version, '/tree_density_maps_PLS_FIA.png'))
pushViewport(viewport(layout = grid.layout(1, 2)))
print(pls.map, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(fia.map, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

write.csv(densitys, paste0("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_alb", version,".csv"))

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
dens.pr <- dens.pr[dens.pr$FIAdensity < 650,]

#estimating density of data
plot(density(dens.pr$PLSdensity))
plot(density(dens.pr$FIAdensity))

find_modes<- function(x) {
  modes <- NULL
  for ( i in 2:(length(x)-1) ){
    if ( (x[i] > x[i-1]) & (x[i] > x[i+1]) ) {
      modes <- c(modes,i)
    }
  }
  if ( length(modes) == 0 ) {
    modes = 'This is a monotonic distribution'
  }
  return(modes)
}



get.modes <- function(x,bw,spar) {  
  den <- density(x, kernel=c("gaussian"),bw=bw)
  den.s <- smooth.spline(den$x, den$y, all.knots=TRUE, spar=spar)
  s.1 <- predict(den.s, den.s$x, deriv=1)
  s.0 <- predict(den.s, den.s$x, deriv=0)
  den.sign <- sign(s.1$y)
  a<-c(1,1+which(diff(den.sign)!=0))
  b<-rle(den.sign)$values
  df<-data.frame(a,b)
  df = df[which(df$b %in% -1),]
  modes<-s.1$x[df$a]
  density<-s.0$y[df$a]
  df2<-data.frame(modes,density)
  df2<-df2[with(df2, order(-density)), ] # ordered by density
  df2
  #df2$modes
}
mm <- c(418, 527, 540, 553, 554, 558, 613, 630, 634, 636, 645, 648, 708, 714, 715, 725, 806, 807, 822, 823, 836, 837, 855, 903, 908, 910, 911, 913, 915, 923, 935, 945, 955, 957, 958, 1003, 1006, 1015, 1021, 1021, 1022, 1034, 1043, 1048, 1051, 1054, 1058, 1100, 1102, 1103, 1117, 1125, 1134, 1138, 1145, 1146, 1150, 1152, 1210, 1211, 1213, 1223, 1226, 1334)
mmdf<-as.data.frame(mm)
library(ggplot2)
ggplot(dens.pr,aes(PLSdensity)) +geom_density(bw= "nrd0")
ggplot(dens.pr,aes(FIAdensity)) +geom_density(bw= "nrd0")
get.modes(dens.pr$PLSdensity,20,0.5)
get.modes(dens.pr$FIAdensity,20,0.5)

#find modes for pls density
find_modes(density(dens.pr[dens.pr$PLSdensity < 450,]$PLSdensity)$y)
plot(density(dens.pr$PLSdensity))

bimodality_coefficient(density(dens.pr$PLSdensity)$y)
diptest::dip.test(dens.pr$PLSdensity)

find_modes(density(dens.pr[dens.pr$FIAdensity < 400,]$FIAdensity)$y)
plot(density(dens.pr$FIAdensity))

bimodality_coefficient(density(dens.pr[dens.pr$FIAdensity < 400,]$FIAdensity)$y)
diptest::dip.test(dens.pr[dens.pr$FIAdensity < 400,]$FIAdensity)

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
colnames(dens.pr)[6:7] <- c('MAP1910', "MAP2011")

#now add the precipitation seasonality to the dataframe
dens.pr <- merge(dens.pr, mod.precip.mo[,c('x', 'y', 'SI')], by = c('x', 'y') )
dens.pr <- merge(dens.pr, past.precip.mo[,c('x', 'y', 'deltaP')], by = c('x', 'y') )
colnames(dens.pr)[8:9]<- c('moderndeltaP', 'pastdeltaP')
nodups <- dens.pr[!duplicated(dens.pr$cell),] #

dens.pr <- nodups
#now add the mean temperature to the dataframe
dens.pr <- merge(dens.pr, mod.tmean[,c('x', 'y', 'modtmean')], by = c('x', 'y') )
dens.pr <- merge(dens.pr, past.tmean[,c('x', 'y', 'Mean', 'deltaT')], by = c('x', 'y') )
colnames(dens.pr)[10:12] <- c('modtmean','pasttmean', 'deltaT')



nodups <- dens.pr[!duplicated(dens.pr$cell),] #

dens.pr <- nodups
hist(nodups$PLSdensity, breaks =50)
dens.pr <- merge(dens.pr, mod.tmean.mo[,c('x', 'y', 'moddeltaT')], by = c('x', 'y') )

write.csv(nodups, paste0("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_density_pr_alb",version,".csv"))


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
#########################
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

##########################################################
# assign density classificaitons of savanna, forests, etc#
##########################################################

#Using the Rheumtella Classification scheme:
# prairie (<0.5 trees/ha)
# savanna (0.5-47 trees/ha)
# forest cover (>47 trees/ha)

dens.pr$ecotype<- 'test'
dens.pr[dens.pr$PLSdensity > 47, ]$ecotype <-  "Forest"
dens.pr[dens.pr$PLSdensity < 47, ]$ecotype <-  "Savanna" 
dens.pr[dens.pr$PLSdensity < 0.5, ]$ecotype <-  "prairie"

ggplot(data = dens.pr, aes(x = x, y = y, color = ecotype)) + geom_point()

#define ecotype for modern landscape
dens.pr$fiaecotype<- 'test'
dens.pr[dens.pr$FIAdensity > 47, ]$fiaecotype <-  "Forest"
dens.pr[dens.pr$FIAdensity < 47, ]$fiaecotype <-  "Savanna" 
dens.pr[dens.pr$FIAdensity < 0.5, ]$fiaecotype <-  "prairie"

ggplot(data = dens.pr, aes(x = x, y = y, color = fiaecotype)) + geom_point()

#or we could use kmeans clustering on the PLS density variable, but this gives us two high density modes
#fit.km <- kmeans(dens.pr$PLSdensity, 4, nstart=25)
#dens.pr$kmeans <- fit.km$cluster
#plot(dens.pr$kmeans, dens.pr$PLSdensity)
#############################################
# PCA analysis of environmental variables:
############################################
# remove the NA values and scale
dens.rm <- na.omit(dens.pr)
dens.rm <- data.frame(dens.rm)
scale.dens <- scale(dens.rm[, c("MAP1910", "pastdeltaP", 
                                "pasttmean", "deltaT", 
                                "sandpct", "awc", "MAP2011", "moderndeltaP", 
                                "modtmean", "moddeltaT")]) #PC all but ksat and diff
dens.dens <- dens.rm[, c('PLSdensity')] # pls density

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
dens.pca <- princomp(scale.dens[,c("MAP1910", "pastdeltaP", 
                                   "pasttmean", "deltaT", 
                                   "sandpct", "awc")],
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
png(paste0("outputs/v", version,"/pca_no_loadings.png"))
ggplot(scores, aes(x = Comp.1, y = Comp.2, color = PLS)) +geom_point()+
  scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +theme_bw()
dev.off()

data <- data.frame(obsnames=row.names(PC$scores[1]), PC$Comp.1)

# using biplot
library(ggbiplot)
g <- ggbiplot(dens.pca, obs.scale = 1, var.scale = 1, labels.size
= 20,alpha = 0)
# layer the points from pls underneath the pca biplot
# using a clever trick to manipulate the layers
g$layers <- c(geom_point(data = scores, aes(x = Comp.1, y = Comp.2, color = PLS)), g$layers)
g <- g + scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +theme_bw(base_size = 15) 

#write to png
png(width = 800, height = 400,"outputs/v1.6/pca_biplot.png")
g + ggtitle('PCA biplot with PLS tree density')
dev.off()

#now plot biplot with the classification colors
g2 <- ggbiplot(dens.pca, obs.scale = 1, var.scale = 1, labels.size
              = 20,alpha = 0)
# layer the points from pls underneath the pca biplot
# using a clever trick to manipulate the layers
g2$layers <- c(geom_point(data = scores, aes(x = Comp.1, y = Comp.2, color = ecotype)), g2$layers)

png(width = 800, height = 400,"outputs/v1.6/pca_biplot_class.png")
g2 + ggtitle('PCA biplot with Rheumtella density classification')
dev.off()

# add the scores from pca to the dens.pr data frame
#this merge is not working
test1 <- merge(dens.pr, unique(dens.rm[,c('x','y','cell', 'PC1', 'PC2')]),  by = c('x','y','cell'), all.x = T)

#convert dens.rm to the new dens.pr---we only lose ~150 grid cells
dens.pr <- test1
write.csv(dens.pr, "data/dens_pr_PLS_FIA_with_cov.csv")

##################################################################
# PCA on FIA dataset
##################################################################
dens_fia <- dens.rm[, c('FIAdensity')] # pls density

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
dens.fia <- princomp(scale.dens[,c("MAP2011", "moderndeltaP", 
                                   "modtmean", "moddeltaT", 
                                   "sandpct", "awc")],
                     na.rm=TRUE) 

plot(dens.fia)
dens.fia$loadings
scores <- data.frame(dens.fia$scores[,1:2])
scores$FIA <- dens_fia
scores$ecotype <- dens.rm$fiaecotype

dens.rm$PC1fia <- scores[,1]
dens.rm$PC2fia <- scores[,2]
dens.rm <- data.frame(dens.rm)
PC <- dens.pca
#plot scores by tree density in trees per hectare
png(paste0("outputs/v", version,"/fia_pca_no_loadings.png"))
ggplot(scores, aes(x = Comp.1, y = Comp.2, color = FIA)) +geom_point()+
  scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +theme_bw()
dev.off()

data <- data.frame(obsnames=row.names(PC$scores[1]), PC$Comp.1)

# using biplot
library(ggbiplot)
g <- ggbiplot(dens.pca, obs.scale = 1, var.scale = 1, labels.size
              = 20,alpha = 0)
# layer the points from pls underneath the pca biplot
# using a clever trick to manipulate the layers
g$layers <- c(geom_point(data = scores, aes(x = Comp.1, y = Comp.2, color = FIA)), g$layers)
g <- g + scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +theme_bw(base_size = 15) 

#write to png
png(width = 800, height = 400,"outputs/v1.6/pca_fia_biplot.png")
g + ggtitle('PCA biplot with PLS tree density')
dev.off()



# add the scores from pca to the dens.pr data frame
#this merge is not working
test1 <- merge(dens.pr, unique(dens.rm[,c('x','y','cell', 'PC1fia', 'PC2fia')]),  by = c('x','y','cell'), all.x = T)

#convert dens.rm to the new dens.pr---we only lose ~150 grid cells
dens.pr <- test1
write.csv(dens.pr, "data/dens_pr_PLS_FIA_with_cov.csv")
#########################################################
# PCA predictions for the different RCP scenarios
ccesm <- read.csv("outputs/CCSM4pr_t_2070_full.csv")
dens.pr <- merge(dens.pr, ccesm, by = c("x", "y"))


# predict PCA from modern climate with the diffrent projections:

res<-princomp(scale.dens[,c("MAP2011", "moderndeltaP", 
                            "modtmean", "moddeltaT", 
                            "sandpct", "awc")])

#created a function to predict the PC scores for the different RCP's using the PCA from PLS
predict.PCA<- function(rcp){
  cc <- scale(dens.pr[,c(paste0("pr.",rcp), 
                         paste0("pr.",rcp,"SI"), 
                         paste0("tn.",rcp),
                         paste0("tn.",rcp, "cv"),
                         "sandpct","awc")])
  colnames(cc) <- c("MAP2011", "moderndeltaP", 
                    "modtmean", "moddeltaT", 
                    "sandpct", "awc")
  
  newscores <- predict(res,newdata=cc) # predict new scores based on the prevous 
  
  dens.pr[,paste0('PC1_cc',rcp)] <- newscores[,1]
  dens.pr[,paste0('PC2_cc',rcp)]  <- newscores[,2]
  dens.pr
}

dens.pr <- predict.PCA("26")
dens.pr <- predict.PCA("45")
dens.pr <- predict.PCA("85")



###############################################################
# Histogram plots
##############################################################

library(ggExtra)
library(ggplot2)

png(paste0('outputs/v',version,'/PLS_precip_hist_prism.png'))
#X11(width = 5)
p <- ggplot(dens.pr, aes(MAP1910, PLSdensity)) + geom_point() + theme_classic() + xlab('Mean Annual Precipitation (mm)') + ylab('Pre-Settlement \n Tree Density \n (Trees/hectare)')+
  xlim(450, 1200) + ylim(0, 800)+theme_bw()+
  theme(text = element_text(size = 20))
ggExtra::ggMarginal(p, type = "histogram",size = 3, colour = 'black', fill = 'red')
dev.off()

png(paste0('outputs/v',version,'/FIA_precip_hist_prism.png'))
p <- ggplot(dens.pr, aes(MAP2011, FIAdensity)) + geom_point() + theme_classic()+ xlab('Mean Annual Precipitation (mm)') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  xlim(450, 1200) + ylim(0, 800)+theme_bw()+theme(text = element_text(size = 20))
ggExtra::ggMarginal(p, type = "histogram", size = 3, colour = 'black', fill = "#0072B2")

dev.off()

#make plots for precipitation seasonality
png(paste0('outputs/v',version,'/PLS_delta_precip_hist_prism.png'))
#X11(width = 5)
p <- ggplot(dens.pr, aes(pastdeltaP, PLSdensity)) + geom_point() + theme_classic() + xlab('Precipitation seasonality') + ylab('Pre-Settlement \n Tree Density \n (Trees/hectare)')+
  xlim(0,1) + ylim(0, 800)+theme_bw()+
  theme(text = element_text(size = 20))
ggExtra::ggMarginal(p, type = "histogram",size = 3, colour = 'black', fill = 'red')
dev.off()

png(paste0('outputs/v',version,'/FIA_delta_precip_hist_prism.png'))
p <- ggplot(dens.pr, aes(moderndeltaP, FIAdensity)) + geom_point() + theme_classic()+ xlab('Precipitation seasonality') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  xlim(0,1) + ylim(0, 800)+theme_bw()+theme(text = element_text(size = 20))
ggExtra::ggMarginal(p, type = "histogram", size = 3, colour = 'black', fill = "#0072B2")

dev.off()

#plot out climate space for PLS:
png(paste0('outputs/v',version,'/precip_vs_temp_pls.png'))
ggplot(dens.pr, aes(x = MAP1910, y = pasttmean, colour = PLSdensity))+geom_point()+
  xlab('Past Mean Annual Prism Precipitation (mm/yr)') + ylab('Past Mean annual temperature (DegC)')+
  scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +theme_bw()
dev.off()

#plot out climate space for FIA:
png(paste0('outputs/v',version,'/precip_vs_temp_FIA.png'))
ggplot(dens.pr, aes(x = MAP2011, y = modtmean, colour = FIAdensity))+geom_point()+
  xlab('Modern Mean Annual Prism Precipitation (mm/yr)') + ylab('Modern Mean annual temperature (DegC)')+
  scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +theme_bw()
dev.off()

# plot out climate space in terms of Precip & Precip SI
png(paste0('outputs/v',version,'/precip_vs_deltap_pls.png'))
ggplot(dens.pr, aes(x = MAP1910, y = pastdeltaP, colour = PLSdensity))+geom_point()+
  xlab('Past Mean Annual Prism Precipitation (mm/yr)') + ylab('Past Precipitation Seasonality Index')+
  scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +theme_bw()
dev.off()

#plot climate space for modern and FIA side by side:
png(width = 800, height = 400, paste0('outputs/v',version,'/precip_vs_temp_FIA_PLS.png'))
pushViewport(viewport(layout = grid.layout(1, 2)))
print(ggplot(dens.pr, aes(x = MAP1910, y = pasttmean, colour = PLSdensity))+geom_point()+
        xlab('Past Mean Annual Prism Precipitation (mm/yr)') + ylab('Past Mean annual temperature (DegC)')+ggtitle('Past climate space')+
        scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +theme_bw()
      , vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(ggplot(dens.pr, aes(x = MAP2011, y = modtmean, colour = FIAdensity))+geom_point()+
        xlab('Modern Mean Annual Prism Precipitation (mm/yr)') + ylab('Modern Mean annual temperature (DegC)')+ggtitle('Modern climate space')+
        scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +theme_bw(), 
      , vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()



sandfia <- ggplot(dens.pr, aes(sandpct, FIAdensity)) + geom_point() + theme_classic()+ xlab('% sand 1-30cm') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  theme_bw()+theme(text = element_text(size = 20))
awcfia <- ggplot(dens.pr, aes(awc, FIAdensity)) + geom_point() + theme_classic()+ xlab('awc 1-30cm') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  theme_bw()+theme(text = element_text(size = 20))
ksatfia <- ggplot(dens.pr, aes(ksat, FIAdensity)) + geom_point() + theme_classic()+ xlab('ksat 1-30cm') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  theme_bw()+theme(text = element_text(size = 20))
png(paste0('outputs/v',version,'/FIA_sand.png'))
sandfia
dev.off()
png(paste0('outputs/v',version,'/FIA_awc.png'))
awcfia
dev.off()
png(paste0('outputs/v',version,'/FIA_ksat.png'))
ksatfia
dev.off()
sandpls <- ggplot(dens.pr, aes(sandpct, PLSdensity)) + geom_point() + theme_classic()+ xlab('% sand 1-30cm') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  theme_bw()+theme(text = element_text(size = 20))
awcpls <- ggplot(dens.pr, aes(awc, PLSdensity)) + geom_point() + theme_classic()+ xlab('AWC 1-30cm') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  theme_bw()+theme(text = element_text(size = 20))
ksatpls <- ggplot(dens.pr, aes(ksat, PLSdensity)) + geom_point() + theme_classic()+ xlab('ksat 1-30cm') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  theme_bw()+theme(text = element_text(size = 20))

png(paste0('outputs/v',version,'/PLS_sand.png'))
sandpls
dev.off()
png(paste0('outputs/v',version,'/PLS_awc.png'))
awcpls
dev.off()
png(paste0('outputs/v',version,'/PLS_ksat.png'))
ksatpls
dev.off()

#print sand characteristics side by side
png(width = 800, height = 400, paste0('outputs/v',version,'/sand_vs_dens_FIA_PLS.png'))
pushViewport(viewport(layout = grid.layout(1, 2)))
print(ggplot(dens.pr, aes(sandpct,FIAdensity)) +geom_hex()+ylim(0,600)+ xlim(0,100) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits = c(1,130))+
        xlab(' % sand') +ylab(" Modern Tree Density (stems/ha)") +ggtitle('Modern')+ theme_bw(base_size = 15)
      , vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(ggplot(dens.pr, aes(sandpct, PLSdensity)) +geom_hex()+ylim(0,600)+ xlim(0,100) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits = c(1,130))+
        xlab(' % sand') +ylab(" Modern Tree Density (stems/ha)")+ggtitle('Past') + theme_bw(base_size = 15)
      , vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
dev.off()

#print awc characteristics side by side
png(width = 800, height = 400, paste0('outputs/v',version,'/awc_vs_dens_FIA_PLS.png'))
pushViewport(viewport(layout = grid.layout(1, 2)))
print(ggplot(dens.pr, aes(awc,FIAdensity)) +geom_hex()+ylim(0,600)+ xlim(0,0.25) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits = c(1,130))+
        xlab(' AWC') +ylab(" Modern Tree Density (stems/ha)") +ggtitle('Modern')+ theme_bw(base_size = 15)
      , vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(ggplot(dens.pr, aes(awc, PLSdensity)) +geom_hex()+ylim(0,600)+ xlim(0,0.25) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits = c(1,130))+
        xlab(' AWC') +ylab(" Modern Tree Density (stems/ha)")+ggtitle('Past') + theme_bw(base_size = 15)
      , vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
dev.off()

#print ksat characteristics side by side
png(width = 800, height = 400, paste0('outputs/v',version,'/ksat_vs_dens_FIA_PLS.png'))
pushViewport(viewport(layout = grid.layout(1, 2)))
print(ggplot(dens.pr, aes(ksat,FIAdensity)) +geom_hex()+ylim(0,600)+ xlim(0,250) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits = c(1,130))+
        xlab('ksat') +ylab(" Modern Tree Density (stems/ha)") +ggtitle('Modern')+ theme_bw(base_size = 15)
      , vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(ggplot(dens.pr, aes(ksat, PLSdensity)) +geom_hex()+ylim(0,600)+ xlim(0,250) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits = c(1,130))+
        xlab('ksat') +ylab(" Modern Tree Density (stems/ha)")+ggtitle('Past') + theme_bw(base_size = 15)
      , vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
dev.off()



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

plot(dens.pr$PLSdensity, dens.pr$diff, xlab='PLS tree density (trees/ha)', xlim= c(0,600),ylim = c(-1000, 1000),ylab='increase in density since PLS (trees/ha)', pch=19, cex=.4)
contour(z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE)
abline(a = 0, b = 0, col = 'red')
legend("topleft", paste("R=", round(cor(dens.pr$PLSdensity, dens.pr$diff),2)), bty="n")



library(hexbin)
#### 
#plot denisity histograms binned by precipitation amount
#100mm precipitation bins
dens.pr$plsprbins <- cut(dens.pr$MAP1910, #labels = c('350-400mm', '400-450mm', '450-500mm', '550-600mm', '600-650mm','650-700mm','700-750mm','750-800mm','800-850mm',  '850-900mm','900-950mm','950-1000mm','1000-1050mm','1050-1100mm', '1100-1150mm','1150-1200mm', '1200-1250mm', '1250-1300mm'),
                         breaks=c(200,250,300,400,500,600, 700,800,900, 1000,1100,1200, 1400))
dens.pr$fiaprbins <- cut(dens.pr$MAP2011, #labels = c('350-400mm', '500-650mm', '650-700mm', '700-850mm', '850-1000mm', '1000-1150mm', '1150-1300mm'),
                         breaks=c( 200,250,300,400,500,600, 700,800,900, 1000,1100,1200, 1400))

# create labeling function that takes the beginning of var range, end of var range, and the value to split by:
label.breaks <- function(beg, end, splitby){
labels.test <- data.frame(first = seq(beg, end, by = splitby), second = seq((beg + splitby), (end + splitby), by = splitby))
labels.test <- paste (labels.test$first, '-' , labels.test$second)
labels.test
}

dens.pr$plsprbins50 <- cut(dens.pr$MAP1910, breaks = seq(250, 1350, by = 50), labels = label.breaks(250, 1300, 50))
dens.pr$fiaprbins50 <- cut(dens.pr$MAP2011, breaks = seq(250, 1350, by = 50), labels = label.breaks(250, 1300, 50))
dens.pr$plsprbins100 <- cut(dens.pr$MAP1910, breaks = seq(250, 1350, by = 100), labels = label.breaks(250, 1250, 100))
dens.pr$fiaprbins100 <- cut(dens.pr$MAP2011, breaks = seq(250, 1350, by = 100), labels = label.breaks(250, 1250,  100))
dens.pr$plsprbins75 <- cut(dens.pr$MAP1910, breaks = seq(250, 1350, by = 75), labels = label.breaks(250, 1275, 75))
dens.pr$fiaprbins75 <- cut(dens.pr$MAP2011, breaks = seq(250, 1350, by = 75), labels = label.breaks(250, 1275, 75))
dens.pr$plsprbins150 <- cut(dens.pr$MAP1910, breaks = seq(250, 1350, by = 150), labels = label.breaks(250, 1250, 150))
dens.pr$fiaprbins150 <- cut(dens.pr$MAP2011, breaks = seq(250, 1350, by = 150), labels = label.breaks(250, 1250, 150))
dens.pr$plsprbins25 <- cut(dens.pr$MAP1910, breaks = seq(250, 1350, by = 25), labels = label.breaks(250, 1325,  25))

dens.pr$fiaprbins25 <- cut(dens.pr$MAP2011, breaks = seq(250, 1350, by = 25), labels = label.breaks(250, 1325,  25))

dens.pr$sandbins <- cut(dens.pr$sandpct, breaks = seq(0, 100, by = 10), labels = label.breaks(0,90, 10))
dens.pr$ksatbins <- cut(dens.pr$ksat, breaks = seq(0,300, by = 10), labels = label.breaks(0,290, 10))
dens.pr$moddeltPbins <- cut(dens.pr$moderndeltaP, breaks = seq(0,1, by = .10), labels = label.breaks(0,0.9, 0.1))
dens.pr$pastdeltPbins <- cut(dens.pr$pastdeltaP, breaks = seq(0,1, by = .10), labels = label.breaks(0,0.9, 0.1))
dens.pr$moddeltTbins <- cut(dens.pr$moddeltaT, breaks = seq(0,7.5, by = .5), labels = label.breaks(0,7.0, 0.5))
dens.pr$pastdeltTbins <- cut(dens.pr$deltaT, breaks = seq(0,7.5, by = .5), labels = label.breaks(0,7.0, 0.5))

dens.pr$pasttmeanbins <- cut(dens.pr$pasttmean, breaks = seq(0,15, by = 1.5), labels = label.breaks(0,14, 1.5))
dens.pr$modtmeanbins <- cut(dens.pr$modtmean, breaks = seq(0,15, by = 1.5), labels = label.breaks(0,14, 1.5))
dens.pr$PC1bins <- cut(dens.pr$PC1, breaks = seq(-5,6, by = 1), labels = label.breaks(-5,5, 1))
dens.pr$PC2bins <- cut(dens.pr$PC2, breaks = seq(-4,3, by = 0.5), labels = label.breaks(-4,2.5, 0.5))
dens.pr$PC1fiabins <- cut(dens.pr$PC1fia, breaks = seq(-5,6, by = 1), labels = label.breaks(-5,5, 1))
dens.pr$PC2fiabins <- cut(dens.pr$PC2fia, breaks = seq(-4,3, by = 0.5), labels = label.breaks(-4,2.5, 0.5))
dens.pr$PC1_cc26fbins <- cut(dens.pr$PC1_cc26, breaks = seq(-5,6, by = 1), labels = label.breaks(-5,5, 1))
dens.pr$PC2_cc26fbins <- cut(dens.pr$PC2_cc26, breaks = seq(-3,4, by = 0.5), labels = label.breaks(-3,3.5, 0.5))
dens.pr$PC1_cc45fbins <- cut(dens.pr$PC1_cc45, breaks = seq(-5,6, by = 1), labels = label.breaks(-5,5, 1))
dens.pr$PC2_cc45fbins <- cut(dens.pr$PC2_cc45, breaks = seq(-3,4, by = 0.5), labels = label.breaks(-3,3.5, 0.5))
dens.pr$PC1_cc85fbins <- cut(dens.pr$PC1_cc85, breaks = seq(-5,6, by = 1), labels = label.breaks(-5,5, 1))
dens.pr$PC2_cc85fbins <- cut(dens.pr$PC2_cc85, breaks = seq(-3,4, by = 0.5), labels = label.breaks(-3,3.5, 0.5))


test<- dens.pr[!is.na(dens.pr),]
melted <- melt(test, id.vars = c("x", 'y', 'cell', 'plsprbins', 'fiaprbins', 'plsprbins50', 'fiaprbins50','plsprbins75', 'fiaprbins75',
                                 'plsprbins100', 'fiaprbins100','plsprbins150', 'fiaprbins150','plsprbins25', 'fiaprbins25',
                                 'MAP1910', "MAP2011", 
                                 'diff', 'sandpct', 'awc', 'ksat', 'sandbins', 'ksatbins', 'moderndeltaP', 
                                 'pastdeltaP','deltaT',"pastdeltTbins","moddeltTbins", 'moddeltPbins', 'pastdeltPbins', 'pasttmeanbins','pasttmean','modtmean','modtmeanbins', "PC1", "PC2",'PC1bins', 'PC2bins','PC1fiabins', 'PC2fiabins', 'ecotype', 'fiaecotype')) 

#map out 
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)

#pdf("outputs/binned_histograms_pr_AGU_12_6_16_large_bins.pdf")
png(paste0('outputs/v',version,'/PLS_density_histogrom.png'))#
ggplot(dens.pr, aes(PLSdensity)) +geom_histogram(fill= "#D55E00",color = "black") +xlim(0, 700)+ xlab("PLS tree density (stems/ha)")+ ylab('# grid cells')+ 
  theme_bw(base_size = 25)#+ facet_wrap(~plsprbins)
dev.off()

png(paste0('outputs/v',version,'/FIA_density_histogram.png'))#,
ggplot(dens.pr, aes(FIAdensity)) +geom_histogram(binwidth = 30,fill ="#0072B2",  color = 'black') +xlim(0, 700)+xlab('Modern Tree density (stems/ha)')+ylab("# grid cells")+
  theme_bw(base_size = 25)#+ facet_wrap(~fiaprbins)
dev.off()

#plot histograms side by side
png(height=400, width=800, filename="outputs/FIA_PLS_hists.png", type="cairo")
pushViewport(viewport(layout = grid.layout(1, 2)))
print(ggplot(dens.pr, aes(PLSdensity)) +geom_histogram(fill= "#D55E00",color = "black") +xlim(0, 700)+ xlab("PLS tree density (stems/ha)")+ ylab('# grid cells')+ 
  theme_bw(base_size = 20), vp = viewport(layout.pos.row = 1, layout.pos.col = 1))#+ facet_wrap(~plsprbins)
print(ggplot(dens.pr, aes(FIAdensity)) +geom_histogram(binwidth = 30,fill ="#0072B2",  color = 'black') +xlim(0, 700)+xlab('Modern Tree density (stems/ha)')+ylab("# grid cells")+
  theme_bw(base_size = 20),vp = viewport(layout.pos.row = 1, layout.pos.col = 2))#+ facet_wrap(~fiaprbins)

dev.off()



#plot precipitaiton hexbins & write to a png file:
png(height=400, width=800, filename="outputs/FIA_PLS_hexbinplots.png", type="cairo")
pushViewport(viewport(layout = grid.layout(1, 2)))

print(ggplot(dens.pr, aes(MAP2011,FIAdensity)) +geom_hex()+ylim(0,600)+ xlim(400,1400) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits = c(1,130))+
  xlab(' Mean Annual Precipitation (mm) \n PRISM') +ylab(" Modern Tree Density (stems/ha)") ,  vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

print(ggplot(dens.pr, aes(MAP1910, PLSdensity)) +geom_hex()+ylim(0,600)+ xlim(400,1400) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits = c(1,130))+
  xlab(' Mean Annual Precipitation (mm) \n PRISM 1900-1910') +ylab(" Past Tree Density (stems/ha)"),  vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
dev.off()

# now write both hexbin plots for temperature to a file
png(height=400, width=800, filename="outputs/FIA_PLS_temp_hexbinplots.png", type="cairo")
pushViewport(viewport(layout = grid.layout(1, 2)))

print(ggplot(dens.pr, aes(modtmean,FIAdensity)) +geom_hex()+ylim(0,600)+ xlim(0,15) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits=c(0,90))+
        xlab(' Mean Annual Temperature (degC) \n PRISM') +ylab(" Modern Tree Density (stems/ha)") ,  vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

print(ggplot(dens.pr, aes(pasttmean, PLSdensity)) +geom_hex()+ylim(0,600)+ xlim(0,15) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits=c(0,90))+
        xlab(' Mean Annual Temperature (degC)\n PRISM 1900-1910') +ylab(" Past Tree Density (stems/ha)") ,  vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
dev.off()

#make hexbin plots for PC1
png(height=400, width=800, filename="outputs/v1.6/FIA_PLS_PC1_hexbinplots.png", type="cairo")
pushViewport(viewport(layout = grid.layout(1, 2)))

print(ggplot(dens.pr, aes(PC1,FIAdensity)) +geom_hex()+ylim(0,600)+ xlim(-9,5) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits=c(0,90))+
        xlab(' Principal component 1') +ylab(" Modern Tree Density (stems/ha)") ,  vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

print(ggplot(dens.pr, aes(PC1, PLSdensity)) +geom_hex()+ylim(0,600)+ xlim(-9,5) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits=c(0,90))+
        xlab(' Principal component 1') +ylab(" Past Tree Density (stems/ha)") ,  vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
dev.off()

#make hexbin plots for PC2
png(height=400, width=800, filename="outputs/v1.6/FIA_PLS_PC2_hexbinplots.png", type="cairo")
pushViewport(viewport(layout = grid.layout(1, 2)))

print(ggplot(dens.pr, aes(PC2,FIAdensity)) +geom_hex()+ylim(0,600)+ xlim(-4,3) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits=c(0,90))+
        xlab(' Principal component 2') +ylab(" Modern Tree Density (stems/ha)") ,  vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

print(ggplot(dens.pr, aes(PC2, PLSdensity)) +geom_hex()+ylim(0,600)+ xlim(-4,3) + theme_bw(base_size = 20)+scale_fill_distiller(palette = "Spectral", limits=c(0,90))+
        xlab(' Principal component 2') +ylab(" Past Tree Density (stems/ha)") ,  vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
dev.off()

#make individual hexbin plots
png(paste0('outputs/v',version,'/fia_precipitation_hexbin.png'))
ggplot(dens.pr, aes(MAP2011,FIAdensity))+geom_bin2d(bins = 75) +ylim(0,600)+ xlim(400,1400)+
  scale_fill_gradient(low='blue', high='black')+theme_bw(base_size = 20)+
  xlab(' Mean Annual Precipitation (mm) \n PRISM 1900-1910') +ylab(" Modern Tree Density (stems/ha)")
dev.off()


#hbin <- hexbin(dens.pr$MAP1910, dens.pr$PLSdensity, xbins = 100)
#plot(hbin)


png(paste0('outputs/v',version,'/PLS_precipitation_hexbin.png'))
ggplot(dens.pr, aes(MAP1910,PLSdensity))+geom_bin2d(bins = 75) +ylim(0,600) + xlim(400, 1400)+
 scale_fill_gradient(low='red', high='black')+theme_bw(base_size = 20)+
  xlab('Mean Annual Precipitation (mm) \n PRISM 1900-1910') + ylab("PLS Tree Density (stems/ha)")
dev.off()

png(paste0('outputs/v',version,'/PLS_tmean_hexbin.png'))
ggplot(dens.pr, aes(pasttmean,PLSdensity))+geom_bin2d(bins = 75) +ylim(0,600) + xlim(0,20)+
  scale_fill_gradient(low='red', high='black')+theme_bw(base_size = 20)+
  xlab('Mean Annual Temperature (degC)') + ylab("PLS Tree Density (stems/ha)")
dev.off()


rbpalette <- c('red', "blue")
#ggplot(melted, aes(value, fill = variable)) +geom_density(alpha = 0.3)  +xlim(0, 400)+ facet_grid(plsprbins~., scales = 'free_y')+scale_fill_brewer(palette = "Set1")


######################################################
# plot out density distributions binned by climate
######################################################

png(width = 600, height = 600, paste0('outputs/v',version,'/precipitation_by_bins_100.png'))
ggplot(na.omit(melted), aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)  +xlim(0, 400)+ facet_wrap(~plsprbins100, scales = 'free_y')+
 scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()
#ggplot(melted, aes(value, fill = variable)) +geom_histogram(binwidth = 35, alpha = 0.3)  +xlim(0, 600)+ facet_wrap(~plsprbins)+scale_fill_brewer(palette = "Set1")

png(width = 600, height = 600, paste0('outputs/v',version,'/PC1_by_bins.png'))
ggplot(na.omit(melted), aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)  +xlim(0, 400)+ facet_wrap(~PC1bins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()

png(width = 600, height = 600, paste0('outputs/v',version,'/PC2_by_bins.png'))
ggplot(na.omit(melted), aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)  +xlim(0, 400)+ facet_wrap(~PC2bins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()

#plot by sandiness
png(paste0('outputs/v',version,'/sand_by_bins.png'))
ggplot(melted, aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)  +xlim(0, 400)+ facet_wrap(~sandbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()

#plot by tmean
png(width = 600, height = 600,paste0('outputs/v',version,'/tmean_by_bins.png'))
ggplot(na.omit(melted), aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)+ xlim(0, 400) + facet_wrap(~pasttmeanbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()


#plot by ksat
png(paste0('outputs/v',version,'/ksat_by_bins.png'))
ggplot(melted, aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)  +xlim(0, 400)+ facet_wrap(~ksatbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()

#plot by past deltaPbins 
png(paste0('outputs/v',version,'/pastDeltaP_by_bins.png'))
ggplot(melted, aes(value, colour = variable))+ geom_density(size = 2, alpha = 0.1) +xlim(0, 400)+ facet_wrap(~pastdeltPbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()

#plot by mod deltaPbins
png(paste0('outputs/v',version,'/moddeltaP_by_bins.png'))
ggplot(melted, aes(value, colour = variable))+ geom_density(size = 2, alpha = 0.1) +xlim(0, 400)+ facet_wrap(~moddeltPbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree density')
dev.off()

#calculate bimodality coefficients
library(modes)

# this function uses the bimodality_coefficient funcition in the modes library to calculate the 
#bimodality coefficient of the density (FIA or PLS) within a given set of bins (climate, sand, etc)

calc.BC <- function(data, binby, density){
bins <- as.character(unique(data[,binby]))
coeffs <- matrix(NA, length(bins), 2)
for (i in 1:length(bins)){
coeffs[i,1]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p
}

coef.bins<- data.frame(cbind(coeffs, bins))
coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
#coef.bins
coef.bins <- coef.bins[order(as.numeric(as.character(coef.bins$bins))),]
coef.bins$bins <- factor(coef.bins$bins, levels = coef.bins$bins[order(as.numeric(as.character(coef.bins$bins)))])# reorder so it plots well
ggplot(coef.bins, aes(x = bins, y = BC))+geom_point()+
 geom_hline( yintercept = 5/9)+ylim(0,1)+theme_bw()+
  theme(axis.text = element_text(angle = 90))+
  xlab('bins') + ylab('Bimodality Coefficient')+
  ggtitle(paste0('Bimodality coefficients for ', binby,' ', density))
}

pdf(paste0('outputs/v',version,'/bimodality_coefficient_binplots.pdf'))
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
calc.BC(data = dens.pr, binby = 'pasttmeanbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'PC1bins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'PC1fiabins', density = "FIAdensity")
calc.BC(data = dens.pr, binby = 'PC2bins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'PC2fiabins', density = "FIAdensity")


dev.off()

#make a png with PC1 and PC2 for PLS and FIA
png(height = 400, width = 800, paste0('outputs/v',version,'/PLS_FIA_PC1_PC2_BC_bins.png'))
pushViewport(viewport(layout = grid.layout(2, 2)))
print(calc.BC(data = dens.pr, binby = 'PC1bins', density = "PLSdensity")+ ggtitle('BC for PC1 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(calc.BC(data = dens.pr, binby = 'PC1bins', density = "FIAdensity") + ggtitle('BC for PC1 FIA'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(calc.BC(data = dens.pr, binby = 'PC2bins', density = "PLSdensity")+ ggtitle('BC for PC2  PLS'),   vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(calc.BC(data = dens.pr, binby = 'PC2bins', density = "FIAdensity") + ggtitle('BC for PC2 FIA'),   vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
dev.off()

calc.BC(data = dens.pr, binby = 'PC1bins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'PC1bins', density = "FIAdensity")
calc.BC(data = dens.pr, binby = 'PC2bins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'PC2bins', density = "FIAdensity")




#this function maps out the region that is bimodal 
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
  merged[merged$BC >= 0.5 & merged$dipP <= 0.05,]$bimodal <- "Bimodal"
  
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
pdf(paste0('outputs/v',version,'/bimodal_maps.pdf'))
map.bimodal(data = dens.pr, binby = 'plsprbins', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'fiaprbins', density = "FIAdensity")
map.bimodal(data = dens.pr, binby = 'plsprbins100', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'fiaprbins100', density = "FIAdensity")
map.bimodal(data = dens.pr, binby = 'plsprbins75', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'fiaprbins75', density = "FIAdensity")
map.bimodal(data = dens.pr, binby = 'plsprbins25', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'fiaprbins25', density = "FIAdensity")
map.bimodal(data = dens.pr, binby = 'fiaprbins', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'sandbins', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'ksatbins', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'pastdeltPbins', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'pasttmeanbins', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'PC1bins', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'PC2bins', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'PC1fiabins', density = "FIAdensity")
map.bimodal(data = dens.pr, binby = 'PC2fiabins', density = "FIAdensity")

dev.off()

png(height = 400, width = 800, paste0('outputs/v',version,'/PLS_PC1_PC2_map.png'))
pushViewport(viewport(layout = grid.layout(1, 2)))
print(map.bimodal(data = dens.pr, binby = 'PC1bins', density = "PLSdensity")+ ggtitle('Bimodal Regions for PC1 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal(data = dens.pr, binby = 'PC2bins', density = "PLSdensity") + ggtitle('Bimodal Regions for PC2 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()


png(height = 400, width = 800, paste0('outputs/v',version,'/FIA_PC1_PC2_map.png'))
pushViewport(viewport(layout = grid.layout(1, 2)))
print(map.bimodal(data = dens.pr, binby = 'PC1bins', density = "FIAdensity")+ ggtitle('Bimodal Regions for PC1 FIA'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal(data = dens.pr, binby = 'PC2bins', density = "FIAdensity") + ggtitle('Bimodal Regions for PC2 FIA'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

png(height = 400, width = 800, paste0('outputs/v',version,'/PLS_FIA_precip_BC_map.png'))
pushViewport(viewport(layout = grid.layout(1, 2)))
print(map.bimodal(data = dens.pr, binby = 'plsprbins', density = "PLSdensity")+ ggtitle('Bimodal Regions for precip PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal(data = dens.pr, binby = 'fiaprbins', density = "FIAdensity") + ggtitle('Bimodal Regions for precip FIA'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()


png(paste0('outputs/v',version,'/PLS_BC_map_100.png'))
map.bimodal(data = dens.pr, binby = 'plsprbins100', density = "PLSdensity")
dev.off()
png(paste0('outputs/v',version,'/FIA_BC_map_100.png'))
map.bimodal(data = dens.pr, binby = 'fiaprbins100', density = "FIAdensity")
dev.off()

png(paste0('outputs/v',version,'/PLS_BC_map_75.png'))
map.bimodal(data = dens.pr, binby = 'plsprbins75', density = "PLSdensity")
dev.off()

png(paste0('outputs/v',version,'/FIA_BC_map_75.png'))
map.bimodal(data = dens.pr, binby = 'fiaprbins75', density = "FIAdensity")
dev.off()
png(paste0('outputs/v',version,'/PLS_BC_map_25.png'))
map.bimodal(data = dens.pr, binby = 'plsprbins25', density = "PLSdensity")
dev.off()
png(paste0('outputs/v',version,'/FIA_BC_map_25.png'))
map.bimodal(data = dens.pr, binby = 'fiaprbins25', density = "FIAdensity")
dev.off()


#make alternative maps that only plot prairie as one type of prairie:
map.bimodal.5c <- function(data, binby, density){
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
  if(merged$BC >= 0.5 & merged$dipP <=0.05){
  merged$bimodal <- "Bimodal"
  }else{
    merged$bimodal <- "Stable"
  }
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
      'black'),  limits = c('Bimodal Forest',"Stable Forest" ,   "Bimodal Savanna", 'Stable Savanna','Prairie')  )+
    theme_bw()+
    xlab("easting") + ylab("northing") +coord_equal()+
    ggtitle(paste0(binby, ' for ',density))
  
}

pdf(paste0('outputs/v',version,'/full/bimodal_maps_5col.pdf'))
map.bimodal.5c(data = dens.pr, binby = 'plsprbins50', density = "PLSdensity")
map.bimodal.5c(data = dens.pr, binby = 'fiaprbins', density = "FIAdensity")
map.bimodal.5c(data = dens.pr, binby = 'plsprbins100', density = "PLSdensity")
map.bimodal.5c(data = dens.pr, binby = 'fiaprbins100', density = "FIAdensity")
map.bimodal.5c(data = dens.pr, binby = 'plsprbins75', density = "PLSdensity")
map.bimodal.5c(data = dens.pr, binby = 'fiaprbins75', density = "FIAdensity")
map.bimodal.5c(data = dens.pr, binby = 'plsprbins25', density = "PLSdensity")
map.bimodal.5c(data = dens.pr, binby = 'fiaprbins25', density = "FIAdensity")
map.bimodal.5c(data = dens.pr, binby = 'fiaprbins', density = "PLSdensity")
map.bimodal.5c(data = dens.pr, binby = 'sandbins', density = "PLSdensity")
map.bimodal.5c(data = dens.pr, binby = 'ksatbins', density = "PLSdensity")
map.bimodal.5c(data = dens.pr, binby = 'pastdeltPbins', density = "PLSdensity")
dev.off()

write.csv(dens.pr, "outputs/v1.6-5/dens_pr_FIA_PLS_df.csv")


png(height = 6, width = 10, units= 'in',  res= 300, paste0('outputs/v',version,'/FIA_PC1_PC2_map_5col.png'))
pushViewport(viewport(layout = grid.layout(1, 2)))
print(map.bimodal.5c(data = dens.pr, binby = 'PC1fiabins', density = "FIAdensity")+ ggtitle(' PC1 FIA'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal.5c(data = dens.pr, binby = 'PC2bins', density = "FIAdensity") + ggtitle('PC2 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

png(height = 6, width = 5, units= 'in',  res= 300, paste0('outputs/v',version,'/FIA_PC1_map_5col.png'))
map.bimodal.5c(data = dens.pr, binby = 'PC1fiabins', density = "FIAdensity")+ ggtitle(' PC1 FIA')
dev.off()


##########################################
# Function to map out future climate
#########################################

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
  #library(plyr)
  coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
  colnames(coef.new) <- c("low", "high")
  coef.bins <- cbind(coef.bins, coef.new)
  
  #merge bins iwth the second binby -> here is is future climate
  merged <- merge(coef.bins, dens.pr, by.x = "bins", by.y = binby2)
  
  
  #define bimodality
  merged$bimodal <- "Stable"
  #criteria for bimodality
  #suppressWarnings(if(merged$BC >= 0.5 & merged$dipP <= 0.05){
   # merged$bimodal <- "Bimodal"
  #}else{
   # merged$bimodal <- "Stable"
  #})
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
  
  #merged
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = bimodal))+ scale_fill_manual(values = c(
      '#2c7bb6', # blue
      '#d7191c'
      # '#01665e', # light green
      #  '#5ab4ac', # dark teal
      # '#8c510a', # red
      #'#d8b365', # light tan
      #'#fee08b', # tan
      #'black'
    ), limits = c('Stable','Bimodal') )+
    theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank())+
    xlab("easting") + ylab("northing") +coord_equal() + ggtitle(binby2)
  
}

source("R/grid_arrange_shared_legend.R")

a <- bimodal.future(data = dens.pr, binby = 'PC1fiabins', density = "FIAdensity", binby2 ='PC1fiabins' ) + ggtitle ("FIA PC1")
b <- bimodal.future(data = dens.pr, binby = 'PC1_cc26fbins', density = "FIAdensity", binby2 ='PC1_cc26fbins' ) + ggtitle("RCP 2.6 PC1")
c <- bimodal.future(data = dens.pr, binby = 'PC1_cc45fbins', density = "FIAdensity", binby2 ='PC1_cc45fbins' )+ ggtitle("RCP 4.5 PC1")
d <- bimodal.future(data = dens.pr, binby = 'PC1_cc85fbins', density = "FIAdensity", binby2 ='PC1_cc85fbins' )+ ggtitle("RCP 8.5 PC1")

png(height = 5, width = 12, units = "in",res = 300, filename = paste0('outputs/v1.6-5/RCP_scenario_PC1_maps_FIA.png'))
grid_arrange_shared_legend(a,b,c,d, nrow = 1, ncol=4, position = c("bottom"))
dev.off()

#############################################
# print out df of future climates
##########################################
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
  #coef.new <- strsplit(as.character(coef.bins$bins), " - ")
  #library(plyr)
  #coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
  #colnames(coef.new) <- c("low", "high")
  #coef.bins <- cbind(coef.bins, coef.new)
  
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

df.new <- bimodal.df(data = dens.pr, binby = 'PC1fiabins', density = "FIAdensity", binby2 = 'PC1fiabins')
df.mod <- bimodal.df(data = dens.pr, binby = 'PC1fiabins', density = "FIAdensity", binby2 = 'PC1fiabins')
df.8.5 <- bimodal.df(data = dens.pr, binby = 'PC1_cc85fbins', density = "FIAdensity", binby2 = "PC1_cc85fbins")
df.4.5 <- bimodal.df(data = dens.pr, binby = 'PC1_cc45fbins', density = "FIAdensity", binby2 = "PC1_cc45fbins")
df.2.6 <- bimodal.df(data = dens.pr, binby = 'PC1_cc26fbins', density = "FIAdensity", binby2 = "PC1_cc26fbins")

# calculate the % that should be bimodal in the modern landscape
a <- nrow(df.mod[df.mod$bimodal == "Bimodal",])/nrow(df.mod)
b <- nrow(df.new[df.new$bimodal == "Bimodal",])/nrow(df.new)
c <- nrow(df.8.5[df.8.5$bimodal == "Bimodal",])/nrow(df.8.5)
d <- nrow(df.4.5[df.4.5$bimodal == "Bimodal",])/nrow(df.4.5)
e <- nrow(df.2.6[df.2.6$bimodal == "Bimodal",])/nrow(df.2.6)




find_modes(density(df.new[df.new$bimodal == "Bimodal" & df.new$FIAdensity <= 250, ]$FIAdensity)$y)

plot(density(df.new[df.new$bimodal == "Bimodal" & df.new$FIAdensity <= 250, ]$FIAdensity))

dip.test(density(df.new[df.new$bimodal == "Bimodal", ]$FIAdensity)$y)




#read in the data from PLS full density processing.R
dens.full <- read.csv("outputs/v1.6-5/full/dens_pr_dataframe_full.csv")

dens.full$ecotypecw <- 'test'
dens.full[dens.full$PLSdensity >= 100, ]$ecotypecw <-  "Forest"
dens.full[dens.full$PLSdensity < 100, ]$ecotypecw <-  "Savanna" 
dens.full[dens.full$PLSdensity < 10, ]$ecotypecw <-  "prairie"

dens.pr$fiaecotypecw <- 'test'
dens.pr[dens.pr$FIAdensity >= 100, ]$fiaecotypecw <-  "Forest"
dens.pr[dens.pr$FIAdensity < 100, ]$fiaecotypecw <-  "Savanna" 
dens.pr[dens.pr$FIAdensity < 10, ]$fiaecotypecw <-  "prairie"

#first lets plot out the ecotypes in FIA and PLS for rheumtella
ecotype.p <- ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
  geom_raster(data = dens.full, aes(x = x, y = y, fill = ecotype))+ scale_fill_manual(values = c(
    '#1b7837',
    '#b2df8a',
    '#d8b365',
    '#5ab4ac'), limits = c("Forest" , 'Savanna', 'prairie') )+
  theme_bw()+
  xlab("easting") + ylab("northing") +coord_equal()+ggtitle('PLS ecotypes')

ecotype.f <- ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
  geom_raster(data = dens.pr, aes(x = x, y = y, fill = fiaecotype))+ scale_fill_manual(values = c(
    '#1b7837',
    '#b2df8a',
    '#d8b365',
    '#5ab4ac'), limits = c("Forest" , 'Savanna', 'prairie') )+
  theme_bw()+
  xlab("easting") + ylab("northing") +coord_equal() +ggtitle('FIA ecotypes')


#make the same plots using chicago wilderness classification:

ecotypecw.p <- ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
  geom_raster(data = dens.full, aes(x = x, y = y, fill = ecotypecw))+ scale_fill_manual(values = c(
    '#1b7837',
    '#b2df8a',
    '#d8b365',
    '#5ab4ac'), limits = c("Forest" , 'Savanna', 'prairie') )+
  theme_bw()+
  xlab("easting") + ylab("northing") +coord_equal()+ggtitle('PLS ecotypes')

ecotypecw.f <- ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
  geom_raster(data = dens.pr, aes(x = x, y = y, fill = fiaecotypecw))+ scale_fill_manual(values = c(
    '#1b7837',
    '#b2df8a',
    '#d8b365',
    '#5ab4ac'), limits = c("Forest" , 'Savanna', 'prairie') )+
  theme_bw()+
  xlab("easting") + ylab("northing") +coord_equal() +ggtitle('FIA ecotypes')


library(grid)
library(gridExtra)
source('R/grid_arrange_shared_legend.R')
#plot the ecotypes side by side
png(width = 8, height = 4, units = 'in', res = 300, 'outputs/v1.6-5/PLS_FIA_ecotype_map.png')
grid_arrange_shared_legend(ecotype.p,ecotype.f,nrow = 1, ncol = 2 )
dev.off()

# plot the cw ecotypes side by side
png(width = 8, height = 4, units = 'in', res = 300, 'outputs/v1.6-5/PLS_FIA_ecotype_cw_map.png')
grid_arrange_shared_legend(ecotypecw.p,ecotypecw.f,nrow = 1, ncol = 2 )
dev.off()

#################################
# side by side PLS and FIAOutput of bimodality ~ ecotypes
#output full PLS density and limited FIA density plots
###################################
#for precipitation space
a <- map.bimodal.5c(data = dens.full, binby = 'plsprbins25', density = "PLSdensity")
b <- map.bimodal.5c(data = dens.pr, binby = 'fiaprbins25', density = "FIAdensity")

png(width = 8, height = 4, units = 'in', res = 300, 'outputs/v1.6-5/PLS_FIA_precip_25_BC_map.png')
grid_arrange_shared_legend(a,b,nrow = 1, ncol = 2 )
dev.off()

#for temperature space
a <- map.bimodal.5c(data = dens.full, binby = 'pasttmeanbins', density = "PLSdensity")
b <- map.bimodal.5c(data = dens.pr, binby = 'modtmeanbins', density = "FIAdensity")

png(width = 8, height = 4, units = 'in', res = 300, 'outputs/v1.6-5/PLS_FIA_temp_1.5_deg_BC_map.png')
grid_arrange_shared_legend(a,b,nrow = 1, ncol = 2 )
dev.off()

#do the same for PC1 (climate)
a <- map.bimodal.5c(data = dens.full, binby = 'PC1bins', density = "PLSdensity")
b <- map.bimodal.5c(data = dens.pr, binby = 'PC1fiabins', density = "FIAdensity")

png(width = 8, height = 4, units = 'in', res = 300, 'outputs/v1.6-5/PLS_FIA_PC1_BC_map.png')
grid_arrange_shared_legend(a,b,nrow = 1, ncol = 2 )
dev.off()

# outputs for PC2 (soils)
a <- map.bimodal.5c(data = dens.full, binby = 'PC2bins', density = "PLSdensity")
b <- map.bimodal.5c(data = dens.pr, binby = 'PC2bins', density = "FIAdensity")

png(width = 8, height = 4, units = 'in', res = 300, 'outputs/v1.6-5/PLS_FIA_PC2_BC_map.png')
grid_arrange_shared_legend(a,b,nrow = 1, ncol = 2 )
dev.off()

##############################################
# function for plotting regions that ar bimodal in two climate spaces
##############################################
map.bimodal.two.5c <- function(data, binby, binby2, density){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 1)
  for (i in 1:length(bins)){
    coeffs[i]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  merged <- merge(coef.bins, data, by.x = "bins",by.y = binby)
  
  #do the same for binby2
  bins <- as.character(unique(data[,binby2]))
  coeffs <- matrix(NA, length(bins), 1)
  for (i in 1:length(bins)){
    coeffs[i]<- bimodality_coefficient(na.omit(data[data[,binby2] %in% bins[i], c(density)]))
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC2 <- as.numeric(as.character(coef.bins$V1))
  merged <- merge(coef.bins, merged, by.x = "bins",by.y = binby2)
  
  #define bimodality
  merged$bimodal <- "Stable"
  merged[merged$BC >= 0.5 & merged$BC2 >= 0.5, ]$bimodal <- "Bimodal"
  
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
      '#1a9641', # dark green
      '#fdae61', # light orange
      '#a6d96a', # light green
      '#d7191c', # red
      '#fee08b', # tan
      'black'), limits = c("Stable Forest" , 'Stable Savanna', 'Bimodal Forest', "Bimodal Savanna", 'Prairie') )+
    theme_bw()+
    xlab("easting") + ylab("northing") +coord_equal()+
    ggtitle(paste0(binby,' & ',binby2,' for ',density))
  
}

a<- map.bimodal.two.5c(data = dens.full, binby = 'PC2bins', binby2 = 'PC1bins', density = "PLSdensity")
b <- map.bimodal.two.5c(data = dens.pr, binby = 'PC2bins', binby2 = 'PC1bins', density = "FIAdensity")

png(width = 8, height = 4, units = 'in', res = 300, 'outputs/v1.6-5/PLS_FIA_PC1_PC2_BC_map.png')
grid_arrange_shared_legend(a,b,nrow = 1, ncol = 2 )
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

pdf(paste0('outputs/v',version,'/rolling_BC_plots_precip.pdf'))
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


ordered.f <- dens.pr[order(dens.pr$MAP2011),]
ordered.f$rownum <- 1:length(ordered.f$MAP2011)

#make figure with both rolling and non rolling bc calculations
png(height = 400, width = 800, paste0('outputs/v',version,'/PLS_FIA_precip_25_BC_bins.png'))
pushViewport(viewport(layout = grid.layout(2, 2)))
print(calc.BC(data = dens.pr, binby = 'plsprbins25', density = "PLSdensity")+ ggtitle('BC for Precip PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(calc.BC(data = dens.pr, binby = 'fiaprbins25', density = "FIAdensity") + ggtitle('BC for Precip FIA'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 25)+ ggtitle('overlapping BC for PrecipPLS'),   vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(rollBC_r(ordered.f$MAP2011, ordered.f$FIAdensity, ordered$MAP2011, 25) + ggtitle('overlapping BC for Precip FIA'),   vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
dev.off()


#create rolling BC plots for past mean annual temperature plots 
ordered.t <- dens.pr[order(dens.pr$pasttmean),]
ordered.t$rownum <- 1:length(ordered.t$pasttmean)

#plot out rolling BC plots for past mean annual temperature:
pdf(paste0('outputs/v', version,'rolling_BC_plots_tmean.pdf'))
rollBC_r(ordered.t$pasttmean, ordered.t$PLSdensity, ordered.t$pasttmean, 0.1)
rollBC_r(ordered.t$pasttmean, ordered.t$PLSdensity, ordered.t$pasttmean, 0.5)
rollBC_r(ordered.t$pasttmean, ordered.t$PLSdensity, ordered.t$pasttmean, 1)
rollBC_r(ordered.t$pasttmean, ordered.t$PLSdensity, ordered.t$pasttmean, 0.25)
rollBC_r(ordered.t$pasttmean, ordered.t$PLSdensity, ordered.t$pasttmean, 0.01)
dev.off()

#create rolling BC plots for PC1 
ordered.pc <- dens.pr[order(dens.pr$PC1),]
ordered.pc$rownum <- 1:length(ordered.t$PC1)

rollBC_r(ordered.pc$PC1, ordered.pc$PLSdensity, ordered.pc$PC1, 0.5)
rollBC_r(ordered.pc$PC1, ordered.pc$PLSdensity, ordered.pc$PC1, 0.25)
rollBC_r(ordered.pc$PC1, ordered.pc$PLSdensity, ordered.pc$PC1, 1)
rollBC_r(ordered.pc$PC1, ordered.pc$PLSdensity, ordered.pc$PC1, 2)

#this version of roll_BC_by10 takes the BC every 10mm of preciptiation
rollBC_by_10_r = function(x,y,xout,width) {
  out = 1:length(seq(200, 1350, by = 10) )
  for( i in 1:length(seq(200, 1350, by = 10))) {
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i] = bimodality_coefficient( y[window] )
  }
  ggplot()+geom_point(aes(x = xout, y = out))+
    geom_hline( yintercept = 5/9)+ylim(0,1)+theme_bw()+
    xlab('Precipiation @ interval center (mm)') + ylab('Bimodality Coefficient') +ggtitle(paste0( 'Bimodality coefficient for binwidth = ', width))
}   
png(paste0('outputs/v',version,'/rolling_BC_plots_100.png'))
rollBC_by_10_r(ordered$MAP1910, ordered$PLSdensity, seq(200, 1350, by = 10)  , 100)
dev.off()

png(paste0('outputs/v',version,'/rolling_BC_plots_75.png'))
rollBC_by_10_r(ordered$MAP1910, ordered$PLSdensity, seq(200, 1350, by = 10)  , 75)
dev.off()

png(paste0('outputs/v',version,'/rolling_BC_plots_25.png'))
rollBC_by_10_r(ordered$MAP1910, ordered$PLSdensity, seq(200, 1350, by = 10)  , 25)
dev.off()

rollBC_by_10 = function(x,y,xout,width) {
  out = 1:length(seq(200, 1350, by = 10) )
  for( i in 1:length(seq(200, 1350, by = 10))) {
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i] = bimodality_coefficient( y[window] )
  }
  df <- data.frame(mid = xout, max = xout + width,min = xout - width,BC = out)
  
}

BC_vals <- rollBC_by_10(ordered$MAP1910, ordered$PLSdensity, seq(200, 1350, by = 10)  , 100)



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
png(paste0('outputs/v',version,'/modern_bimodal_climate.png'))
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

