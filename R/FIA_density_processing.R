#read in FIA from Sean's repository
setwd( "C:/Users/JMac/Documents/Kelly/biomodality")
library(data.table)
FIA <- read.csv('data/FIA_species_plot_parameters_paleongrid.csv')
speciesconversion <- read.csv('data/FIA_conversion-SGD_remove_dups.csv')

FIA.pal <- merge(FIA, speciesconversion, by = 'spcd' )

density.FIA.table <- dcast(FIA.pal, x + y + cell ~ PalEON, mean, na.rm=TRUE, value.var = 'density')
density.FIA.table$FIAdensity <- rowSums(density.FIA.table[,5:24], na.rm = TRUE)
summary(density.FIA.table$FIAdensity)
hist(density.FIA.table$FIAdensity, breaks = 100)

#if you want to do a quick plot
#coordinates(density.FIA.table) <- ~x + y
#gridded(density.FIA.table) <- TRUE
#spplot(density.FIA.table, 'total')

#merge inil pls and inil FIA

#read in tree level data
pls.inil<- read.csv('outputs/density_tables.csv')
pls.inil <- dcast(pls.inil, x + y + cell ~., mean, na.rm = TRUE, value.var = 'density')

colnames(pls.inil) <- c('x', 'y', 'cell','PLSdensity')

umdw <- read.csv('data/plss_density_alb_v0.9-6.csv')
umdw$total <- rowSums(umdw[,5:33])
umdw.new <- umdw[,c('x', 'y', 'cell', 'total')]
colnames(umdw.new) <- c('x', 'y', 'cell', 'PLSdensity')

pls.inil <- rbind(pls.inil, umdw.new)
#write.csv(pls.inil,C:/Users/JMac/Documents/Kelly/biomodality/outputs )
#merge inil pls and inilFIA
densitys <- merge(pls.inil[,c('x', 'y', 'cell', 'PLSdensity')], density.FIA.table[,c('x', 'y', 'cell', 'FIAdensity')],
                  by = c('x', 'y', 'cell'))

write.csv(densitys, "C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_alb.csv")
#this merge yields only 457 grid cells across indiana and illinois where we have both PLS and FIA data

#past.precip <- read.csv('data/pr_alb_1895_1935_GHCN.csv')
#mod.precip <- read.csv('data/pr_alb_1975_2014_GHCN.csv')

past.precip <- read.csv('outputs/pr_monthly_Prism_1895_1905.csv')
mod.precip <- read.csv('data/spec_table_30yr_prism.csv')


dens.pr <- merge(densitys, past.precip[,c('x', 'y', 'total_.')], by =c('x', 'y'))

dens.pr <- merge(dens.pr, mod.precip[,c('x', 'y', 'pr30yr')], by = c('x', 'y'))
colnames(dens.pr) <- c('x', 'y', 'cell', 'PLSdensity', 'FIAdensity', 'MAP1910', "MAP2011")


write.csv(dens.pr, "C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_pr_alb.csv")

dens.pr[dens.pr$PLSdensity>1000,]$PLSdensity <- 700
#plot histograms
hist(dens.pr$PLSdensity, breaks = 50)
hist(dens.pr$FIAdensity, breaks = 50)

#plot raw data
plot(dens.pr$MAP1910,dens.pr$PLSdensity, xlab = 'Past MAP', ylab = 'PLS density')
plot(dens.pr$MAP2011,dens.pr$FIAdensity, xlab = 'Modern MAP', ylab = 'Modern density')
plot(dens.pr$MAP2011, dens.pr$PLSdensity, xlab = 'Modern MAP', ylab = 'PLS density')
plot(dens.pr$MAP1910, dens.pr$FIAdensity, xlab = 'Past MAP', ylab = 'Modern density')

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

dens.pr$diff <- dens.pr$FIAdensity - dens.pr$PLSdensity
plot(dens.pr$PLSdensity, dens.pr$diff, xlab='PLS tree density (trees/ha)', ylab='increase in density since PLS (trees/ha)')

#dens.pr<- data.frame(dens.pr)
library(ggplot2)
pls.map <- ggplot()+ geom_raster(data=dens.pr, aes(x=x, y=y, fill = PLSdensity))+
  labs(x="easting", y="northing", title="Tree PLS density") + 
  scale_fill_gradientn(colours = rainbow(4), name ="Tree Dens. \n (stems/ha)")
pls.map

FIA.map <- ggplot()+ geom_raster(data=dens.pr, aes(x=x, y=y, fill = FIAdensity))+
  labs(x="easting", y="northing", title="Tree FIA density") + 
  scale_fill_gradientn(colours = rainbow(4), name ="Tree Dens. \n (stems/ha)")
FIA.map

diff.map <- ggplot()+ geom_raster(data=dens.pr, aes(x=x, y=y, fill = diff))+
  labs(x="easting", y="northing", title="Tree FIA density")  + 
  scale_fill_gradientn(colours = rainbow(4), name ="Tree Dens. \n (stems/ha)")
diff.map

pr1901.map <- ggplot()+ geom_raster(data=dens.pr, aes(x=x, y=y, fill = MAP1910))+
  labs(x="easting", y="northing", title="1895-1935 MAP")  + 
  scale_fill_gradientn(colours = rainbow(4), name ="Precip. \n (mm/year)",limits = c(0,1300))
pr1901.map

pr2011.map <- ggplot()+ geom_raster(data=dens.pr, aes(x=x, y=y, fill = MAP2011))+
  labs(x="easting", y="northing", title="1975-2011 MAP")  + 
  scale_fill_gradientn(colours = rainbow(4), name ="TPrecip. \n (mm/year)",limits = c(0,1300))
pr2011.map

#use ggplot to plot data and regression line
pls.pr <- ggplot()+ geom_point(data=dens.pr, aes(x=MAP1910, y=PLSdensity))+
  geom_smooth(data=dens.pr, aes(x=MAP1910, y=PLSdensity),method=lm) 
pls.pr


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

