#read in FIA from Sean's repository
# set pls version to use
version <- "1.5-2"
setwd( "C:/Users/JMac/Documents/Kelly/biomodality")
library(data.table)
FIA <- read.csv('data/FIA_species_plot_parameters_paleongrid.csv')
speciesconversion <- read.csv('data/FIA_conversion-SGD_remove_dups.csv')

FIA.pal <- merge(FIA, speciesconversion, by = 'spcd' )
density.FIA.table <- dcast(FIA.pal, plt_cn ~ PalEON, sum, na.rm=TRUE, value.var = 'density') #sum all species in common taxa in FIA grid cells
density.FIA.table <- dcast(FIA.pal, x + y + cell ~ PalEON, mean, na.rm=TRUE, value.var = 'density') #average density of plots in grid cell
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

#can aggregate by species
pls.spec<- read.csv(paste0('outputs/density_tables',version, '.csv')
pls.spec <- dcast(pls.spec, x + y + cell ~spec, mean, na.rm = TRUE, value.var = 'density')
pls.spec$total <- rowSums(pls.spec[4:35], na.rm=TRUE)

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
FIA.full$total <- rowSums(FIA.full[,5:41], na.rm = TRUE)
summary(FIA.full$total)
hist(FIA.full$total, breaks = 50, xlim = c(0,600))




################################################################
#comparison of FIA and PLS datasets to climate
###############################################################

#past.precip <- read.csv('data/pr_alb_1895_1935_GHCN.csv')
#mod.precip <- read.csv('data/pr_alb_1975_2014_GHCN.csv')

past.precip <- read.csv('outputs/pr_monthly_Prism_1895_1905.csv')
mod.precip <- read.csv('data/spec_table_30yr_prism.csv')


dens.pr <- merge(full.spec, past.precip[,c('x', 'y', 'total_.')], by =c('x', 'y'))
dens.pr <- merge(dens.pr, mod.precip[,c('x', 'y', 'pr30yr')], by = c('x', 'y'))
colnames(dens.pr)[42:44] <- c('PLSdensity', 'MAP1910', "MAP2011")

fia.dens.pr <- merge(FIA.full, past.precip[,c('x', 'y', 'total_.')], by =c('x', 'y'))
fia.dens.pr <- merge(fia.dens.pr, mod.precip[,c('x', 'y', 'pr30yr')], by = c('x', 'y'))
colnames(fia.dens.pr)[42:44] <- c('FIAdensity', 'MAP1910', "MAP2011")

write.csv(dens.pr, "C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_density_pr_alb.csv")
write.csv(fia.dens.pr, "C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_FIA_density_pr_alb.csv")

#nine.five.pct<- quantile(dens.pr$PLSdensity, probs = .95, na.rm=TRUE)
#dens.pr[dens.pr$PLSdensity>nine.five.pct,]$PLSdensity <- nine.five.pct #patch fix the overestimates of density

#plot histograms
hist(dens.pr$PLSdensity, breaks = 50, xlim = c(0,550), xlab = 'PLS density (stems/ha)', main = 'PLS Midwest Density')
hist(fia.dens.pr$FIAdensity, breaks = 50, xlim = c(0,550), xlab = 'FIA density(stems/ha)',main = 'FIA Midwest Density')

#plot raw data
plot(dens.pr$MAP1910,dens.pr$PLSdensity, xlab = 'Past MAP', ylab = 'PLS density')
plot(fia.dens.pr$MAP2011,fia.dens.pr$FIAdensity, xlab = 'Modern MAP', ylab = 'Modern density')
plot(dens.pr$MAP2011, dens.pr$PLSdensity, xlab = 'Modern MAP', ylab = 'PLS density')
plot(fia.dens.pr$MAP1910, fia.dens.pr$FIAdensity, xlab = 'Past MAP', ylab = 'Modern density')

fia.dens.pr[is.na(fia.dens.pr)] <- 0

dens.pr <- merge(dens.pr, fia.dens.pr[,c('x', 'y', 'cell', 'FIAdensity')], by = c('x', 'y', 'cell'))



summary(rowSums(dens.pr != 0, na.rm=TRUE))

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



library(ggExtra)
library(ggplot2)
png('outputs/PLS_precip_hist.png')
X11(width = 5)
p <- ggplot(dens.pr, aes(MAP1910, PLSdensity)) + geom_point() + theme_classic() + xlab('Mean Annual Precipitation (mm)') + ylab('Pre-Settlement \n Tree Density \n (Trees/hectare)')+
  xlim(450, 1200) + ylim(0, 800)+theme_bw()+
  theme(text = element_text(size = 20))
ggExtra::ggMarginal(p, type = "histogram",size = 3, colour = 'black', fill = 'red')
dev.off()

png('outputs/FIA_precip_hist.png')
p <- ggplot(dens.pr, aes(MAP2011, FIAdensity)) + geom_point() + theme_classic()+ xlab('Mean Annual Precipitation (mm)') + ylab('Modern Tree Density \n (Trees/hectare)') + 
  xlim(450, 1200) + ylim(0, 800)+theme_bw()+theme(text = element_text(size = 20))
ggExtra::ggMarginal(p, type = "histogram",  margins = 'y', size = 3, colour = 'black', fill = "#0072B2")
  
dev.off()
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

ggplot(P.data, aes(long, lat)) + coord_map() + geom_polygon(aes(group=group)) + 
  geom_polygon(data=subset(P.data, subregion %in% c("patrick", "henry", "franklin",
                                                "pittsylvania")), aes(group=group), size=1, color="white",fill="white")

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

pdf('outputs/maps.pdf')
pls.map

sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))


pls.map <- ggplot()+ geom_raster(data=dens.pr, aes(x=x, y=y, fill = PLSdensity))+
  labs(x="easting", y="northing", title="PLS tree density") + 
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat, colour= 'black'), fill = NA)+
  scale_fill_gradientn(colours = rev(terrain.colors(4)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)") +
  coord_equal()
pls.map

FIA.map <- ggplot()+ geom_raster(data=dens.pr, aes(x=x, y=y, fill = FIAdensity.x))+
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

