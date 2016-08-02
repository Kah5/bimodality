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
#merge inil pls and inilFIA
densitys <- merge(pls.inil[,c('x', 'y', 'cell', 'PLSdensity')], density.FIA.table[,c('x', 'y', 'cell', 'FIAdensity')],
                  by = c('x', 'y', 'cell'))

write.csv(densitys, "C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_alb.csv")
#this merge yields only 457 grid cells across indiana and illinois where we have both PLS and FIA data

past.precip <- read.csv('data/pr_alb_1895_1935_GHCN.csv')
mod.precip <- read.csv('data/pr_alb_1975_2014_GHCN.csv')

dens.pr <- merge(densitys, past.precip[,c('x', 'y', 'total_.')], by =c('x', 'y'))

dens.pr <- merge(dens.pr, mod.precip[,c('x', 'y', 'total_.')], by = c('x', 'y'))
colnames(dens.pr) <- c('x', 'y', 'cell', 'PLSdensity', 'FIAdensity', 'MAP1910', "MAP2011")

#plot histograms
hist(dens.pr$PLSdensity, breaks = 50)
hist(dens.pr$FIAdensity, breaks = 50)

#plot raw data
plot(dens.pr$MAP1910,dens.pr$PLSdensity)
plot(dens.pr$MAP2011,dens.pr$FIAdensity)
plot(dens.pr$MAP2011, dens.pr$PLSdensity)

PLS.lm<- lm(dens.pr$PLSdensity ~dens.pr$MAP1910)
FIA.lm<- lm(dens.pr$FIAdensity ~dens.pr$MAP2011)
PLS_mod.lm<- lm(dens.pr$PLSdensity ~dens.pr$MAP2011)

summary(PLS.lm)
summary(FIA.lm)
summary(PLS_mod.lm)


dens.pr$diff <- dens.pr$PLSdensity - dens.pr$FIAdensity


#dens.pr<- data.frame(dens.pr)

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


#use ggplot to plot data and regression line
pls.pr <- ggplot()+ geom_point(data=dens.pr, aes(x=MAP1910, y=PLSdensity))+
  geom_smooth(data=dens.pr, aes(x=MAP1910, y=PLSdensity),method=lm) 
pls.pr


fia.pr <- ggplot()+ geom_point(data=dens.pr, aes(x=MAP2011, y=FIAdensity))+
  geom_smooth(data=dens.pr, aes(x=MAP2011, y=FIAdensity),method=lm) 
fia.pr

dif.pr <- ggplot()+ geom_point(data=dens.pr, aes(x=MAP2011, y=diff))+
  geom_smooth(data=dens.pr, aes(x=MAP2011, y=diff),method=lm) 
dif.pr
