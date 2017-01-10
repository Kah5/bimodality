# doing bimodality analysis on the crown cover from Indiana & Illinois
# we are using species.table that has the "coverscenter" column generated from calcualate_dens_v2.r here
library(ggplot2)
library(raster)
library(reshape2)
library(dplyr)
version <- "1.6"

# read in file
spec.table <- read.csv("outputs/species_table_pls_coverscenter.csv")

# spec.table lists each xy tree coordinate and is given a 1 if the tree's crown covers the PLS point, and a 0 if the trees crown does not cover the PLS point
X11(width = 12)
ggplot(spec.table, aes(x = Pointx, y = Pointy, color = spec))+geom_point()


count.table <- dcast(spec.table, x + y + cell ~ spec, sum, na.rm=TRUE, value.var = 'count')
count.table$total <- rowSums(count.table[,4:36], na.rm = TRUE)

covered.table <- dcast(spec.table, x + y  + cell ~ spec, sum, na.rm = TRUE, value.var = 'coverscenter')
covered.table$total <- rowSums(covered.table[,4:36], na.rm = TRUE)

covered.table$pct.cov <- (covered.table$total/count.table$total)*100

count.table.pt <- dcast(spec.table, Pointx + Pointy + cell ~ spec, sum, na.rm=TRUE, value.var = 'count')
covered.table.pt <- dcast(spec.table, Pointx + Pointy +cell+x+y   ~ spec, sum, na.rm = TRUE, value.var = 'coverscenter')
covered.table.pt$total <- rowSums(covered.table.pt[,6:38], na.rm = TRUE)/2

covered.df <- ddply(covered.table.pt, .(x, y), summarize, total_cov = sum(total, na.rm = TRUE))
#covered.table.pt <- dcast(spec.table, Pointx + Pointy + cell ~ spec, sum, na.rm=TRUE, value.var = "coverscenter")
count <- merge(covered.df, count.table[,c('x', 'y', 'total')])
count$pct <- (count$total_cov/count$total)*100
count$pct_half <- (count$total_cov/(count$total/2))*100
hist(count$pct_half, breaks = 25)
hist(count$pct, breaks = 25)




# state shapefiles for mapping
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata.inil<-spTransform(states, CRS('+init=epsg:3175'))
mapdata.inil <- data.frame(mapdata.inil)
cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

png(paste0("outputs/v",version,"/PLS_pct_cover_map.png"))
ggplot()+ geom_polygon(data = mapdata.inil, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data = count, aes(x = x, y = y, fill = pct_half))+
  scale_fill_gradientn(colours = cbpalette, limits = c(0,100), name ="% canopy cover", na.value = 'darkgrey') +
  geom_polygon(data = mapdata.inil, aes(group = group,x=long, y =lat),  color = 'black', fill = NA)+
  theme_bw()
dev.off()

#create variable for precipitation seasonality
past.precip.mo <- read.csv(paste0('outputs/pr_monthly_Prism_1895_1905.csv'))
past.precip.mo$max <- apply(past.precip.mo[ , 2:13], 1, max)
past.precip.mo$min <- apply(past.precip.mo[ , 2:13], 1, min) 
past.precip.mo$deltaP <- (past.precip.mo$max-past.precip.mo$min)/(past.precip.mo$max+past.precip.mo$min)



past.precip <- read.csv('outputs/pr_monthly_Prism_1900_1910.csv')
past.tmean <- read.csv('outputs/tmean_yr_Prism_1900-1910.csv')


cover <- merge(count, past.precip[,c('x', 'y', '.')], by =c('x', 'y'))
#cover <- merge(cover, mod.precip[,c('x', 'y', 'pr30yr')], by = c('x', 'y'))
colnames(cover)[7] <- c('MAP1910')

#now add the precipitation seasonality to the dataframe
#cover <- merge(cover, mod.precip.mo[,c('x', 'y', 'deltaP')], by = c('x', 'y') )
cover <- merge(cover, past.precip.mo[,c('x', 'y', 'deltaP')], by = c('x', 'y'), all.x = TRUE)
colnames(cover)[8]<- c( 'pastdeltaP')

#now add the mean temperature to the dataframe
#cover <- merge(cover, mod.tmean[,c('x', 'y', 'prism30yr')], by = c('x', 'y') )
cover <- merge(cover, past.tmean[,c('x', 'y', '.')], by = c('x', 'y') )
colnames(cover)[9] <- c('pasttmean')
cover$pasttmean<- cover$pasttmean/10 # convert from C*10 to Celcius
write.csv(cover, paste0("C:/Users/JMac/Documents/Kelly/biomodality/data/inil_pls_pct_cover_pr_alb",version,".csv"))

#nine.five.pct<- quantile(cover$pct_half, probs = .95, na.rm=TRUE)
#cover[cover$pct_half>nine.five.pct,]$pct_half <- nine.five.pct #patch fix the overestimates of cover

#plot histograms
hist(cover$pct_half, breaks = 50, xlim = c(0,550), xlab = 'PLS cover (stems/ha)', main = 'PLS Midwest cover')
#hist(cover$FIAcover, breaks = 50, xlim = c(0,550), xlab = 'FIA cover(stems/ha)',main = 'FIA Midwest cover')

#plot raw data
plot(cover$MAP1910,cover$pct_half, xlab = 'Past MAP', ylab = 'PLS cover')
#plot(cover$MAP2011,cover$FIAcover, xlab = 'Modern MAP', ylab = 'Modern cover')
plot(cover$pastdeltaP, cover$pct_half, xlab = "Past P seasonality", ylab = "PLS cover")
#plot(cover$moderndeltaP, cover$FIAcover, xlab = "Modern P seasonality", ylab = "FIA cover")
plot(cover$pasttmean, cover$pct_half, xlab = 'Past Tmean', ylab = "pct_half")
#plot(cover$MAP2011, cover$pct_half, xlab = 'Modern MAP', ylab = 'PLS cover')
#plot(cover$MAP1910, cover$FIAcover, xlab = 'Past MAP', ylab = 'Modern cover')
plot(cover$pasttmean, cover$MAP1910, xlab = 'Past Tmean', ylab = "Past Precip")
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
cover$sandpct <- extract(sand8km.alb, cover[,c('x', 'y')], method = 'bilinear')
cover$awc <- extract(awc8km.alb, cover[,c('x', 'y')])
cover$ksat <- extract(ksat8km.alb, cover[,c('x', 'y')])



summary(rowSums(cover != 0, na.rm=TRUE))
#cover$diff <- cover$FIAcover - cover$pct_half



PLS.lm<- lm(cover$pct_half ~cover$MAP1910)
#FIA.lm<- lm(cover$FIAcover ~cover$MAP2011)
#PLS_mod.lm<- lm(cover$pct_half ~cover$MAP2011)
#FIA_pas.lm <- lm(cover$FIAcover~cover$MAP1910)
#diff.lm <- lm(cover$diff ~cover$pct_half)

summary(PLS.lm)

library(mgcv)
#make gams 
PLS.gam <- gam(cover$pct_half ~ cover$MAP1910  +cover$pasttmean +cover$sandpct + cover$awc, method = "ML")
summary(PLS.gam) # explains 10.5% of deviance

PLS.gam1 <- gam(cover$pct_half ~ cover$MAP1910 +cover$pasttmean+ cover$sandpct, method = "ML")
summary(PLS.gam1) #explains 10.2% deviance

PLS.gam3 <- gam(cover$pct_half ~ cover$MAP1910 +cover$pasttmean +cover$awc , method = "ML")
summary(PLS.gam3) #explains 10.4% of deviance

PLS.gam4 <- gam(cover$pct_half ~ cover$awc , method = "ML")
summary(PLS.gam4) #explains 0.223% of deviance

PLS.gam5 <- gam(cover$pct_half ~ cover$awc +cover$sandpct , method = "ML")
summary(PLS.gam5) #explains 0.334% of deviance

PLS.gam2 <- gam(cover$pct_half ~ cover$MAP1910 , method = "ML")
summary(PLS.gam2) #explains 6.85% deviance

PLS.gam6 <- gam(cover$pct_half ~ cover$pastdeltaP , method = "ML")
summary(PLS.gam6) #explains 6.8% deviance

PLSgam7 <- gam(pct_half ~ pasttmean , method = "ML", data = cover)
summary(PLSgam7) #explains 1.3% deviance
plot(PLS.gam7, residuals = TRUE)

#split cover up into bins
cover$plsprbins <- cut(cover$MAP1910, #labels = c('350-400mm', '400-450mm', '450-500mm', '550-600mm', '600-650mm','650-700mm','700-750mm','750-800mm','800-850mm',  '850-900mm','900-950mm','950-1000mm','1000-1050mm','1050-1100mm', '1100-1150mm','1150-1200mm', '1200-1250mm', '1250-1300mm'),
                         breaks=c(200,250,300,400,500,600, 700,800,900, 1000,1100,1200, 1400))
#make cuts for sliding window plots
#cover$plsprbins <- cut(cover$MAP1910, labels = c('200-400mm', '400-550mm', '550-600mm', '600-850mm', '850-1000mm','1000-1150mm','1150-1300mm','1300-1450mm'),
#                        breaks=c(200,400,550,700,850, 1000,1150,1300, 1450))
#cover$fiaprbins <- cut(cover$MAP2011, labels = c('200-400mm', '400-550mm', '550-600mm', '600-850mm', '850-1000mm','1000-1150mm','1150-1300mm','1300-1450mm'),
#                        breaks=c(200,400,550,700,850, 1000,1150,1300, 1450))
#create multiple sets of bins for precipitation:

cover$plsprbins50 <- cut(cover$MAP1910, breaks = seq(250, 1350, by = 50), labels = seq(250, 1300, by = 50))
#cover$fiaprbins50 <- cut(cover$MAP2011, breaks = seq(250, 1350, by = 50), labels = seq(250, 1300, by = 50))
cover$plsprbins100 <- cut(cover$MAP1910, breaks = seq(250, 1350, by = 100), labels = seq(250, 1250, by = 100))
#cover$fiaprbins100 <- cut(cover$MAP2011, breaks = seq(250, 1350, by = 100), labels = seq(250, 1250, by = 100))
cover$plsprbins75 <- cut(cover$MAP1910, breaks = seq(250, 1350, by = 75), labels = seq(250, 1275, by = 75))
#cover$fiaprbins75 <- cut(cover$MAP2011, breaks = seq(250, 1350, by = 75), labels = seq(250, 1275, by = 75))
cover$plsprbins150 <- cut(cover$MAP1910, breaks = seq(250, 1350, by = 150), labels = seq(250, 1250, by = 150))
#cover$fiaprbins150 <- cut(cover$MAP2011, breaks = seq(250, 1350, by = 150), labels = seq(250, 1250, by = 150))
cover$plsprbins25 <- cut(cover$MAP1910, breaks = seq(250, 1350, by = 25), labels = seq(250, 1325, by = 25))

#cover$fiaprbins25 <- cut(cover$MAP2011, breaks = seq(250, 1350, by = 25), labels = seq(250, 1325, by = 25))

cover$sandbins <- cut(cover$sandpct, breaks = seq(0, 100, by = 10))
cover$ksatbins <- cut(cover$ksat, breaks = seq(0,300, by = 10))
#cover$moddeltPbins <- cut(cover$moderndeltaP, breaks = seq(0,1, by = .10))
cover$pastdeltPbins <- cut(cover$pastdeltaP, breaks = seq(0,1, by = .10))

test<- cover[!is.na(cover),]
melted <- melt(test, id.vars = c("x", 'y', 'plsprbins',  'plsprbins50', 'plsprbins75', 
                                 'plsprbins100','plsprbins150', 'plsprbins25', 
                                 'MAP1910',  'sandpct', 'awc', 'ksat', 'sandbins', 'ksatbins', 
                                 'pastdeltaP','pastdeltPbins', 'pasttmean', 'total_cov', 'total', 'pct')) 

#load map data for future maps

mapdata <- mapdata.inil

#pdf("outputs/binned_histograms_pr_AGU_12_6_16_large_bins.pdf")
png(paste0('outputs/v',version,'/PLS_cover_histogrom.png'))#
ggplot(cover, aes(pct_half)) +geom_histogram(fill= "#D55E00",color = "black", bins = 50) +xlim(0, 100)+ylim(0,150)+ xlab("PLS tree cover (stems/ha)")+ ylab('# grid cells')+ 
  theme_bw(base_size = 25)#+ facet_wrap(~plsprbins)
dev.off()



library(lattice)
#hexbin plots to show the cover of points in precipitatoins

png(paste0('outputs/v',version,'/PLS_cover_precipitation_hexbin.png'))
ggplot(cover, aes(MAP1910,pct_half))+geom_bin2d(bins = 75) +ylim(0,100) + xlim(400, 1400)+
  scale_fill_gradient(low='red', high='black')+theme_bw(base_size = 20)+
  xlab('Mean Annual Precipitation (mm) \n PRISM 1900-1910') + ylab("PLS Tree cover (stems/ha)")
dev.off()

rbpalette <- c('red', "blue")
ggplot(melted, aes(value, fill = variable)) +geom_density(alpha = 0.3)  +xlim(0, 50)+ facet_grid(plsprbins~., scales = 'free_y')+scale_fill_brewer(palette = "Set1")

png(paste0('outputs/v',version,'/precipitation_cover_by_bins.png'))
ggplot(melted, aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)  +xlim(0, 60)+ facet_wrap(~plsprbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree cover')
dev.off()

#plot by sandiness
png(paste0('outputs/v',version,'/sand_cover_by_bins.png'))
ggplot(melted, aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)  +xlim(0, 60)+ facet_wrap(~sandbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree cover')
dev.off()

#plot out climate space:
png(paste0('outputs/v',version,'/precip_vs_temp_cover_pls.png'))
ggplot(cover, aes(x = MAP1910, y = pasttmean, colour = pct_half))+geom_point()+
  scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,60), name ="Tree \n cover \n (trees/hectare)", na.value = 'darkgrey') +theme_bw()
dev.off()

#plot by ksat
png(paste0('outputs/v',version,'/ksat_cover_by_bins.png'))
ggplot(melted, aes(value, colour = variable)) +geom_density(size = 2, alpha = 0.1)  +xlim(0, 60)+ facet_wrap(~ksatbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree cover')
dev.off()

#plot by past deltaPbins 
png(paste0('outputs/v',version,'/pastDeltaP_cover_by_bins.png'))
ggplot(melted, aes(value, colour = variable))+ geom_density(size = 2, alpha = 0.1) +xlim(0, 60)+ facet_wrap(~pastdeltPbins, scales = 'free_y')+
  scale_color_manual(values = c( "#D55E00", "#0072B2")) + theme_bw()+theme(strip.background = element_rect(fill="black"), strip.text.x = element_text(size = 12, colour = "white")) + xlab('tree cover')
dev.off()

#calculate bimodality coefficients
library(modes)

#this function uses the bimodality_coefficient funcition in the modes library to calculate the 
#bimodality coefficient of the cover (FIA or PLS) within a given set of bins (climate, sand, etc)

calc.BC <- function(data, binby, cover){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 1)
  for (i in 1:length(bins)){
    coeffs[i]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(cover)]))
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

pdf(paste0('outputs/v',version,'/bimodality_coefficient_inil_cover_pls_binplots.pdf'))
calc.BC(data = cover, binby = 'plsprbins', cover = "pct_half")
#calc.BC(data = cover, binby = 'fiaprbins', cover = "FIAcover")
calc.BC(data = cover, binby = 'plsprbins100', cover = "pct_half")
#calc.BC(data = cover, binby = 'fiaprbins100', cover = "FIAcover")
calc.BC(data = cover, binby = 'plsprbins75', cover = "pct_half")
#calc.BC(data = cover, binby = 'fiaprbins75', cover = "FIAcover")
calc.BC(data = cover, binby = 'plsprbins25', cover = "pct_half")
#calc.BC(data = cover, binby = 'fiaprbins25', cover = "FIAcover")

#calc.BC(data = cover, binby = 'fiaprbins', cover = "pct_half")

calc.BC(data = cover, binby = 'sandbins', cover = "pct_half")
calc.BC(data = cover, binby = 'ksatbins', cover = "pct_half")
calc.BC(data = cover, binby = 'pastdeltPbins', cover = "pct_half")
dev.off()


#this function maps out the region that is bimodal 
map.bimodal <- function(data, binby, val){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 1)
  for (i in 1:length(bins)){
    coeffs[i]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(val)]))
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  merged <- merge(coef.bins, data, by.x = "bins", by.y = binby)
  merged$bimodal <- "Not bimodal"
  merged[merged$BC >= 0.5,]$bimodal <- "Bimodal"
  write.csv(merged, paste0('outputs/v',version,'/PLS_cover_bimodal_', binby,'.csv'))
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'grey')+
    geom_raster(data = merged, aes(x = x, y = y, fill = bimodal))+ scale_fill_manual(values = c('purple', 'forestgreen'))+theme_bw()+
    xlab("easting") + ylab("northing") +coord_equal()+
    ggtitle(paste0('Bimodal regions for ', binby))
  
}

#map out bimodalities--note the region varies by bin size
pdf(paste0('outputs/v',version,'/bimodal_maps_cover_inil.pdf'))
map.bimodal(data = cover, binby = 'plsprbins50', val = "pct_half")
#map.bimodal(data = cover, binby = 'fiaprbins', cover = "FIAcover")
map.bimodal(data = cover, binby = 'plsprbins100', val = "pct_half")
#map.bimodal(data = cover, binby = 'fiaprbins100', cover = "FIAcover")
map.bimodal(data = cover, binby = 'plsprbins75', val = "pct_half")
#map.bimodal(data = cover, binby = 'fiaprbins75', cover = "FIAcover")
map.bimodal(data = cover, binby = 'plsprbins25', val = "pct_half")
#map.bimodal(data = cover, binby = 'fiaprbins25', cover = "FIAcover")
#map.bimodal(data = cover, binby = 'fiaprbins', cover = "pct_half")
map.bimodal(data = cover, binby = 'sandbins', val = "pct_half")
map.bimodal(data = cover, binby = 'ksatbins', val = "pct_half")
map.bimodal(data = cover, binby = 'pastdeltPbins', val = "pct_half")
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

#need to order the 
ordered <- cover[order(cover$MAP1910),]
ordered$rownum <- 1:length(ordered$MAP1910)

pdf(paste0('outputs/v',version,'/rolling_BC_plots_PLS_cover_500_cutoff.pdf'))
rollBC_r(ordered$MAP1910, ordered$pct_half, ordered$MAP1910, 150)
rollBC_r(ordered$MAP1910, ordered$pct_half, ordered$MAP1910, 200)
rollBC_r(ordered$MAP1910, ordered$pct_half, ordered$MAP1910, 300)
rollBC_r(ordered$MAP1910, ordered$pct_half, ordered$MAP1910, 250)
rollBC_r(ordered$MAP1910, ordered$pct_half, ordered$MAP1910, 100)
rollBC_r(ordered$MAP1910, ordered$pct_half, ordered$MAP1910, 75)
rollBC_r(ordered$MAP1910, ordered$pct_half, ordered$MAP1910, 50)
rollBC_r(ordered$MAP1910, ordered$pct_half, ordered$MAP1910, 25)
rollBC_r(ordered$MAP1910, ordered$pct_half, ordered$MAP1910, 10)
dev.off()
