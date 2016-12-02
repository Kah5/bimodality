
##########################################
#extracting precip & plotting Crown cover#
#run calculate_dens_v2.r first
#Kelly Heilman               
#January 20, 2016            
##########################################

#getting gridded climate data
#need to clean this up

#library(reshape2)
library(data.table)
library(sp)
library(raster)
library(ggplot2)

setwd('C:/Users/JMac/Documents/Kelly/biomodality/data/precip_2014/')
years <- 1900:1950
yr <- "1900_1950"
month.abb <- c('Jan', 'Feb', 'Mar', "Apr", "May", 
  'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")

#this loop extracts the data and adds the year to a dataframe
for (i in years) {
  filenames <- list.files(pattern=paste("precip.",i, sep = ""))
  s <- read.table(filenames)
  y <- data.frame(s) #covert to dataframe
  colnames(y) <- c("Lon", "Lat", month.abb)
  y$year <- i
}

y$total <- rowSums(y[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
                        'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")])

require(data.table)
#this averages for each month within each gridcell
full <- dcast(setDT(y), Lon + Lat ~ ., value.var=c('Jan', 'Feb', 'Mar', "Apr", "May", 
                                                   'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec",'total'))
library(plyr)
library(reshape2)

min.pr <- dcast(data.frame(y), Lon + Lat ~.,min , value.var= 'total')
max.pr <-  dcast(data.frame(y), Lon + Lat ~.,max , value.var= 'total')


#convert to rasterstack
coordinates(full) <- ~Lon + Lat
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages
projection(avgs) <- CRS("+init=epsg:4326") # assign the projection from GHCN
avg.alb <- projectRaster(avgs, crs='+init=epsg:3175') # project in great lakes albers

##read in the FIA and PLS pct cover points that we are interested in 

#spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/spec.table.csv')
#coordinates(spec.table) <- ~x + y
#tree.dens <- dcast(spec.table, x+y~., mean, na.rm=TRUE, value.var = 'density')
#dens.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_alb.csv')
FIAplots <- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/outputs/FIA_plot_agg_fuzzed_alb.csv")
PLSpoints.agg <- read.csv ("C:/Users/JMac/Documents/Kelly/biomodality/outputs/PLS_pct_cov_by_pt_inil.csv")

#extract for FIA
precip.alb <- crop(avg.alb, extent(FIAplots)) 
spec.table <- data.frame(FIAplots)

precip <- data.frame(extract(avg.alb, FIAplots[,c('x', 'y')]))
precip$x <- FIAplots$x
precip$y <- FIAplots$y

write.csv(precip, paste0('C:/Users/JMac/Documents/Kelly/biomodality/data/FIAplots_pr_alb_',yrs,'_GHCN.csv'))

#extract for PLS
precip.alb <- crop(avg.alb, extent(PLSpoints)) 
PLSpoints <- data.frame(PLSpoints)

precip <- data.frame(extract(avg.alb, PLSpoints[,c('x', 'y')]))
precip$x <- PLSpoints$x
precip$y <- PLSpoints$y

write.csv(precip, paste0('C:/Users/JMac/Documents/Kelly/biomodality/data/PLSpoints_pr_alb_',yrs,'_GHCN.csv'))

######################
##For Temperature now
#####################
setwd("C:/Users/JMac/Documents/Kelly/biomodality/data/air_temp_2014/")
years <- 1951:2000
yrs <- '1951_2000'
month.abb <- c('Jan', 'Feb', 'Mar', "Apr", "May", 
               'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")

#this loop extracts the data and adds the year to a dataframe
for (i in years) {
  filenames <- list.files(pattern=paste("air_temp.",i, sep = ""))
  s <- read.table(filenames)
  y <- data.frame(s) #covert to dataframe
  colnames(y) <- c("Lon", "Lat", month.abb)
  y$year <- i
}

y$annual <- rowMeans(y[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
                        'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")])

#also calculate the rrange of temperature
y$range <- apply(y[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
                        'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")],1, function(x) max(x)-min(x))

#this averages for each month within each gridcell
full <- dcast(setDT(y), Lon + Lat ~ ., value.var=c('Jan', 'Feb', 'Mar', "Apr", "May", 
                                                   'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec", 'annual', 'range'))

#convert to rasterstack
coordinates(full) <- ~Lon + Lat
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages
projection(avgs) <- CRS("+init=epsg:4326") # assign the projection from GHCN
avg.alb <- projectRaster(avgs, crs='+init=epsg:3175') # project in great lakes albers


#extract for FIA

air_temp.alb <- crop(avg.alb, extent(FIAplots)) 
spec.table <- data.frame(FIAplots)

air_temp <- data.frame(extract(air_temp.alb, FIAplots[,c('x', 'y')]))
air_temp$x <- FIAplots$x
air_temp$y <- FIAplots$y

write.csv(air_temp, paste0('C:/Users/JMac/Documents/Kelly/biomodality/data/FIA_air_temp_alb_',yrs,'_GHCN.csv'))

#extract for PLS
air_temp.alb <- crop(avg.alb, extent(PLSpoints)) 
spec.table <- data.frame(PLSpoints)

air_temp <- data.frame(extract(air_temp.alb, PLSpoints[,c('x', 'y')]))
air_temp$x <- PLSpoints$x
air_temp$y <- PLSpoints$y

write.csv(air_temp, paste0('C:/Users/JMac/Documents/Kelly/biomodality/data/PLSpoints_air_temp_alb_',yrs,'_GHCN.csv'))


######################
##For PET (Potential Evapotranspiation) now
#####################
setwd("C:/Users/JMac/Documents/Kelly/biomodality/data/Eo150_2014/")
years <- 1900:1950
yrs <- '1900-1950'
month.abb <- c('Jan', 'Feb', 'Mar', "Apr", "May", 
               'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")

#this loop extracts the data and adds the year to a dataframe
for (i in years) {
  filenames <- list.files(pattern=paste("Eo150.",i, sep = ""))
  s <- read.table(filenames)
  y <- data.frame(s) #covert to dataframe
  colnames(y) <- c("Lon", "Lat", month.abb)
  y$year <- i
}

y$annual <- rowSums(y[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
                        'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")])

#this averages for each month within each gridcell
full <- dcast(setDT(y), Lon + Lat ~. , value.var=c('Jan', 'Feb', 'Mar', "Apr", "May", 
                                                   'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec", 'annual'))

#convert to rasterstack
coordinates(full) <- ~Lon + Lat
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages
projection(avgs) <- CRS("+init=epsg:4326") # assign the projection from GHCN
avg.alb <- projectRaster(avgs, crs='+init=epsg:3175') # project in great lakes albers


#for FIA

Eo150.alb <- crop(avg.alb, extent(FIAplots)) 
spec.table <- data.frame(FIAplots)

Eo150 <- data.frame(extract(Eo150.alb, FIAplots[,c('x', 'y')]))
Eo150$x <- FIAplots$x
Eo150$y <- FIAplots$y

write.csv(Eo150, paste0('C:/Users/JMac/Documents/Kelly/biomodality/data/FIAplots_Eo150_alb_',yrs,'_GHCN.csv'))

#for PLS

Eo150.alb <- crop(avg.alb, extent(PLSpoints)) 
spec.table <- data.frame(PLSpoints)

Eo150 <- data.frame(extract(Eo150.alb, PLSpoints[,c('x', 'y')]))
Eo150$x <- PLSpoints$x
Eo150$y <- PLSpoints$y

write.csv(Eo150, paste0('C:/Users/JMac/Documents/Kelly/biomodality/data/PLSpoints_Eo150_alb_',yrs,'_GHCN.csv'))

######################
##For Actual Evapotranspiration now
#####################
setwd("C:/Users/JMac/Documents/Kelly/biomodality/data/E150_2014/")
years <- 1900:1950
yrs <-'1900_1905'
month.abb <- c('Jan', 'Feb', 'Mar', "Apr", "May", 
               'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")

#this loop extracts the data and adds the year to a dataframe
for (i in years) {
  filenames <- list.files(pattern=paste("E150.",i, sep = ""))
  s <- read.table(filenames)
  y <- data.frame(s) #covert to dataframe
  colnames(y) <- c("Lon", "Lat", month.abb)
  y$year <- i
}

y$annual <- rowSums(y[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
                        'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")])

require(reshape2)
#this averages for each month within each gridcell
full <- dcast(setDT(y), Lon + Lat ~ ., value.var=c('Jan', 'Feb', 'Mar', "Apr", "May", 
                                                   'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec", 'annual'))

#convert to rasterstack
coordinates(full) <- ~Lon + Lat
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages
projection(avgs) <- CRS("+init=epsg:4326") # assign the projection from GHCN
avg.alb <- projectRaster(avgs, crs='+init=epsg:3175') # project in great lakes albers


#for FIA

E150.alb <- crop(avg.alb, extent(FIAplots)) 
spec.table <- data.frame(FIAplots)

E150 <- data.frame(extract(E150.alb, FIAplots[,c('x', 'y')]))
E150$x <- FIAplots$x
E150$y <- FIAplots$y

write.csv(E150, paste0('C:/Users/JMac/Documents/Kelly/biomodality/data/FIAplots_E150_alb_',yrs,'_GHCN.csv'))
#for PLS

E150.alb <- crop(avg.alb, extent(PLSpoints)) 
spec.table <- data.frame(PLSpoints)

E150 <- data.frame(extract(E150.alb, PLSpoints[,c('x', 'y')]))
E150$x <- PLSpoints$x
E150$y <- PLSpoints$y

write.csv(E150, paste0('C:/Users/JMac/Documents/Kelly/biomodality/data/FIAplotsPLSpoints_E150_alb_',yrs,'_GHCN.csv'))


############################################
##caculate the P-ET or effective rainfall
############################################

#extract ET again
setwd("C:/Users/JMac/Documents/Kelly/biomodality/data/E150_2014/")
years <- 1900:1950
yrs <- '1900_1950'
month.abb <- c('Jan', 'Feb', 'Mar', "Apr", "May", 
               'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")

#this loop extracts the data and adds the year to a dataframe
for (i in years) {
  filenames <- list.files(pattern=paste("E150.",i, sep = ""))
  s <- read.table(filenames)
  y <- data.frame(s) #covert to dataframe
  colnames(y) <- c("Lon", "Lat", month.abb)
  y$year <- i
}

y$total <- rowSums(y[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
                          'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")])
ET <- y

#extract precip again
setwd('C:/Users/JMac/Documents/Kelly/biomodality/data/precip_2014/')

month.abb <- c('Jan', 'Feb', 'Mar', "Apr", "May", 
               'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")

#this loop extracts the data and adds the year to a dataframe
for (i in years) {
  filenames <- list.files(pattern=paste("precip.",i, sep = ""))
  s <- read.table(filenames)
  y <- data.frame(s) #covert to dataframe
  colnames(y) <- c("Lon", "Lat", month.abb)
  y$year <- i
}

y$total <- rowSums(y[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
                        'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")])

pr <- y


#subtract PR- ET
P.ET<- pr[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
      'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec",'total')] - ET[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
                                                              'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec",'total')]

P.ET$Lat <- pr$Lat
P.ET$Lon <- pr$Lon
P.ET$year <- P.ET$year


#P.ET$total <- rowSums(P.ET[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
 #                         'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")])

#this averages for each month within each gridcell
full <- dcast(setDT(P.ET), Lon + Lat ~ ., value.var=c('Jan', 'Feb', 'Mar', "Apr", "May", 
                                                   'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec", 'total'))

#convert to rasterstack
coordinates(full) <- ~Lon + Lat
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages
projection(avgs) <- CRS("+init=epsg:4326") # assign the projection from GHCN
avg.alb <- projectRaster(avgs, crs='+init=epsg:3175') # project in great lakes albers



#for FIA
P.ET.alb <- crop(avg.alb, extent(FIAplots)) 
spec.table <- data.frame(FIAplots)

PET <- data.frame(extract(P.ET.alb, FIAplots[,c('x', 'y')]))
PET$x <- FIAplots$x
PET$y <- FIAplots$y

write.csv(PET, paste0('C:/Users/JMac/Documents/Kelly/biomodality/data/FIAplots_PET_alb_',yrs,'_GHCN.csv'))

#for FIA
P.ET.alb <- crop(avg.alb, extent(PLSpoints)) 
spec.table <- data.frame(PLSpoints)

PET <- data.frame(extract(P.ET.alb, PLSpoints[,c('x', 'y')]))
PET$x <- PLSpoints$x
PET$y <- PLSpoints$y

write.csv(PET, paste0('C:/Users/JMac/Documents/Kelly/biomodality/data/PLSpoints_PET_alb_',yrs,'_GHCN.csv'))


##############################################################
#Density & cover aggregations (also in crown_cover_nongeoref)#
##############################################################


#avg.prism.p<- dcast(spec.table, x + y ~. , mean, na.rm=TRUE, value.var = 'prism.1900p')
#need to aggregate the spec.table cover values by each point
Crown.width <- dcast(spec.table, x + y ~. , sum, na.rm=TRUE, value.var = 'CW')
Crown.area <- dcast(spec.table, x + y ~. , sum, na.rm=TRUE, value.var = 'crown.area')
Crown.scales <- dcast(spec.table, x + y ~. , mean, na.rm=TRUE, value.var = 'crown.scaled')
cover <- dcast(spec.table, x + y ~. , mean, na.rm = TRUE, value.var = "cover") 
CC.adj <- dcast(spec.table, x + y ~., mean, na.rm = TRUE, value.var = "CC.adj")

tree.dens <- dcast(spec.table, x+y~., mean, na.rm=TRUE, value.var = 'density')
basal <- dcast(spec.table, x+y~., mean, na.rm=TRUE, value.var = 'basal')
tree.dens.sd <- dcast(spec.table, x+y~., sd, na.rm=TRUE, value.var = 'density')


#lets make some plots of cover, %cover, density, and basal area
#carla staver uses a cut off of 40-45% cover for forest regions

#make all cover over 100& cover == 100
cover[cover$./10000 > 1,]<- 1*10000
Crown.scales[Crown.scales$. > 100,]<-100

##########
#plotting#
##########


#for precipitaiton and tree cover metrics
x11(width = 8)
pdf('outputs/inil_cover_ghcn.pdf')
#par(xpd=TRUE)
plot(precip$total_.,cover$./10000, col=ifelse(cover$./10000>=0.45,"red","black"), ylab = 'porportion cover', xlab = '1900 prism precipitation (mm)')
rect(1400,-10, 1000,2, col = rgb(0.75,0.75,0.75,1/4)) # add the range of tropical savanna intermediate climate
rect(1400, -10, 725, 2, col = rgb(red = 1, 0, 0, alpha = 1/4))
rect(1400,-10, 1000,2, col = rgb(0.75,0.75,0.75,1/2)) # add the range of tropical savanna intermediate climate
legend("topleft", 
       cex = 1, 
       bty = "n", 
       legend = c("Midwest intermediate climate", "Tropical intermediate", '>40% cover', '<40% cover'), 
       
       col = c(rgb(red = 1, 0, 0, alpha = 1/4), rgb(0.75,0.75,0.75,1/2), 'red', 'black'), 
       pch = c(15,15,1,1))

hist(CC.adj$., breaks = 50, xlab = "porportion cover", main = 'Histogram of porporiton of cover')

plot(precip$total_.,Crown.scales$.,  col=ifelse(Crown.scales$.>=45,"red","black"),ylab = '% cover', xlab = '1900 prism precipitation (mm)')
rect(1400,-10, 1000,200, col = rgb(0.5,0.5,0.5,1/4))
rect(1400, -10, 725, 200, col = rgb(red = 1, 0, 0, alpha = 1/4))
rect(1400,-10, 1000,200, col = rgb(0.75,0.75,0.75,1/2)) # add the range of tropical savanna intermediate climate
legend("topleft", 
       cex = 1,  
       bty = "n", 
       legend = c("Midwest intermediate climate", "Tropical intermediate", '>40% cover', '<40% cover'), 
       
       col = c(rgb(red = 1, 0, 0, alpha = 1/4), rgb(0.75,0.75,0.75,1/2), 'red', 'black'), 
       pch = c(15,15,1,1))

hist(Crown.scales$., breaks = 55, xlab = '% cover', main = 'Histogram of % Cover')


#25 trees per acre is savanna limit
#25 trees per acre = 25 trees/acre / 0.404686 hectares /acre = 61.77629 trees /hectare
plot(precip$total_.,tree.dens$.,  col=ifelse(tree.dens$.>=61,"red","black"),ylab = 'Tree Density (stems/ha)', xlab = "Prism 1900 precipitaiton (mm)")
rect(1400,-10, 1000,300, col = rgb(0.5,0.5,0.5,1/4))
rect(1400, -10, 725, 300, col = rgb(red = 1, 0, 0, alpha = 1/4))
rect(1400,-10, 1000,300, col = rgb(0.75,0.75,0.75,1/2)) # add the range of tropical savanna intermediate climate
legend("topleft", 
       cex = 1, 
       bty = "n", 
       legend = c("Midwest intermediate climate", "Tropical intermediate", '>60 trees/ha cover', '<60trees/ha'), 
       col = c(rgb(red = 1, 0, 0, alpha = 1/4), rgb(0.75,0.75,0.75,1/2), 'red', 'black'), 
       pch = c(15,15,1,1))

hist(tree.dens$.)
hist(tree.dens$., breaks = 25, xlab = "density (stems/ha)", main = 'Histogram of Density')

hist(basal$., breaks = 50, xlab = 'basal area (m^2/m^2)', main = 'Histogram of Basal Area')

#plot(avg.prism.p$., basal$.)
plot(precip$total_., basal$., ylim = c(0, 100), col=ifelse(basal$.>=40,"red","black"), ylab = 'Basal area (m^2/m^2)', xlab = 'GHCN 1900-1910 precipitation')
rect(1200,-10, 1000,300, col = rgb(0.5,0.5,0.5,1/4))
rect(1200, -10, 725, 300, col = rgb(red = 1, 0, 0, alpha = 1/4))
rect(1200,-10, 1000,300, col = rgb(0.75,0.75,0.75,1/2)) # add the range of tropical savanna intermediate climate
legend("topleft", 
       cex = 1, 
       bty = "n", 
       legend = c("Midwest intermediate climate", "Tropical intermediate", '>40 m^/m2 BA', '<40% m^2/m^2 BA'), 
       col = c(rgb(red = 1, 0, 0, alpha = 1/4), rgb(0.75,0.75,0.75,1/2), 'red', 'black'), 
       pch = c(15,15,1,1))

plot(air_temp$range_., tree.dens$.,col=dens.discrete$bins, ylab = 'Tree density (stems/ha)', xlab = 'GHCN mean annual Temp range')

plot(air_temp$annual_., tree.dens$.,col=dens.discrete$bins, ylab = 'Tree density (stems/ha)', xlab = 'GHCN mean annual Temp')

plot(PET$total_., dens.discrete$.,col= dens.discrete$bins, ylab = 'Tree density (stems/ha)', xlab = 'GHCN annual P-ET')

plot(PET$Jul_., tree.dens$.,col=dens.discrete$bins, ylab = 'Tree density (stems/ha)', xlab = 'GHCN annual P-ET')

plot(PET$Jun_., tree.dens$.,col=ifelse(tree.dens$.>=53,"red","black"), ylab = 'Tree density (stems/ha)', xlab = 'GHCN annual P-ET')

plot(PET$May_., tree.dens$.,col=ifelse(tree.dens$.>=53,"red","black"), ylab = 'Tree density (stems/ha)', xlab = 'GHCN annual P-ET')

plot(PET$Apr_., tree.dens$.,col=ifelse(tree.dens$.>=53,"red","black"), ylab = 'Tree density (stems/ha)', xlab = 'GHCN annual P-ET')

plot(PET$Aug_., tree.dens$.,col=ifelse(tree.dens$.>=53,"red","black"), ylab = 'Tree density (stems/ha)', xlab = 'GHCN annual P-ET')

plot(PET$Sep_., tree.dens$.,col=ifelse(tree.dens$.>=53,"red","black"), ylab = 'Tree density (stems/ha)', xlab = 'GHCN annual P-ET')

plot(PET$Oct_., tree.dens$.,col=ifelse(tree.dens$.>=53,"red","black"), ylab = 'Tree density (stems/ha)', xlab = 'GHCN annual P-ET')

plot(Eo150$annual_., tree.dens$.,col=ifelse(tree.dens$.>=53,"red","black"), ylab = 'Tree density (stems/ha)', xlab = 'GHCN annual Potential ET')

plot(E150$annual_., tree.dens$.,col=ifelse(tree.dens$.>=53,"red","black"), ylab = 'Tree density (stems/ha)', xlab = 'GHCN annual ET')


dev.off()





#for now we focus on density and climate space

#assign discrete values to density
dens.discrete <- tree.dens
dens.discrete$bins <- dens.discrete$.
dens.discrete[dens.discrete$.>80, ]$bins <- '1'
dens.discrete[dens.discrete$.<=53, ]$bins <- '2'
dens.discrete[dens.discrete$.<=80 & dens.discrete$. >53, ]$bins <- 3

#plot air temp annnual and precip annual
plot( air_temp$annual_., precip$total_.,col= as.numeric(dens.discrete$bins), main = ' Climate space of midwest GHCN 1900-1910')
legend('topleft', col = as.numeric(dens.discrete$bins))

#plot p-ET annual and annual temperature range
plot(air_temp$range_., PET$total_., col = dens.discrete$bins)


tropics <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/1247355_Dataset_S1.csv')

plot(tropics$Annual.temperature.range/10, tropics$Effective.Rainfall, 
     ylab = 'Effective rainfall (mm)', xlab = 'Annual temperature range (DegC)')
points(air_temp$range_., PET$total_., col = 'red')
legend('bottomleft', pch =1, col = c('black', 'red'), c('Tropics', 'midwest'))
points(air_temp$range_., PET$total_., col = 'red')
# the mean annual precipitation is not listed in the dataset
hist(tropics$Tree.Basal.Area..m2.ha.)
hist(basal$., breaks = 50)



#maps of where tree.dens is 
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois", "indiana" ) )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))


mapdata<-data.frame(mapdata)

#make the map in GGPLOT

dens.map <- ggplot()+ geom_raster(data=tree.dens, aes(x=x, y=y, fill = .))+
  labs(x="easting", y="northing", title="Tree raw density") + 
  scale_fill_gradientn(colours = rainbow(4), name ="Tree Dens. \n (stems/ha)")
#dens.map <- sites.map + geom_point(data = priority, aes(x = coords.x1, y = coords.x2, shape = Description))+geom_text_repel(data = priority,aes(x = coords.x1, y = coords.x2,label=code))
dens.map <- dens.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "Black", fill = NA)
dens.map


dens.sd.map <- ggplot()+ geom_raster(data=tree.dens.sd, aes(x=x, y=y, fill = .))+
  labs(x="easting", y="northing", title="Tree density standard deviation") + 
  scale_fill_gradientn(colours = rainbow(3), name ="Tree Dens. sd ")
#dens.map <- sites.map + geom_point(data = priority, aes(x = coords.x1, y = coords.x2, shape = Description))+geom_text_repel(data = priority,aes(x = coords.x1, y = coords.x2,label=code))
dens.sd.map <- dens.sd.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "Black", fill = NA)
dens.sd.map

sd.points <- ggplot(data=dens.discrete, aes(x=tree.dens$., y = tree.dens.sd$., color= type)) + geom_point()+
  xlab('Tree density (stems/ha)')+ylab('Standard deviation of tree density')

dens.discrete$type <- dens.discrete$bins
dens.discrete[dens.discrete$type == 1, ]$type <- '>85 stems/ha'
dens.discrete[dens.discrete$type == 2, ]$type <- '<53 stems/ha'
dens.discrete[dens.discrete$type == 3, ]$type <- '53-85 stems/ha'

dens.dis.map <- ggplot()+ geom_raster(data=dens.discrete, aes(x=x, y=y, fill =type))+
  labs(x="easting", y="northing", title="Tree dens. \n binned") #+ 
 # scale_fill_gradientn(colours = rainbow(4), name ="Tree Density (stems/ha)")
#dens.map <- sites.map + geom_point(data = priority, aes(x = coords.x1, y = coords.x2, shape = Description))+geom_text_repel(data = priority,aes(x = coords.x1, y = coords.x2,label=code))
dens.dis.map <- dens.dis.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "Black", fill = NA)
dens.dis.map

#put all these into pdf document
pdf('C:/Users/JMac/Documents/Kelly/biomodality/outputs/density.maps.pdf')
dens.hist <- ggplot(data=dens.discrete, aes(x=., fill=type) )+
  geom_histogram(binwidth=5, alpha=.5, position="identity") + xlab('Tree density (stems/ha)') 

ggplot(data=dens.discrete, aes(x=precip$total_., y = ., color=type)) + geom_point() +
  xlab('Mean annual precip. (mm)') + ylab('Density (stems/ha)')

ggplot(data=dens.discrete, aes(x=air_temp$annual_., y = ., color=type)) + geom_point() +
  xlab('Annual Air Temp (DegC)') + ylab('Density (stems/ha)')

ggplot(data=dens.discrete, aes(x=PET$total_., y = ., color=type)) + geom_point() +
  xlab('P-ET (mm)') + ylab('Density (stems/ha)')

ggplot(data=dens.discrete, aes(x= E150$annual_., y = ., color=type)) + geom_point() +
  xlab('Mean annual PET') + ylab('Density (stems/ha)')

ggplot(data=dens.discrete, aes(x=Eo150$annual_., y = ., color=type)) + geom_point() +
  xlab('Mean annual ET)') + ylab('Density (stems/ha)')

ggplot(data=air_temp, aes(x=precip$total_., y = air_temp$annual_., color=dens.discrete$type)) + geom_point() +
  xlab('Mean annual precip. (mm)') + ylab('Annual air temp. (DegC)')

ggplot(data=air_temp, aes(x=PET$total_., y = air_temp$range_., color=dens.discrete$type)) + geom_point() +
  xlab('Effective rainfall. (mm)') + ylab('Annual Air temperature range (DegC)')

plot(tropics$Annual.temperature.range/10, tropics$Effective.Rainfall, 
     ylab = 'Effective rainfall (mm)', xlab = 'Annual temperature range (DegC)')
points(air_temp$range_., PET$total_., col = 'red')
legend('bottomleft', pch =1, col = c('black', 'red'), c('Tropics', 'midwest'))
points(air_temp$range_., PET$total_., col = 'red')


sd.points
dens.map
dens.dis.map
dens.hist
dens.sd.map
dev.off()


# based on this, I expect tree cover to be bimodal above 725 mm/year (1900-1910), with GHCN data, and 850 with 1895-1905 prism data
# the low frequency intermediate of density occurs from 50-85 stems/ha