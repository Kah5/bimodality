#Kelly Heilman 
#October 7th, 2015
#this script is intended as a first pass at estimating crown or % covere in the pls data
#this script  uses 'final.data' produced in step_one_clean_IN.r
library(plyr)
library(reshape2)
library(ggplot2)
read.csv("data/pointwise.ests_v1.5.RDS")
#but perhaps we should do this by township to better estimate?

crown.full$avg <- rowMeans(crown.full[,4:34], na.rm=TRUE)
density.full$avg <- rowMeans(density.full[,4:34], na.rm=TRUE)
density.full$total <- rowSums(density.full[,4:34], na.rm=TRUE)

cover <- density.full$total/10000*crown.full$avg

hist(cover)
summary(cover*10000/64000000)
hist(cover*10000/(8000*8000))
cover.oaks <- density.full$total*crown.full$Oak


final.data<- read.csv("ndilinpls_for_density_v1.5.csv")
correction.factor <- read.csv("Data//correction_factors.csv", header = TRUE)

## Morisita estimates for indiana densities and basal area with charlies correction factors
# & no diameter veil
source('R/morisita.r')

#make sure density is really being calculated correctly in morisita
estimates <- morisita(final.data, correction.factor, veil = FALSE)

stem.density <- estimates[[1]]
basal.area <- estimates[[2]]
summary(stem.density)
summary(basal.area)
zero.trees <- is.na(stem.density) & (species[,2] %in% c('No tree', 'Water', 'Wet') | species[,1] %in% c('No tree', 'Water', 'Wet'))

#set stem.density where there are zero trees due to No tree or Wet or Water to 0
stem.density[zero.trees] <- 0
basal.area[zero.trees] <- 0

summary(stem.density)
summary(basal.area)

stem.density <- data.frame(stem.density, basal.area, final.data)
#write.csv(stem.density, 'IN_ILdensestimates_v1.5.csv')



b0 <- 0.529302
b1 <- 0.137036

crown.area.1 <- pi*((b0 + b1 * stem.density$diam1)^2) # assuming diams in cm
crown.area.2 <- pi*((b0 + b1 * stem.density$diam2)^2)

stem.density$ca1 <- crown.area.1
stem.density$ca2 <- crown.area.2
stem.density$ca.mean <- (crown.area.1 +crown.area.2)/2
stem.density$ca.total <- crown.area.1 +crown.area.2
stem.density$ca.total.8km <- (stem.density$ca1+stem.density$ca2)/(10000)


coordinates(stem.density)<- ~PointX+PointY

proj4string(stem.density)<-CRS('+init=epsg:3175')

numbered.rast <- setValues(base.rast, 1:ncell(base.rast))
numbered.cell <- extract(numbered.rast, spTransform(stem.density,CRSobj=CRS('+init=epsg:3175')))

#species[species==""]<- "No tree" #gets rid of blank listing for no trees

#stem.dens.agg <- 
  stem.density <- data.frame(stem.density)
table <- data.frame(xyFromCell(base.rast, numbered.cell),
                        count = 1,
                         cell = numbered.cell,
                         stem.density,
                         stringsAsFactors = FALSE)
table$stem.density.m <- table$stem.density*6400 #this converts stems  per hectare to stems per grid cell
table$stem.density.8km <- (table$stem.density*6400)

table$ca.pt.i <- table$stem.density.8km*table$ca.total.8km # what each corner point takes up within grid cell

ca.totals<- ddply(table,~ cell,  summarise,  x = mean(x), y= mean(y), mean.ca = mean(ca.total, na.rm=TRUE), mean.dens=mean(stem.density, na.rm=TRUE), 
                  mean.dens.m = mean(stem.density.m, na.rm=TRUE), ca.dens.pt = mean((ca.total*stem.density.m), na.rm=TRUE), 
                  sum.ca = sum(ca.pt.i, na.rm=TRUE),
                  mean.ca = mean(ca.pt.i, na.rm=TRUE),
                  mean.ca2 = mean(ca.total, na.rm=TRUE),
                  total.pts = sum(count, na.rm= TRUE))


#ca.totals$uncorrected.cover<- (((ca.totals$mean.ca*ca.totals$mean.dens)*6400)/(8000*8000))*100

ca.totals$uncorrected.cover<- (((ca.totals$mean.ca2*ca.totals$mean.dens.m)/(8000*8000))*100

#uncorrected.cover<- 100*(ca.totals$sum.ca)/(8000*8000)

#ca.totals$pct.cov <- ca.totals$mean.ca*ca.totals$mean.dens/10000
#ca.totals$dens.8km <- ca.totals$mean.dens.m

#FIA correctiong for overlapping trees
ca.totals$C <- 100*(1-exp(-0.01*ca.totals$uncorrected.cover))
pdf("percent.cover.pdf")
par(mfrow=c(2,1))

hist(ca.totals$uncorrected.cover, breaks = 25, main = "Uncorrected Cover", xlab = "%cover")
hist (ca.totals$C, breaks = 25, main = "Corrected Cover", xlab = "Corrected % Cover")

dev.off()
coordinates(ca.totals) <- ~x+y
spplot(ca.totals, "C")

# make a better plot than this
ca.totals<- data.frame(ca.totals)

library(maps)
#load us map data
all_states <- map_data("state")
states <- subset(all_states, region %in% c( "indiana" , "illinois" ) )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS(great_lakes))
mapdata<-data.frame(mapdata)

pdf("crown_maps.pdf")
ca.map <- ggplot() +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group))+ scale_x_continuous(limits = c(250000, 1000000)) +
  scale_y_continuous(limits=c(0, 800000))
ca.map <- ca.map + geom_point(data=ca.totals, aes(x=x, y=y, color = C)) + labs(x="easting", y="northing", title="PLS average C in Indiana and Illinois") + scale_color_gradientn(colours = rainbow(4), name ="Total density")
ca.map

ca.un.map <- ggplot() +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group))+ scale_x_continuous(limits = c(250000, 1000000)) +
  scale_y_continuous(limits=c(0, 800000))
ca.un.map <- ca.un.map + geom_point(data=ca.totals, aes(x=x, y=y, color = uncorrected.cover)) + labs(x="easting", y="northing", title="PLS uncorrected C in Indiana and Illinois") + scale_color_gradientn(colours = rainbow(4), name ="Total density")
ca.un.map
dev.off()

#ca.totals$pct.cover.m2 <- ca.totals$mean.dens.m * ca.totals$mean.ca
#ca.totals$pct.cover.ha <- ca.totals$mean.ca * ca.totals$mean.dens 

#pct.cover[pct.cover>1] <- 1
#assume that if pct.cover is over 1 that it is = 100%


##aggregated or not?
# need P --point to tree distances, and I--tree to tree distances
