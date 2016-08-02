#Kelly Heilman 
#April 19th, 2016
#this script is intended as a first pass at estimating crown or % covere in the pls data
#this script  uses 'final.data' produced in step_one_clean_IN.r
library(plyr)
library(reshape2)
library(ggplot2)
library(aspace)

final.data <- read.csv("ndilinpls_for_density_v1.5.csv")

###need to make sure that the distances to trees are in meters

final.data$TreeX1 <- final.data$PointX 
final.data$TreeY1 <- final.data$PointY 
final.data$TreeX2 <- final.data$PointX 
final.data$TreeY2 <- final.data$PointY 


correction.factor <- read.csv("Data//correction_factors.csv", header = TRUE)

## Morisita estimates for indiana densities and basal area with charlies correction factors
# & no diameter veil
source('R/morisita.r')

#make sure density is really being calculated correctly in morisita
estimates <- morisita(final.data, correction.factor, veil = FALSE)

stem.density <- estimates[[1]]
basal.area <- estimates[[2]]
plot.area <- estimates[[3]]
summary(stem.density)
summary(basal.area)
zero.trees <-  (species[,2] %in% c('No tree', 'Water', 'Wet') | species[,1] %in% c('No tree', 'Water', 'Wet'))

#set stem.density where there are zero trees due to No tree or Wet or Water to 0
stem.density[zero.trees] <- 0
basal.area[zero.trees] <- 0

summary(stem.density)
summary(basal.area)

stem.dens <- data.frame(stem.density, basal.area)
#write.csv(stem.density, 'IN_ILdensestimates_v1.5.csv')

nine.nine.pct <- apply(stem.dens, 2, quantile, probs = 0.99, na.rm=TRUE)
stem.dens$stem.density[stem.dens$stem.density > nine.nine.pct['stem.density']] <- nine.nine.pct['stem.density']
stem.dens$basal.area[stem.dens$basal.area > nine.nine.pct['basal']] <- nine.nine.pct['basal']





##create base raster that is extent of midwest domain

base.rast <- raster(xmn = -71000, xmx = 2297000, ncols=296,
                    ymn = 58000,  ymx = 1498000, nrows = 180,
                    crs = '+init=epsg:3175')
coordinates(final.data)<- ~PointX+PointY

#create spatial object with density, basal area & diameters data
density <- SpatialPointsDataFrame(coordinates(final.data), 
                                       data=data.frame(density = estimates[[1]],
                                                       basal   = estimates[[2]],
                                                       diams = rowMeans(diams[,1:2], na.rm=TRUE) * 2.54))

proj4string(density)<-CRS('+init=epsg:3175')
numbered.rast <- setValues(base.rast, 1:ncell(base.rast))
numbered.cell <- extract(numbered.rast, spTransform(density,CRSobj=CRS('+init=epsg:3175')))

species[species==""]<- "No tree" #gets rid of blank listing for no trees

final.data <- data.frame(final.data)
#create dataframe with stem density, speceies
spec.table <- data.frame(xyFromCell(base.rast, numbered.cell),
                         cell = numbered.cell,
                         PointX = final.data$PointX, 
                         PointY = final.data$PointY,
                         TreeX = c(final.data$TreeX1, final.data$Treex2),
                         TreeY = c(final.data$TreeY1, final.data$TreeY2),
                         spec = c(as.character(final.data$species1), as.character(final.data$species2)),
                         density = rep(stem.dens$stem.density/2, 2),
                         basal =  rep(stem.dens$basal.area/2, 2),
                         plot.area = plot.area,
                         diams = c(final.data$diam1, final.data$diam2),
                         stringsAsFactors = FALSE)



#classify trees as zero or as wet trees
zero.trees <- is.na(stem.dens$stem.density) & (final.data$species1 %in% c('No tree') | final.data$species2 %in% c('No tree'))
wet.trees <- (final.data$species1 %in% c('Wet', "Water") | final.data$species2 %in% c('Wet','Water'))

#designate all zero trees as density of 0
stem.dens$stem.density[zero.trees] <- 0
stem.dens$basal.area[zero.trees] <- 0

#desgnate all wet trees as 'NA'
stem.dens$stem.density[wet.trees] <- NA
stem.dens$basal.area[wet.trees] <- NA

#fix the captalized "No tree" problem
spec.table$spec[spec.table$spec == 'No Tree'] <- 'No tree'

#change all No tree densities to 0
spec.table$density[spec.table$spec == 'No tree'] <- 0

#from global fhm allometry 
b0 <- 1.6926
b1 <- 0.177

CW <- pi*((b0 + b1 * spec.table$diams*2.54)) # diams need to be in cm

##omit the species specific allomtries for now
hist(CW, xlim = c(0,80), breaks = 40)


##this section estimates point level crown widths using density and allometic equation s 
#allometric equations from simon
CW.table <- read.csv('data/FHM_paleon_crown_allometry_coeff.csv', 
                     stringsAsFactors=FALSE)

form <- function(x){
  
  eqn <- match(x$spec, CW.table[,2])
  eqn[is.na(eqn)] <- 1  #  Sets it up for non-tree.
  
  b0 <- CW.table[eqn,3]
  b1 <- CW.table[eqn,4]
  
  CW <- (b0 + b1 * (x$diams*2.54))
  CW
}

CW <- rep(NA, nrow(spec.table))

for(i in 1:nrow(spec.table)){
  CW[i] <- form(spec.table[i,])
  cat(i,'\n')
}

summary(CW)

spec.table$CW <- CW #add crown width to the spec.table
spec.table<- spec.table[complete.cases(spec.table),] #get rid of NA values
#spec.table[spec.table$density < 1,] <- NA # make densities less than one NA

spec.table<- spec.table[complete.cases(spec.table),] #get rid of na values

nine.nine.pct <- apply(spec.table[,9:ncol(spec.table)], 2, quantile, probs = 0.99, na.rm=TRUE)
#point    density      basal      diams       biom 
#49506.0000   504.5722   520.1407    55.0000  3520.7191   
nine.five.pct <- apply(spec.table[,9:ncol(spec.table)], 2, quantile, probs = 0.95, na.rm=TRUE)
#point    density      basal      diams       biom 
#47511.0000   207.6151   217.5150    55.0000   930.0320 

#assign all species greater than the 99th percentile to 99th percentile values
#spec.table$density[spec.table$density > nine.nine.pct['density']] <- nine.nine.pct['density']
spec.table$basal[spec.table$basal > nine.nine.pct['basal']] <- nine.nine.pct['basal']
spec.table$CW[spec.table$CW > nine.nine.pct['CW']] <- nine.nine.pct['CW']
#spec.table$crown.scaled[spec.table$crown.scaled > nine.nine.pct['crown.scaled']] <- nine.nine.pct['crown.scaled']

spec.table$crown.area <- 0.25*pi*(spec.table$CW^2) #crown area of each tree
spec.table$ratio <- spec.table$crown.area/spec.table$plot.area
spec.table$cover <- spec.table$crown.area*spec.table$density
spec.table$crown.scaled <-100*((spec.table$crown.area*spec.table$density)/10000) #(crown area (m2))/tree)*(trees/hectare)

CC.adj <-  100*(1-exp(-0.01*spec.table$crown.scaled))
spec.table$CC.adj <- CC.adj

hist(spec.table$crown.scaled, breaks = 200, xlim = c(0,200))

hist(spec.table$cover/10000, breaks = 200)
hist(CC.adj)


#now write species table as a shapefile with the TreeX and TreeY as coordinates


coordinates(spec.table)<- ~TreeX+TreeY 
geo.tree <- SpatialPointsDataFrame(coordinates(spec.table), 
                                  data=data.frame(spec.table))

write.csv(spec.table, "C:/Users/JMac/Documents/Kelly/biomodality/outputs/spec.table.csv")

#prism<- raster("data/PRISM_1900/PRISM_ppt_stable_4kmM2_1900_asc.asc")
#prism.alb<- projectRaster(prism, crs='+init=epsg:3175')

spec.table <- data.frame(spec.table)
spec.table$prism.1900p <- extract(prism.alb, spec.table[,c("x","y")])

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

pr.alb <- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/outputs/pr_monthly_Prism_1895_1905.csv")

#lets make some plots of cover, %cover, density, and basal area
#carla staver uses a cut off of 40-45% cover for forest regions

#make all cover over 100& cover == 100
cover[cover$./10000 > 1,]<- 1*10000
Crown.scales[Crown.scales$. > 100,]<-100

x11(width = 8)
pdf('outputs/inil_cover.pdf')
#par(xpd=TRUE)
plot(pr.alb$total_.,cover$./10000, col=ifelse(cover$./10000>=0.45,"red","black"), ylab = 'porportion cover', xlab = '1900 prism precipitation (mm)')
rect(1400,-10, 1000,2, col = rgb(0.75,0.75,0.75,1/4)) # add the range of tropical savanna intermediate climate
rect(1400, -10, 850, 2, col = rgb(red = 1, 0, 0, alpha = 1/4))
rect(1400,-10, 1000,2, col = rgb(0.75,0.75,0.75,1/2)) # add the range of tropical savanna intermediate climate
legend("topleft", 
       cex = 1, 
       bty = "n", 
       legend = c("Midwest intermediate climate", "Tropical intermediate", '>40% cover', '<40% cover'), 
     
       col = c(rgb(red = 1, 0, 0, alpha = 1/4), rgb(0.75,0.75,0.75,1/2), 'red', 'black'), 
       pch = c(15,15,1,1))

hist(CC.adj$., breaks = 50, xlab = "porportion cover", main = 'Histogram of porporiton of cover')

plot(pr.alb$total_.,Crown.scales$.,  col=ifelse(Crown.scales$.>=45,"red","black"),ylab = '% cover', xlab = '1900 prism precipitation (mm)')
rect(1400,-10, 1000,200, col = rgb(0.5,0.5,0.5,1/4))
rect(1400, -10, 850, 200, col = rgb(red = 1, 0, 0, alpha = 1/4))
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
plot(pr.alb$total_.,tree.dens$.,  col=ifelse(tree.dens$.>=61,"red","black"),ylab = 'Tree Density (stems/ha)', xlab = "Prism 1900 precipitaiton (mm)")
rect(1400,-10, 1000,300, col = rgb(0.5,0.5,0.5,1/4))
rect(1400, -10, 850, 300, col = rgb(red = 1, 0, 0, alpha = 1/4))
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
plot(avg.prism.p$., basal$., ylim = c(0, 100), col=ifelse(basal$.>=40,"red","black"), ylab = 'Basal area (m^2/m^2)', xlab = 'Prism 1900 precipitation')
rect(1200,-10, 1000,300, col = rgb(0.5,0.5,0.5,1/4))
rect(1200, -10, 850, 300, col = rgb(red = 1, 0, 0, alpha = 1/4))
rect(1200,-10, 1000,300, col = rgb(0.75,0.75,0.75,1/2)) # add the range of tropical savanna intermediate climate
legend("topleft", 
       cex = 1, 
       bty = "n", 
       legend = c("Midwest intermediate climate", "Tropical intermediate", '>40 m^/m2 BA', '<40% m^2/m^2 BA'), 
       col = c(rgb(red = 1, 0, 0, alpha = 1/4), rgb(0.75,0.75,0.75,1/2), 'red', 'black'), 
       pch = c(15,15,1,1))
dev.off()







spec.table <- data.frame(spec.table)


##plot these across the region 
ggplot(data = tree.dens, aes(x = x, y = y, color = .)) + geom_point()
ggplot(data = spec.table, aes(x = PointX, y = PointY, color = CW)) + geom_point()
ggplot(data = spec.table, aes(x = PointX, y = PointY, color = basal)) + geom_point()
ggplot(data = spec.table, aes(x = PointX, y = PointY, color = crown.scaled)) + geom_point()
ggplot(data = spec.table, aes(x = PointX, y = PointY, color = CC.adj)) + geom_point()
ggplot(data = spec.table, aes(x = PointX, y = PointY, color = cover)) + geom_point()

ggplot(data = Crown.area, aes(x = x, y = y, color = .)) + geom_point()

ggplot(data = Crown.scales, aes(x = x, y = y, color = .)) + geom_point()




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

ca.totals$uncorrected.cover<- ((ca.totals$mean.ca2*ca.totals$mean.dens.m)/(8000*8000))*100

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
write.csv(spec.table, "C:/Users/JMac/Documents/Kelly/biomodality/outputs/density_tables.csv")
