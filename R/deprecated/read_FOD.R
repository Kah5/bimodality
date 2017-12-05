# Working with the FPA FOD dataset
# Author: kheilman

#-----------------------------------------------------
#Analyses in this script:
#-----------------------------------------------------
# 1. Are fires large or small on the modern landscape?
# 2. how often do they occur?
# 3. Are they caused by anthropogenic/natural causes
# 4. Do the past bimodal places have more fires than past stable places? ** key question

# ----------------------------------------------------
# datasets used: 
# 1. PLS output from PLS_density_processing.R
# 2. Midwestern selection of FPA-FOD fire database version 1: https://www.fs.usda.gov/rds/archive/Product/RDS-2013-0009


# reading in the FPA-FOD fire database version 1:
# the original database had issues being read in...
# I first created a new shapefile with only MI, MN, WI, IL, IN. Resulting shapefile = "UMW_FIRES.shp


require(rgdal)
library(raster)
library(ggplot2)

# The input file geodatabase
file = "C:/Users/JMac/Documents/Kelly/biomodality/data/UMW_FIRES/UMW_FIRES.shp"


# Read the feature class
fc = readOGR(dsn=file,layer="UMW_FIRES")

# project in great lakes albers
fc.alb <- spTransform(fc, CRSobj = '+init=epsg:3175')

# Determine the extent, projection, and attribute information
summary(fc.alb)
#-383250.7, 1258096, 69207.67, 1477623  (xmin, xmax, ymin, ymax)

fires <- as.data.frame(fc.alb)
summary(fires$FIRE_SIZE)
ggplot(fires, aes(coords.x1, coords.x2, color= FIRE_SIZE_))+geom_point()

# convert fire size to square km
fires$FIRE_SIZE <- fires$FIRE_SIZE * 247.105 
summary(fires$FIRE_SIZE)

hist(fires$FIRE_SIZE, xlim = c(0,1000),breaks = 1000000)

summary(fires[fires$FIRE_SIZE >= 22902201,])
length(fires[fires$FIRE_SIZE >= 22902201,])


#looking at fire causes:
library(plyr)
fires$STAT_CAU_1 <- as.character(fires$STAT_CAU_1)
bycause <- count(fires$STAT_CAU_1)
bycause$freq <- bycause$freq/82205

png("outputs/fire/proportion_byfirecause.png")
ggplot(bycause, aes(x= x, y = freq))+geom_bar(stat = "identity")+theme_bw()+
  ylab("Porportion of total Upper Midwest Fires \n 1992 - 2011") + xlab("Cause of fire")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

# plot the fire class 

fires$FIRE_SIZE_ <- as.character(fires$FIRE_SIZE_)
bysize <- count(fires$FIRE_SIZE_)
bysize$freq <- bysize$freq/82205

sizelookup <- data.frame(FIRE_SIZE_ = c("A", "B", "C", "D", "E", "F", "G"),
                sizes = c("0 - 0.25", "0.26 - 9.9", "10.0-99.9", "100-299",
                          "300 - 999", "1000 - 4999", "1000 - 4999"))

bysize <- merge(bysize, sizelookup, by.x = "x",by.y = "FIRE_SIZE_")

png("outputs/fire/proportion_byfiresize.png")
ggplot(bysize, aes(x= sizes, y = freq))+geom_bar(stat = "identity")+theme_bw()+
  ylab("Proportion of total Upper Midwest Fires \n 1992 - 2011") + xlab("Size of Fire (acres)")
dev.off()


# plot the size by the fire class
test <- count(df = fires, vars = c('FIRE_SIZE_', 'STAT_CAU_1'))
test$pct <- test$freq/82205
test <- merge(test, sizelookup, by= "FIRE_SIZE_")

png("outputs/fire/proportion_by_fire_size__and_cause.png")
ggplot(test, aes(sizes, y = pct, fill = STAT_CAU_1))+geom_bar(stat="identity")+theme_bw()+
  xlab("Fire Class") + ylab("Proportion of fires by size and cause 1992 - 2011") + guides(fill=guide_legend(title="Cause"))
dev.off()

#---------------------------------------------------------
# working on the scale of the 8km grid
#--------------------------------------------------------
# find average fire size per 8km grid
base.rast <- raster(xmn = -71000, xmx = 2297000, ncols=296,
                    ymn = 58000,  ymx = 1498000, nrows = 180,
                    crs = '+init=epsg:3175')



numbered.rast <- setValues(base.rast, 1:ncell(base.rast))

#coordinates(fires) <- ~coords.x1 + coords.x2

# add paleon cells to the fire data
fc.alb$cell <- extract(x = numbered.rast, y = fc.alb)
fc.alb <- data.frame(fc.alb)

fc.alb[,40:41]<- data.frame(xyFromCell(numbered.rast, fc.alb$cell))
colnames(fc.alb[,40:41])<- c("x", "y")
fc.alb$count <- 1 #
#fc.alb$STAT_CAU_1

# remove the one point that is outside of the region...
fc.alb <- fc.alb[!is.na(fc.alb$cell),]

#map out states for plotting later
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)


# map out the total number of fires for each cause by Paleon grid cell
library(reshape2)
cause.table <- dcast(fc.alb, coords.x1 + coords.x2 + cell ~ STAT_CAU_1, sum, na.rm=TRUE, value.var = 'count')
cause.table$Total <- rowSums(cause.table[,4:16])

ggplot(cause.table, aes(x = coords.x1, y = coords.x2, fill = Total))+geom_raster()+coord_equal()+theme_bw()

cause.m <- melt(cause.table, id.vars = c("coords.x1", "coords.x2", "cell"), variable.name = "FireCause", value.name = "Countfires")

png("outputs/fire/map_causes.png")
ggplot(cause.m, aes(x = coords.x1, y = coords.x2, fill = Countfires))+geom_raster()+coord_equal()+theme_bw()+facet_wrap(~FireCause)+
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,50))+theme(axis.text = element_blank())
dev.off()


# map out the number of fires within each size class in each grid cell
size.table <- dcast(fc.alb, coords.x1 + coords.x2 + cell ~ FIRE_SIZE_, sum, na.rm=TRUE, value.var = 'count')
#size.table$Total <- rowSums(size.table[,4:10])
#ggplot(size.table, aes(x = coords.x1, y = coords.x2, fill = Total))+geom_raster()+coord_equal()+theme_bw()

size.m <- melt(size.table, id.vars = c("coords.x1", "coords.x2", "cell"), variable.name = "FireSize", value.name = "Countfires")

png("outputs/fire/map_sizes.png")
ggplot(size.m, aes(x = coords.x1, y = coords.x2, fill = Countfires))+geom_raster()+coord_equal()+theme_bw()+facet_wrap(~FireSize)+
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,50))+theme(axis.text = element_blank())
dev.off()



# Find the average size of fires in each grid cell:

size.avgs <- dcast(fc.alb, coords.x1 + coords.x2 + cell ~ ., mean, na.rm=TRUE, value.var = 'FIRE_SIZE')
colnames(size.avgs)<- c('x','y',"cell","MeanSize")

ggplot(size.avgs, aes(x = x, y = y, fill = MeanSize))+geom_raster()+coord_equal()+theme_bw()+
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,2000))+theme(axis.text = element_blank())


# calculate the # fires per year per grid cell
fireperyr <- dcast(fc.alb, coords.x1 + coords.x2 + cell ~ FIRE_YEAR, sum, na.rm=TRUE, value.var = 'count')


year.m <- melt(fireperyr, id.vars = c("coords.x1", "coords.x2", "cell"), variable.name = "Year", value.name = "Countfires")

X11(width = 12)
ggplot(year.m, aes(x = coords.x1, y = coords.x2, fill = Countfires))+geom_raster()+coord_equal()+theme_bw()+facet_wrap(~Year)+
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,50))+theme(axis.text = element_blank())

# calculate a Fire Rotation per grid cell (this is probably too big of an area)

size.tots <- dcast(fc.alb, coords.x1 + coords.x2 + cell ~ ., sum, na.rm=TRUE, value.var = 'FIRE_SIZE')
colnames(size.tots)<- c('x','y',"cell","TotalAreaBurned")
summary(size.tots$TotalAreaBurned/20)
size.tots$AvgAreaBurnedYr <- size.tots$TotalAreaBurned/20 # avg area burned per year fire record
size.tots$FireRotation <- 64/size.tots$AvgAreaBurnedYr # 64 km-sq area grid cell / average km-sq buned/yr = years to burn whole grid cell


ggplot(size.tots, aes(x = x, y = y, fill = FireRotation))+geom_raster()+coord_equal()+theme_bw()+
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,5000))+theme(axis.text = element_blank())

# there are alot of high Fire Rotations, lets classify
size.tots$FRdiscrete <- cut(size.tots$FireRotation, labels = c('0-1', '1-5', '5-10', '10-50', '50-100','100-500','500-1000','1000-2000',"> 2000"),
    breaks=c(0,1,5,10,50,100,500,1000,2000,128000))



# map out the calculated fire rotation for these grid cells
png(width=4,height=4,units="in", res=300, "outputs/paper_figs/Fire_rotation_map.png")
p <- ggplot(size.tots, aes(x = x, y = y, fill = FRdiscrete))+geom_raster()+coord_equal()+theme_bw()+
  scale_fill_manual(values = rev(c('#ffffcc',
    '#ffeda0',
    '#fed976',
    '#feb24c',
    '#fd8d3c',
    '#fc4e2a',
    '#e31a1c',
    '#bd0026',
    '#800026')))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  theme(axis.text = element_blank())+ xlab(' ')+ylab(' ')+ggtitle("Fire Rotation (# years to burn area of gridcell)")+
  guides(fill=guide_legend(title="Fire Rotation \n (years)"))+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.4,'lines'), legend.position = c(0.205, 0.267),legend.background = element_rect(fill=alpha('transparent', 0)),
                                                                    panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))+ggtitle("")

p
dev.off()


# read in the pls bimodal dataset
bimodalpls <- read.csv("outputs/PLS_full_dens_pr_bins_with_bimodality_for_PC1.csv") 

# merge datasets together
plsfire <- merge(bimodalpls, size.tots, by= c('x','y','cell'))

#plot out summary of stable  and bimodal gridcells
summary(plsfire[plsfire$bimodal == 'Bimodal',]$TotalAreaBurned)
summary(plsfire[plsfire$bimodal == 'Stable',]$TotalAreaBurned)

summary(plsfire[plsfire$classification == 'Bimodal Forest',]$TotalAreaBurned)
summary(plsfire[plsfire$classification == 'Stable Forest',]$TotalAreaBurned)


# -------------------------------------------------------------
# Do the stable places burn less now than the bimodal places??
# -------------------------------------------------------------


plsfire.m <- melt(plsfire[, c("x", "y", "cell", "TotalAreaBurned", "classification")], id.vars = c("x", "y", "cell"), variable.name = "classification", value.name = "TotalAreaBurned")

# plot the total area burned in each class
ggplot(plsfire, aes(x= classification, y = TotalAreaBurned))+geom_bar(stat = "identity")+theme_bw()+
  ylab("Total area burned in each class Upper Midwest Fires \n 1992 - 2011 (acres)") + xlab("Class of grid cell in PLS")

# but we would want the area burned normalized by the total area in each class...

area.burn.avg <- ddply(plsfire[, c("x", "y", "cell", "TotalAreaBurned", "classification")],~classification,summarise,mean=mean(TotalAreaBurned),sd=sd(TotalAreaBurned))
area.burn.avg.bimodal <- ddply(plsfire[, c("x", "y", "cell", "TotalAreaBurned", "bimodal")],~bimodal,summarise,mean=mean(TotalAreaBurned),sd=sd(TotalAreaBurned))

ggplot(plsfire, aes(x= classification, y = TotalAreaBurned))+geom_boxplot()

sem<-sd(x)/sqrt(length(x))
#95% confidence intervals of the mean
c(mean(x)-2*sem,mean(x)+2*sem)

# fire rotation averages (how many years it would take to burn entire grid cell)
FR.avg <- ddply(plsfire[, c("x", "y", "cell", "FireRotation", "classification")],~classification,summarise,mean=mean(FireRotation),sd=sd(FireRotation), se = sd(FireRotation)/sqrt(length(FireRotation)))
FR.avg.bimodal <- ddply(plsfire[, c("x", "y", "cell", "FireRotation", "bimodal")],~bimodal,summarise,mean=mean(FireRotation),sd=sd(FireRotation))

FR.avg$bimodal <- c('Bimodal', 'Bimodal', 'Savanna', 'Stable', 'Stable')

# make error bars as SEM
limits <- aes(ymax = FR.avg$mean + FR.avg$se,
             ymin = FR.avg$mean - FR.avg$se)

png(width = 8, height= 4, units = 'in', res=300, 'outputs/fire/FR_mean_by_bimodal_.png')
ggplot(FR.avg, aes(x= classification, y = mean, fill=bimodal))+geom_bar(stat = "identity") +
  geom_errorbar(aes(ymax = FR.avg$mean + FR.avg$se,
                    ymin = FR.avg$mean - FR.avg$se), width = 0.25) +ylab("Mean Fire Rotation (years)") +xlab("")+theme_bw()
dev.off()

# are these statistically significant?

anova(lm(plsfire$FireRotation ~ plsfire$classification))
