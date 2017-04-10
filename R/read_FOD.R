# reading in the FPA-FOD fire database:
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

# find average fire size per 8km grid
base.rast <- raster(xmn = -71000, xmx = 2297000, ncols=296,
                    ymn = 58000,  ymx = 1498000, nrows = 180,
                    crs = '+init=epsg:3175')

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

# we could average 
# View the feature class
#plot(fc)