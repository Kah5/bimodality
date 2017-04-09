# reading in the FPA-FOD fire database:
# the original database had issues being read in...
# I first created a new shapefile with only MI, MN, WI, IL, IN. Resulting shapefile = "UMW_FIRES.shp


require(rgdal)

# The input file geodatabase
file = "C:/Users/JMac/Documents/Kelly/biomodality/data/UMW_FIRES/UMW_FIRES.shp"


# Read the feature class
fc = readOGR(dsn=file,layer="UMW_FIRES")

# project in great lakes albers
fc.alb<- spTransform(fc, CRSobj = '+init=epsg:3175')

# Determine the extent, projection, and attribute information
summary(fc.alb)

fires <- as.data.frame(fc.alb)
summary(fires$FIRE_SIZE)

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

png("outputs/proportion_byfirecause.png")
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
png("outputs/proportion_byfiresize.png")
ggplot(bysize, aes(x= sizes, y = freq))+geom_bar(stat = "identity")+theme_bw()+
  ylab("Proportion of total Upper Midwest Fires \n 1992 - 2011") + xlab("Size of Fire (acres)")
dev.off()


# plot the size by the fire class
test <- count(df = fires, vars = c('FIRE_SIZE_', 'STAT_CAU_1'))
test$pct <- test$freq/82205
test <- merge(test, sizelookup, by= "FIRE_SIZE_")

png("outputs/proportion_by_fire_size__and_cause.png")
ggplot(test, aes(sizes, y = pct, fill = STAT_CAU_1))+geom_bar(stat="identity")+theme_bw()+
  xlab("Fire Class") + ylab("Proportion of fires by size and cause 1992 - 2011") + guides(fill=guide_legend(title="Cause"))
dev.off()

# find average fire size per grid scale
MW <- raster("data/upper_midwest.tif")


# there are 40 points with very large fire size, not sure if any of these might be NA values?


# we could average 
# View the feature class
#plot(fc)