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

# calculate the proportion of each fire class in the region:
A <- nrow(fires[fires$FIRE_SIZE_ %in% "A",])/nrow(fires)
B <- nrow(fires[fires$FIRE_SIZE_ %in% "B",])/nrow(fires)
C <- nrow(fires[fires$FIRE_SIZE_ %in% "C",])/nrow(fires)
D <- nrow(fires[fires$FIRE_SIZE_ %in% "D",])/nrow(fires)
E <- nrow(fires[fires$FIRE_SIZE_ %in% "E",])/nrow(fires)
f <- nrow(fires[fires$FIRE_SIZE_ %in% "F",])/nrow(fires)
G <- nrow(fires[fires$FIRE_SIZE_ %in% "G",])/nrow(fires)

pct.fireclass <- data.frame(fireclass = c("A", "B", "C", "D", "E", "F", "G"), 
                            pct = c(A, B, C, D, E, f, G), 
                            classize= c("0 - 0.25", "0.26- 9.9", "10 - 99.9", "100 - 299", 
                                        "300 - 999", "1000 - 4999", "5000+"))

ggplot(pct.fireclass, aes(x= classize, y = pct))+geom_bar(stat = "identity")+theme_bw()+
    ylab("Porportion of total Upper Midwest Fires 1992 - 2011") + xlab("Fire size class (acres)")


# these very large fires seem to be accurate:

# find average fire size per grid scale
MW <- raster("data/upper_midwest.tif")


# there are 40 points with very large fire size, not sure if any of these might be NA values?


# we could average 
# View the feature class
#plot(fc)