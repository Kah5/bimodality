##########################################
#extracting precip & plotting Crown cover#
#run calculate_dens_v2.r first
#Kelly Heilman               
#January 20, 2016            
##########################################

#getting gridded climate data
#need to clean this up

precip.1900<- read.table("./Global2011P.tar/Global2011P/precip.1900")
precip.1901<- read.table("./Global2011P.tar/Global2011P/precip.1901")
precip.1902<- read.table("./Global2011P.tar/Global2011P/precip.1902")
precip.1903<- read.table("./Global2011P.tar/Global2011P/precip.1903")
precip.1904<- read.table("./Global2011P.tar/Global2011P/precip.1904")
precip.1905<- read.table("./Global2011P.tar/Global2011P/precip.1905")
precip.1906<- read.table("./Global2011P.tar/Global2011P/precip.1906")
precip.1907<- read.table("./Global2011P.tar/Global2011P/precip.1907")
precip.1908<- read.table("./Global2011P.tar/Global2011P/precip.1908")
precip.1909<- read.table("./Global2011P.tar/Global2011P/precip.1909")
precip.1910<- read.table("./Global2011P.tar/Global2011P/precip.1910")
precip.2010<- read.table("./Global2011P.tar/Global2011P/precip.2010")

Lat <- precip.1900[,2]
Long <- precip.1900[,1]

p.1900<- rowSums(precip.1900[,3:14], na.rm = TRUE)
p.1901<- rowSums(precip.1901[,3:14], na.rm = TRUE)
p.1902<- rowSums(precip.1902[,3:14], na.rm = TRUE)
p.1903<- rowSums(precip.1903[,3:14], na.rm = TRUE)
p.1904<- rowSums(precip.1904[,3:14], na.rm = TRUE)
p.1905<- rowSums(precip.1905[,3:14], na.rm = TRUE)
p.1906<- rowSums(precip.1906[,3:14], na.rm = TRUE)
p.1907<- rowSums(precip.1907[,3:14], na.rm = TRUE)
p.1908<- rowSums(precip.1908[,3:14], na.rm = TRUE)
p.1909<- rowSums(precip.1909[,3:14], na.rm = TRUE)
p.1910<- rowSums(precip.1910[,3:14], na.rm = TRUE)

avg.p<- rowMeans(data.frame(p.1900,
                    p.1901, 
                    p.1902,
                    p.1903, 
                    p.1904, 
                    p.1905, 
                    p.1906, 
                    p.1907,
                    p.1908,
                    p.1909, 
                    p.1910))/11
                    
averages <- data.frame(Lat = Lat, 
                       Long = Long, 
                       avg = avg.p)

coordinates(averages) <- ~Long + Lat
gridded(averages) <- TRUE
avg.rast <- raster(averages)
projection(avg.rast) <- CRS("+init=epsg:4326")

avg.alb <- projectRaster(avg.rast, crs='+init=epsg:3175')
avg.paleon <- mask(avg.alb, extent(CW.rast))
CW.df <- as.data.frame(CC.adj, xy = TRUE)
CW.df$precip <- extract(avg.alb, CW.df[,1:2])
plot(CW.df$precip, CW.df$layer)

#extract just crown area
CA.df <- as.data.frame(CW.adj, xy = TRUE)
CA.df$precip <- extract(avg.alb, CA.df[,1:2])
plot(CA.df$precip, CA.df$layer)

#CW.df$precip2 <- as.numeric(cut2(CW.df$precip, g=25))

#df <- ddply(CW.df,~ precip2,summarise,mean=mean(layer, na.rm= TRUE),sd=sd(layer, na.rm = TRUE))

#ggplot(data=df, aes(x=precip2, y=mean)) +
 # geom_bar(stat="identity")
hist(CW.df$precip)
hist(CW.df$layer, breaks = 25)

#if we designate savanna < 50 and forest > 50 % cover, 
CW.df.sav <- CW.df[ CW.df$layer < 50, ]
CW.df.forest <- CW.df[CW.df$layer >= 50, ]

CA.df.sav <- CA.df[ CA.df$layer < 50, ]
CA.df.forest <- CA.df[CA.df$layer >= 50, ]




pdf("Precip_hists.pdf")
hist(CA.df$layer, breaks = 25, xlab = "Average projected Crown area (m^2) per tree in each grid cell", 
     ylab = "Number of grid cells", 
     main = "Histogram of Crown area")

hist(CA.df.forest$precip, xlim = c(70, 110), breaks = 15,
     xlab = "MAP (cm)", 
     ylab = "Number of Forest grid cells", 
     main = "Precipitation of Forest grid cells")

hist(CA.df.sav$precip, xlim = c(70, 110), breaks = 15,
     xlab = "MAR (cm)", 
     ylab = "Number of Non-forest grid cells", 
     main = "Precipitation of Savanna grid cells")

par(mfrow=c(2,1))

hist(CW.df$layer, breaks = 25, xlab = "% cover per grid cell", 
     ylab = "Number of grid cells", main = "Histogram of %cover")

hist(CW.df$precip, xlim= c(70, 110), breaks = 30, 
     xlab = "MAP (cm)", ylab = "Number of grid cells", 
     main = "average 1900-1910 Precipitation")

hist(CW.df.forest$precip, xlim = c(70, 110), breaks = 15,
     xlab = "MAP (cm)", 
     ylab = "Number of Forest grid cells", 
     main = "Precipitation of Forest grid cells")

hist(CW.df.sav$precip, xlim = c(70, 110), breaks = 15,
     xlab = "MAR (cm)", 
     ylab = "Number of Non-forest grid cells", 
     main = "Precipitation of Savanna grid cells")

dev.off()



##Make cover plots
pdf("cover_by_precip.pdf")
par(mfrow=c(2,2))

CW.df.70.80 <- CW.df[ CW.df$precip < 80 & CW.df$precip > 70, ]
hist(CW.df.70.80$layer, xlim = c(0, 100),
     xlab = "% cover",
     main = "cover 70-80cm")

CW.df.80.90 <- CW.df[ CW.df$precip < 90 & CW.df$precip > 80, ]
hist(CW.df.80.90$layer, xlim = c(0, 100), 
     xlab = "% cover",
     main = "cover 80-90cm")

CW.df.90.100 <- CW.df[ CW.df$precip < 100 & CW.df$precip > 90, ]
hist(CW.df.90.100$layer, xlim = c(0, 100), 
     xlab = "% cover",
     main = "cover 90-100cm")

CW.df.100.110 <- CW.df[ CW.df$precip < 110 & CW.df$precip > 100, ]
hist(CW.df.100.110$layer, xlim = c(0, 100), 
     xlab = "% cover",
     main = "cover 100-110cm")

#CW.df.110.120 <- CW.df[ CW.df$precip < 120 & CW.df$precip > 110, ]
#hist(CW.df.110.120$layer, xlim = c(0, 100))
dev.off()


pdf("crown_area_by_precip.pdf")
par(mfrow=c(2,2))

CA.df.70.80 <- CA.df[ CA.df$precip < 80 & CA.df$precip > 70, ]
hist(CA.df.70.80$layer, xlim = c(0, 100),
     xlab = "projected Crown Area",
     main = "cover 70-80cm")

CA.df.80.90 <- CA.df[ CA.df$precip < 90 & CA.df$precip > 80, ]
hist(CA.df.80.90$layer, xlim = c(0, 100), 
     xlab = "projected Crown Area",
     main = "cover 80-90cm")

CA.df.90.100 <- CA.df[ CA.df$precip < 100 & CA.df$precip > 90, ]
hist(CA.df.90.100$layer, xlim = c(0, 100), 
     xlab = "projected Crown Area",
     main = "cover 90-100cm")

CA.df.100.110 <- CA.df[ CA.df$precip < 110 & CA.df$precip > 100, ]
hist(CA.df.100.110$layer, xlim = c(0, 100), breaks = 1,
     xlab = "projected Crown Area",
     main = "cover 100-110cm")

#CA.df.110.120 <- CA.df[ CA.df$precip < 120 & CA.df$precip > 110, ]
#hist(CA=.df.110.120$layer, xlim = c(0, 100))
dev.off()