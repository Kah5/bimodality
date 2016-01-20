#getting gridded climate data

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

avg.alb<- projectRaster(avg.rast, crs='+init=epsg:3175')
avg.paleon<- mask(avg.alb, extent(CW.rast))
CW.df<- as.data.frame(CW.rast, xy = TRUE)
CW.df$precip <- extract(avg.alb, CW.df[,1:2])
plot(CW.df$precip, CW.df$layer)

precip<- unlist(extract(avg.alb, ind))
CW.trans <- unlist(extract(CW.rast, ind, xy =TRUE))
