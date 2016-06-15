# read in and average prism data
prism<- raster("data/PRISM_ppt_stable_4kmM2_189501_198012_bil/PRISM_ppt_stable_4kmM2_189501_bil.bil")
prism.alb<- projectRaster(prism, crs='+init=epsg:3175')

spec.table <- data.frame(spec.table)
spec.table$prism.1900p <- extract(prism.alb, spec.table[,c("x","y")])


#try prims
install.packages('prism')
#library(prism)
#process_zip("data/PRISM_ppt_stable_4kmM2_189501_198012_bil")

#get_prism_annual(type = 'ppt', years = 1901:1905, keepZip = FALSE)

#setwd to data directory
setwd('C:/Users/JMac/Documents/Kelly/biomodality/data/PRISM_ppt_stable_4kmM2_189501_198012_bil/')


#try this loop, takes awhile, but works

library(raster)
years <- 1900:1910
for (i in years) {
  filenames <- list.files(pattern=paste(".*_",i,".*\\.bil$", sep = ""))
  s <- stack(filenames)
  y <- data.frame(rasterToPoints(s))
  colnames(y) <- c("lon", "lat", month.abb)
  y$year <- i
  y$gridNumber <- cellFromXY(s, y[, 1:2])
  # write.csv( ) ?
}