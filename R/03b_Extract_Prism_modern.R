library(plyr)
library(raster)
library(data.table)
library(rgdal)
version <- "1.7-5"

# set the working dir (where the prism data folder is)
workingdir <- "/Users/kah/Documents/bimodality/data/"



#################################################################################
#                 Extract modern precipitation data
#################################################################################

# read in and average prism data (this is modern 30year normals)
prism <- raster(paste0(workingdir,"PRISM_ppt_30yr_normal_4kmM2_all_bil/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil"))
prism.alb <- projectRaster(prism, crs='+init=epsg:3175')


# spec. table is a data frame that has the xy coordinates of the grid that you want the prism data on
spec.table <- read.csv(paste0(workingdir,"midwest_pls_full_density_alb", version,".csv"))

# extract 30 year normals at the xy points of teh grid
spec.table$pr30yr <- extract(prism.alb, spec.table[,c("x","y")])

write.csv(spec.table[,c('x', 'y', 'cell', 'pr30yr')], paste0(workingdir,'spec_table_30yr_prism_full.csv'))



#get the monthly averages for the modern 30 year normals
setwd(paste0(workingdir,'PRISM_ppt_30yr_normal_4kmM2_all_bil/'))


coordinates(spec.table) <- ~x +y
proj4string(spec.table) <- '+init=epsg:3175'
spec.table.ll<- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))

month <- sprintf("%02d", 1:12)

# read in and stack the prism rasters, then extract rasters to points
for (i in 1:length(month)) {
  filenames <- list.files(pattern=paste0(".*_", month[i],".*\\.bil$", sep = ""))
  s <- stack(filenames)
  t <- crop(s, extent(spec.table.ll))#make all into a raster
  s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
   #crop to the extent of indiana & illinois 
  y <- data.frame(rasterToPoints(t)) #covert to dataframe
  #colnames(y) <- c("x", "y", month.abb)
  y$month <- month[i]
  y$gridNumber <- cellFromXY(t, y[, 1:2])
  write.csv(y ,paste0('/Users/kah/Documents/bimodality/outputs/30yrnorm_',month[i],'_precip.csv' )) 
}

jan<- read.csv('/Users/kah/Documents/bimodality/outputs/30yrnorm_01_precip.csv')
feb<- read.csv('/Users/kah/Documents/bimodality/outputs/30yrnorm_02_precip.csv')
mar<- read.csv('/Users/kah/Documents/bimodality/outputs/30yrnorm_03_precip.csv')
apr<- read.csv('/Users/kah/Documents/bimodality/outputs/30yrnorm_04_precip.csv')
may<- read.csv('/Users/kah/Documents/bimodality/outputs/30yrnorm_05_precip.csv')
jun<- read.csv('/Users/kah/Documents/bimodality/outputs/30yrnorm_06_precip.csv')
jul<- read.csv('/Users/kah/Documents/bimodality/outputs/30yrnorm_07_precip.csv')
aug<- read.csv('/Users/kah/Documents/bimodality/outputs/30yrnorm_08_precip.csv')
sep<- read.csv('/Users/kah/Documents/bimodality/outputs/30yrnorm_09_precip.csv')
oct<- read.csv('/Users/kah/Documents/bimodality/outputs/30yrnorm_10_precip.csv')
nov<- read.csv('/Users/kah/Documents/bimodality/outputs/30yrnorm_11_precip.csv')
dec<- read.csv('/Users/kah/Documents/bimodality/outputs/30yrnorm_12_precip.csv')

# create a full dataset with all months
full <- cbind(jan[,1:4], feb[,4],mar[,4], apr[,4], may[,4], jun[,4], jul[,4], aug[,4], sep[,4], oct[, 4], nov[,4], dec[,4])
colnames(full) <- c('X','x', 'y' ,'Jan', 'Feb', 'Mar', "Apr", "May", 
                       'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")
full$total<- rowSums(full[,4:15], na.rm=TRUE)
full$SI <- rowSums(abs(full[,4:15]-(full[,16]/12)))/full[,16]
#full$cv <- ((apply(full[,4:15],1, sd, na.rm = TRUE)/full[,16]))*100


coordinates(full) <- ~x + y
gridded(full) <- TRUE
avgs <- stack(full) 
proj4string(avgs) <- crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 ')
avgs<- projectRaster(avgs, crs='+init=epsg:3175')
plot(avgs) #plots averages


spec.table <- data.frame(spec.table)
avgs.df <- data.frame(extract(avgs, spec.table[,c("x","y")]))
avgs.df$x <- spec.table$x
avgs.df$y <- spec.table$y

write.csv(avgs.df, "/Users/kah/Documents/bimodality/outputs/pr_monthly_Prism_30yrnorms_full.csv")


#################################################################################
#                 Extract modern temperature data
#################################################################################

# read in and average prism data
prism <- raster(paste0(workingdir,"PRISM_tmean_30yr_normal_4kmM2_annual_bil/PRISM_tmean_30yr_normal_4kmM2_annual_bil.bil"))
prism.alb <- projectRaster(prism, crs='+init=epsg:3175')
#spec.table<- read.csv(paste0(workingdir,"midwest_pls_fia_density_alb.csv"))
#spec.table <- read.csv('/Users/kah/Documents/bimodality/data/midwest_pls_fia_density_alb1.6.csv')

spec.table <- data.frame(spec.table)
temp30yr <- data.frame(extract(prism.alb, spec.table[,c("x","y")]))
temp30yr$x <- spec.table$x
temp30yr$y <- spec.table$y
colnames(temp30yr) <- c('modtmean', 'x', 'y')
write.csv(temp30yr, paste0(workingdir,'outputs/tmean_30yr_prism.csv'))

#coordinates(spec.table) <- ~x +y
#proj4string(spec.table) <- '+init=epsg:3175'
#spec.table.ll <- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))
spec.table.ll <- data.frame(spec.table.ll)



#monthly seasonal temperature
#get the monthly averages
setwd(paste0(workingdir,'PRISM_tmean_30yr_normal_4kmM2_all_bil/'))
coordinates(spec.table) <- ~x +y
proj4string(spec.table) <- '+init=epsg:3175'
spec.table.ll <- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))

month <- sprintf("%02d", 1:12)
for (i in 1:length(month)) {
  filenames <- list.files(pattern=paste0(".*_", month[i],".*\\.bil$", sep = ""))
  s <- stack(filenames)
  t <- crop(s, extent(spec.table.ll))#make all into a raster
  s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
  #crop to the extent of indiana & illinois 
  y <- data.frame(rasterToPoints(s)) #covert to dataframe
  #colnames(y) <- c("x", "y", month.abb)
  y$month <- month[i]
  y$gridNumber <- cellFromXY(t, y[, 1:2])
  write.csv(y ,paste0(workingdir,'30yrnorm_',month[i],'_tmean.csv' )) 
}

# above loop doesn't add all teh 
jan<- read.csv(paste0(workingdir,'/30yrnorm_01_tmean.csv'))
feb<- read.csv(paste0(workingdir,'/30yrnorm_02_tmean.csv'))
mar<- read.csv(paste0(workingdir,'/30yrnorm_03_tmean.csv'))
apr<- read.csv(paste0(workingdir,'/30yrnorm_04_tmean.csv'))
may<- read.csv(paste0(workingdir,'/30yrnorm_05_tmean.csv'))
jun<- read.csv(paste0(workingdir,'/30yrnorm_06_tmean.csv'))
jul<- read.csv(paste0(workingdir,'/30yrnorm_07_tmean.csv'))
aug<- read.csv(paste0(workingdir,'/30yrnorm_08_tmean.csv'))
sep<- read.csv(paste0(workingdir,'/30yrnorm_09_tmean.csv'))
oct<- read.csv(paste0(workingdir,'/30yrnorm_10_tmean.csv'))
nov<- read.csv(paste0(workingdir,'/30yrnorm_11_tmean.csv'))
dec<- read.csv(paste0(workingdir,'/30yrnorm_12_tmean.csv'))

# rearragen into a dataframe
full <- cbind(jan[,1:4], feb[,4],mar[,4], apr[,4], may[,4], jun[,4], jul[,4], aug[,4], sep[,4], oct[, 4], nov[,4], dec[,4])
colnames(full) <- c('X','x', 'y' ,'Jan', 'Feb', 'Mar', "Apr", "May", 
                    'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")

#calculate annual means
full$Mean <- rowMeans(full[,4:15], na.rm=TRUE)
colnames(full) <- c("X",'x','y','Jan', 'Feb', 'Mar', "Apr", "May", 
                    'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec",'Mean')

test <- full
#calculate the CV temperature seasonality:
#TSI = sd(m1....m12)/Tavgannual *100
test$cv <- (apply(test[,4:15],1, sd, na.rm = TRUE)/test[,16])*100
full <- test

coordinates(full) <- ~x + y
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages

# extract mean temp to the grid 
spec.table <- data.frame(spec.table)
avgs.df <- data.frame(raster::extract(avgs, Precip[,c("x","y")]))
avgs.df$x <- Precip$x
avgs.df$y <- Precip$y
avgs.df$cv <- (apply(avgs.df[,2:13],1, sd, na.rm = TRUE)/avgs.df[,14])*100


write.csv(avgs.df, "/Users/kah/Documents/bimodality/outputs/tmean_monthly_Prism_30yrnorms_full.csv")
avgs.df <- read.csv("/Users/kah/Documents/bimodality/outputs/tmean_monthly_Prism_30yrnorms_full.csv")
avgs.df$ind <- row.names(avgs.df)
coordinates(avgs.df) <- ~x +y
proj4string(avgs.df) <- '+init=epsg:3175'

avgs.df.ll <- spTransform(avgs.df, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))
avgs.df.ll <- data.frame(avgs.df.ll)
avgs.df <- data.frame(avgs.df)
ggplot(avgs.df.ll, aes(x=x, y=y, color = Jan)) + geom_point()
#pet<- stack(avgs.df.ll)

head(avgs.df.ll)
my.list <- list()
# get PET for the modern landscape:
y <- na.omit(avgs.df.ll[,c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec","ind", "x", "y")])
z <- na.omit(avgs.df[,c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec","ind", "x", "y")])

#source("/afs/crc.nd.edu/user/k/kheilman/bimodality/R/Thornthwaite_PET.R")
source("R/Thornthwaite_PET.R")

system.time(for(i in 1:length(y$y)){
  
  ynew <- t(y[i,1:12])
  lat <- y[i,]$y
  long <- y[i,]$x
  cellID <- y[i,]$ind
  # get month an year from row.names
  year <-  data.frame(year = substring(row.names(ynew), first = 26, last = 29))
  month <- data.frame(month = row.names(ynew))
  ynew2 <- data.frame(Tave = ynew[,1], 
                      year <- year, 
                      month <- month)
  #ynew2 <- ynew2[ynew2$month %in% c("01","03","04","05","06", "07", "08", "09", "10", "11", "12"),]
  # use the thorthwaite equation to attach the PET data to the 
  
  ynew2$PET_tho <- as.numeric(thornthwaite_PET(ynew2$Tave, lat))
  ynew2$lat <- lat
  ynew2$long <- long
  ynew2$CellID <- cellID
  
  my.list[[i]] <- ynew2
  
})

my.list.df <- do.call(rbind, my.list)

my.list.df <- data.frame(my.list.df)
coordinates(my.list.df) <- ~long + lat
proj4string(my.list.df) <-  '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
#gridded(my.list.df) <- TRUE
my.list.df<- spTransform(my.list.df, crs('+init=epsg:3175'))
my.list.df <- data.frame(my.list.df)

PET.means <- dcast(my.list.df, lat + long + CellID  ~ month , mean , value.var='PET_tho', na.rm = TRUE)
#colnames(PET.means) <- c("lat", "long",
 #                        "apr","may","jun", "jul", "aug", "sep", "oct")
ggplot(data.frame(PET.means), aes(long, lat, fill = Jul))+geom_point()
# get the precipitation data in the same format:
saveRDS(PET.means, paste0( "/Users/kah/Documents/bimodality/outputs/fia_full.PET_full_reg.rds"))


# now need to calculate P-PET:

Precip <- read.csv(paste0("/Users/kah/Documents/bimodality/outputs/pr_monthly_Prism_30yrnorms_full.csv"))
PET.means<- readRDS("/Users/kah/Documents/bimodality/outputs/fia_full.PET_full_reg.rds")
PETjja <- PET.means
PrJJA <- Precip[,c("x", "y","Mar","Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov")]
colnames(PrJJA) <- c("x", "y", "Mar_pr","Apr_pr", "May_pr","Jun_pr", "Jul_pr", "Aug_pr", "Sep_pr", "Oct_pr", "Nov_pr")
PrJJA <- merge(PrJJA, avgs.df[,c("x", "y", "ind")], by = c("x", "y"))

colnames(PETjja) <- c("y", "x","ind","Apr_pet", "Aug_pet","Dec_pet","Feb_pet", "Jan_pet","Jul_pet", "Jun_pet", "Mar_pet", "May_pet", "Nov_pet", "Oct_pet","Sep_pet")
PETjja <- PETjja[,!names(PETjja) %in% c("Jan_pet", "Dec_pet")] # remove january

ggplot(PrJJA, aes(x,y, fill = Jun_pr),color = "red")+geom_point(size = 0.05)+geom_point(data = PETjja, aes(x,y, fill = Jun_pet), color = "blue",size = 0.05)
#ggplot(PETjja, aes(x,y, fill = Jun_pet))+geom_raster()

no.match <- PETjja[!unique(PETjja[,2:1]) %in% unique(PrJJA[,1:2]),]

P.PET <- merge(PrJJA, PETjja, by = c("ind"))
ggplot(P.PET, aes(x.x,y.y, color = Jun_pr))+geom_point()



# now lets calculaate P - PET for Jun - Aug
P.PET$Mar_ppet <- P.PET$Mar_pr - P.PET$Mar_pet
P.PET$Apr_ppet <- P.PET$Apr_pr - P.PET$Apr_pet
P.PET$May_ppet <- P.PET$May_pr - P.PET$May_pet
P.PET$Jun_ppet <- P.PET$Jun_pr - P.PET$Jun_pet
P.PET$Jul_ppet <- P.PET$Jul_pr - P.PET$Jul_pet
P.PET$Aug_ppet <- P.PET$Aug_pr - P.PET$Aug_pet
P.PET$Sep_ppet <- P.PET$Sep_pr - P.PET$Sep_pet
P.PET$Oct_ppet <- P.PET$Oct_pr - P.PET$Oct_pet
P.PET$Nov_ppet <- P.PET$Nov_pr - P.PET$Nov_pet

P.PET$GS_ppet <- rowSums(P.PET[,4:12], na.rm=TRUE) - rowSums(P.PET[,c("Mar_pet", "Apr_pet", "May_pet", "Jun_pet", 
                                                                      "Jul_pet","Aug_pet", "Sep_pet","Oct_pet","Nov_pet")], na.rm=TRUE)

ggplot(P.PET, aes(x.x,y.y, color = GS_ppet))+geom_point()
ggplot(P.PET, aes(x,y, fill = Jul_ppet))+geom_raster()
ggplot(P.PET, aes(x,y, fill = GS_ppet))+geom_raster()

P.PET <- P.PET[,c("x.x", "y.x", "Mar_ppet",
        "Apr_ppet", "May_ppet" ,"Jun_ppet", "Jul_ppet" ,"Aug_ppet",
        "Sep_ppet", "Oct_ppet", "Nov_ppet", "GS_ppet" )]
colnames(P.PET) <- c("x", "y", "Mar_ppet",
                     "Apr_ppet", "May_ppet" ,"Jun_ppet", "Jul_ppet" ,"Aug_ppet",
                     "Sep_ppet", "Oct_ppet", "Nov_ppet", "GS_ppet" )

write.csv(P.PET, "/Users/kah/Documents/bimodality/outputs/P.PET_prism_modern_Mar_Nov.csv")

ggplot(P.PET, aes(x, y, color = GS_ppet))+geom_point()
