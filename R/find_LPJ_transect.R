#finding places where GUESS and PLS data transition (and are bimodal)
library(ggplot2)

LPJ.dens <- read.csv('C:/Users/Jmac/Box Sync/biomodality/data/LPJ-GUESS/LPJ-GUESS_annual_dens.csv')
dens.1850 <- LPJ.dens[LPJ.dens$Year %in% 1850,]
ggplot(dens.1850, aes(lon, lat, fill = Dens))+geom_raster() +coord_equal()+ facet_grid(PFT~.)
dens.1850.df <- dens.1850
dens.1850 <- dens.1850[dens.1850$PFT %in% "Deciduous", ] # lets look at decidious only
dens.1850.con <- dens.1850.df[dens.1850.df$PFT %in% "Evergreen", ] # lets look at decidious only
merged <- merge(dens.1850, dens.1850.con, by = c("lon", "lat"))
merged$Dens.total <- sum(merged$Dens.x, merged$Dens.y, na.rm = TRUE)

#need to convert lat long to albers
coordinates(dens.1850) <- ~lon + lat
gridded(dens.1850) <- TRUE
#dens.1850 <- raster(dens.1850)
proj4string(dens.1850) <- CRS('+init=epsg:4326')
dens.1850.df <- data.frame(dens.1850)

PLS <- read.csv("outputs/PLS_full_bimodal_plsprbins75.csv")
coordinates(PLS) <- ~x + y
gridded(PLS) <- TRUE
proj4string(PLS) <- CRS('+init=epsg:3175')
PLS.ll <- spTransform(PLS, crs('+init=epsg:4326'))

PLS.ll.df <- data.frame(PLS.ll)

PLS.ll


ggplot() + geom_raster(data = dens.1850.df, aes(x = lon, y = lat, fill = Dens)) + 
  geom_raster(data = PLS.ll.df, aes(x = x, y = y, fill = PLSdensity))


stacked<- stack(dens.1850)
plot(stacked)
extracted <- extract(stacked, PLS.ll.df[,c('x', "y")])

PLS.ll.df$LPJ_Dens <- extracted[,2]
PLS.ll.df$LPJ_cell <- extracted[,1]

plot(PLS.ll.df$LPJ_Dens, PLS.ll.df$PLSdensity)
ggplot(data = PLS.ll.df, aes(x = x, y = y, color = LPJ_Dens))+geom_point()
ggplot(data = PLS.ll.df, aes(x = x, y = y, color = bimodal))+geom_point() +
  geom_point(aes(x = -96.5, y = 45.75, color = 'black')) +geom_point(aes(x = -90, y = 45.75), color = " black")+
  geom_point(aes(x = -91, y = 40.75), color = " black")+ geom_point(aes(x = -85, y = 40.75), color = " black")


#Transect 1: 45.75 lon = 
Transect1.lon <- c(-97.25, -96.75, -96.25, -95.75, -95.25, -94.75 ,-94.25,
                     -93.75, -93.25, -92.75, -92.25, -91.75, -91.25, -90.75, -90.25)
tran.1 <- data.frame(lon = Transect1.lon, lat = 45.75)
tran.1$transect <- 'one'

#Transect 2: 40.75
Transect2.lon <- c(-91.75, -91.25, -90.75, -90.25, -89.75, -89.25, -88.75, -88.25,
-87.75, -87.25, -86.75, -86.25, -85.75, -85.25, -84.75)

tran.2 <- data.frame(lon = Transect2.lon, lat = 40.75)
tran.2$transect <- 'two'
transects <- rbind(tran.1, tran.2)
write.csv(transects, "outputs/LPJ_potential_transects.csv")


#now lets see what taxa are represented in these regions & transects:
library(ggplot2)
library(raster)
library(reshape2)
library(dplyr)
version <- "1.6"

# read in file
spec.table <- read.csv("outputs/species_table_pls_coverscenter.csv")

# spec.table lists each xy tree coordinate and is given a 1 if the tree's crown covers the PLS point, and a 0 if the trees crown does not cover the PLS point
X11(width = 12)
ggplot(spec.table, aes(x = Pointx, y = Pointy, color = spec))+geom_point()


count.table <- dcast(spec.table, x + y + cell ~ spec, sum, na.rm=TRUE, value.var = 'count')
count.table$total <- rowSums(count.table[,4:36], na.rm = TRUE)
numbersoftrees <- data.frame(names = colnames(count.table[,4:36]),counts = colSums(count.table[,4:36]))

png(paste0('outputs/v',version,'counts_of_inil_spec.png'))
ggplot() + geom_point(data = numbersoftrees, aes(x = names, y = counts))+theme_bw()+theme(axis.text = element_text(angle = 90))
dev.off()

umw <- read.csv("data/plss_plots_taxa_alb_v0.9-10.csv")
numbersoftrees.umw <- data.frame(names = colnames(umw[,4:32]),counts = colSums(umw[,4:32]))


png(paste0('outputs/v',version,'counts_of_umw_spec.png'))
ggplot() + geom_point(data = numbersoftrees.umw, aes(x = names, y = counts))+theme_bw()+theme(axis.text = element_text(angle = 90))
dev.off()