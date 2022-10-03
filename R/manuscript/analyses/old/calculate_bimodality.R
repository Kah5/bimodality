# this script calculates the bimodality coefficients for PLS data:


library(modes)
library(ggplot2)

#--------------------------------load PLS data------------------------------------------
dens.pr <- read.csv("data/PLS_full_dens_pr_with_bins.csv")
#this function uses the bimodality_coefficient funcition in the modes library to calculate the 
#bimodality coefficient of the density (FIA or PLS) within a given set of bins (climate, sand, etc)

calc.BC <- function(data, binby, density){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 1)
  for (i in 1:length(bins)){
    coeffs[i]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
  }
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$V1 <- as.numeric(as.character(coef.bins$V1))
  #coef.bins
  coef.bins <- coef.bins[order(as.numeric(as.character(coef.bins$bins))),]
  coef.bins$bins <- factor(coef.bins$bins, levels = coef.bins$bins[order(as.numeric(as.character(coef.bins$bins)))])# reorder so it plots well
  a <- ggplot(coef.bins, aes(x = bins, y = V1))+geom_point()+
    geom_hline( yintercept = 5/9)+ylim(0,1)+theme_bw()+
    theme(axis.text = element_text(angle = 90))+
    xlab('Mean annual Precipitaiton range (mm/yr)') + ylab('Bimodality Coefficient')+
    ggtitle(paste0('Bimodality coefficients for ', binby))
  a
  
}

pdf(paste0('outputs/v',version,'/bimodality_coefficient_binplots.pdf'))
calc.BC(data = dens.pr, binby = 'plsprbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'plsprbins100', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'plsprbins75', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'plsprbins25', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'sandbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'ksatbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'pastdeltPbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'pasttmeanbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'PC1bins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'PC2bins', density = "PLSdensity")
dev.off()

png(height = 400, width = 400, paste0('outputs/v',version,'/full/PLS_PC1_PC2_BC_bins.png'))
pushViewport(viewport(layout = grid.layout(2, 1)))
print(calc.BC(data = dens.pr, binby = 'PC1bins', density = "PLSdensity")+ ggtitle('BC for PC1 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(calc.BC(data = dens.pr, binby = 'PC2bins', density = "PLSdensity")+ ggtitle('BC for PC2  PLS'),   vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
dev.off()

calc.BC(data = dens.pr, binby = 'sandbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'ksatbins', density = "PLSdensity")
calc.BC(data = dens.pr, binby = 'pastdeltPbins', density = "PLSdensity")

#------------------------------Map out regions of bimodality-----------------------------------

#this function maps out the region that is bimodal & uses the ecotypes to classify this
map.bimodal <- function(data, binby, density){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 2)
  for (i in 1:length(bins)){
    coeffs[i,1]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
    coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
  merged <- merge(coef.bins, dens.pr, by.x = "bins",by.y = binby)
  #define bimodality
  merged$bimodal <- "Stable"
  merged[merged$BC >= 0.55 & merged$dipP <= 0.05,]$bimodal <- "Bimodal"
  
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  if(density == "PLSdensity"){
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$ecotype)
  }else{
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$fiaecotype)
    
  }
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = classification))+ scale_fill_manual(values = c(
      '#1a9641', # dark green
      '#fdae61', # light orange
      '#a6d96a', # light green
      '#d7191c', # red
      '#fee08b', # tan
      'black'), limits = c("Stable Forest" , 'Stable Savanna', 'Bimodal Forest', "Bimodal Savanna", 'Bimodal prairie', 'Stable prairie') )+
    theme_bw()+
    xlab("easting") + ylab("northing") +coord_equal()+
    ggtitle(paste0(binby, ' for ',density))
  
}


#map out bimodalities--note the region varies by bin size
pdf(paste0('outputs/v',version,'/full/bimodal_maps.pdf'))
map.bimodal(data = dens.pr, binby = 'plsprbins50', density = "PLSdensity")
#map.bimodal(data = dens.pr, binby = 'fiaprbins', density = "FIAdensity")
map.bimodal(data = dens.pr, binby = 'plsprbins100', density = "PLSdensity")
#map.bimodal(data = dens.pr, binby = 'fiaprbins100', density = "FIAdensity")
map.bimodal(data = dens.pr, binby = 'plsprbins75', density = "PLSdensity")
#map.bimodal(data = dens.pr, binby = 'fiaprbins75', density = "FIAdensity")
map.bimodal(data = dens.pr, binby = 'plsprbins25', density = "PLSdensity")
#map.bimodal(data = dens.pr, binby = 'fiaprbins25', density = "FIAdensity")
#map.bimodal(data = dens.pr, binby = 'fiaprbins', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'sandbins', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'ksatbins', density = "PLSdensity")
map.bimodal(data = dens.pr, binby = 'pastdeltPbins', density = "PLSdensity")
dev.off()

png(height = 6, width = 10, units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_PC1_PC2_map.png'))
pushViewport(viewport(layout = grid.layout(1, 2)))
print(map.bimodal(data = dens.pr, binby = 'PC1bins', density = "PLSdensity")+ ggtitle('Bimodal Regions for PC1 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal(data = dens.pr, binby = 'PC2bins', density = "PLSdensity") + ggtitle('Bimodal Regions for PC2 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

png(height = 6, width = 15, units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_Precip_25_50_75_map.png'))
pushViewport(viewport(layout = grid.layout(1, 3)))
print(map.bimodal(data = dens.pr, binby = 'plsprbins25', density = "PLSdensity")+ ggtitle('Bimodal Regions for Precip25 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal(data = dens.pr, binby = 'plsprbins50', density = "PLSdensity") + ggtitle('Bimodal Regions for  Precip50 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(map.bimodal(data = dens.pr, binby = 'plsprbins75', density = "PLSdensity") + ggtitle('Bimodal Regions for  Precip75 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
dev.off()


png(height = 6, width = 15, units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_tmean_delta_p_map.png'))
pushViewport(viewport(layout = grid.layout(1, 3)))
print(map.bimodal(data = dens.pr, binby = 'pastdeltPbins', density = "PLSdensity")+ ggtitle('Bimodal Regions for deltaP PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal(data = dens.pr, binby = 'pasttmeanbins', density = "PLSdensity") + ggtitle('Bimodal Regions for  tmean PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(map.bimodal(data = dens.pr, binby = 'sandbins', density = "PLSdensity") + ggtitle('Bimodal Regions for sand PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
dev.off()



#make alternative maps that only plot prairie as one type of prairie:
map.bimodal.5c <- function(data, binby, density){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 2)
  for (i in 1:length(bins)){
    coeffs[i,1]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)])) # should we also do BC on the denisty estimated function?
    coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
  merged <- merge(coef.bins, dens.pr, by.x = "bins",by.y = binby)
  #define bimodality
  merged$bimodal <- "Stable"
  merged[merged$BC >= 0.55 & merged$dipP <= 0.05,]$bimodal <- "Bimodal"
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  if(density == "PLSdensity"){
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$ecotype)
    merged[merged$classification %in% 'Bimodal prairie',]$classification <- "Prairie"
    merged[merged$classification %in% 'Stable prairie',]$classification <- "Prairie"
    
  }else{
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$fiaecotype)
    
  }
  
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = classification))+ scale_fill_manual(values = c(
      '#01665e', # light green
      '#5ab4ac', # dark teal
      '#8c510a', # red
      '#d8b365', # light tan
      '#fee08b', # tan
      'black'), limits = c('Bimodal Forest',"Stable Forest" ,   "Bimodal Savanna", 'Stable Savanna','Prairie') )+
    theme_bw()+
    xlab("easting") + ylab("northing") +coord_equal()+
    ggtitle(paste0(binby, ' for ',density))
  
}

pdf(paste0('outputs/v',version,'/full/bimodal_maps_5col.pdf'))
map.bimodal.5c(data = dens.pr, binby = 'plsprbins50', density = "PLSdensity")
#map.bimodal.5c(data = dens.pr, binby = 'fiaprbins', density = "FIAdensity")
map.bimodal.5c(data = dens.pr, binby = 'pasttmeanbins', density = "PLSdensity")
map.bimodal.5c(data = dens.pr, binby = 'plsprbins100', density = "PLSdensity")
#map.bimodal.5c(data = dens.pr, binby = 'fiaprbins100', density = "FIAdensity")
map.bimodal.5c(data = dens.pr, binby = 'plsprbins75', density = "PLSdensity")
#map.bimodal.5c(data = dens.pr, binby = 'fiaprbins75', density = "FIAdensity")
map.bimodal.5c(data = dens.pr, binby = 'plsprbins25', density = "PLSdensity")
#map.bimodal.5c(data = dens.pr, binby = 'fiaprbins25', density = "FIAdensity")
#map.bimodal.5c(data = dens.pr, binby = 'fiaprbins', density = "PLSdensity")
map.bimodal.5c(data = dens.pr, binby = 'sandbins', density = "PLSdensity")
map.bimodal.5c(data = dens.pr, binby = 'ksatbins', density = "PLSdensity")
map.bimodal.5c(data = dens.pr, binby = 'pastdeltPbins', density = "PLSdensity")
dev.off()


png(height = 6, width =5 , units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_PC1_map_5col.png'))
map.bimodal.5c(data = dens.pr, binby = 'PC1bins', density = "PLSdensity")
dev.off()

png(height = 6, width =5 , units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_PC1fiaclimate_map_5col.png'))
map.bimodal.5c(data = dens.pr, binby = 'PC1fiabins', density = "PLSdensity")
dev.off()

png(height = 6, width = 10, units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_PC1_PC2_map_5col.png'))
pushViewport(viewport(layout = grid.layout(1, 2)))
print(map.bimodal.5c(data = dens.pr, binby = 'PC1bins', density = "PLSdensity")+ ggtitle(' PC1 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal.5c(data = dens.pr, binby = 'PC2bins', density = "PLSdensity") + ggtitle('PC2 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

png(height = 6, width = 15, units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_Precip_25_50_75_map_5col.png'))
pushViewport(viewport(layout = grid.layout(1, 3)))
print(map.bimodal.5c(data = dens.pr, binby = 'plsprbins25', density = "PLSdensity")+ ggtitle(' Precip25 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal.5c(data = dens.pr, binby = 'plsprbins50', density = "PLSdensity") + ggtitle('Precip50 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(map.bimodal.5c(data = dens.pr, binby = 'plsprbins75', density = "PLSdensity") + ggtitle('Precip75 PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
dev.off()


png(height = 6, width = 15, units= 'in',  res= 300, paste0('outputs/v',version,'/full/PLS_tmean_delta_p_map_5col.png'))
pushViewport(viewport(layout = grid.layout(1, 3)))
print(map.bimodal.5c(data = dens.pr, binby = 'pastdeltPbins', density = "PLSdensity")+ ggtitle('deltaP PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.bimodal.5c(data = dens.pr, binby = 'pasttmeanbins', density = "PLSdensity") + ggtitle('tmean PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(map.bimodal.5c(data = dens.pr, binby = 'sandbins', density = "PLSdensity") + ggtitle('sand PLS'),   vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
dev.off()




#------------------------------Bimodality in the future projections----------------------

#function for plotting where bimodality would be under future climate (assuming pls relationship with cliamte)
library(splitstackshape)
library(modes)

bimodal.future <- function(data, binby, density, binby2){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 2)
  for (i in 1:length(bins)){
    coeffs[i,1]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
    coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
  coef.new <- strsplit(as.character(coef.bins$bins), " - ")
  library(plyr)
  coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
  colnames(coef.new) <- c("low", "high")
  coef.bins <- cbind(coef.bins, coef.new)
  
  #merge bins iwth the second binby -> here is is future climate
  merged <- merge(coef.bins, dens.pr, by.x = "bins", by.y = binby2)
  
  
  #define bimodality
  merged$bimodal <- "Stable"
  #criteria for bimodality
  merged[merged$BC >= 0.55 & merged$dipP <= 0.05,]$bimodal <- "Bimodal"
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  if(density == "PLSdensity"){
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$ecotype)
    merged[merged$classification %in% 'Bimodal prairie',]$classification <- "Prairie"
    merged[merged$classification %in% 'Stable prairie',]$classification <- "Prairie"
    merged[merged$classification %in% 'Bimodal Savanna',]$classification <- "Bimodal"
    merged[merged$classification %in% 'Bimodal Forest',]$classification <- "Bimodal"
  }else{
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$fiaecotype)
    
  }
  
  #merged
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = classification))+ scale_fill_manual(values = c(
      '#1a9641',
      '#fdae61',
      '#d7191c',
      '#ffffbf'
    ), limits = c('Stable Forest',"Stable Savanna",'Bimodal', "Prairie") )+
    theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank())+
    xlab("easting") + ylab("northing") +coord_equal() + ggtitle(binby2)
  
}

source("R/grid_arrange_shared_legend.R")

a <- bimodal.future(data = dens.pr, binby = 'PC1bins', density = "PLSdensity", binby2 ='PC1bins' ) + ggtitle ("PLS PC1")
b <- bimodal.future(data = dens.pr, binby = 'PC1_cc26bins', density = "PLSdensity", binby2 ='PC1_cc26bins' ) + ggtitle("RCP 2.6 PC1")
c <- bimodal.future(data = dens.pr, binby = 'PC1_cc26bins', density = "PLSdensity", binby2 ='PC1_cc45bins' )+ ggtitle("RCP 4.5 PC1")
d <- bimodal.future(data = dens.pr, binby = 'PC1_cc26bins', density = "PLSdensity", binby2 ='PC1_cc85bins' )+ ggtitle("RCP 8.5 PC1")


e<-bimodal.future(data = dens.pr, binby = 'PC1bins', density = "PLSdensity", binby2 ='PC1fiabins' ) + ggtitle ("PLS PC1")

png(height = 4, width = 12, units = "in",res = 300, filename = paste0('outputs/v1.6-5/full/RCP_scenario_PC1_maps.png'))
grid_arrange_shared_legend(a,b,c,d, nrow = 1, ncol=4, position = c("bottom"))
dev.off()

#######################################
#plot the projecitons into the future, 
#but highlight the no-analog climates:
#######################################
bimodal.future.NA <- function(data, binby, density, binby2, rcp){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 2)
  for (i in 1:length(bins)){
    coeffs[i,1]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
    coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
  coef.new <- strsplit(as.character(coef.bins$bins), " - ")
  library(plyr)
  coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
  colnames(coef.new) <- c("low", "high")
  coef.bins <- cbind(coef.bins, coef.new)
  
  #merge bins iwth the second binby -> here is is future climate
  merged <- merge(coef.bins, dens.pr, by.x = "bins", by.y = binby2)
  
  
  #define bimodality
  merged$bimodal <- "Stable"
  #criteria for bimodality
  merged[merged$BC >= 0.55 & merged$dipP <= 0.05,]$bimodal <- "Bimodal"
  merged[merged[,c(paste0("rcp",rcp,"NA"))] %in% 'no-analog',]$bimodal <- "no-analog"
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  
  
  #merged
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = bimodal))+ scale_fill_manual(values = c(
      '#1a9641',
      'black',
      '#d7191c'
    ), limits = c('Stable',"no-analog",'Bimodal') )+
    theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank())+
    xlab("easting") + ylab("northing") +coord_equal() + ggtitle(binby2)
  
}
a <- bimodal.future.NA(data = dens.pr, binby = 'PC1_cc26bins', density = "PLSdensity", binby2 ='PC1_cc60bins', rcp = "60" ) + ggtitle ("RCP 6.0 PC1")
b <- bimodal.future.NA(data = dens.pr, binby = 'PC1_cc26bins', density = "PLSdensity", binby2 ='PC1_cc26bins',  rcp = "26") + ggtitle("RCP 2.6 PC1")
c <- bimodal.future.NA(data = dens.pr, binby = 'PC1_cc26bins', density = "PLSdensity", binby2 ='PC1_cc45bins', rcp = '45' )+ ggtitle("RCP 4.5 PC1")
d <- bimodal.future.NA(data = dens.pr, binby = 'PC1_cc26bins', density = "PLSdensity", binby2 ='PC1_cc85bins', rcp = '85' )+ ggtitle("RCP 8.5 PC1")

png(height = 4, width = 12, units = "in",res = 300, filename = paste0('outputs/v1.6-5/full/RCP_scenario_PC1_noanalog_maps.png'))
grid_arrange_shared_legend(b,c,a,d, nrow = 1, ncol=4, position = c("bottom"))
dev.off()

###################################################################
# bimodal.df function outputs the dataframe of bimodal/not bimodal 

bimodal.df <- function(data, binby, density, binby2){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 2)
  for (i in 1:length(bins)){
    coeffs[i,1]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
    coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
  coef.new <- strsplit(as.character(coef.bins$bins), " - ")
  library(plyr)
  coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
  colnames(coef.new) <- c("low", "high")
  coef.bins <- cbind(coef.bins, coef.new)
  
  #merge bins iwth the second binby -> here is is future climate
  merged <- merge(coef.bins, dens.pr, by.x = "bins", by.y = binby2)
  
  
  #define bimodality
  merged$bimodal <- "Stable"
  #criteria for bimodality
  merged[merged$BC >= 0.55 & merged$dipP <= 0.05,]$bimodal <- "Bimodal"
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  if(density == "PLSdensity"){
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$ecotype)
    merged[merged$classification %in% 'Bimodal prairie',]$classification <- "Prairie"
    merged[merged$classification %in% 'Stable prairie',]$classification <- "Prairie"
    
  }else{
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$fiaecotype)
    
  }
  
  merged
}

df.new <- bimodal.df(data = dens.pr, binby = 'PC1bins', density = "PLSdensity", binby2 = 'PC1bins')
df.mod <- bimodal.df(data = dens.pr, binby = 'PC1bins', density = "PLSdensity", binby2 = 'PC1fiabins')
df.8.5 <- bimodal.df(data = dens.pr, binby = 'PC1bins', density = "PLSdensity", binby2 = "PC1_cc85bins")
df.4.5 <- bimodal.df(data = dens.pr, binby = 'PC1bins', density = "PLSdensity", binby2 = "PC1_cc45bins")
df.2.6 <- bimodal.df(data = dens.pr, binby = 'PC1bins', density = "PLSdensity", binby2 = "PC1_cc26bins")

# calculate the % that should be bimodal in the modern landscape
a <- nrow(df.mod[df.mod$bimodal == "Bimodal",])/nrow(df.mod)
b <- nrow(df.new[df.new$bimodal == "Bimodal",])/nrow(df.new)
c <- nrow(df.8.5[df.8.5$bimodal == "Bimodal",])/nrow(df.8.5)
d <- nrow(df.4.5[df.4.5$bimodal == "Bimodal",])/nrow(df.4.5)
e <- nrow(df.2.6[df.2.6$bimodal == "Bimodal",])/nrow(df.2.6)


ggplot(df.new, aes(x = MAP1910, y = PLSdensity, color = classification))+geom_point()+ 
  stat_density2d(data = df.new, aes(colour = bimodal),fill = "transparent",geom="polygon") +
  theme_bw()

dens.pr<- read.csv("data/PLS_full_dens_pr_with_bins.csv")
write.csv(df.new, "outputs/PLS_full_dens_pr_bins_with_bimodality_for_PC1.csv")
# plot out climate space that is bimodal
png(height = 4, width = 6, units = "in", res = 300, filename = "outputs/v1.6-5/MAP_TEMP_bimodal_space.png")
ggplot(df.new, aes(x = MAP1910, y = pasttmean))+ geom_point()+ 
  stat_density2d(data = df.new, aes(colour = bimodal),fill = "transparent",geom="polygon") +
  theme_bw()+ ylab ("Mean Temperature (degC), 1895-1925") + xlab("Mean Annual Precipitation (mm/yr), 1895-1925")
dev.off()

png(height = 4, width = 6, units = "in", res = 300, filename = "outputs/v1.6-5/MAP_deltaTEMP_bimodal_space.png")
ggplot(df.new, aes(x = MAP1910, y = deltaT))+ geom_point()+ 
  stat_density2d(data = df.new, aes(colour = bimodal),fill = "transparent",geom="polygon") +
  theme_bw()+ ylab ("Mean Temperature (degC), 1895-1925") + xlab("Mean Annual Precipitation (mm/yr), 1895-1925")
dev.off()

png(height = 4, width = 6, units = "in", res = 300, filename = "outputs/v1.6-5/MAP_deltaTEMP_bimodal_space.png")
ggplot(df.new, aes(x = MAP1910, y = deltaT))+ geom_point()+ 
  stat_density2d(data = df.new, aes(colour = bimodal),fill = "transparent",geom="polygon") +
  theme_bw()+ ylab ("Tempearature Seasonality (degC), 1895-1925") + xlab("Mean Annual Precipitation (mm/yr), 1895-1925")
dev.off()




ggplot(df.new, aes(x = pastdeltaP, y = deltaT, color = classification)) + stat_density2d()+ #+ geom_point()
  scale_color_manual(values = c(
    '#01665e', # light green
    '#5ab4ac', # dark teal
    '#8c510a', # red
    '#d8b365', # light tan
    '#fee08b', # tan
    'black'), limits = c('Bimodal Forest',"Stable Forest" ,   "Bimodal Savanna", 'Stable Savanna','Prairie') )+
  theme_bw() 

library(rgl)
plot3d(x = df.new$MAP1910, y = df.new$PLSdensity, z = df.new$BC, groups = df.new$bimodal,
       surface=FALSE, ellipsoid = TRUE)

source_url("https://raw.github.com/low-decarie/FAAV/master/r/stat-ellipse.R")    
qplot(data=df, x=x, y=y, colour=colour)+stat_ellipse()
library(scatterplot3d)
scatterplot3d(df.new$pasttmean, df.new$MAP1910, df.new$sandpct,color = as.character(df.new$bimodal), angle=20) 
df.new$color <- 1
df.new[df.new$bimodal %in% c("Stable"),]$color <- 2

#3d plot of climate space that is bimodal
png(height = 3, width = 4, units = 'in',res = 300, filename = "outputs/v1.6-5/full/3d-plot-pr_t_sand.png")
s3d <- with(df.new, scatterplot3d( MAP1910, pasttmean, sandpct,color = color, pch = 19,xlab ="MAP(mm/year)", ylab="temp. (degC)"),zlab = "% sand", angle = -180 )
legend("topleft", inset=.01,      # location and inset
       bty="n", cex=1,              # suppress legend box, shrink text 50%
       title="Climate space",
       c("Bimodal", "Stable"), fill=c("red", "black"))
dev.off()

#another dimension of climate
png(height = 3, width = 4, units = 'in', res = 300, filename = "outputs/v1.6-5/full/3d-plot-pr_dt_sand.png")
s3d <- with(df.new, scatterplot3d( MAP1910, deltaT,sandpct, color = color, pch = 19), angle = -180)
legend("topleft", inset=.01,      # location and inset
       bty="n", cex=1,              # suppress legend box, shrink text 50%
       title="Climate space",
       c("Bimodal", "Stable"), fill=c("red", "black"))
dev.off()

plot3d(df.new$pasttmean, df.new$MAP1910, df.new$sandpct, col = df.new$color)
write.csv(df.new , "outputs/v1.6-5/full/dens_pr_dataframe_full_w_bimodal.csv")
write.csv(dens.pr, "outputs/v1.6-5/full/dens_pr_dataframe_full.csv")

#rolling BC
rollBC_r = function(x,y,xout,width) {
  out = numeric(length(xout))
  for( i in seq_along(xout) ) {
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i] = bimodality_coefficient( y[window ] ) # what is the BC for places with less than 300 trees per hectare
  }
  ggplot()+geom_point(aes(x = xout, y = out))+
    geom_hline( yintercept = 5/9)+ylim(0,1)+theme_bw()+
    xlab('interval center') + ylab('Bimodality Coefficient') +ggtitle(paste0( 'Bimodality coefficient for binwidth = ', width))
  
}

#need to order the 
ordered <- dens.pr[order(dens.pr$MAP1910),]
ordered$rownum <- 1:length(ordered$MAP1910)

pdf(paste0('outputs/v',version,'/full/rolling_BC_plots_500_cutoff.pdf'))

png(paste0('outputs/v',version,'/full/bimodality_coefficient_roll_pls_25bins.png'))

rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 150)
dev.off()

rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 200)
rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 300)
rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 250)
rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 100)
rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 75)
rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 50)
rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 25)
rollBC_r(ordered$MAP1910, ordered$PLSdensity, ordered$MAP1910, 10)
dev.off()

#this version of roll_BC_by10 takes the BC every 10mm of preciptiation
rollBC_by_10_r = function(x,y,xout,width) {
  out = 1:length(seq(200, 1350, by = 10) )
  for( i in 1:length(seq(200, 1350, by = 10))) {
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i] = bimodality_coefficient( y[window] )
  }
  ggplot()+geom_point(aes(x = xout, y = out))+
    geom_hline( yintercept = 5/9)+ylim(0,1)+theme_bw()+
    xlab('interval center') + ylab('Bimodality Coefficient') +ggtitle(paste0( 'Bimodality coefficient for binwidth = ', width))
}   
rollBC_by_10_r(ordered$MAP1910, ordered$PLSdensity, seq(200, 1350, by = 10)  , 100)


rollBC_by_10 = function(x,y,xout,width) {
  out = 1:length(seq(200, 1350, by = 10) )
  for( i in 1:length(seq(200, 1350, by = 10))) {
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i] = bimodality_coefficient( y[window] )
  }
  df <- data.frame(mid = xout, max = xout + width,min = xout - width,BC = out)
  
}
