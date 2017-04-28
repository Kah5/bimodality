# this script contins functions to determine places projected to be bimodal under future climate
# and will map out these in space:

#right now there are separate functions for the modern and the pls veg-environment relationships

# function for the FIA Vegetation-environment relationships

bimodal.future.rNA <- function(data, binby, density, binby2, rcp){
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
  merged <- merge(coef.bins, data, by.x = "bins", by.y = binby2)
  
  
  #define bimodality
  merged$bimodal <- "Stable"
  #criteria for bimodality
  if(merged$BC >= 0.55 & merged$dipP <= 0.05){
    merged$bimodal <- "Bistable"
  }else{ merged$bimodal <- "Stable"}
  
  merged[merged[,c(paste0("rcp",rcp,"NA"))] %in% 'no-analog',]$bimodal <- "no-analog"
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  
  
  #merged
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = bimodal))+ scale_fill_manual(values = c(
      '#2c7bb6',
      'black',
      '#d7191c'
    ), limits = c('Stable',"no-analog",'Bistable') )+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'NA')+
    theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank())+
    xlab("easting") + ylab("northing") +coord_equal() + ggtitle(binby2)
  
}


# for PLS veg-envrionment relationships:
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
  merged[merged$BC >= 0.55 & merged$dipP <= 0.05,]$bimodal <- "Bistable"
  merged[merged[,c(paste0("rcp",rcp,"NA"))] %in% 'no-analog',]$bimodal <- "no-analog"
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  
  
  #merged
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = bimodal))+ scale_fill_manual(values = c(
      '#2c7bb6',
      'black',
      '#d7191c'
    ), limits = c('Stable',"no-analog",'Bistable') )+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'NA')+
    theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank())+
    xlab("easting") + ylab("northing") +coord_equal() +ggtitle(binby)
  
}