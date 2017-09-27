# this script contins functions to determine places projected to be bimodal under future climate
# and will map out these in space:

#right now there are separate functions for the modern and the pls veg-environment relationships

# function for the FIA Vegetation-environment relationships

bimodal.future.rNA <- function(data, binby, density, binby2, rcp){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 4)
  for (i in 1:length(bins)){
    coeffs[i,1] <- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)])) # calculation bimoality coefficient
    coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p # calculate p-value for hte diptest
    peaks <-  find_modes(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y)) # calculate the modes or peaks of the distribution
    
    # if there is more than one peak, list the first 2 peaks
    if(length(peaks > 1)) {
      coeffs[i,3]  <- peaks[1]
      coeffs[i,4]  <- peaks[2]
    }else{
      coeffs[i,3]  <- 0
      coeffs[i,4]  <- 0
    }
  }
  
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins <- data.frame(cbind(coeffs, bins))
  colnames(coef.bins) <- c("BC", "dipP", "mode1", "mode2", "bins") # rename columns
  coef.bins$BC <- as.numeric(as.character(coef.bins$BC))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$dipP))
  coef.bins$mode1 <- as.numeric(as.character(coef.bins$mode1))
  coef.bins$mode2 <- as.numeric(as.character(coef.bins$mode2))
  
  #merge bins iwth the second binby -> here is is future climate
  merged <- merge(coef.bins, data, by.x = "bins", by.y = binby2)
  
  
  #define bimodality
  merged$bimodal <- "Unimodal"
  #criteria for bimodality
  bimodal<- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05 & na.omit(merged$mode1) <= 99 & na.omit(merged$mode2) >=99, 
   "Bimodal", "Unimodal")
  merged$bimodal <- bimodal
  
  merged[merged[,c(paste0("rcp",rcp,"NA"))] %in% 'out-of-sample',]$bimodal <- "out-of-sample"
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  
  
  #merged
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = bimodal))+ scale_fill_manual(values = c(
      '#2c7bb6',
      'black',
      '#d7191c'
    ), limits = c('Unimodal',"out-of-sample",'Bimodal') )+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'NA')+
    theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank())+
    xlab("easting") + ylab("northing") +coord_equal() + ggtitle(binby2)
  
}


# for PLS veg-envrionment relationships:
bimodal.future.NA <- function(data, binby, density, binby2, rcp){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 4)
  
  for (i in 1:length(bins)){
    coeffs[i,1] <- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)])) # calculation bimoality coefficient
    coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p # calculate p-value for hte diptest
    peaks <-  find_modes(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y)) # calculate the modes or peaks of the distribution
    
    # if there is more than one peak, list the first 2 peaks
    if(length(peaks > 1)) {
      coeffs[i,3]  <- peaks[1]
      coeffs[i,4]  <- peaks[2]
    }else{
      coeffs[i,3]  <- 0
      coeffs[i,4]  <- 0
    }
  }
  
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins <- data.frame(cbind(coeffs, bins))
  colnames(coef.bins) <- c("BC", "dipP", "mode1", "mode2", "bins") # rename columns
  coef.bins$BC <- as.numeric(as.character(coef.bins$BC))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$dipP))
  coef.bins$mode1 <- as.numeric(as.character(coef.bins$mode1))
  coef.bins$mode2 <- as.numeric(as.character(coef.bins$mode2))
  
  
  #merge bins iwth the second binby -> here is is future climate
  merged <- merge(coef.bins, dens.pr, by.x = "bins", by.y = binby2)
  
  
  #define bimodality
  merged$bimodal <- "Unimodal"
  #criteria for bimodality
  merged[merged$BC >= 0.55 & merged$dipP <= 0.05 & na.omit(merged$mode1) <= 99 & na.omit(merged$mode2) >=99, ]$bimodal <- "Bimodal"
  merged[merged[,c(paste0("rcp",rcp,"NA"))] %in% 'out-of-sample',]$bimodal <- "out-of-sample"
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  
  
  #merged
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = bimodal))+ scale_fill_manual(values = c(
      '#2c7bb6',
      'black',
      '#d7191c'
    ), limits = c('Unimodal',"out-of-sample",'Bimodal') )+
    theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank())+
    xlab("easting") + ylab("northing") +coord_equal() + ggtitle(binby2)
  
}



bimodal.future <- function(data, binby,binby2, density){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 4)
  
  
  for (i in 1:length(bins)){
    if(nrow(na.omit(data[data[,binby] %in% bins[i],])) > 1){
      coeffs[i,1] <- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)])) # calculation bimoality coefficient
      coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p # calculate p-value for hte diptest
      peaks <-  find_modes(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y)) # calculate the modes or peaks of the distribution
      
      # if there is more than one peak, list the first 2 peaks
      if(length(peaks > 1)) {
        coeffs[i,3]  <- peaks[1]
        coeffs[i,4]  <- peaks[2]
      }else{
        coeffs[i,3]  <- 0
        coeffs[i,4]  <- 0
      }
    }else{
      coeffs[i,1] <- "NA"
      coeffs[i,2] <- "NA"
    }
  }
  
  coeffs[is.nan(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins <- data.frame(cbind(coeffs, bins))
  colnames(coef.bins) <- c("BC", "dipP", "mode1", "mode2", "bins") # rename columns
  coef.bins$BC <- as.numeric(as.character(coef.bins$BC))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$dipP))
  coef.bins$mode1 <- as.numeric(as.character(coef.bins$mode1))
  coef.bins$mode2 <- as.numeric(as.character(coef.bins$mode2))
  
  #merge bins iwth the second binby -> here is is future climate
  merged <- merge(coef.bins, dens.pr, by.x = "bins", by.y = binby2)
  
  
  #define bimodality
  
  merged$bimodal <- "Unimodal"
  #criteria for bimodality
  bimodal<- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05 & na.omit(merged$mode1) <= 99 & na.omit(merged$mode2) >=99, 
                   "Bimodal", "Unimodal")
  merged$bimodal <- bimodal
  #define bimodal savanna/forest and not bimodal savanna & forest 
  
  
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = bimodal))+ scale_fill_manual(values = c(
      '#d7191c','#2c7bb6'
      #'black',
      ), limits = c('Bimodal',"Unimodal") )+
    geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'NA')+theme_classic()+ xlim(-150000, 1150000)+
    xlab("easting") + ylab("northing")+coord_equal()+xlim(-150000, 1150000) 
  
  
}

# bimodal.df function outputs the dataframe of bimodal/not bimodal 

bimodal.df <- function(data, binby, density, binby2){
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 4)
  
  
  for (i in 1:length(bins)){
    if(nrow(na.omit(data[data[,binby] %in% bins[i],])) > 1){
      coeffs[i,1] <- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)])) # calculation bimoality coefficient
      coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p # calculate p-value for hte diptest
      peaks <-  find_modes(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y)) # calculate the modes or peaks of the distribution
      
      # if there is more than one peak, list the first 2 peaks
      if(length(peaks > 1)) {
        coeffs[i,3]  <- peaks[1]
        coeffs[i,4]  <- peaks[2]
      }else{
        coeffs[i,3]  <- 0
        coeffs[i,4]  <- 0
      }
    }else{
      coeffs[i,1] <- "NA"
      coeffs[i,2] <- "NA"
    }
  }
  
  coeffs[is.nan(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins <- data.frame(cbind(coeffs, bins))
  colnames(coef.bins) <- c("BC", "dipP", "mode1", "mode2", "bins") # rename columns
  coef.bins$BC <- as.numeric(as.character(coef.bins$BC))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$dipP))
  coef.bins$mode1 <- as.numeric(as.character(coef.bins$mode1))
  coef.bins$mode2 <- as.numeric(as.character(coef.bins$mode2))
  
  #merge bins iwth the second binby -> here is is future climate
  merged <- merge(coef.bins, dens.pr, by.x = "bins", by.y = binby2)
  
  
  #define bimodality
  
  merged$bimodal <- "Unimodal"
  #criteria for bimodality
  bimodal<- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05 & na.omit(merged$mode1) <= 99 & na.omit(merged$mode2) >=99, 
                   "Bimodal", "Unimodal")
  merged$bimodal <- bimodal
  
  #define bimodal savanna/forest and not bimodal savanna & forest 
  if(density == "PLSdensity"){
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$ecotype)
    merged[merged$classification %in% 'Bimodal prairie',]$classification <- "Prairie"
    merged[merged$classification %in% 'Unimodal prairie',]$classification <- "Prairie"
    
  }else{
    merged$classification <- "test"
    merged$classification <- paste(merged$bimodal, merged$fiaecotype)
    
  }
  
  merged
}

