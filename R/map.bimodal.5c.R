# this function is used to determine if a climate space is significantly bimodal, and then maps out bimodal classificaiton onto a map

map.bimodal.5c <- function(data, binby, density){
 
  data <- data[!is.na(data[,c(density)]),] # gets rid of 4 NA values
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
  
  merged <- merge(coef.bins, data, by.x = "bins",by.y = binby)
  #define bimodality
  merged$bimodal <- "Unimodal"
  merged$bimodal <- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05 & na.omit(merged$mode1) <= 99 & na.omit(merged$mode2) >=99, "Bimodal","Unimodal")
  
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
  
  ggplot()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = classification))+ scale_fill_manual(values = c(
      '#01665e', # light green
      '#5ab4ac', # dark teal
      '#8c510a', # red
      '#d8b365', # light tan
      '#fee08b', # tan
      'black'), limits = c('Bimodal Forest',"Unimodal Forest" ,   "Bimodal Savanna", 'Unimodal Savanna','Prairie') )+
    geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'NA')+theme_classic()+ xlim(-150000, 1150000)+
    xlab("easting") + ylab("northing")+coord_equal()+xlim(-150000, 1150000) 
  
  
}

map.bimodal <- function(data, binby, density){
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
  
  merged <- merge(coef.bins, data, by.x = "bins",by.y = binby)
  #define bimodality
  merged$bimodal <- "Unimodal"
  merged$bimodal<- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05 & na.omit(merged$mode1) <= 99 & na.omit(merged$mode2) >=99, "Bimodal", "Unimodal")
  
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
      'black'), limits = c("Unimodal Forest" , 'Unimodal Savanna', 'Bimodal Forest', "Bimodal Savanna", 'Bimodal prairie', 'Unimodal prairie') )+
    theme_bw()+
    xlab("easting") + ylab("northing") +coord_equal()+
    ggtitle(paste0(binby, ' for ',density))
  
}

