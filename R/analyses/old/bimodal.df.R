bimodal.df<- function(data, binby, density, binby2){
 
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

# comp.bimodal.df produces a DF with bimodality assessment for composition data
comp.bimodal.df <- function(data = fc.m, binby, density, time){
  
  
  data <- data[data[,"period"] %in% time,]
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
  
  #merge bins with the "binby" column
  merged <- merge(coef.bins, data, by.x = "bins", by.y = binby)
  
  
  #define bimodality
  #merged$bimodal <- "Unimodal"
  #criteria for bimodality
  
  bi <- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05, "Bimodal", "Unimodal")
  merged[,c(paste0('bimodal_',density))] <- bi
  
  
  unique(merged$bimodal)
  
  
  merged
}



# comp.bimodal function  produces maps of bimodal regions and applies to the composition data 
comp.bimodal <- function(data = fc.m, binby, density, time){
  
  
  data <- data[data[,"period"] %in% time,]
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
  
  #merge bins with the "binby" column
  merged <- merge(coef.bins, data, by.x = "bins", by.y = binby)
  
  
  #define bimodality
  #merged$bimodal <- "Unimodal"
  #criteria for bimodality
  
  bi <- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05, "Bimodal", "Unimodal")
  merged$bimodal <- bi
  
  
  unique(merged$bimodal)
  
  
  ggplot()+ # geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = bimodal))+
    theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank())+
    scale_fill_manual(values = c("red", 'blue'), limits= c("Bimodal", "Unimodal"))+
    xlab("easting") + ylab("northing") +coord_equal() 
  
}


# for the full composition dataset: FIA and pls:
comp.bimodal.full <- function(data = fc.m, binby, density){
  
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
  
  #merge bins with the "binby" column
  merged <- merge(coef.bins, data, by.x = "bins", by.y = binby)
  
  
  #define bimodality
  #merged$bimodal <- "Unimodal"
  #criteria for bimodality
  
  bi <- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05, "Bimodal", "Unimodal")
  merged$bimodal <- bi
  
  
  unique(merged$bimodal)
  ggplot(merged, aes(pc2, fill = period))+geom_histogram(alpha=0.6,position = 'identity')+facet_wrap(~bins)
  ggplot(merged, aes(PC1, pc2, color = bimodal, shape = period))+geom_point()
  
  ggplot()+ # geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = bimodal))+
    theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank())+
    scale_fill_manual(values = c("red", 'blue'), limits= c("Bimodal", "Unimodal"))+
    xlab("easting") + ylab("northing") +coord_equal() +facet_grid(~period)
  
}
