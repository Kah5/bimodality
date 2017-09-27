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
