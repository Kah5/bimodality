# this function is used to determine if a climate space is significantly bimodal, and then maps out bimodal classificaiton onto a map

map.bimodal.5c <- function(data, binby, density){
  data <- data[!is.na(data[,c(density)]),] # gets rid of 4 NA values
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
  merged <- merge(coef.bins, data, by.x = "bins",by.y = binby)
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
    geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'NA')+theme_classic()+ xlim(-150000, 1150000)+
    xlab("easting") + ylab("northing")+coord_equal()+xlim(-150000, 1150000) 
  
  
}

