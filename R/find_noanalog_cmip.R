find.noanalog<- function(clim,rcp){
  
  clim[,c(paste0("rcp",rcp,"NA"))] <- "Within Range"
  clim[,c(paste0('rcp',rcp,"NAclim"))] <- "Within Range" # column labeling the reason climate is out of range
  #clim[,c(paste0('rcp',rcp,"NAclimhigh"))] <- "Within Range" # column labeling the reason climate is out of range
  
  # ranges for historic climate
  prange <- range(data.frame(clim$MAP1910, clim$MAP2011), na.rm=TRUE)
  trange <- range(clim$pasttmean, clim$modtmean, na.rm=TRUE)
  
  pcvrange <- range(clim$pastdeltaP, clim$moderndeltaP, na.rm=TRUE)
  tcvrange <- range(clim$deltaT, clim$moddeltaT, na.rm=TRUE)
  
  # identify climate space outside of the modern/pls range
  precip.out <- clim[clim[,c(paste0("pr.",rcp))] < prange[1] | clim[,c(paste0("pr.",rcp))] > prange[2], ]
  pcv.out <- clim[clim[,c(paste0("pr.",rcp,"SI"))] < pcvrange[1] | clim[,c(paste0("pr.",rcp,"SI"))]> pcvrange[2], ]
  temp.out <- clim[clim[,c(paste0("tn.",rcp))] < trange[1] | clim[,c(paste0("tn.",rcp))]> trange[2], ]
  tcv.out <- clim[clim[,c(paste0("tn.",rcp,"cv"))] < tcvrange[1] | clim[,c(paste0("tn.",rcp,"cv"))]> tcvrange[2], ]
  
  # identify grid cells with climate space lower than modern/pls
  pcv.low <- clim[clim[,c(paste0("pr.",rcp,"SI"))] < pcvrange[1],  ]
  tcv.low <- clim[clim[,c(paste0("tn.",rcp,"cv"))] < tcvrange[1] , ]
  precip.low <- clim[clim[,c(paste0("pr.",rcp))] < prange[1], ]
  temp.low <- clim[clim[,c(paste0("tn.",rcp))] < trange[1] , ]
  
  clim[clim$cell %in% precip.low$cell, c(paste0("rcp",rcp,"NAclim"))] <- "Low precip"
  clim[clim$cell %in% temp.low$cell, c(paste0("rcp",rcp,"NAclim"))] <- "Low temp"
  clim[clim$cell %in% tcv.low$cell, c(paste0("rcp",rcp,"NAclim"))] <- "Low temp CV"
  clim[clim$cell %in% pcv.low$cell, c(paste0("rcp",rcp,"NAclim"))] <- "Low precip SI"
  
  # identify the gird cells that are higher than modern/pls
  pcv.high <- clim[ clim[,c(paste0("pr.",rcp,"SI"))]> pcvrange[2], ]
  tcv.high <- clim[ clim[,c(paste0("tn.",rcp,"cv"))]> tcvrange[2], ]
  precip.high <- clim[ clim[,c(paste0("pr.",rcp))] > prange[2], ]
  temp.high <- clim[ clim[,c(paste0("tn.",rcp))]> trange[2], ]
  
  clim[clim$cell %in% precip.high$cell, c(paste0("rcp",rcp,"NAclim"))] <- "High precip"
  clim[clim$cell %in% temp.high$cell, c(paste0("rcp",rcp,"NAclim"))] <- "High temp"
  clim[clim$cell %in% tcv.high$cell, c(paste0("rcp",rcp,"NAclim"))] <- "High temp CV"
  clim[clim$cell %in% pcv.high$cell, c(paste0("rcp",rcp,"NAclim"))] <- "High precip SI"
  #clim[is.na(clim$cell), c(paste0("rcp",rcp,"NAclim"))] <- "NOdata"
  
  clim[clim$cell %in% na.omit(precip.out$cell), c(paste0("rcp",rcp,"NA"))] <- "out-of-sample"
  clim[clim$cell %in% na.omit(temp.out$cell), c(paste0("rcp",rcp,"NA"))] <- "out-of-sample"
  clim[clim$cell %in% na.omit(tcv.out$cell), c(paste0("rcp",rcp,"NA"))] <- "out-of-sample"
  clim[clim$cell %in% na.omit(pcv.out$cell), c(paste0("rcp",rcp,"NA"))] <- "out-of-sample"
  
  
  clim[,c("x","y", "cell", paste0("rcp",rcp,"NA"), paste0('rcp', rcp,"NAclim"))]
  
}