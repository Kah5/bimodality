find.noanalog<- function(dens.pr,rcp){
  dens.pr[,c(paste0("rcp",rcp,"NA"))] <- "Within Range"
  dens.pr[,c(paste0('rcp',rcp,"NAclim"))] <- "Within Range" # column labeling the reason climate is out of range
  #dens.pr[,c(paste0('rcp',rcp,"NAclimhigh"))] <- "Within Range" # column labeling the reason climate is out of range
  
  # rcp 4.5
  prange <- range(dens.pr$MAP1910)
  trange <- range(dens.pr$pasttmean)
  pcvrange <- range(dens.pr$pastdeltaP)
  tcvrange <- range(dens.pr$deltaT)
  
  # identify climate space outside of the modern/pls range
  precip.out <- dens.pr[dens.pr[,c(paste0("pr.",rcp))] < prange[1] | dens.pr[,c(paste0("pr.",rcp))] > prange[2], ]
  pcv.out <- dens.pr[dens.pr[,c(paste0("pr.",rcp,"SI"))] < pcvrange[1] | dens.pr[,c(paste0("pr.",rcp,"SI"))]> pcvrange[2], ]
  temp.out <- dens.pr[dens.pr[,c(paste0("tn.",rcp))] < trange[1] | dens.pr[,c(paste0("tn.",rcp))]> trange[2], ]
  tcv.out <- dens.pr[dens.pr[,c(paste0("tn.",rcp,"cv"))] < tcvrange[1] | dens.pr[,c(paste0("tn.",rcp,"cv"))]> tcvrange[2], ]
  
  # identify grid cells with climate space lower than modern/pls
  pcv.low <- dens.pr[dens.pr[,c(paste0("pr.",rcp,"SI"))] < pcvrange[1],  ]
  tcv.low <- dens.pr[dens.pr[,c(paste0("tn.",rcp,"cv"))] < tcvrange[1] , ]
  precip.low <- dens.pr[dens.pr[,c(paste0("pr.",rcp))] < prange[1], ]
  temp.low <- dens.pr[dens.pr[,c(paste0("tn.",rcp))] < trange[1] , ]
  
  dens.pr[dens.pr$cell %in% precip.low$cell, c(paste0("rcp",rcp,"NAclim"))] <- "Low precip"
  dens.pr[dens.pr$cell %in% temp.low$cell, c(paste0("rcp",rcp,"NAclim"))] <- "Low temp"
  dens.pr[dens.pr$cell %in% tcv.low$cell, c(paste0("rcp",rcp,"NAclim"))] <- "Low temp CV"
  dens.pr[dens.pr$cell %in% pcv.low$cell, c(paste0("rcp",rcp,"NAclim"))] <- "Low precip SI"
  
  # identify the gird cells that are higher than modern/pls
  pcv.high <- dens.pr[ dens.pr[,c(paste0("pr.",rcp,"SI"))]> pcvrange[2], ]
  tcv.high <- dens.pr[ dens.pr[,c(paste0("tn.",rcp,"cv"))]> tcvrange[2], ]
  precip.high <- dens.pr[ dens.pr[,c(paste0("pr.",rcp))] > prange[2], ]
  temp.high <- dens.pr[ dens.pr[,c(paste0("tn.",rcp))]> trange[2], ]
  
  dens.pr[dens.pr$cell %in% precip.high$cell, c(paste0("rcp",rcp,"NAclim"))] <- "High precip"
  dens.pr[dens.pr$cell %in% temp.high$cell, c(paste0("rcp",rcp,"NAclim"))] <- "High temp"
  dens.pr[dens.pr$cell %in% tcv.high$cell, c(paste0("rcp",rcp,"NAclim"))] <- "High temp CV"
  dens.pr[dens.pr$cell %in% pcv.high$cell, c(paste0("rcp",rcp,"NAclim"))] <- "High precip SI"
  
  
  dens.pr[dens.pr$cell %in% precip.out$cell, c(paste0("rcp",rcp,"NA"))] <- "no-analog"
  dens.pr[dens.pr$cell %in% temp.out$cell, c(paste0("rcp",rcp,"NA"))] <- "no-analog"
  dens.pr[dens.pr$cell %in% tcv.out$cell, c(paste0("rcp",rcp,"NA"))] <- "no-analog"
  dens.pr[dens.pr$cell %in% pcv.out$cell, c(paste0("rcp",rcp,"NA"))] <- "no-analog"
  
  dens.pr[,c("x","y", "cell", paste0("rcp",rcp,"NA"), paste0('rcp', rcp,"NAclim"))]
  
}