# this script runs the same analyses as figure 3 (kde_bimodal_preds.R)
# need to run the same analysis but only on grid cells that are similar across PLS and FIA:
# Specifically, we need to:
# 1. Remove grid cells that do not have FIA grid cells in them
# 2. Remove grid cells that are not with the overlapping FIA and PLS climate space (need to do this for each climate param)
# 3. Remove grid cells in PLS that have less than 14 trees / HA

library("MASS")
library(ggplot2)

pls.df <- read.csv("data/PLS_FIA_density_climate_full.csv")

# get the differece btween # of FIA grid cells and PLS grid cells:

((nrow(pls.df[!is.na(pls.df$PLSdensity),]) - nrow(pls.df[!is.na(pls.df$FIAdensity),])) / nrow(pls.df[!is.na(pls.df$PLSdensity),]))*100

pls.nona <- pls.df[!is.na(pls.df$PLSdensity) & pls.df$PLSdensity <= 550,]

pls.nona <- pls.nona[!is.na(pls.nona$PC1),]

pls.nona <- pls.nona [ pls.nona$PLSdensity >= 14 & !is.na(pls.nona$FIAdensity), ]


ggplot(pls.nona, aes(x, y, fill = PLSdensity))+geom_raster()


pls.df <- pls.nona

#pls.df <- data.frame(PC1 = pls.nona$PC1, PLSdensity =pls.nona$PLSdensity, x = pls.nona$x, y = pls.nona$y)

pls.df[duplicated(pls.df),]
pls.df<- pls.df[pls.df$PLSdensity < 550,]






#<<<<<<<<<<<<<<<<<<<<<<<< estimate PDF of data using kde >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# estimate surfaces:
library(ks)
# get the range of PC1 climate for PLS and FIA:
range(pls.df$PC1)
range(pls.df$PC1fia)

pls.pc1 <- pls.df[pls.df$PC1 <= range(pls.df$PC1fia)[2] & pls.df$PC1 >= range(pls.df$PC1)[1],]

H <- Hpi.diag(x=na.omit(cbind(pls.pc1$PC1, pls.pc1$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(pls.pc1$PC1, pls.pc1$PLSdensity)), #H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,25,50,60,75,80,85,95))
#points(na.omit(cbind(pls.pc1$PC1, pls.pc1$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)

# get points to evaluate:

diptest::dip.test(kde(x=na.omit(cbind(pls.pc1$PC1, pls.pc1$PLSdensity)), #H=H, 
                      compute.cont = TRUE, eval.points = cbind(rep(2,contour_95[which.min(abs(contour_95$x - -4)),]$y), 0:round(contour_95[which.min(abs(contour_95$x - -4)),]$y, 1)))$estimate)

# need to evaluate diptest over the density values where we have 95% probability of data:


interp.densp <- function(pc1val){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - pc1val)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(pc1val, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(pc1val, 401 ), y=0:400)
  points <- expand.grid(seq(round(pc1val, 2) - 0.05, round(pc1val, 2) + 0.05, by = 0.1), y=0:maxy)
  colnames(points) <- c("x", "y")
  if(max(kde(x=na.omit(cbind(pls.pc1$PC1, pls.pc1$PLSdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    
    dipP <- NA
    
  }else{
    
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.pc1$PC1, pls.pc1$PLSdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    samp <- sample(x=df$points, prob = df$freq, size = 10000, replace = TRUE)
    dipP <- diptest::dip.test(samp)$p.value
    pks <- amps(samp)$Peaks[,1]
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    samp2 <- sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE)
    dipP2 <- diptest::dip.test(samp2)$p.value
    pks2 <- amps(samp2)$Peaks[,1]
    
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    dipP <- ifelse(length(pks) >= 2 & max(pks) >= 100 & dipP2 <= 0.05, dipP, 1) 
    
    #plot(density(samp))
  }
  dipP
  #pks
}

interp.densp(pc1val = 0.3)




pls.pc1$dipPint <- NA
pls.pc1 <- pls.pc1[!is.na(pls.pc1$PC1),]



pls.pc1$dipPint <- apply(data.frame(pls.pc1$PC1), 1, interp.densp)
#pls.pc1$dipPint <- as.numeric(pls.pc1$dipPint)
pls.pc1$bimclass <- ifelse(pls.pc1$dipPint <= 0.05 , "bimodal", "unimodal")
png("outputs/pls_dipP_kdeest_0.1_bin_mode_crit_1000_non_ag.png")
ggplot(pls.pc1, aes(x,y, fill = dipPint))+geom_raster()
dev.off()

png("outputs/pls_bimodal_kdeest_0.1_bin_mode_crit_1000_non_ag.png")
ggplot(pls.pc1, aes(x,y, fill = bimclass))+geom_raster()
dev.off()

write.csv(pls.pc1, "outputs/new_bim_surface_PC1_pls_0.1_mode_crit_1000_non_ag.csv", row.names = FALSE)

# >>>>>>>>>>>>>>>>>>>>>>>>>do the same for P-PET:

range(pls.df$GS_ppet)
range(pls.df$GS_ppet_mod)

pls.ppet <- pls.df[pls.df$GS_ppet <= range(pls.df$GS_ppet)[2] & pls.df$GS_ppet >= range(pls.df$GS_ppet_mod)[1],]


H <- Hpi.diag(x=na.omit(cbind(pls.ppet$GS_ppet, pls.ppet$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(pls.ppet$GS_ppet, pls.ppet$PLSdensity)), H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,15,25,40,35,45,50,60,75,80,85,95))
#points(na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)



interp.densp.ppet <- function(ppetval){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - ppetval)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(ppetval, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(ppetval, 401 ), y=0:400)
  points <- expand.grid(seq(round(ppetval, 2) - 4, round(ppetval, 2) + 4, by = 0.5), y=0:maxy)
  colnames(points) <- c("x", "y")
  if(max(kde(x=na.omit(cbind(pls.df$GS_ppet, pls.df$PLSdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$GS_ppet, pls.df$PLSdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    samp <- sample(x=df$points, prob = df$freq, size = 10000, replace = TRUE)
    dipP <- diptest::dip.test(samp)$p.value
    pks <- amps(samp)$Peaks[,1]
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    samp2 <- sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE)
    dipP2 <- diptest::dip.test(samp2)$p.value
    pks2 <- amps(samp2)$Peaks[,1]
    
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    dipP <- ifelse(length(pks) >= 2 & max(pks) >= 100 & dipP2 <= 0.05, dipP, 1) 
    
    #plot(density(samp))
  }
  dipP
  #pks
}
interp.densp.ppet(ppetval = 100)


pls.ppet$dipPint_ppet <- apply(data.frame(pls.ppet$GS_ppet), 1, interp.densp.ppet)
#pls.df$dipPint <- as.numeric(pls.df$dipPint)
pls.ppet$bimclass_ppet <- ifelse(pls.ppet$dipPint_ppet <= 0.05 , "bimodal", "unimodal")
png("outputs/pls_dipP_kdeest_ppet_4_mode_crit_1000_nonag.png")
ggplot(pls.ppet, aes(x,y, fill = dipPint_ppet))+geom_raster()
dev.off()

png("outputs/pls_bimodal_kdeest_ppet_4_mode_crit_1000_non_ag.png")
ggplot(pls.ppet, aes(x,y, fill = bimclass_ppet))+geom_raster()
dev.off()

write.csv(pls.ppet, "outputs/new_bim_surface_PPET_pls_4_mode_crit_1000_non_ag.csv", row.names = FALSE)


# >>>>>>>>>>>>>>>>>>>>>>>>>do the same for GS_soil:
range(pls.df$mean_GS_soil)
range(pls.df$mean_GS_soil_m)

pls.soil <- pls.df[pls.df$mean_GS_soil <= range(pls.df$mean_GS_soil_m)[2] & pls.df$mean_GS_soil >= range(pls.df$mean_GS_soil)[1],]


H <- Hpi.diag(x=na.omit(cbind(pls.soil$mean_GS_soil, pls.soil$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(pls.soil$mean_GS_soil, pls.soil$PLSdensity)), H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,15,25,40,35,45,50,60,75,80,85,90,95))
#points(na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)



interp.densp.soil <- function(soilval){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - soilval)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(soilval, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(soilval, 401 ), y=0:400)
  points <- expand.grid(seq(round(soilval, 2) - 0.05, round(soilval, 2) + 0.05, by = 0.001), y=0:maxy)
  colnames(points) <- c("x", "y")
  if(max(kde(x=na.omit(cbind(pls.df$mean_GS_soil, pls.df$PLSdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$mean_GS_soil, pls.df$PLSdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    samp <- sample(x=df$points, prob = df$freq, size = 10000, replace = TRUE)
    dipP <- diptest::dip.test(samp)$p.value
    pks <- amps(samp)$Peaks[,1]
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    samp2 <- sample(x=df$points, prob = df$freq, size = 10000, replace = TRUE)
    dipP2 <- diptest::dip.test(samp2)$p.value
    pks2 <- amps(samp2)$Peaks[,1]
    
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    dipP <- ifelse(length(pks) >= 2 & max(pks) >= 100 & dipP2 <= 0.05, dipP, 1) 
    
    #plot(density(samp))
  }
  dipP
  #pks
}


interp.densp.soil(soilval = 0.57)


pls.soil$dipPint_soil <- apply(data.frame(pls.soil$mean_GS_soil), 1, interp.densp.soil)
#pls.df$dipPint <- as.numeric(pls.df$dipPint)
pls.soil$bimclass_soil <- ifelse(pls.soil$dipPint_soil <= 0.05 , "bimodal", "unimodal")
png("outputs/pls_dipP_kdeest_soil_0.1_mode_crit_1000_non_ag.png")
ggplot(pls.soil, aes(x,y, fill = dipPint_soil))+geom_raster()
dev.off()

png("outputs/pls_bimodal_kdeest_soil_0.1_mode_crit_1000_non_ag.png")
ggplot(pls.soil, aes(x,y, fill = bimclass_soil))+geom_raster()
dev.off()

write.csv(pls.soil, "outputs/new_bim_surface_soil_moist_pls_0.1_mode_crit_1000_non_ag.csv", row.names = FALSE)



# >>>>>>>>>>>>>>>> get bimodality for FIA data <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

H <- Hpi.diag(x=na.omit(cbind(pls.pc1$PC1fia, pls.pc1$FIAdensity)) )
fhat <- kde(x=na.omit(cbind(pls.pc1$PC1fia, pls.pc1$FIAdensity)), #H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,25,50,60,75,80,85,95))
#points(na.omit(cbind(pls.pc1$PC1, pls.pc1$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
plot(fhat, display = "persp", xlab = "PC1fia", ylab = "FIAdensity")
contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["16%"])[[1]])
contour_95 <- data.frame(contour_95)



interp.densp <- function(pc1val){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - pc1val)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(pc1val, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(pc1val, 401 ), y=0:400)
  points <- expand.grid(seq(round(pc1val, 2) - 0.07, round(pc1val, 2) + 0.07, by = 0.005), y=0:maxy)
  colnames(points) <- c("x", "y")
  if(max(kde(x=na.omit(cbind(pls.df$PC1fia, pls.df$FIAdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    
    dipP <- NA
    
  }else{
    
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$PC1fia, pls.df$FIAdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    samp <- sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE)
    dipP <- diptest::dip.test(samp, simulate.p.value = TRUE, B = 500)$p.value
    pks <- amps(samp)$Peaks[,1]
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    dipP <- ifelse(length(pks) >= 2 & max(pks) >= 100, dipP, 1) # only take bimodal places where we can identify the modes and that have one forest mode
    
    # draw from a second distribution--if it is actally bimodal, the second distribution is likely to also show bimodality
    # however, if the sampled population is bimodal by chance, then drawing from a second distribution will prevent this:
    samp2 <- sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE)
    dipP2 <- diptest::dip.test(samp2)$p.value
    pks2 <- amps(samp2)$Peaks[,1]
    
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    dipP <- ifelse(length(pks) >= 2 & max(pks) >= 100 & dipP2 <= 0.05, dipP, 1) 
    
  }
  dipP
  #pks
}


interp.densp(pc1val = 0.37403230)

pls.pc1$dipPint_f <- NA
pls.pc1 <- pls.pc1[!is.na(pls.pc1$PC1fia),]



pls.pc1$dipPint_f <- apply(data.frame(pls.pc1$PC1fia), 1, interp.densp)

pls.pc1$bimclass_f <- ifelse(pls.pc1$dipPint_f <= 0.05 , "bimodal", "unimodal")
png("outputs/fia_dipP_kdeest_0.1_mode_crit_1000_non_ag.png")
ggplot(pls.pc1, aes(x,y, fill = dipPint_f))+geom_raster()
dev.off()

png("outputs/fia_bimodal_kdeest_0.1_mode_crit_1000_non_ag.png")
ggplot(pls.pc1, aes(x,y, fill = bimclass_f))+geom_raster()
dev.off()

write.csv(pls.pc1, "outputs/new_bim_surface_PC1_fia_0.1_mode_crit_1000_non_ag.csv", row.names = FALSE)

# >>>>>>>>>>>>>>>>>>>>>>>>>do the same for P-PET on the modern landscape:
H <- Hpi.diag(x=na.omit(cbind(pls.ppet$GS_ppet_mod, pls.ppet$FIAdensity)) )
fhat <- kde(x=na.omit(cbind(pls.ppet$GS_ppet_mod, pls.ppet$FIAdensity)), H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,15,25,40,35,45,50,60,75,80,85,95))
#points(na.omit(cbind(pls.ppet$PC1, pls.ppet$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)

interp.densp.ppet <- function(ppetval){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - ppetval)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(ppetval, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(ppetval, 401 ), y=0:400)
  points <- expand.grid(seq(round(ppetval, 2) - 5, round(ppetval, 2) + 5, by = 0.5), y=0:maxy)
  colnames(points) <- c("x", "y")
  if(max(kde(x=na.omit(cbind(pls.df$GS_ppet_mod, pls.df$FIAdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$GS_ppet_mod, pls.df$FIAdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    samp <- sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE)
    dipP <- diptest::dip.test(samp)$p.value
    pks <- amps(samp)$Peaks[,1]
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    samp2 <- sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE)
    dipP2 <- diptest::dip.test(samp2)$p.value
    pks2 <- amps(samp2)$Peaks[,1]
    
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    dipP <- ifelse(length(pks) >= 2 & max(pks) >= 100 & dipP2 <= 0.05, dipP, 1) 
    
    #plot(density(samp))
  }
  dipP
  #pks
}
interp.densp.ppet(ppetval = 150)


pls.ppet$dipPint_ppet_f <- apply(data.frame(pls.ppet$GS_ppet_mod), 1, interp.densp.ppet)
#pls.ppet$dipPint <- as.numeric(pls.ppet$dipPint)
pls.ppet$bimclass_ppet_f <- ifelse(pls.ppet$dipPint_ppet_f <= 0.05 , "bimodal", "unimodal")
png("outputs/fia_dipP_kdeest_ppet_4_mode_crit_1000_non_ag.png")
ggplot(pls.ppet, aes(x,y, fill = dipPint_ppet_f))+geom_raster()
dev.off()

png("outputs/fia_bimodal_kdeest_ppet_4_mode_crit_1000_non_ag.png")
ggplot(pls.ppet, aes(x,y, fill = bimclass_ppet_f))+geom_raster()
dev.off()

write.csv(pls.ppet, "outputs/new_bim_surface_PPET_fia_4_mode_crit_1000_non_ag.csv", row.names = FALSE)


# >>>>>>>>>>>>>>>>>>>>>>>>>do the same for GS_soil:

H <- Hpi.diag(x=na.omit(cbind(pls.soil$mean_GS_soil_m, pls.soil$FIAdensity)) )
fhat <- kde(x=na.omit(cbind(pls.soil$mean_GS_soil_m, pls.soil$FIAdensity)), H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,15,25,40,35,45,50,60,75,80,85,95))
#points(na.omit(cbind(pls.soil$PC1, pls.soil$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)


interp.densp.soil <- function(soilval){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - soilval)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(soilval, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(soilval, 401 ), y=0:400)
  points <- expand.grid(seq(round(soilval, 2) - 0.05, round(soilval, 2) + 0.05, by = 0.001), y=0:maxy)
  colnames(points) <- c("x", "y")
  if(max(kde(x=na.omit(cbind(pls.df$mean_GS_soil_m, pls.df$FIAdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$mean_GS_soil_m, pls.df$FIAdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    samp <- sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE)
    dipP <- diptest::dip.test(samp)$p.value
    pks <- amps(samp)$Peaks[,1]
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    samp2 <- sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE)
    dipP2 <- diptest::dip.test(samp2)$p.value
    pks2 <- amps(samp2)$Peaks[,1]
    
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    dipP <- ifelse(length(pks) >= 2 & max(pks) >= 100 & dipP2 <= 0.05, dipP, 1) 
    
    #plot(density(samp))
  }
  dipP
  #pks
}


interp.densp.soil(soilval = 1.22)
interp.densp.soil(round(0.6112, 2))

pls.soil$dipPint_soil_f <- apply(data.frame(pls.soil$mean_GS_soil_m), 1, interp.densp.soil)
#pls.soil$dipPint <- as.numeric(pls.soil$dipPint)
pls.soil$bimclass_soil_f <- ifelse(pls.soil$dipPint_soil_f <= 0.05 , "bimodal", "unimodal")
png("outputs/fia_dipP_kdeest_soil_0.01_mode_crit_1000.png")
ggplot(pls.soil, aes(x,y, fill = dipPint_soil_f))+geom_raster()
dev.off()

png("outputs/fia_bimodal_kdeest_soil_0.01_mode_crit_1000.png")
ggplot(pls.soil, aes(x,y, fill = bimclass_soil_f))+geom_raster()
dev.off()

write.csv(pls.soil, "outputs/new_bim_surface_soil_moist_fia_0.01_mode_crit_1000_non_ag.csv", row.names = FALSE)





#>>>>>>>>>>>>>>>>>>>>>>>>>>>> Figure 3 repeat with non-ag cells <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ---------------------------------- PLS PC1 figure --------------------------------------------

library(ks)
library(ggplotify)
# for PC1:
H <- Hpi.diag(x=na.omit(cbind(pls.pc1$PC1, pls.pc1$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(pls.pc1$PC1, pls.pc1$PLSdensity)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550))
#points(na.omit(cbind(pls.pc1$PC1, pls.pc1$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)


bimodal.pls.pc1 <- read.csv("outputs/new_bim_surface_PC1_pls_0.1_mode_crit_1000_non_ag.csv")
pc1.bim.line <- data.frame(PC1 = unique(bimodal.pls.pc1[bimodal.pls.pc1$bimclass %in% "bimodal",]$PC1), y = -37, bimodal = "bimodal")
pls.kde.plot.pc1 <- recordPlot()
library(ggplotify)

# make the plot with GGPLOT:
pls.kde.plot.pc1.gg <- ggplot(pls.pc1, aes(x=PC1, y=PLSdensity) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_distiller(palette= c("YlOrRd"), direction=1 )+ylab("Tree Density")+theme(legend.position = "none")


pls.kde.plot.pc1.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7 ) + points(data = pc1.bim.line[pc1.bim.line$bimodal %in% "bimodal",], y~PC1, cex = 0.9,  pch = 15,col = "darkblue")+ text(-5.5,500, "A"))
pls.kde.plot.pc1.gg


# ---------------------------------- PLS PPET figure --------------------------------------------

H <- Hpi.diag(x=na.omit(cbind(pls.ppet$GS_ppet, pls.ppet$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(pls.ppet$GS_ppet, pls.ppet$PLSdensity)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,75,80,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(0,550))
#points(na.omit(cbind(pls.ppet$PC1, pls.ppet$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)


bimodal.pls.ppet <- read.csv("outputs/new_bim_surface_PPET_pls_4_mode_crit_1000_non_ag.csv")
ppet.bim.line <- data.frame(PPET = unique(bimodal.pls.ppet[bimodal.pls.ppet$bimclass_ppet %in% "bimodal",]$GS_ppet), y = -37, bimodal = "bimodal")


# ggplotify the kde plots here:
pls.kde.plot.ppet.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim = c(-200, 300), cex.axis = 0.7) + points(data = ppet.bim.line[ppet.bim.line$bimodal %in% "bimodal",], y~PPET, cex = 0.9,  pch = 15,col = "darkblue") + text(-170,500, "C"))
pls.kde.plot.ppet.gg


# ---------------------------------- PLS Growing Season soil moisture figure --------------------------------------------
H <- Hpi.diag(x=na.omit(cbind(pls.soil$mean_GS_soil, pls.soil$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(pls.soil$mean_GS_soil, pls.soil$PLSdensity)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,65,75,80,85,95), xlab = "Growing season soil", ylab = "Tree density", ylim = c(-40,550))

plot(fhat, display="slice", cont=c(85), add = TRUE)

bimodal.pls.soil <- read.csv("outputs/new_bim_surface_soil_moist_pls_0.1_mode_crit_1000_non_ag.csv")
sm.bim.line <- data.frame(SM = unique(bimodal.pls.soil[bimodal.pls.soil$bimclass_soil %in% "bimodal",]$mean_GS_soil), y = -37, bimodal = "bimodal")

# ggplotify the kde plots here:
pls.kde.plot.sm.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7) + points(data = sm.bim.line[sm.bim.line$bimodal %in% "bimodal",], y~SM, cex = 0.9,  pch = 15,col = "darkblue") + text(-0,500, "E"))
pls.kde.plot.sm.gg


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> PLOT FIA FIGURES <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ---------------------------------- FIA PC1 figure --------------------------------------------

H <- Hpi.diag(x=na.omit(cbind(pls.pc1$PC1fia, pls.pc1$FIAdensity)) )
fhat <- kde(x=na.omit(cbind(pls.pc1$PC1fia, pls.pc1$FIAdensity)), H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density")
#points(na.omit(cbind(pls.pc1$PC1, pls.pc1$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

bimodal.fia.pc1 <- read.csv("outputs/new_bim_surface_PC1_fia_0.1_mode_crit_1000.csv")

pc1.f.bim.line <- data.frame(PC1 = ifelse(is.null(nrow(unique(bimodal.fia.pc1[bimodal.fia.pc1$bimclass_f %in% "bimodal",]$PC1fia))),NA, 
                                          unique(bimodal.fia.pc1[bimodal.fia.pc1$bimclass_f %in% "bimodal",]$PC1fia)), y = -37, bimodal = "bimodal")


fia.kde.plot.pc1.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550),  yaxt="n", cex.axis=0.7) + points(data = pc1.f.bim.line[pc1.f.bim.line$bimodal %in% "bimodal",], y~PC1, cex = 0.9,  pch = 15,col = "red")+ text(-5.5,500, "B"))+ xlab("P-PET")
fia.kde.plot.pc1.gg + xlab("PC1")



# ---------------------------------- FIA PPET figure --------------------------------------------

H <- Hpi.diag(x=na.omit(cbind(pls.ppet$GS_ppet_mod, pls.ppet$FIAdensity)) )
fhat <- kde(x=na.omit(cbind(pls.ppet$GS_ppet_mod, pls.ppet$FIAdensity)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,75,80,85,95), xlab = "P-PET", ylab = "Tree density")

plot(fhat, display="slice", cont=c(85), add = TRUE)

bimodal.fia.ppet <- read.csv("outputs/new_bim_surface_PPET_fia_4_mode_crit_1000_non_ag.csv")


ppet.f.bim.line <- data.frame(PPET = ifelse(length(unique(bimodal.fia.ppet[bimodal.fia.ppet$bimclass_ppet_f %in% "bimodal",]$PC1fia)) < 1, NA, 
                                            unique(bimodal.fia.ppet[bimodal.fia.ppet$bimclass_ppet %in% "bimodal",]$GS_ppet_mod)), y = -37, bimodal = "bimodal")


fia.kde.plot.ppet.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab="P-PET",ylab=NA,  ylim = c(-40,550), xlim = c(-200, 300),  yaxt="n" , cex.axis=0.7) + points(data = ppet.f.bim.line[ppet.f.bim.line$bimodal %in% "bimodal",], y~PPET, cex = 0.9,  pch = 15,col = "red")+ text(-170,500, "D"))+xlab("P-PET")
fia.kde.plot.ppet.gg +xlab("P-PET")

# ------------------------ FIA Growing Season soil moisture figure --------------------------------------------

H <- Hpi.diag(x=na.omit(cbind(pls.soil$mean_GS_soil_m, pls.soil$FIAdensity)) )
fhat <- kde(x=na.omit(cbind(pls.soil$mean_GS_soil_m, pls.soil$FIAdensity)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,65,75,80,85,95), xlab = "Growing season soil", ylab = "Tree density")
#points(na.omit(cbind(pls.soil$PC1, pls.soil$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

bimodal.fia.sm <- read.csv("outputs/new_bim_surface_soil_moist_fia_0.01_mode_crit_1000_non_ag.csv")

sm.f.bim.line <- data.frame(SM = ifelse(is.null(nrow(unique(bimodal.fia.sm[bimodal.fia.sm$bimclass_soil %in% "bimodal",]$mean_GS_soil_m))),NA, 
                                        unique(bimodal.fia.sm[bimodal.fia.sm$bimclass_soil %in% "bimodal",]$mean_GS_soil_m)), y = -37, bimodal = "bimodal")




fia.kde.plot.sm.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = NA, ylim = c(-40,550), yaxt="n",  cex.axis=0.9) + points(data = sm.f.bim.line[sm.f.bim.line$bimodal %in% "bimodal",], y~SM, cex = 0.8,  pch = 15,col = "red")+ text(-0.05,500, "F"))
fia.kde.plot.sm.gg 

# >>>>>>>>>>>>>>>>>>>>>>>>>> Make the histograms of data <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# need to merge together all of the bimodal/unimodal tags
kde.surf.pc1.pls.df <- read.csv("outputs/new_bim_surface_PC1_pls_0.1_mode_crit_1000_non_ag.csv")
kde.surf.ppet.pls.df <- read.csv("outputs/new_bim_surface_PPET_pls_4_mode_crit_1000_non_ag.csv")
kde.surf.soilm.pls.df <- read.csv("outputs/new_bim_surface_soil_moist_pls_0.1_mode_crit_1000_non_ag.csv")

kde.surf.pc1.fia.df <- read.csv("outputs/new_bim_surface_PC1_fia_0.1_mode_crit_1000_non_ag.csv")
kde.surf.ppet.fia.df <- read.csv("outputs/new_bim_surface_PPET_fia_4_mode_crit_1000_non_ag.csv")
kde.surf.soilm.fia.df <- read.csv("outputs/new_bim_surface_soil_moist_fia_0.01_mode_crit_1000_non_ag.csv")

kde.surf.pc1.df <- merge(kde.surf.pc1.pls.df[,c("x", "y",  "PLSdensity",  "bimclass")], kde.surf.pc1.fia.df[,c("x", "y",  "FIAdensity","bimclass_f")], by = c("x", "y"))
kde.surf.ppet.df <- merge(kde.surf.ppet.pls.df[,c("x", "y",  "PLSdensity",  "bimclass_ppet")], kde.surf.ppet.fia.df[,c("x", "y",  "FIAdensity","bimclass_ppet_f")], by = c("x", "y"))
kde.surf.soilm.df <- merge(kde.surf.soilm.pls.df[,c("x", "y",  "PLSdensity",  "bimclass_soil")], kde.surf.soilm.fia.df[,c("x", "y",  "FIAdensity","bimclass_soil_f")], by = c("x", "y"))




flipped.pc1.hist <- ggplot(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",], aes(PLSdensity))+geom_density(color = "blue")+
  geom_density(data = kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",], aes(FIAdensity), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

flipped.ppet.hist <- ggplot(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",], aes(PLSdensity))+geom_density(color = "blue")+
  geom_density(data = kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",], aes(FIAdensity), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

flipped.soilm.hist <- ggplot(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",], aes(PLSdensity))+geom_density(color = "blue")+
  geom_density(data = kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",], aes(FIAdensity), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)


# alternative: get density lines then ggplotify them to align:
pls.soilm.density.df <- data.frame(y = density(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$PLSdensity)$y, 
                                   x = density(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$PLSdensity)$x)

fia.soilm.density.df <- data.frame(y = density(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$FIAdensity)$y, 
                                   x = density(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$FIAdensity)$x)
flipped.soilm.hist.gg <- as.ggplot(~plot(fia.soilm.density.df[fia.soilm.density.df$x < 550,], type = "l", col = "red", ylim = c(-40, 550), yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.soilm.density.df[pls.soilm.density.df$x < 550 & pls.soilm.density.df$x > -41,], type = "l", col = "blue"))



pls.ppet.density.df <- data.frame(y = density(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$PLSdensity)$y, 
                                  x = density(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$PLSdensity)$x)

fia.ppet.density.df <- data.frame(y = density(na.omit(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$FIAdensity))$y, 
                                  x = density(na.omit(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$FIAdensity))$x)
flipped.ppet.hist.gg <- as.ggplot(~plot(fia.ppet.density.df[fia.ppet.density.df$x < 550,], type = "l", col = "red", ylim = c(-40, 550),yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines( pls.ppet.density.df[pls.ppet.density.df$x < 550 & pls.ppet.density.df$x > -41,], type = "l", col = "blue"))

plot(fia.ppet.density.df[fia.ppet.density.df$x < 550,], type = "l", col = "red", ylim = c(-40, 550),yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines( pls.ppet.density.df[pls.ppet.density.df$x < 550 & pls.ppet.density.df$x > -41,], type = "l", col = "blue")

pls.pc1.density.df <- data.frame(y = density(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$PLSdensity)$y, 
                                 x = density(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$PLSdensity)$x)

fia.pc1.density.df <- data.frame(y = density(na.omit(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$FIAdensity))$y, 
                                 x = density(na.omit(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$FIAdensity))$x)
flipped.pc1.hist.gg <- as.ggplot(~plot(fia.pc1.density.df[fia.pc1.density.df$x < 550 & fia.pc1.density.df$x > -41,], type = "l", col = "red", ylim = c(-40, 550), yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.pc1.density.df[pls.pc1.density.df$x < 550 & pls.pc1.density.df$x > -41,], type = "l", col = "blue"))


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Make 3 color bimodal plots <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# for all PLS grid cells
bim.class <- merge(kde.surf.pc1.pls.df[,c("x", "y", "bimclass")], kde.surf.ppet.pls.df[,c("x", "y", "bimclass_ppet")], by = c("x", "y"), all = TRUE)

bim.class <- merge(bim.class, kde.surf.soilm.pls.df[,c("x", "y", "bimclass_soil")])

bim.class$nbimod <- as.character(rowSums(bim.class[,3:5] == "bimodal", na.rm = TRUE))

three.color.bimodal.plots.non.ag <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bim.class, aes(x=x, y=y, fill = nbimod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red"
  ), labels = c("0","1", "2", "3", "0")) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")

# for All grid cells in the FIA
bim.class.f <- merge(kde.surf.pc1.fia.df[,c("x", "y", "bimclass_f")], kde.surf.ppet.fia.df[,c("x", "y", "bimclass_ppet_f")], by = c("x", "y"), all = TRUE)

bim.class.f <- merge(bim.class.f, kde.surf.soilm.fia.df[,c("x", "y", "bimclass_soil_f")])

bim.class.f$nbimod <- as.character(rowSums(bim.class.f[,3:5] == "bimodal", na.rm = TRUE))

three.color.bimodal.plots.f.non.ag <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bim.class.f, aes(x=x, y=y, fill = nbimod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red"
  ), labels = c("0","1", "2", "3", "0")) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")



# >>>>>>>>>>>>>>>>>>>>>>>>>> Combine all Figures into 1 large fig <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

library(gtable)
g1 <- ggplotGrob(pls.kde.plot.pc1.gg+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-0.1), "cm")))
g2 <- ggplotGrob(fia.kde.plot.pc1.gg+theme(plot.margin=unit(c(-0.7,-1,-0.5,-1), "cm")))
g3 <- ggplotGrob(flipped.pc1.hist.gg+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g4 <- ggplotGrob(pls.kde.plot.ppet.gg+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g5 <- ggplotGrob(fia.kde.plot.ppet.gg+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g6 <- ggplotGrob(flipped.ppet.hist.gg+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g7 <- ggplotGrob(pls.kde.plot.sm.gg+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g8 <- ggplotGrob(fia.kde.plot.sm.gg+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g9 <- ggplotGrob(flipped.soilm.hist.gg+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))
g10 <- ggplotGrob(three.color.bimodal.plots.non.ag)
g11 <- ggplotGrob(three.color.bimodal.plots.f.non.ag)

g <- cbind(g1 ,g2, g3, size = "first")
g$heights <-unit.pmax(g1$heights, g2$heights, g3$heights)#, 

grow2 <- cbind(g4 ,g5, g6, size = "first")
grow2$heights <-unit.pmax(g4$heights, g5$heights, g6$heights)#, 

grow3 <- cbind(g7 ,g8, g9, size = "first")
grow3$heights <-unit.pmax(g7$heights, g8$heights, g9$heights)#, 
grow4 <- cbind(g10, g11, size = "first")
grow4$heights <-unit.pmax(g10$heights, g11$heights)
#png(height = 9, width = 6, units = "in", res = 300, "outputs/paper_figs/new_figure_3.png")
grid.arrange(arrangeGrob(g1,g2,g3, ncol=3, nrow=1, widths = c(1,1,0.2)), 
             arrangeGrob(g4, g5, g6, ncol = 3, nrow = 1, widths = c(1,1,0.2)) ,
             arrangeGrob(g7,g8, g9, ncol = 3, nrow = 1, widths = c(1,1,0.2)), 
             arrangeGrob(g10, g11, ncol = 3, nrow = 1, widths = c(1,1, 0)))
#dev.off()


png(height = 10, width = 6, units = "in", res = 300, "outputs/paper_figs/new_figure_3_kde_plot_with_hist_non_ag.png")
fig3 <- grid.arrange(g, grow2, grow3,grow4, ncol = 1)
fig3
dev.off()



# calculate the % ranges of the dataset with at least 1 bimodality for PLS:

# number of grid cells with at least 1 bimodal class
(nrow(bim.class[bim.class$nbimod >= 1,])/nrow(!is.na(bim.class)))*100

# number of grid cells with at least 2 bimodal class
(nrow(bim.class[bim.class$nbimod >= 2,])/nrow(!is.na(bim.class)))*100

# number of grid cells with at least 3 bimodal class
(nrow(bim.class[bim.class$nbimod >= 3,])/nrow(!is.na(bim.class)))*100

# for FIA:

# number of grid cells with at least 1 bimodal class
(nrow(bim.class.f[bim.class.f$nbimod >= 1,])/nrow(!is.na(bim.class.f)))*100

# number of grid cells with at least 2 bimodal class
(nrow(bim.class.f[bim.class.f$nbimod >= 2,])/nrow(!is.na(bim.class.f)))*100

# number of grid cells with at least 3 bimodal class
(nrow(bim.class.f[bim.class.f$nbimod >= 3,])/nrow(!is.na(bim.class.f)))*100


# >>>>>>>>>>>>> Predict modern based on pls relationship with climate <<<<<<<<<<<<<<<<<<<<<<<<<<


H <- Hpi.diag(x=na.omit(cbind(pls.nona$PC1fia, pls.nona$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(pls.nona$PC1fia, pls.nona$PLSdensity)), #H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,25,50,60,75,80,85,95))
#points(na.omit(cbind(pls.nona$PC1, pls.nona$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
#plot(fhat, display = "persp", xlab = "PC1fia", ylab = "FIAdensity")
contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)

# get points to evaluate:

diptest::dip.test(kde(x=na.omit(cbind(pls.nona$PC1fia, pls.nona$FIAdensity)), #H=H, 
                      compute.cont = TRUE, eval.points = cbind(rep(2,contour_95[which.min(abs(contour_95$x - -4)),]$y), 0:round(contour_95[which.min(abs(contour_95$x - -4)),]$y, 1)))$estimate)

# need to evaluate diptest over the density values where we have 95% probability of data:

ggplot(data=pls.nona, aes(PC1fia, PLSdensity)) +
  geom_point(size = 0.5) +
  geom_path(aes(x, y, color = "red"), data=contour_95) +
  theme_bw()




interp.densp <- function(pc1val){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - pc1val)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(pc1val, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(pc1val, 401 ), y=0:400)
  points <- expand.grid(seq(round(pc1val, 2) - 0.05, round(pc1val, 2) + 0.05, by = 0.1), y=0:maxy)
  colnames(points) <- c("x", "y")
  if(max(kde(x=na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    
    dipP <- NA
    
  }else{
    
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    samp <- sample(x=df$points, prob = df$freq, size = 10000, replace = TRUE)
    dipP <- diptest::dip.test(samp)$p.value
    pks <- amps(samp)$Peaks[,1]
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    dipP <- ifelse(length(pks) >= 2 & max(pks) >= 47, dipP, 1) # only take bimodal places where we can identify the modes and that have one forest mode
    
    #plot(density(samp))
  }
  dipP
  #pks
}

interp.densp(pc1val = -1.567)

pls.nona$dipPint_f_pred_pls <- NA
pls.nona <- pls.nona[!is.na(pls.nona$PC1fia),]





pls.nona$dipPint_f_pred_pls <- apply(data.frame(pls.nona$PC1fia), 1, interp.densp)

pls.nona$bimclass_f_pred_pls <- ifelse(pls.nona$dipPint_f_pred_pls <= 0.05 , "bimodal", "unimodal")
png("outputs/fia_dipP_kdeest_pred_by_pls_0.1_modes_crit_1000.png")
ggplot(pls.nona, aes(x,y, fill = dipPint_f_pred_pls))+geom_raster()
dev.off()

png("outputs/fia_bimodal_kdeest_pred_by_pls_modes_crit_1000.png")
ggplot(pls.nona, aes(x,y, fill = bimclass_f_pred_pls))+geom_raster()
dev.off()


bimod.pc1.mod_by_pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.nona, aes(x=x, y=y, fill = bimclass_f_pred_pls))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c( '#d73027', '#4575b4'
  ), labels = c("bimodal", "unimodal")) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle(paste("bimodal region =", ppet.bimpct.f, "%"))



write.csv(pls.nona, "outputs/new_bim_surface_PC1_fia_pred_by_pls_0.1_modes.crit_1000.csv", row.names = FALSE)

png(height = 4, width = 6, units = "in", res = 300, "outputs/paper_figs/supplemental_bimodal_pls_modern_climate_change_1000.png")
plot_grid(bimod.pc1.mod_by_pls.map+ggtitle(" "), bimod.pc.fia.map+ggtitle(" "), labels = c("A",  "B"))  
dev.off()

# now lets predict future scenarios:
# >>>>>>>>>>>>> Predict future based on pls relationship with climate <<<<<<<<<<<<<<<<<<<<<<<<<<
# get future climates:
future.pr <- read.csv("outputs/Future_PCA.csv")
future <- future.pr
future.pr<- pls.df <- future.pr[!is.na(future.pr$PLSdensity) & future.pr$PLSdensity <= 550,]
H <- Hpi.diag(x=na.omit(cbind(future.pr$PC1, future.pr$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(future.pr$PC1, future.pr$PLSdensity)), #H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,25,50,60,75,80,85,95))
#points(na.omit(cbind(future.pr$PC1, future.pr$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
#plot(fhat, display = "persp", xlab = "PC1fia", ylab = "FIAdensity")
contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)

# get points to evaluate:


# need to evaluate diptest over the density values where we have 95% probability of data:






interp.densp <- function(pc1val){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - pc1val)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(pc1val, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(pc1val, 401 ), y=0:400)
  points <- expand.grid(seq(round(pc1val, 2) - 0.05, round(pc1val, 2) + 0.05, by = 0.1), y=0:maxy)
  colnames(points) <- c("x", "y")
  if(max(kde(x=na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    
    dipP <- NA
    
  }else{
    
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    samp <- sample(x=df$points, prob = df$freq, size = 10000, replace = TRUE)
    dipP <- diptest::dip.test(samp)$p.value
    pks <- amps(samp)$Peaks[,1]
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    samp2 <- sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE)
    dipP2 <- diptest::dip.test(samp2)$p.value
    pks2 <- amps(samp2)$Peaks[,1]
    
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    dipP <- ifelse(length(pks) >= 2 & max(pks) >= 100 & dipP2 <= 0.05, dipP, 1) 
    
    #plot(density(samp))
  }
  dipP
  #pks
}


interp.densp(pc1val = -1.54)

future$dipPint_f_pred_pls <- NA
future <- future[!is.na(future$PC1_cc85),]



future$dipPint_f_pred_pls_85 <- apply(data.frame(future$PC1_cc85), 1, interp.densp)

future$bimclass_f_pred_pls_85 <- ifelse(future$dipPint_f_pred_pls_85 <= 0.05 , "bimodal", "unimodal")
png("outputs/rcp85_dipP_kdeest_pred_by_pls_0.1_mode_crit_1000.png")
ggplot(future, aes(x,y, fill = dipPint_f_pred_pls_85))+geom_raster()
dev.off()

png("outputs/rcp85_bimodal_kdeest_by_pls_0.1_mode_crit_1000.png")
ggplot(future, aes(x,y, fill = bimclass_f_pred_pls_85))+geom_raster()
dev.off()

# for 6.0
future$dipPint_f_pred_pls_60 <- apply(data.frame(future$PC1_cc60), 1, interp.densp)

future$bimclass_f_pred_pls_60 <- ifelse(future$dipPint_f_pred_pls_60 <= 0.05 , "bimodal", "unimodal")
png("outputs/rcp60_dipP_kdeest_pred_by_pls0.1_mode_crit_1000.png")
ggplot(future, aes(x,y, fill = dipPint_f_pred_pls_60))+geom_raster()
dev.off()

png("outputs/rcp60_bimodal_kdeest_by_pls_0.1_mode_crit_1000.png")
ggplot(future, aes(x,y, fill = bimclass_f_pred_pls_60))+geom_raster()
dev.off()

# for 45
future$dipPint_f_pred_pls_45 <- apply(data.frame(future$PC1_cc45), 1, interp.densp)

future$bimclass_f_pred_pls_45 <- ifelse(future$dipPint_f_pred_pls_45 <= 0.05 , "bimodal", "unimodal")
png("outputs/rcp45_dipP_kdeest_pred_by_pls_0.1_mode_crit_1000.png")
ggplot(future, aes(x,y, fill = dipPint_f_pred_pls_45))+geom_raster()
dev.off()

png("outputs/rcp45_bimodal_kdeest_by_pls_0.1_mode_crit_1000.png")
ggplot(future, aes(x,y, fill = bimclass_f_pred_pls_45))+geom_raster()
dev.off()

# for 2.6;
future$dipPint_f_pred_pls_26 <- apply(data.frame(future.pr$PC1_cc26), 1, interp.densp)

future$bimclass_f_pred_pls_26 <- ifelse(future.pr$dipPint_f_pred_pls_26 <= 0.05 , "bimodal", "unimodal")
png("outputs/rcp26_dipP_kdeest_pred_by_pls_0.1_mode_crit_1000.png")
ggplot(future, aes(x,y, fill = dipPint_f_pred_pls_26))+geom_raster()
dev.off()

png("outputs/rcp26_bimodal_kdeest_by_pls_0.1_mode_crit_1000.png")
ggplot(future, aes(x,y, fill = bimclass_f_pred_pls_26))+geom_raster()
dev.off()

write.csv(future,"outputs/new_bim_surface_PC1_future_8.5_pred_by_pls_0.1_mode_crit_1000.csv", row.names = FALSE)

########################################################################
# make predictions for the future from FIA relationship to climate
#future.pr <- future.pr[!is.na(future.pr$FIAdensity),]

future.pr <- read.csv("outputs/Future_PCA.csv")
H <- Hpi.diag(x=na.omit(cbind(future.pr$PC1fia, future.pr$FIAdensity)) )
fhat <- kde(x=na.omit(cbind(future.pr$PC1fia, future.pr$FIAdensity)), #H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,25,50,60,75,80,85,95))
#points(na.omit(cbind(future.pr$PC1, future.pr$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
#plot(fhat, display = "persp", xlab = "PC1fia", ylab = "FIAdensity")
contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["5%"])[[1]])
contour_95 <- data.frame(contour_95)

# get points to evaluate:


# need to evaluate diptest over the density values where we have 95% probability of data:



interp.densp <- function(pc1val){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - pc1val)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(pc1val, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(pc1val, 401 ), y=0:400)
  points <- expand.grid(seq(round(pc1val, 2) - 0.07, round(pc1val, 2) + 0.07, by = 0.005), y=0:maxy)
  colnames(points) <- c("x", "y")
  if(max(kde(x=na.omit(cbind(pls.df$PC1fia, pls.df$FIAdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    
    dipP <- NA
    
  }else{
    
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$PC1fia, pls.df$FIAdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    samp <- sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE)
    dipP <- diptest::dip.test(samp, simulate.p.value = TRUE, B = 500)$p.value
    pks <- amps(samp)$Peaks[,1]
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    dipP <- ifelse(length(pks) >= 2 & max(pks) >= 100, dipP, 1) # only take bimodal places where we can identify the modes and that have one forest mode
    
    # draw from a second distribution--if it is actally bimodal, the second distribution is likely to also show bimodality
    # however, if the sampled population is bimodal by chance, then drawing from a second distribution will prevent this:
    samp2 <- sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE)
    dipP2 <- diptest::dip.test(samp2)$p.value
    pks2 <- amps(samp2)$Peaks[,1]
    
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    dipP <- ifelse(length(pks) >= 2 & max(pks) >= 100 & dipP2 <= 0.05, dipP, 1) 
    
  }
  dipP
  #pks
}

interp.densp(pc1val = -1.567)

future.pr$dipPint_f_pred_fia_85 <- NA
future.pr <- future.pr[!is.na(future.pr$PC1_cc85),]

#future.pr

future.pr$dipPint_f_pred_fia_85 <- apply(data.frame(future.pr$PC1_cc85), 1, interp.densp)

future.pr$bimclass_f_pred_fia_85 <- ifelse(future.pr$dipPint_f_pred_fia_85 <= 0.05 , "bimodal", "unimodal")

write.csv(future.pr, "outputs/new_bim_surface_PC1_future_8.5_pred_by_fia.csv", row.names = FALSE)

ggplot(future.pr, aes(x,y, fill = bimclass_f_pred_fia_85))+geom_raster()

future.pr.1 <- read.csv( "outputs/new_bim_surface_PC1_future_scenarios_pred_by_pls.csv")
future.test <- merge(future.pr.1, future.pr[,c("x", "y","cell", "bimclass_f_pred_fia_85", "dipPint_f_pred_fia_85")], by = c("x", "y", "cell"))
#future.pr <- future.test

bimod.pc.fia.85.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=future.test, aes(x=x, y=y, fill = bimclass_f_pred_fia_85))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c( '#4575b4','#d73027'
  ), labels = c("unimodal", "bimodal")) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")


bimod.pc.pls.85.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=future.test, aes(x=x, y=y, fill = bimclass_f_pred_pls_85))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c( '#4575b4','#d73027'
  ), labels = c("unimodal", "bimodal")) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")

png(height = 4, width = 6, units = 'in', res = 300, "outputs/paper_figs/fig3_preds_kde_surfaces.png")
grid.arrange(bimod.pc.pls.85.map, bimod.pc.fia.85.map, ncol = 2)
dev.off()gd



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> bimodality based on future P-PET <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# get future climates:
future.pr <- read.csv("outputs/Future_PCA.csv")
pls.df <- read.csv("data/PLS_FIA_density_climate_full.csv")


pls.nona <- pls.df[!is.na(pls.df$PLSdensity) & pls.df$PLSdensity <= 550,]

colnames(future.pr)[58] <- 'mean_ppet_GS_8.5'
future.pr <- merge(future.pr[! names(future.pr) %in% c("GS_ppet", "GS_ppet_mod", "mean_GS_soil", "mean_GS_soil_m")], pls.nona[,c("x", "y", "GS_ppet" ,"GS_ppet_mod", "mean_GS_soil", "mean_GS_soil_m")], by = c("x", "y"))

#future.pr <- future.pr[!is.na(future.pr$PLSdensity) & future.pr$PLSdensity <= 550,]
H <- Hpi.diag(x=na.omit(cbind(future.pr$GS_ppet, future.pr$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(future.pr$GS_ppet, future.pr$PLSdensity)), #H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,25,50,60,75,80,85,95))
#points(na.omit(cbind(future.pr$PC1, future.pr$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
#plot(fhat, display = "persp", xlab = "PC1fia", ylab = "FIAdensity")
contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["5%"])[[1]])
contour_95 <- data.frame(contour_95)

# get points to evaluate:


# need to evaluate diptest over the density values where we have 95% probability of data:



interp.densp.ppet <- function(ppetval){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - ppetval)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(ppetval, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(ppetval, 401 ), y=0:400)
  points <- expand.grid(seq(round(ppetval, 2) - 4, round(ppetval, 2) + 4, by = 0.5), y=0:maxy)
  colnames(points) <- c("x", "y")
  if(max(kde(x=na.omit(cbind(pls.df$GS_ppet, pls.df$PLSdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$GS_ppet, pls.df$PLSdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    samp <- sample(x=df$points, prob = df$freq, size = 10000, replace = TRUE)
    dipP <- diptest::dip.test(samp)$p.value
    pks <- amps(samp)$Peaks[,1]
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    samp2 <- sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE)
    dipP2 <- diptest::dip.test(samp2)$p.value
    pks2 <- amps(samp2)$Peaks[,1]
    
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    dipP <- ifelse(length(pks) >= 2 & max(pks) >= 100 & dipP2 <= 0.05, dipP, 1) 
    
    #plot(density(samp))
  }
  dipP
  #pks
}

interp.densp.ppet(50)

future.pr$dipPint_f_pred_pls_ppet <- NA
future.pr <- future.pr[!is.na(future.pr$mean_ppet_GS_8.5),]



future.pr$dipPint_f_pred_pls_ppet <- apply(data.frame(future.pr$mean_ppet_GS_8.5), 1, interp.densp.ppet)
future.pr$bimclass_f_pred_pls_85_ppet <- NA
future.pr$bimclass_f_pred_pls_85_ppet <- ifelse(future.pr$dipPint_f_pred_pls_ppet <= 0.05 , "bimodal", "unimodal")

png("outputs/rcp85_dipP_kdeest_pred_by_pls_ppet_1000_mode_crit.png")
ggplot(future.pr, aes(x,y, fill = dipPint_f_pred_pls_ppet))+geom_raster()
dev.off()

png("outputs/rcp85_bimodal_kdeest_by_pls_ppet_1000_mode_crit.png")
ggplot(future.pr, aes(x,y, fill = bimclass_f_pred_pls_85_ppet))+geom_raster()
dev.off()

write.csv(future.pr, "outputs/new_bim_surface_PPET_rcp85_pred_by_pls_1000_mode_crit.csv")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> bimodality based on future P-PET <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# get future climates:
future.pr <- read.csv("outputs/Future_PCA.csv")
colnames(future.pr)[58] <- 'mean_ppet_GS_8.5'
future.pr <- merge(future.pr[! names(future.pr) %in% c("GS_ppet", "GS_ppet_mod", "mean_GS_soil", "mean_GS_soil_m")], pls.nona[,c("x", "y", "GS_ppet" ,"GS_ppet_mod", "mean_GS_soil", "mean_GS_soil_m")], by = c("x", "y"))

pls.nona.f <- future.pr[!is.na(future.pr$FIAdensity) & future.pr$FIAdensity <= 550,]
H <- Hpi.diag(x=na.omit(cbind(pls.nona.f$GS_ppet_mod, pls.nona.f$FIAdensity)) )
fhat <- kde(x=na.omit(cbind(pls.nona.f$GS_ppet_mod, pls.nona.f$FIAdensity)), #H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,25,50,60,75,80,85,95))
#points(na.omit(cbind(pls.nona.f$PC1, pls.nona.f$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
#plot(fhat, display = "persp", xlab = "PC1fia", ylab = "FIAdensity")
contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["5%"])[[1]])
contour_95 <- data.frame(contour_95)

# get points to evaluate:


# need to evaluate diptest over the density values where we have 95% probability of data:



interp.densp.ppet <- function(ppetval){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - ppetval)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(ppetval, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(ppetval, 401 ), y=0:400)
  points <- expand.grid(seq(round(ppetval, 2) - 4, round(ppetval, 2) + 4, by = 0.5), y=0:maxy)
  colnames(points) <- c("x", "y")
  if(max(kde(x=na.omit(cbind(pls.nona.f$GS_ppet_mod, pls.nona.f$FIAdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.nona.f$GS_ppet_mod, pls.nona.f$FIAdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    samp <- sample(x=df$points, prob = df$freq, size = 10000, replace = TRUE)
    dipP <- diptest::dip.test(samp)$p.value
    pks <- amps(samp)$Peaks[,1]
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    samp2 <- sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE)
    dipP2 <- diptest::dip.test(samp2)$p.value
    pks2 <- amps(samp2)$Peaks[,1]
    
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    dipP <- ifelse(length(pks) >= 2 & max(pks) >= 100 & dipP2 <= 0.05, dipP, 1) 
    
    #plot(density(samp))
  }
  dipP
  #pks
}
interp.densp.ppet(-50)

future.pr$dipPint_f_pred_fia_ppet <- NA
future.pr$bimclass_f_pred_fia_85_ppet <- NA
future.pr <- future.pr[!is.na(future.pr$mean_ppet_GS_8.5),]





future.pr$dipPint_f_pred_fia_ppet <- apply(data.frame(future.pr$mean_ppet_GS_8.5), 1, interp.densp.ppet)

future.pr$bimclass_f_pred_fia_85_ppet <- ifelse(future.pr$dipPint_f_pred_fia_ppet <= 0.05 , "bimodal", "unimodal")
# all of the future is out of sample, so we will just call it unimodal for now, but we dont know

future.pr$bimclass_f_pred_fia_85_ppet <- "unimodal"
png("outputs/rcp85_dipP_kdeest_pred_by_fia_ppet_1000_mode_crit.png")
ggplot(future.pr, aes(x,y, fill = dipPint_f_pred_fia_ppet ))+geom_raster()
dev.off()

png("outputs/rcp85_bimodal_kdeest_by_fia_ppet_1000_mode_crit.png")
ggplot(future.pr, aes(x,y, fill = bimclass_f_pred_fia_85_ppet))+geom_raster()
dev.off()

write.csv(future.pr, "outputs/new_bim_surface_PPET_rcp85_pred_by_fia_1000_mode_crit.csv")




# >>>>>>>>>>>>>>>>>>>>>>>>> bimodality based on future soil moisture modeled <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# get future climates:
future.pr <- read.csv("outputs/Future_PCA.csv")
#colnames(future.pr)[56] <- 'mean_GS_soil_8.5'
colnames(future.pr)[56:58] <- c("mean_GS_soil_8.5", "mean_GS_soil_8.5_post_spin", "mean_GS_ppet_8.5")

future.pr <- merge(future.pr[! names(future.pr) %in% c("GS_ppet", "GS_ppet_mod", "mean_GS_soil", "mean_GS_soil_m")], pls.nona[,c("x", "y", "GS_ppet" ,"GS_ppet_mod", "mean_GS_soil", "mean_GS_soil_m")], by = c("x", "y"))

#future.pr <- future.pr[!is.na(future.pr$PLSdensity) & future.pr$PLSdensity <= 550,]
H <- Hpi.diag(x=na.omit(cbind(future.pr$mean_GS_soil, future.pr$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(future.pr$mean_GS_soil, future.pr$PLSdensity)), #H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,25,50,60,75,80,85,95))
#points(na.omit(cbind(future.pr$PC1, future.pr$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
#plot(fhat, display = "persp", xlab = "PC1fia", ylab = "FIAdensity")
contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)

# get points to evaluate:


# need to evaluate diptest over the density values where we have 95% probability of data:



interp.densp.soil <- function(soilval){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - soilval)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(soilval, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(soilval, 401 ), y=0:400)
  points <- expand.grid(seq(round(soilval, 2) - 0.05, round(soilval, 2) + 0.05, by = 0.001), y=0:maxy)
  colnames(points) <- c("x", "y")
  if(max(kde(x=na.omit(cbind(pls.df$mean_GS_soil, pls.df$PLSdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$mean_GS_soil, pls.df$PLSdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    samp <- sample(x=df$points, prob = df$freq, size = 10000, replace = TRUE)
    dipP <- diptest::dip.test(samp)$p.value
    pks <- amps(samp)$Peaks[,1]
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    samp2 <- sample(x=df$points, prob = df$freq, size = 10000, replace = TRUE)
    dipP2 <- diptest::dip.test(samp2)$p.value
    pks2 <- amps(samp2)$Peaks[,1]
    
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    dipP <- ifelse(length(pks) >= 2 & max(pks) >= 100 & dipP2 <= 0.05, dipP, 1) 
    
    #plot(density(samp))
  }
  dipP
  
}

interp.densp.soil( soilval = 1.26 )

future.pr$dipPint_f_pred_pls_soil <- NA
future.pr$bimclass_f_pred_pls_85_soil <- NA
future.pr <- future.pr[!is.na(future.pr$mean_GS_soil_8.5),]



future.pr$dipPint_f_pred_pls_soil <- apply(data.frame(future.pr$mean_GS_soil_8.5), 1, interp.densp.soil)

future.pr$bimclass_f_pred_pls_85_soil <- ifelse(future.pr$dipPint_f_pred_pls_soil <= 0.05 , "bimodal", "unimodal")

png("outputs/rcp85_dipP_kdeest_pred_by_pls_ppet.png")
ggplot(future.pr, aes(x,y, fill = dipPint_f_pred_pls_soil))+geom_raster()
dev.off()

png("outputs/rcp85_bimodal_kdeest_by_pls_ppet.png")
ggplot(future.pr, aes(x,y, fill = dipPint_f_pred_pls_soil))+geom_raster()
dev.off()

write.csv(future.pr, "outputs/new_bim_surface_soil_m_rcp85_pred_by_pls.csv")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> bimodality based on fia soil moisture <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# get future climates:
future.pr <- read.csv("outputs/Future_PCA.csv")
colnames(future.pr)[56:58] <- c("mean_GS_soil_8.5", "mean_GS_soil_8.5_post_spin", "mean_GS_ppet_8.5")
future.pr<- merge(future.pr[! names(future.pr) %in% c("GS_ppet", "GS_ppet_mod", "mean_GS_soil", "mean_GS_soil_m")], pls.nona[,c("x", "y", "GS_ppet" ,"GS_ppet_mod", "mean_GS_soil", "mean_GS_soil_m")], by = c("x", "y"))

future.pr <- future.pr[!is.na(future.pr$FIAdensity) & future.pr$FIAdensity <= 550,]
H <- Hpi.diag(x=na.omit(cbind(future.pr$mean_GS_soil_m, future.pr$FIAdensity)) )
fhat <- kde(x=na.omit(cbind(future.pr$mean_GS_soil_m, future.pr$FIAdensity)), #H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,25,50,60,75,80,85,95))
#points(na.omit(cbind(future.pr$PC1, future.pr$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
#plot(fhat, display = "persp", xlab = "PC1fia", ylab = "FIAdensity")
contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["5%"])[[1]])
contour_95 <- data.frame(contour_95)

# get points to evaluate:


# need to evaluate diptest over the density values where we have 95% probability of data:



interp.densp.soil <- function(soilval){
  # find the closest PC1 value in the contour_95 df:
  contour_95 <- contour_95[contour_95$y >=0,]
  closest <- contour_95[which.min(abs(contour_95$x - soilval)),]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  #points <- data.frame(x=rep(soilval, maxy+1 ), y=0:maxy) # points all at the pc1 value, and along a grid of density
  #points <- data.frame(x=rep(soilval, 401 ), y=0:400)
  points <- expand.grid(seq(round(soilval, 2) - 0.05, round(soilval, 2) + 0.05, by = 0.001), y=0:maxy)
  colnames(points) <- c("x", "y")
  if(max(kde(x=na.omit(cbind(pls.df$mean_GS_soil_m, pls.df$FIAdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
    dipP <- NA
  }else{
    df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$mean_GS_soil_m, pls.df$FIAdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
    samp <- sample(x=df$points, prob = df$freq, size = 10000, replace = TRUE)
    dipP <- diptest::dip.test(samp)$p.value
    pks <- amps(samp)$Peaks[,1]
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    samp2 <- sample(x=df$points, prob = df$freq, size = 10000, replace = TRUE)
    dipP2 <- diptest::dip.test(samp2)$p.value
    pks2 <- amps(samp2)$Peaks[,1]
    
    #dipP <- diptest::dip.test( df$freq)$p.value
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    dipP <- ifelse(length(pks) >= 2 & max(pks) >= 100 & dipP2 <= 0.05, dipP, 1) 
    
    
  }
  dipP
  
}

interp.densp.soil(pc1val = 0.25)

future.pr$dipPint_f_pred_fia_soil_8.5 <- NA
future.pr$bimclass_f_pred_fia_soil_8.5 <- NA
future.pr <- future.pr[!is.na(future.pr$mean_GS_soil_8.5),]





future.pr$dipPint_f_pred_fia_soil_8.5 <- apply(data.frame(future.pr$mean_GS_soil_8.5), 1, interp.densp.soil)

future.pr$bimclass_f_pred_fia_85_soil <- ifelse(future.pr$dipPint_f_pred_fia_soil_8.5 <= 0.05 , "bimodal", "unimodal")
png("outputs/rcp85_dipP_kdeest_pred_by_fia_soil_8.5.png")
ggplot(future.pr, aes(x,y, fill = dipPint_f_pred_fia_soil_8.5 ))+geom_raster()
dev.off()

png("outputs/rcp85_bimodal_kdeest_by_fia_soil_8.5.png")
ggplot(future.pr, aes(x,y, fill = bimclass_f_pred_fia_85_soil))+geom_raster()
dev.off()

write.csv(future.pr, "outputs/new_bim_surface_soil_m_rcp85_pred_by_fia.csv")

