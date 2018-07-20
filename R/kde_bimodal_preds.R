
pls.df <- read.csv("data/PLS_FIA_density_climate_full.csv")

library("MASS")
library(ggplot2)


pls.nona <- pls.df[!is.na(pls.df$PLSdensity) & pls.df$PLSdensity <= 550,]
pls.nona <- pls.nona[!is.na(pls.nona$PC1),]





png("outputs/kde_color_contour_plot_with_line.png")
image(f1, xlab="PC1", ylab = "PLS density", main = "Data contours of density")
abline(v = 0)
dev.off()



points <- data.frame(x=rep(-3, 557 ), y=c(0:556)) # points all at the pc1 value, and along a grid of density

png("outputs/kde_estimates_at_-3.png")
plot(na.omit(fields::interp.surface(f1, points)), ylab = "frequency", xlab = "Tree density")
dev.off()

na.omit(fields::interp.surface(f1, points))


diptest::dip.test(density(na.omit(fields::interp.surface(f1, points)))$y)


#pls.df <- data.frame(PC1 = pls.nona$PC1, PLSdensity =pls.nona$PLSdensity, x = pls.nona$x, y = pls.nona$y)

pls.df[duplicated(pls.df),]
pls.df<- pls.df[pls.df$PLSdensity < 550,]


png("outputs/bimodal_interp_map.png")
ggplot(pls.nona[pls.nona$PC1 > -2 & pls.nona$PC1 < 0,], aes(x,y, fill = bimclass))+geom_raster()
dev.off()




#<<<<<<<<<<<<<<<<<<<<<<<< estimate PDF of data using kde >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# estimate surfaces:
library(ks)
H <- Hpi.diag(x=na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), #H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,25,50,60,75,80,85,95))
#points(na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                     z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                    z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)

set.seed(10)

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

interp.densp(pc1val = -2.5)


pls.df$dipPint <- NA
pls.df <- pls.df[!is.na(pls.df$PC1),]




pls.df$dipPint <- apply(data.frame(pls.df$PC1), 1, interp.densp)

pls.df$bimclass <- ifelse(pls.df$dipPint <= 0.05 , "bimodal", "unimodal")
png("outputs/pls_dipP_kdeest_0.1_bin_mode_crit_1000.png")
ggplot(pls.df, aes(x,y, fill = dipPint))+geom_raster()
dev.off()

png("outputs/pls_bimodal_kdeest_0.1_bin_mode_crit_1000.png")
ggplot(pls.df, aes(x,y, fill = bimclass))+geom_raster()
dev.off()

write.csv(pls.df, "outputs/new_bim_surface_PC1_pls_0.1_mode_crit_1000.csv", row.names = FALSE)

# >>>>>>>>>>>>>>>>>>>>>>>>>do the same for P-PET:
H <- Hpi.diag(x=na.omit(cbind(pls.nona$GS_ppet, pls.nona$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(pls.nona$GS_ppet, pls.nona$PLSdensity)), H=H, 
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


pls.nona$dipPint_ppet <- apply(data.frame(pls.nona$GS_ppet), 1, interp.densp.ppet)
#pls.df$dipPint <- as.numeric(pls.df$dipPint)
pls.nona$bimclass_ppet <- ifelse(pls.nona$dipPint_ppet <= 0.05 , "bimodal", "unimodal")
png("outputs/pls_dipP_kdeest_ppet_4_mode_crit_1000.png")
ggplot(pls.nona, aes(x,y, fill = dipPint_ppet))+geom_raster()
dev.off()

png("outputs/pls_bimodal_kdeest_ppet_4_mode_crit_1000.png")
ggplot(pls.nona, aes(x,y, fill = bimclass_ppet))+geom_raster()
dev.off()

write.csv(pls.nona, "outputs/new_bim_surface_PPET_pls_4_mode_crit_1000.csv", row.names = FALSE)


# >>>>>>>>>>>>>>>>>>>>>>>>>do the same for GS_soil:

H <- Hpi.diag(x=na.omit(cbind(pls.nona$mean_GS_soil, pls.nona$PLSdensity)) )
fhat <- kde(x=na.omit(cbind(pls.nona$mean_GS_soil, pls.nona$PLSdensity)), H=H, 
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


pls.nona$dipPint_soil <- apply(data.frame(pls.nona$mean_GS_soil), 1, interp.densp.soil)
#pls.df$dipPint <- as.numeric(pls.df$dipPint)
pls.nona$bimclass_soil <- ifelse(pls.nona$dipPint_soil <= 0.05 , "bimodal", "unimodal")
png("outputs/pls_dipP_kdeest_soil_0.1_mode_crit_1000.png")
ggplot(pls.nona, aes(x,y, fill = dipPint_soil))+geom_raster()
dev.off()

png("outputs/pls_bimodal_kdeest_soil_0.1_mode_crit_1000.png")
ggplot(pls.nona, aes(x,y, fill = bimclass_soil))+geom_raster()
dev.off()

write.csv(pls.nona, "outputs/new_bim_surface_soil_moist_pls_0.1_mode_crit_1000.csv", row.names = FALSE)



# >>>>>>>>>>>>>>>> get bimodality for FIA data <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

H <- Hpi.diag(x=na.omit(cbind(pls.nona$PC1fia, pls.nona$FIAdensity)) )
fhat <- kde(x=na.omit(cbind(pls.nona$PC1fia, pls.nona$FIAdensity)), #H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,25,50,60,75,80,85,95))
#points(na.omit(cbind(pls.nona$PC1, pls.nona$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
plot(fhat, display = "persp", xlab = "PC1fia", ylab = "FIAdensity")
contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["16%"])[[1]])
contour_95 <- data.frame(contour_95)

# get points to evaluate:

diptest::dip.test(kde(x=na.omit(cbind(pls.nona$PC1fia, pls.nona$FIAdensity)), #H=H, 
                      compute.cont = TRUE, eval.points = cbind(rep(2,contour_95[which.min(abs(contour_95$x - -4)),]$y), 0:round(contour_95[which.min(abs(contour_95$x - -4)),]$y, 1)))$estimate)


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

pls.nona$dipPint_f <- NA
pls.nona <- pls.nona[!is.na(pls.nona$PC1fia),]





pls.nona$dipPint_f <- apply(data.frame(pls.nona$PC1fia), 1, interp.densp)

pls.nona$bimclass_f <- ifelse(pls.nona$dipPint_f <= 0.05 , "bimodal", "unimodal")
png("outputs/fia_dipP_kdeest_0.1_mode_crit_1000.png")
ggplot(pls.nona, aes(x,y, fill = dipPint_f))+geom_raster()
dev.off()

png("outputs/fia_bimodal_kdeest_0.1_mode_crit_1000.png")
ggplot(pls.nona, aes(x,y, fill = bimclass_f))+geom_raster()
dev.off()

write.csv(pls.nona, "outputs/new_bim_surface_PC1_fia_0.1_mode_crit_1000.csv", row.names = FALSE)

# >>>>>>>>>>>>>>>>>>>>>>>>>do the same for P-PET on the modern landscape:
H <- Hpi.diag(x=na.omit(cbind(pls.nona$GS_ppet_mod, pls.nona$FIAdensity)) )
fhat <- kde(x=na.omit(cbind(pls.nona$GS_ppet_mod, pls.nona$FIAdensity)), H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,15,25,40,35,45,50,60,75,80,85,95))
#points(na.omit(cbind(pls.nona$PC1, pls.nona$PLSdensity)), cex=0.3, pch=16)
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


pls.nona$dipPint_ppet_f <- apply(data.frame(pls.nona$GS_ppet_mod), 1, interp.densp.ppet)
#pls.nona$dipPint <- as.numeric(pls.nona$dipPint)
pls.nona$bimclass_ppet_f <- ifelse(pls.nona$dipPint_ppet_f <= 0.05 , "bimodal", "unimodal")
png("outputs/fia_dipP_kdeest_ppet_4_mode_crit_1000.png")
ggplot(pls.nona, aes(x,y, fill = dipPint_ppet_f))+geom_raster()
dev.off()

png("outputs/fia_bimodal_kdeest_ppet_4_mode_crit_1000.png")
ggplot(pls.nona, aes(x,y, fill = bimclass_ppet_f))+geom_raster()
dev.off()

write.csv(pls.nona, "outputs/new_bim_surface_PPET_fia_4_mode_crit_1000.csv", row.names = FALSE)


# >>>>>>>>>>>>>>>>>>>>>>>>>do the same for GS_soil:
pls.nona <- pls.nona[!is.na(pls.nona$FIAdensity),]
H <- Hpi.diag(x=na.omit(cbind(pls.nona$mean_GS_soil_m, pls.nona$FIAdensity)) )
fhat <- kde(x=na.omit(cbind(pls.nona$mean_GS_soil_m, pls.nona$FIAdensity)), H=H, 
            compute.cont = TRUE )
plot(fhat, display="filled.contour2", cont=c(1,5,15,25,40,35,45,50,60,75,80,85,95))
#points(na.omit(cbind(pls.nona$PC1, pls.nona$PLSdensity)), cex=0.3, pch=16)
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

pls.nona$dipPint_soil_f <- apply(data.frame(pls.nona$mean_GS_soil_m), 1, interp.densp.soil)
#pls.nona$dipPint <- as.numeric(pls.nona$dipPint)
pls.nona$bimclass_soil_f <- ifelse(pls.nona$dipPint_soil_f <= 0.05 , "bimodal", "unimodal")
png("outputs/fia_dipP_kdeest_soil_0.01_mode_crit_1000.png")
ggplot(pls.nona, aes(x,y, fill = dipPint_soil_f))+geom_raster()
dev.off()

png("outputs/fia_bimodal_kdeest_soil_0.01_mode_crit_1000.png")
ggplot(pls.nona, aes(x,y, fill = bimclass_soil_f))+geom_raster()
dev.off()

write.csv(pls.nona, "outputs/new_bim_surface_soil_moist_fia_0.01_mode_crit_1000.csv", row.names = FALSE)




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



# apply interp function over the whole dataset

future$dipPint_f_pred_pls_85 <- apply(data.frame(future$PC1_cc85), 1, interp.densp)

future$bimclass_f_pred_pls_85 <- ifelse(future$dipPint_f_pred_pls_85 <= 0.05 , "bimodal", "unimodal")
png("outputs/rcp85_dipP_kdeest_pred_by_pls_0.1_mode_crit_1000.png")
ggplot(future, aes(x,y, fill = dipPint_f_pred_pls_85))+geom_raster()
dev.off()

png("outputs/rcp85_bimodal_kdeest_by_pls_0.1_mode_crit_1000.png")
ggplot(future, aes(x,y, fill = bimclass_f_pred_pls_85))+geom_raster()
dev.off()

# -----------------------for 6.0
future$dipPint_f_pred_pls_60 <- apply(data.frame(future$PC1_cc60), 1, interp.densp)

future$bimclass_f_pred_pls_60 <- ifelse(future$dipPint_f_pred_pls_60 <= 0.05 , "bimodal", "unimodal")
png("outputs/rcp60_dipP_kdeest_pred_by_pls0.1_mode_crit_1000.png")
ggplot(future, aes(x,y, fill = dipPint_f_pred_pls_60))+geom_raster()
dev.off()

png("outputs/rcp60_bimodal_kdeest_by_pls_0.1_mode_crit_1000.png")
ggplot(future, aes(x,y, fill = bimclass_f_pred_pls_60))+geom_raster()
dev.off()

# ---------------------------for 45
future$dipPint_f_pred_pls_45 <- apply(data.frame(future$PC1_cc45), 1, interp.densp)

future$bimclass_f_pred_pls_45 <- ifelse(future$dipPint_f_pred_pls_45 <= 0.05 , "bimodal", "unimodal")
png("outputs/rcp45_dipP_kdeest_pred_by_pls_0.1_mode_crit_1000.png")
ggplot(future, aes(x,y, fill = dipPint_f_pred_pls_45))+geom_raster()
dev.off()

png("outputs/rcp45_bimodal_kdeest_by_pls_0.1_mode_crit_1000.png")
ggplot(future, aes(x,y, fill = bimclass_f_pred_pls_45))+geom_raster()
dev.off()

#----------------------- for 2.6;
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
#
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
ggplot(future.pr, aes(x,y, fill = bimclass_f_pred_pls_85_soil))+geom_raster()
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

interp.densp.soil(soilval = 0.25)

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

