# this script is similar to kde_bimodal_sample_bins, except that rather than sampling from the KDE estimates in each pc1 bin, 
# we sample from chris's statistical estimates for each climate bin:

pls.df <- read.csv("data/PLS_FIA_density_climate_full.csv")
# read in draws of total PLS density:
total.m <- read.csv("data/extracted_total_PLS_density_draws.csv")

dens.summary <- total.m %>% group_by(x, y) %>% summarize(mean_dens = mean(value, na.rm=TRUE),
                                                         ci.low_dens = quantile(value, 0.025, na.rm=TRUE), 
                                                         ci.high_dens = quantile(value, 0.975, na.rm=TRUE))

ggplot(dens.summary, aes(x,y, fill =  mean_dens))+geom_raster()+ scale_fill_distiller(palette = "Spectral")

dens <- merge(dens.pr, dens.summary, by = c("x", "y"), all.y = TRUE)

dens <- dens[!is.na(dens$mean_dens), ]


# add cell numbers + climate to the total.m dataset
pls.df <- left_join(dens[,c("x", "y", "cell", "PC1", "GS_ppet","mean_GS_soil" )], total.m, by = c("x", "y"))


library("MASS")
library(ggplot2)


#<<<<<<<<<<<<<<<<<<<<<<<< estimate PDF of data using kde >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

pls.df$PC1bins <- cut(pls.df$PC1, breaks=seq(-5.5, 4.5, by = 0.25))


ordered.cuts <- data.frame(PC1bins = unique(cut(pls.df[order(pls.df$PC1),]$PC1, breaks=seq(-5.5, 4.5, by = 0.25))),
                            mids = seq(-5.375, 4.5, by = 0.25))

pls.df <- pls.df[!is.na(pls.df$PC1),]
pls.df$pc1_bins <- cut(pls.df$PC1, breaks=seq(-5.5, 4.5, by = 0.25))



# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pls.df[order(pls.df$PC1),]$pc1_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(pc1_bins = unique(pls.df[order(pls.df$PC1),]$pc1_bins),
                           pc1_mids = as.numeric(substring(firsts, first = 2,last = 6))+.75)


ordered.cuts <- ordered.cuts[!is.na(ordered.cuts$pc1_bins),]
pls.df <- merge(ordered.cuts, pls.df, by = 'pc1_bins')

# binning sampling from statistical estimates:


sample.densp.bins <- function(pc1bin){
 
      dipP <- list()
      df <- data.frame(points = pls.df[pls.df$pc1_bins %in% pc1bin, ]$value)
      for(i in 1:1000){
      samp <- sample(x=df$points, size = 1000, replace = TRUE)
      dipfull <- diptest::dip.test(samp)
      P <- dipfull$p.value
      dipstat <- dipfull$statistic
      pks <- amps(samp)$Peaks[,1]
      
      #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
      P <- ifelse(length(pks) >= 2 & max(pks) >= 100, P, NA) 
      
      #plot(density(samp))
      
      dipP[[i]] <- data.frame(pvalue = P, 
                              dip = dipstat, 
                              pc1_bins = pc1bin )
      }
      dipP2 <- do.call(rbind, dipP)
      dipP2
}


dipandp <- sample.densp.bins(pc1bin = "(-2.5,-2.25]")
test.df <- do.call(rbind, dipandp)

 out <- apply(data.frame(ordered.cuts$pc1_bins), 1, sample.densp.bins)
 out.df <- as.data.frame(do.call(rbind, out))
 
 pvalues <- out.df %>% group_by(pc1_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                    median.p = median(pvalue, na.rm = TRUE),
                                                                    ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                    ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                              mean.d = mean(dip, na.rm = TRUE),
                                                              median.d = median(dip, na.rm = TRUE),
                                                              ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                              ci.high.d = quantile(dip, 0.975, na.rm = TRUE))
 

ggplot(pvalues, aes(pc1_bins, median.p))+geom_point()+geom_errorbar(data = pvalues, aes(ymin=ci.low.d, ymax=ci.high.d), color = "red", alpha = 0.5, width = 0.1)+theme_bw()
#ggplot(pvalues, aes(pc1_mids, pvalue))+geom_point()

saveRDS(out.df, "outputs/bimodal_bins_p_value_dipP_PLS_PC1_stat.rds")

# this would sample a value for each grid cell of PLS, 


# >>>>>>>>>>>>>>>>>>>>>>>>>do the same for P-PET:


# new bimodality estimates for PPET:

pls.df <- pls.df[!is.na(pls.df$GS_ppet),]
pls.df$ppet_bins <- cut(pls.df$GS_ppet, breaks=seq(-170, 205, by = 15))


ordered.cuts <- data.frame(PPETbins = unique(pls.df$ppet_bins),
                           mids = seq(-162.5, 205, by = 15))

# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pls.df[order(pls.df$GS_ppet),]$ppet_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(ppet_bins = unique(pls.df[order(pls.df$GS_ppet),]$ppet_bins),
                           ppet_mids = as.numeric(mids)+7.5)


ordered.cuts <- ordered.cuts[!is.na(ordered.cuts$ppet_bins),]
pls.df <- merge(ordered.cuts, pls.df, by = 'ppet_bins')

# binning sampling from KDE estimates:

sample.ppet.bins <- function(pc1bin){
  
  dipP <- list()
  df <- data.frame(points = pls.df[pls.df$ppet_bins %in% pc1bin, ]$value)
  for(i in 1:1000){
    samp <- sample(x=df$points, size = 1000, replace = TRUE)
    dipfull <- diptest::dip.test(samp)
    P <- dipfull$p.value
    dipstat <- dipfull$statistic
    pks <- amps(samp)$Peaks[,1]
    
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    P <- ifelse(length(pks) >= 2 & max(pks) >= 100, P, NA) 
    
    #plot(density(samp))
    
    dipP[[i]] <- data.frame(pvalue = P, 
                            dip = dipstat, 
                            ppet_bins = pc1bin )
  }
  dipP2 <- do.call(rbind, dipP)
  dipP2
}
dipandp <- sample.ppet.bins(pc1bin = "(-170,-155]")
summary(dipandp)
#test.df <- do.call(rbind, dipandp)

out <- apply(data.frame(ordered.cuts$ppet_bins), 1, sample.ppet.bins)
out.df <- as.data.frame(do.call(rbind, out))

pvalues <- out.df %>% group_by(ppet_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                             median.p = median(pvalue, na.rm = TRUE),
                                                             ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                             ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                             mean.d = mean(dip, na.rm = TRUE),
                                                             median.d = median(dip, na.rm = TRUE),
                                                             ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                             ci.high.d = quantile(dip, 0.975, na.rm = TRUE))


ggplot(pvalues, aes(ppet_bins, mean.p))+geom_point()+geom_errorbar(data = pvalues, aes(ymin=ci.low.p, ymax=ci.high.p), color = "red", alpha = 0.5, width = 0.1)+theme_bw()+geom_hline(yintercept = 0.05)

#ggplot(ordered.cuts, aes(mids, pvalue))+geom_point()

saveRDS(out.df, "outputs/bimodal_bins_p_value_dipP_PLS_PPET_stat.rds")



# >>>>>>>>>>>>>>>>>>>>>>>>>do the same for GS_soil:


# new bimodality estimates for PPET:

pls.df <- pls.df[!is.na(pls.df$mean_GS_soil),]
pls.df$soil_bins <- cut(pls.df$mean_GS_soil, breaks=seq(0, 1.8, by = 0.05))

# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pls.df$soil_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(soil_bins = unique(pls.df$soil_bins),
                           soil_mids = as.numeric(substring(firsts, first = 2,last = 6))+0.025)

pls.df <- merge(ordered.cuts, pls.df, by = "soil_bins")
#ordered.cuts <- ordered.cuts[!is.na(ordered.cuts$soilbins),]

# binning sampling from KDE estimates:
interp.soil.bins <- function(pc1bin){
  
  # find the closest PC1 value in the contour_95 df:
  closest <- contour_95[which.min(abs(contour_95$x - mean(pls.df[pls.df$soil_bins %in% pc1bin,]$soil_mids))) & contour_95$y >= 0,]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  
  
  points <- expand.grid(unique(pls.df[pls.df$soil_bins %in% pc1bin,]$mean_GS_soil), y=0:maxy)
  
  colnames(points) <- c("x", "y")
  dipP <- list()
  for(i in 1:100){
    if(max(kde(x=na.omit(cbind(pls.df$mean_GS_soil, pls.df$PLSdensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
      
      P <- NA
      dipstat <- NA
      
    }else{
      
      df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(pls.df$mean_GS_soil, pls.df$PLSdensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
      samp <- sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE)
      #samp <- sample(x=pls.df[pls.df$soil_bins %in% pc1bin,]$PLSdensity, size = 1000, replace = TRUE)
      dipfull <- diptest::dip.test(samp)
      dipfull
      P <- dipfull$p.value
      dipstat <- dipfull$statistic
      pks <- amps(samp)$Peaks[,1]
      
      #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
      P <- ifelse(length(pks) >= 2 & max(pks) >= 100, P, 1) 
      
      #plot(density(samp))
    }
    dipP[[i]] <- data.frame(pvalue = P, 
                            dip = dipstat, 
                            soil_bins = pc1bin)
  }
  dipP2 <- do.call(rbind, dipP)
  dipP2
}


sample.soil.bins <- function(pc1bin){
  
  dipP <- list()
  df <- data.frame(points = pls.df[pls.df$soil_bins %in% pc1bin, ]$value)
  for(i in 1:1000){
    samp <- sample(x=df$points, size = 1000, replace = TRUE)
    dipfull <- diptest::dip.test(samp)
    P <- dipfull$p.value
    dipstat <- dipfull$statistic
    pks <- amps(samp)$Peaks[,1]
    
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    P <- ifelse(length(pks) >= 2 & max(pks) >= 100, P, NA) 
    
    #plot(density(samp))
    
    dipP[[i]] <- data.frame(pvalue = P, 
                            dip = dipstat, 
                            soil_bins = pc1bin )
  }
  dipP2 <- do.call(rbind, dipP)
  dipP2
}


dipandp <- sample.soil.bins(pc1bin = "(0.8,0.85]")


out <- apply(data.frame(ordered.cuts$soil_bins), 1, sample.soil.bins)
out.df <- as.data.frame(do.call(rbind, out))

pvalues <- out.df %>% group_by(soil_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                             median.p = median(pvalue, na.rm = TRUE),
                                                             ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                             ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                             mean.d = mean(dip, na.rm = TRUE),
                                                             median.d = median(dip, na.rm = TRUE),
                                                             ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                             ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

pvalues <- merge(pvalues, ordered.cuts, by.x = "soil_bins")
ggplot(pvalues, aes(soil_mids, median.d))+geom_point()+geom_errorbar(data = pvalues, aes(ymin=ci.low.d, ymax=ci.high.d), color = "red", alpha = 0.5, width = 0.1)+theme_bw()
ggplot(pvalues, aes(soil_mids, median.p))+geom_point()+geom_errorbar(data = pvalues, aes(ymin=ci.low.p, ymax=ci.high.p), color = "red", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.05, linetype = "dashed")

#ggplot(ordered.cuts, aes(mids, pvalue))+geom_point()

saveRDS(out.df, "outputs/bimodal_bins_p_value_dipP_PLS_soil_15bins_kde_stat.rds")




# >>>>>>>>>>>>>>>> get bimodality for FIA data <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

pls.df <- read.csv("data/PLS_FIA_density_climate_full.csv")
# read in draws of total PLS density:
total.m <- read.csv("data/extracted_total_FIA_density_draws.csv")

dens.summary <- total.m %>% group_by(x, y) %>% summarize(mean_dens = mean(value, na.rm=TRUE),
                                                         ci.low_dens = quantile(value, 0.025, na.rm=TRUE), 
                                                         ci.high_dens = quantile(value, 0.975, na.rm=TRUE))

ggplot(dens.summary, aes(x,y, fill =  mean_dens))+geom_raster()+ scale_fill_distiller(palette = "Spectral")

dens <- merge(dens.pr, dens.summary, by = c("x", "y"), all.y = TRUE)

dens <- dens[!is.na(dens$mean_dens), ]


# add cell numbers + climate to the total.m dataset
fia.df <- left_join(dens[,c("x", "y", "cell", "PC1fia", "GS_ppet_mod","mean_GS_soil_m" )], total.m, by = c("x", "y"))


library("MASS")
library(ggplot2)
library(modes)


#<<<<<<<<<<<<<<<<<<<<<<<< estimate PDF of data using kde >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

fia.df$PC1fiabins <- cut(fia.df$PC1fia, breaks=seq(-5.5, 4.5, by = 0.25))


ordered.cuts <- data.frame(PC1fiabins = unique(cut(fia.df[order(fia.df$PC1fia),]$PC1fia, breaks=seq(-5.5, 4.5, by = 0.25))),
                           mids = seq(-5.375, 4.5, by = 0.25))

fia.df <- fia.df[!is.na(fia.df$PC1fia),]
fia.df$PC1fia_bins <- cut(fia.df$PC1fia, breaks=seq(-5.5, 4.5, by = 0.25))



# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(fia.df[order(fia.df$PC1fia),]$PC1fia_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(PC1fia_bins = unique(fia.df[order(fia.df$PC1fia),]$PC1fia_bins),
                           PC1fia_mids = as.numeric(substring(firsts, first = 2,last = 6))+.75)


ordered.cuts <- ordered.cuts[!is.na(ordered.cuts$PC1fia_bins),]
fia.df <- merge(ordered.cuts, fia.df, by = 'PC1fia_bins')

# binning sampling from statistical estimates:


sample.densp.bins <- function(PC1fiabin){
  
  dipP <- list()
  df <- data.frame(points = fia.df[fia.df$PC1fia_bins %in% PC1fiabin, ]$value)
  for(i in 1:1000){
    samp <- sample(x=df$points, size = 1000, replace = TRUE)
    dipfull <- diptest::dip.test(samp)
    P <- dipfull$p.value
    dipstat <- dipfull$statistic
    pks <- amps(samp)$Peaks[,1]
    
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    P <- ifelse(length(pks) >= 2 & max(pks) >= 100, P, NA) 
    
    #plot(density(samp))
    
    dipP[[i]] <- data.frame(pvalue = P, 
                            dip = dipstat, 
                            PC1fia_bins = PC1fiabin )
  }
  dipP2 <- do.call(rbind, dipP)
  dipP2
}


dipandp <- sample.densp.bins(PC1fiabin = "(-2.5,-2.25]")
test.df <- do.call(rbind, dipandp)

out <- apply(data.frame(ordered.cuts$PC1fia_bins), 1, sample.densp.bins)
out.df <- as.data.frame(do.call(rbind, out))

pvalues <- out.df %>% group_by(PC1fia_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                              median.p = median(pvalue, na.rm = TRUE),
                                                              ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                              ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                              mean.d = mean(dip, na.rm = TRUE),
                                                              median.d = median(dip, na.rm = TRUE),
                                                              ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                              ci.high.d = quantile(dip, 0.975, na.rm = TRUE))


ggplot(pvalues, aes(PC1fia_bins, median.p))+geom_point()+geom_errorbar(data = pvalues, aes(ymin=ci.low.d, ymax=ci.high.d), color = "red", alpha = 0.5, width = 0.1)+theme_bw()
#ggplot(pvalues, aes(PC1fia_mids, pvalue))+geom_point()

saveRDS(out.df, "outputs/bimodal_bins_p_value_dipP_PLS_PC1fia_stat.rds")



# >>>>>>>>>>>>>>>>>>>>>>>>>do the same for P-PET on the modern landscape:

# new bimodality estimates for PPET:

fia.df <- fia.df[!is.na(fia.df$GS_ppet_mod),]
fia.df$ppet_bins <- cut(fia.df$GS_ppet_mod, breaks=seq(-170, 205, by = 15))


ordered.cuts <- data.frame(PPETbins = levels(unique(fia.df$ppet_bins)),
                           mids = seq(-162.5, 205, by = 15))

# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(fia.df[order(fia.df$GS_ppet_mod),]$ppet_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(ppet_bins = unique(fia.df[order(fia.df$GS_ppet_mod),]$ppet_bins),
                           ppet_mids = as.numeric(mids)+7.5)


ordered.cuts <- ordered.cuts[!is.na(ordered.cuts$ppet_bins),]
fia.df <- merge(ordered.cuts, fia.df, by = 'ppet_bins')

# binning sampling from KDE estimates:

sample.ppet.bins <- function(pc1bin){
  
  dipP <- list()
  df <- data.frame(points = fia.df[fia.df$ppet_bins %in% pc1bin, ]$value)
  for(i in 1:1000){
    samp <- sample(x=df$points, size = 1000, replace = TRUE)
    dipfull <- diptest::dip.test(samp)
    P <- dipfull$p.value
    dipstat <- dipfull$statistic
    pks <- amps(samp)$Peaks[,1]
    
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    P <- ifelse(length(pks) >= 2 & max(pks) >= 100, P, NA) 
    
    #plot(density(samp))
    
    dipP[[i]] <- data.frame(pvalue = P, 
                            dip = dipstat, 
                            ppet_bins = pc1bin )
  }
  dipP2 <- do.call(rbind, dipP)
  dipP2
}
dipandp <- sample.ppet.bins(pc1bin = "(-170,-155]")
summary(dipandp)
#test.df <- do.call(rbind, dipandp)

out <- apply(data.frame(ordered.cuts$ppet_bins), 1, sample.ppet.bins)
out.df <- as.data.frame(do.call(rbind, out))

pvalues <- out.df %>% group_by(ppet_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                               median.p = median(pvalue, na.rm = TRUE),
                                                               ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                               ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                               mean.d = mean(dip, na.rm = TRUE),
                                                               median.d = median(dip, na.rm = TRUE),
                                                               ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                               ci.high.d = quantile(dip, 0.975, na.rm = TRUE))


ggplot(pvalues, aes(ppet_bins, mean.p))+geom_point()+geom_errorbar(data = pvalues, aes(ymin=ci.low.p, ymax=ci.high.p), color = "red", alpha = 0.5, width = 0.1)+theme_bw()+geom_hline(yintercept = 0.05)

#ggplot(ordered.cuts, aes(mids, pvalue))+geom_point()

saveRDS(out.df, "outputs/bimodal_bins_p_value_dipP_fia_PPET_stat.rds")



# >>>>>>>>>>>>>>>>>>>>>>>>>do the same for GS_soil:


# new bimodality estimates for PPET:

fia.df <- fia.df[!is.na(fia.df$mean_GS_soil_m),]
fia.df$soil_bins <- cut(fia.df$mean_GS_soil_m, breaks=seq(0, 1.8, by = 0.05))

# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(fia.df$soil_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(soil_bins = unique(fia.df$soil_bins),
                           soil_mids = as.numeric(substring(firsts, first = 2,last = 6))+0.025)

fia.df <- merge(ordered.cuts, fia.df, by = "soil_bins")
#ordered.cuts <- ordered.cuts[!is.na(ordered.cuts$soilbins),]

# binning sampling from KDE estimates:
interp.soil.bins <- function(pc1bin){
  
  # find the closest PC1 value in the contour_95 df:
  closest <- contour_95[which.min(abs(contour_95$x - mean(fia.df[fia.df$soil_bins %in% pc1bin,]$soil_mids))) & contour_95$y >= 0,]
  maxy <- ceiling(closest$y) # get the closest y value and round up
  
  
  points <- expand.grid(unique(fia.df[fia.df$soil_bins %in% pc1bin,]$mean_GS_soil_m), y=0:maxy)
  
  colnames(points) <- c("x", "y")
  dipP <- list()
  for(i in 1:100){
    if(max(kde(x=na.omit(cbind(fia.df$mean_GS_soil_m, fia.df$fiadensity)), H=H, compute.cont = TRUE, eval.points = points[,c("x", "y")])$estimate) < unique(contour_95$level)){ # this value is the 95% contour level
      
      P <- NA
      dipstat <- NA
      
    }else{
      
      df <- data.frame(points = points$y, freq = kde(x=na.omit(cbind(fia.df$mean_GS_soil_m, fia.df$fiadensity)), H=H,compute.cont = TRUE, eval.points = points)$estimate)
      samp <- sample(x=df$points, prob = df$freq, size = 1000, replace = TRUE)
      #samp <- sample(x=fia.df[fia.df$soil_bins %in% pc1bin,]$fiadensity, size = 1000, replace = TRUE)
      dipfull <- diptest::dip.test(samp)
      dipfull
      P <- dipfull$p.value
      dipstat <- dipfull$statistic
      pks <- amps(samp)$Peaks[,1]
      
      #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
      P <- ifelse(length(pks) >= 2 & max(pks) >= 100, P, 1) 
      
      #plot(density(samp))
    }
    dipP[[i]] <- data.frame(pvalue = P, 
                            dip = dipstat, 
                            soil_bins = pc1bin)
  }
  dipP2 <- do.call(rbind, dipP)
  dipP2
}


sample.soil.bins <- function(pc1bin){
  
  dipP <- list()
  df <- data.frame(points = fia.df[fia.df$soil_bins %in% pc1bin, ]$value)
  for(i in 1:1000){
    samp <- sample(x=df$points, size = 1000, replace = TRUE)
    dipfull <- diptest::dip.test(samp)
    P <- dipfull$p.value
    dipstat <- dipfull$statistic
    pks <- amps(samp)$Peaks[,1]
    
    #dipP <- diptest::dip.test(df$freq, simulate.p.value = TRUE, B = 50)$p.value
    P <- ifelse(length(pks) >= 2 & max(pks) >= 100, P, NA) 
    
    #plot(density(samp))
    
    dipP[[i]] <- data.frame(pvalue = P, 
                            dip = dipstat, 
                            soil_bins = pc1bin )
  }
  dipP2 <- do.call(rbind, dipP)
  dipP2
}


dipandp <- sample.soil.bins(pc1bin = "(0.8,0.85]")


out <- apply(data.frame(ordered.cuts$soil_bins), 1, sample.soil.bins)
out.df <- as.data.frame(do.call(rbind, out))

pvalues <- out.df %>% group_by(soil_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                               median.p = median(pvalue, na.rm = TRUE),
                                                               ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                               ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                               mean.d = mean(dip, na.rm = TRUE),
                                                               median.d = median(dip, na.rm = TRUE),
                                                               ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                               ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

pvalues <- merge(pvalues, ordered.cuts, by.x = "soil_bins")
ggplot(pvalues, aes(soil_mids, median.d))+geom_point()+geom_errorbar(data = pvalues, aes(ymin=ci.low.d, ymax=ci.high.d), color = "red", alpha = 0.5, width = 0.1)+theme_bw()
ggplot(pvalues, aes(soil_mids, median.p))+geom_point()+geom_errorbar(data = pvalues, aes(ymin=ci.low.p, ymax=ci.high.p), color = "red", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.05, linetype = "dashed")

#ggplot(ordered.cuts, aes(mids, pvalue))+geom_point()

saveRDS(out.df, "outputs/bimodal_bins_p_value_dipP_fia_soil_15bins_kde_stat.rds")

fia.soil <- merge(pvalues, pls.nona, by = c("soil_bins", "soil_mids"))

fia.soil$bimodal.dip <- ifelse(as.numeric(fia.soil$median.p) <= 0.05, "bimodal", "unimodal")
ggplot(fia.soil, aes(x, y, fill = bimodal.dip))+geom_raster()
ggplot(fia.soil, aes(x, y, fill = soil_bins))+geom_raster()


# future not uptdated as of 11.12.18
# >>>>>>>>>>>>> Predict future based on pls relationship with climate <<<<<<<<<<<<<<<<<<<<<<<<<<
# get future climates:
future.pr <- read.csv("outputs/Future_PCA.csv")
future <- future.pr


pls.total.m <- read.csv("data/extracted_total_PLS_density_draws.csv")
# merge future pr & 
future.pr <- pls.df <- left_join(future[,c("x", "y", "cell", "PC1","PC1_cc85", "mean_ppet_GS","mean_GS_soil_8.5" )], pls.total.m, by = c("x", "y"))


ordered.cuts <- data.frame(pc1_bins = levels(cut(future.pr[order(future.pr$PC1_cc85),]$PC1_cc85, breaks=seq(-5.5, 4.5, by = 0.25))),
                           mids = seq(-5.375, 4.5, by = 0.25))

future.pr <- future.pr[!is.na(future.pr$PC1_cc85),]
future.pr$pc1_bins_cc85 <- cut(future.pr$PC1_cc85, breaks=seq(-5.5, 4.5, by = 0.25))

ordered.cuts <- ordered.cuts[!is.na(ordered.cuts$pc1_bins),]
future.pr <- merge(ordered.cuts, future.pr, by.x = 'pc1_bins', by.y ="pc1_bins_cc85")

future.pr <- future.pr[!is.na(future.pr$value),]


# read in the estimates of p values and dip stats to merge with future climate bins
pls.stat <- readRDS( "outputs/bimodal_bins_p_value_dipP_PLS_PC1_stat.rds")

pvalues <- pls.stat  %>% group_by(pc1_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                               median.p = median(pvalue, na.rm = TRUE),
                                                               ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                               ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                               mean.d = mean(dip, na.rm = TRUE),
                                                               median.d = median(dip, na.rm = TRUE),
                                                               ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                               ci.high.d = quantile(dip, 0.975, na.rm = TRUE))




pvalues <- left_join(ordered.cuts, pvalues, by = "pc1_bins")
bimod.pc.pls <- left_join(future.pr, pvalues, by = "pc1_bins" )


# merge with the envt + pc data

bimod.pc.pls$bimclass <- ifelse(bimod.pc.pls$mean.p <= 0.05, "bimodal", "unimodal")

bimod.pc.pls.fut.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc.pls, aes(x=x, y=y, fill = bimclass))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal"='#d73027',"unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")



pc1.dip.pls.8.5 <- ggplot(pvalues, aes(pc1_mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS PC1")+ylab("DIP value")+xlim(-6.4, 4.5)
pc1.pval.pls.8.5 <- ggplot(pvalues, aes(pc1_mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS PC1")+ylab("P value")+xlim(-6.4, 4.5)


png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/pls_dip_pvalues_unc_pc1_ccesm_8.5.png")
plot_grid(pc1.dip.pls, pc1.pval.pls)
dev.off()



future$bimclass_f_pred_pls_85 <- ifelse(future$dipPint_f_pred_pls_85 <= 0.05 , "bimodal", "unimodal")
png("outputs/rcp85_dipP_pred_by_pls_0.1_map.png")
ggplot(bimod.pc.pls, aes(x,y, fill = mean.p))+geom_raster()
dev.off()

png("outputs/rcp85_bimodal_pred_by_pls_0.1_map.png")
ggplot(bimod.pc.pls, aes(x,y, fill = bimclass))+geom_raster()
dev.off()


# now do the same for P-PET 

future.pr$ppet_bins <- cut(future.pr$mean_ppet_GS, breaks=seq(-170, 205, by = 15))


ordered.cuts <- data.frame(ppet_bins = levels(future.pr$ppet_bins),
                           ppet_mids = seq(-162.5, 205, by = 15))

future.pr <- future.pr[!is.na(future.pr$mean_ppet_GS),]
#future.pr$pc1_bins_cc85 <- cut(future.pr$mean_ppet_GS, breaks=seq(-5.5, 4.5, by = 0.25))



ordered.cuts <- ordered.cuts[!is.na(ordered.cuts$ppet_bins),]
future.pr <- merge(ordered.cuts, future.pr, by= "ppet_bins")

future.pr <- future.pr[!is.na(future.pr$value),]


# read in the estimates of p values and dip stats to merge with future climate bins
ppet.stat <- readRDS( "outputs/bimodal_bins_p_value_dipP_PLS_PPET_stat.rds")

pvalues <- ppet.stat  %>% group_by(ppet_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                 median.p = median(pvalue, na.rm = TRUE),
                                                                 ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                 ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                 mean.d = mean(dip, na.rm = TRUE),
                                                                 median.d = median(dip, na.rm = TRUE),
                                                                 ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                 ci.high.d = quantile(dip, 0.975, na.rm = TRUE))




pvalues <- left_join(ordered.cuts, pvalues, by = "ppet_bins")
bimod.ppet.pls <- left_join(future.pr, pvalues, by = "ppet_bins" )


# merge with the envt + pc data

bimod.ppet.pls$bimclass_ppet <- ifelse(bimod.ppet.pls$mean.p <= 0.05, "bimodal", "unimodal")

bimod.ppet.pls.fut.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.pls, aes(x=x, y=y, fill = bimclass_ppet))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal"='#d73027',"unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")



ppet.dip.pls.8.5 <- ggplot(pvalues, aes(ppet_mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS PC1")+ylab("DIP value")+xlim(-6.4, 4.5)
ppet.pval.pls.8.5 <- ggplot(pvalues, aes(ppet_mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS PC1")+ylab("P value")+xlim(-6.4, 4.5)


png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/pls_dip_pvalues_unc_ppet_ccesm_8.5.png")
plot_grid(ppet.dip.pls.8.5, ppet.pval.pls.8.5)
dev.off()



png("outputs/rcp85_dipP_pred_by_pls_0.1_map_ppet.png")
ggplot(bimod.ppet.pls, aes(x,y, fill = mean.p))+geom_raster()
dev.off()

png("outputs/rcp85_bimodal_pred_by_pls_0.1_map_ppet.png")
ggplot(bimod.ppet.pls, aes(x,y, fill = bimclass))+geom_raster()
dev.off()

# get future soil moisture estimates:


# now do the same for P-PET and soil moisture

future.pr$soil_bins <- cut(future.pr$mean_GS_soil_8.5,  breaks=seq(0, 1.8, by = 0.05))


ordered.cuts <- data.frame(soil_bins = levels(future.pr$soil_bins),
                           soil_mids = seq(0.025, 1.8, by = 0.05))

future.pr <- future.pr[!is.na(future.pr$mean_GS_soil_8.5),]
#future.pr$pc1_bins_cc85 <- cut(future.pr$mean_GS_soil_8.5, breaks=seq(-5.5, 4.5, by = 0.25))



ordered.cuts <- ordered.cuts[!is.na(ordered.cuts$soil_bins),]
future.pr <- merge(ordered.cuts, future.pr, by= "soil_bins")

future.pr <- future.pr[!is.na(future.pr$value),]


# read in the estimates of p values and dip stats to merge with future climate bins
soil.stat <- readRDS( "outputs/bimodal_bins_p_value_dipP_PLS_soil_15bins_kde_stat.rds")

pvalues <- soil.stat  %>% group_by(soil_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                   median.p = median(pvalue, na.rm = TRUE),
                                                                   ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                   ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                   mean.d = mean(dip, na.rm = TRUE),
                                                                   median.d = median(dip, na.rm = TRUE),
                                                                   ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                   ci.high.d = quantile(dip, 0.975, na.rm = TRUE))




pvalues <- left_join(ordered.cuts, pvalues, by = "soil_bins")
bimod.soil.pls <- left_join(future.pr, pvalues, by = "soil_bins" )


# merge with the envt + pc data

bimod.soil.pls$bimclass_soil <- ifelse(bimod.soil.pls$mean.p <= 0.05, "bimodal", "unimodal")

bimod.soil.pls.fut.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.soil.pls, aes(x=x, y=y, fill = bimclass_soil))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal"='#d73027',"unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")



soil.dip.pls.8.5 <- ggplot(pvalues, aes(soil_mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS PC1")+ylab("DIP value")+xlim(-6.4, 4.5)
soil.pval.pls.8.5 <- ggplot(pvalues, aes(soil_mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS PC1")+ylab("P value")+xlim(-6.4, 4.5)


png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/pls_dip_pvalues_unc_soil_ccesm_8.5.png")
plot_grid(soil.dip.pls.8.5, soil.pval.pls.8.5)
dev.off()



png("outputs/rcp85_dipP_pred_by_pls_0.1_map_soil.png")
ggplot(bimod.soil.pls, aes(x,y, fill = mean.p))+geom_raster()
dev.off()

png("outputs/rcp85_bimodal_pred_by_pls_0.1_map_soil.png")
ggplot(bimod.soil.pls, aes(x,y, fill = bimclass_soil))+geom_raster()
dev.off()




########################################################################
# make predictions for the future from FIA relationship to climate
#
future.pr <- read.csv("outputs/Future_PCA.csv")
future <- future.pr


fia.total.m <- read.csv("data/extracted_total_FIA_density_draws.csv")
# merge future pr & 
future.fia <- fia.df <- left_join(future[,c("x", "y", "cell", "PC1fia","PC1_cc85", "mean_ppet_GS","mean_GS_soil_8.5" )], fia.total.m, by = c("x", "y"))


ordered.cuts <- data.frame(pc1_bins = levels(cut(future.fia[order(future.fia$PC1_cc85),]$PC1_cc85, breaks=seq(-5.5, 4.5, by = 0.25))),
                           mids = seq(-5.375, 4.5, by = 0.25))

future.fia <- future.fia[!is.na(future.fia$PC1_cc85),]
future.fia$pc1_bins_cc85 <- cut(future.fia$PC1_cc85, breaks=seq(-5.5, 4.5, by = 0.25))

ordered.cuts <- ordered.cuts[!is.na(ordered.cuts$pc1_bins),]
future.fia <- merge(ordered.cuts, future.fia, by.x = 'pc1_bins', by.y ="pc1_bins_cc85")

future.fia <- future.fia[!is.na(future.fia$value),]


# read in the estimates of p values and dip stats to merge with future climate bins
fia.stat <- readRDS( "outputs/bimodal_bins_p_value_dipP_PLS_PC1fia_stat.rds")
colnames(fia.stat) <- c("pvalue", "dip", "pc1_bins")
pvalues <- fia.stat  %>% group_by(pc1_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                 median.p = median(pvalue, na.rm = TRUE),
                                                                 ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                 ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                 mean.d = mean(dip, na.rm = TRUE),
                                                                 median.d = median(dip, na.rm = TRUE),
                                                                 ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                 ci.high.d = quantile(dip, 0.975, na.rm = TRUE))




pvalues <- left_join(ordered.cuts, pvalues, by = "pc1_bins")
bimod.pc.fia <- left_join(future.fia, pvalues, by = "pc1_bins" )


# merge with the envt + pc data

bimod.pc.fia$bimclass <- ifelse(bimod.pc.fia$mean.p <= 0.05, "bimodal", "unimodal")

bimod.pc.fia.fut.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc.fia, aes(x=x, y=y, fill = bimclass))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal"='#d73027',"unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")



pc1.dip.fia.8.5 <- ggplot(pvalues, aes(pc1_mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("fia PC1")+ylab("DIP value")+xlim(-6.4, 4.5)
pc1.pval.fia.8.5 <- ggplot(pvalues, aes(pc1_mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("fia PC1")+ylab("P value")+xlim(-6.4, 4.5)


png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/fia_dip_pvalues_unc_pc1_ccesm_8.5.png")
plot_grid(pc1.dip.fia.8.5, pc1.pval.fia.8.5)
dev.off()



png("outputs/rcp85_dipP_pred_by_fia_0.1_map.png")
ggplot(bimod.pc.fia, aes(x,y, fill = mean.p))+geom_raster()
dev.off()

png("outputs/rcp85_bimodal_pred_by_fia_0.1_map.png")
ggplot(bimod.pc.fia, aes(x,y, fill = bimclass))+geom_raster()
dev.off()


# now do the same for P-PET 

future.fia$ppet_bins <- cut(future.fia$mean_ppet_GS, breaks=seq(-170, 205, by = 15))


ordered.cuts <- data.frame(ppet_bins = levels(future.fia$ppet_bins),
                           ppet_mids = seq(-162.5, 205, by = 15))

future.fia <- future.fia[!is.na(future.fia$mean_ppet_GS),]
#future.fia$pc1_bins_cc85 <- cut(future.fia$mean_ppet_GS, breaks=seq(-5.5, 4.5, by = 0.25))



ordered.cuts <- ordered.cuts[!is.na(ordered.cuts$ppet_bins),]
future.fia <- merge(ordered.cuts, future.fia, by= "ppet_bins")

future.fia <- future.fia[!is.na(future.fia$value),]


# read in the estimates of p values and dip stats to merge with future climate bins
ppet.stat <- readRDS( "outputs/bimodal_bins_p_value_dipP_FIA_PPET_stat.rds")

pvalues <- ppet.stat  %>% group_by(ppet_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                   median.p = median(pvalue, na.rm = TRUE),
                                                                   ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                   ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                   mean.d = mean(dip, na.rm = TRUE),
                                                                   median.d = median(dip, na.rm = TRUE),
                                                                   ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                   ci.high.d = quantile(dip, 0.975, na.rm = TRUE))




pvalues <- left_join(ordered.cuts, pvalues, by = "ppet_bins")
bimod.ppet.fia <- left_join(future.fia, pvalues, by = "ppet_bins" )


# merge with the envt + pc data

bimod.ppet.fia$bimclass_ppet <- ifelse(bimod.ppet.fia$mean.p <= 0.05, "bimodal", "unimodal")

bimod.ppet.fia.fut.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.fia, aes(x=x, y=y, fill = bimclass_ppet))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal"='#d73027',"unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")



ppet.dip.fia.8.5 <- ggplot(pvalues, aes(ppet_mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("fia PC1")+ylab("DIP value")+xlim(-6.4, 4.5)
ppet.pval.fia.8.5 <- ggplot(pvalues, aes(ppet_mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("fia PC1")+ylab("P value")+xlim(-6.4, 4.5)


png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/fia_dip_pvalues_unc_ppet_ccesm_8.5.png")
plot_grid(ppet.dip.fia.8.5, ppet.pval.fia.8.5)
dev.off()



png("outputs/rcp85_dipP_pred_by_fia_0.1_map_ppet.png")
ggplot(bimod.ppet.fia, aes(x,y, fill = mean.p))+geom_raster()
dev.off()

png("outputs/rcp85_bimodal_pred_by_fia_0.1_map_ppet.png")
ggplot(bimod.ppet.fia, aes(x,y, fill = bimclass))+geom_raster()
dev.off()

# get future soil moisture estimates:


# now do the same for P-PET and soil moisture

future.fia$soil_bins <- cut(future.fia$mean_GS_soil_8.5,  breaks=seq(0, 1.8, by = 0.05))


ordered.cuts <- data.frame(soil_bins = levels(future.fia$soil_bins),
                           soil_mids = seq(0.025, 1.8, by = 0.05))

future.fia <- future.fia[!is.na(future.fia$mean_GS_soil_8.5),]
#future.fia$pc1_bins_cc85 <- cut(future.fia$mean_GS_soil_8.5, breaks=seq(-5.5, 4.5, by = 0.25))



ordered.cuts <- ordered.cuts[!is.na(ordered.cuts$soil_bins),]
future.fia <- merge(ordered.cuts, future.fia, by= "soil_bins")

future.fia <- future.fia[!is.na(future.fia$value),]


# read in the estimates of p values and dip stats to merge with future climate bins
soil.stat <- readRDS( "outputs/bimodal_bins_p_value_dipP_FIA_soil_15bins_kde_stat.rds")

pvalues <- soil.stat  %>% group_by(soil_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                   median.p = median(pvalue, na.rm = TRUE),
                                                                   ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                   ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                   mean.d = mean(dip, na.rm = TRUE),
                                                                   median.d = median(dip, na.rm = TRUE),
                                                                   ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                   ci.high.d = quantile(dip, 0.975, na.rm = TRUE))




pvalues <- left_join(ordered.cuts, pvalues, by = "soil_bins")
bimod.soil.fia <- left_join(future.fia, pvalues, by = "soil_bins" )


# merge with the envt + pc data

bimod.soil.fia$bimclass_soil <- ifelse(bimod.soil.fia$mean.p <= 0.05, "bimodal", "unimodal")

bimod.soil.fia.fut.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.soil.fia, aes(x=x, y=y, fill = bimclass_soil))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal"='#d73027',"unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")



soil.dip.fia.8.5 <- ggplot(pvalues, aes(soil_mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("fia PC1")+ylab("DIP value")+xlim(-6.4, 4.5)
soil.pval.fia.8.5 <- ggplot(pvalues, aes(soil_mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("fia PC1")+ylab("P value")+xlim(-6.4, 4.5)


png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/fia_dip_pvalues_unc_soil_ccesm_8.5.png")
plot_grid(soil.dip.fia.8.5, soil.pval.fia.8.5)
dev.off()



png("outputs/rcp85_dipP_pred_by_fia_0.1_map_soil.png")
ggplot(bimod.soil.fia, aes(x,y, fill = mean.p))+geom_raster()
dev.off()

png("outputs/rcp85_bimodal_pred_by_fia_0.1_map_soil.png")
ggplot(bimod.soil.fia, aes(x,y, fill = bimclass_soil))+geom_raster()
dev.off()



# >>>>>>>>>>>>>>>>> make plots of future bimodality or not <<<<<<<<<<<<<<<<<<<
