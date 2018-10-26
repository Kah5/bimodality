# read in the density estimates + uncertainty:

library(sp)
library(raster)
library(ncdf4)
library(reshape2)
library(tidyr)
library(dplyr)
library(ggplot2)
pls.nc <- nc_open(filename = "data/PLS_density_western_v0.999.nc")

# data structure: x = 146, y = 180, sample = 250 MCMC samples
# has x, y, sample for each taxa and for Total density
# same structure for the pls.point.nc
# not sure exactly how these differ:

pls.point.nc <- nc_open(filename = "data/PLS_density_western_point_v0.999.nc")


x <- ncvar_get(pls.nc, "x")
y <- ncvar_get(pls.nc, "y")
n <- ncvar_get(pls.nc, "sample")

total.array <- ncvar_get(pls.nc, "Total") # store the data in a 3-dimensional array
nc_close(pls.nc) # close the file

dim(total.array)# an x by y by sample array
rownames(total.array) <- x # assign column and row names to the array
colnames(total.array) <- y

# melt arry into a dataframe
total.m <- melt(total.array, varnames=c("x","y","sample"), as.is = TRUE)
# convert x + y to numerics
total.m$x <- as.numeric(total.m$x)
total.m$y <- as.numeric(total.m$y)

# write the extracted total density values to a csv:
write.csv(total.m, "data/extracted_total_PLS_density_draws.csv", row.names = FALSE)

dens.summary <- total.m %>% group_by(x, y) %>% summarize(mean_dens = mean(value, na.rm=TRUE),
                                        ci.low_dens = quantile(value, 0.025, na.rm=TRUE), 
                                        ci.high_dens = quantile(value, 0.975, na.rm=TRUE))

ggplot(dens.summary, aes(x,y, fill =  mean_dens))+geom_raster()+ scale_fill_distiller(palette = "Spectral")


# read in old estimates of density + climate data:
dens.pr <- read.csv("data/PLS_FIA_density_climate_full.csv")

head(dens.summary)
head(dens.pr)

dens <- merge(dens.pr, dens.summary, by = c("x", "y"))


ggplot()+geom_errorbar(data= dens, aes(ymin = ci.low_dens, ymax = ci.high_dens), color = "grey")+
  geom_point(data = dens, aes(PLSdensity, mean_dens), size = 0.2)#+geom_abline(intercept = 0, slope = 1, color = "red")+theme_bw()


pred.old.plot <- ggplot(dens, aes(PLSdensity, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(PLSdensity, mean_dens), color = "black", size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+theme_bw()+ylab("Smoothed Density")+xlab("Previous grid cell density")

png(height = 5, width = 5, units = "in", res = 200, "outputs/chris_estimates_vs_previous_estimates.png")
pred.old.plot
dev.off()



pc1_unc <- ggplot(dens, aes(PC1, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(PC1, mean_dens), color = "black", size = 0.5)+theme_bw()

ppet_unc <- ggplot(dens, aes(GS_ppet, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(GS_ppet, mean_dens), color = "black", size = 0.5)+theme_bw()


soil_unc <- ggplot(dens, aes(mean_GS_soil, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(mean_GS_soil, mean_dens), color = "black", size = 0.5)+theme_bw()


ggplot()+geom_histogram(data = dens, aes(mean_dens), fill = "red")+geom_histogram(data= dens, aes(ci.high_dens), aes = 2, color = "grey")+geom_histogram(data= dens, aes(ci.low_dens), alpha = 0.2, fill = "blue")


