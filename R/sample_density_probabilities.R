# script for sampling p(forest) within environmental bin of each grid cell:
library(ggplot2)


#------------------------------p(forest) for PLS-----------------------------------------
pls <- read.csv("data/PLS_FIA_density_climate_full.csv")


pls$ecotype <- ifelse(pls$PLSdensity > 47, "Forest", ifelse(pls$PLSdensity > 0.5, "Savanna","Prairie"))

# dummyvariables for logistic regression:
pls$ecocode <- 0
pls[pls$ecotype %in% 'Forest', ]$ecocode <- 1
pls <- pls[!pls$ecotype %in% 'Prairie',]


# get posterior mean probability of forest for each grid cell, base on climate space +/- 0.15 PC1 away from grid cell:

pls$prob_forest <- NA


# this for loop is not ideal, but it works:
for(i in 1:length(pls$prob_forest)){
      
      x <- pls[i,]
      low <- x$PC1 - 0.15
      high <- x$PC1 + 0.15
      # sample the number of forests and savannas in each climate range, with replacement:
      forest.cell <- sample(pls[pls$PC1 >= low  & pls$PC1 < high,]$cell, size = 100, replace = TRUE)
      forest.num <- pls[pls$cell %in% forest.cell, ]$ecocode
      forest.dens <- pls[pls$cell %in% forest.cell, ]$PLSdensity
      
      bimodality_coefficient(forest.dens)
      diptest::dip.test(na.omit(density(forest.dens)$y))$p
      
      N = length(forest.num) # sample size should be 500
      nForest = sum(forest.num == 1) # number of forests
      nSav = sum(forest.num== 0 ) # number of savannas
      
      theta = seq(from=1/(N+1), to=N/(N+1), length=N) # theta 
     
       ### prior distribution
      
      pTheta = pmin(theta, 1-theta) # beta prior with mean = .5
      
      pTheta = pTheta/sum(pTheta) # Normalize so sum to 1
      
      # calculate the likelihood given theta
      pDataGivenTheta = choose(N, nForest) * theta^nForest * (1-theta)^nSav
      
      
      # calculate the denominator from Bayes theorem; this is the marginal # probability of y
      pData = sum(pDataGivenTheta*pTheta)
      pThetaGivenData = pDataGivenTheta*pTheta / pData # Bayes theorem
      
      # prints out df
      round(data.frame(theta, prior=pTheta, likelihood=pDataGivenTheta, posterior=pThetaGivenData), 3)
      
      # get the posterior mean probability of forest given data
      posteriorMean = sum(pThetaGivenData*theta) 
      
      pls[i,]$prob_forest <- posteriorMean
}

# plot out the probability of forests in the pls region:
ggplot(pls, aes(x,y, fill = prob_forest))+geom_raster()


# create discrete probability cuts
label.breaks <- function(beg, end, splitby){
  labels.test <- data.frame(first = seq(beg, end, by = splitby), second = seq((beg + splitby), (end + splitby), by = splitby))
  labels.test <- paste (labels.test$first, '-' , labels.test$second)
  labels.test
}


pls$pforest <- cut(pls$prob_forest, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))



cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")
pls$pforest <- as.character(pls$pforest)
#ggplot(full, aes(x, y, color = ypreddiscrete)) + geom_point()




# plot the discrete probability of forest 
p.forest <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls, aes(x=x, y=y, fill = pforest))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= cbpalette, labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.forest


# plot PLS forests
pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls, aes(x=x, y=y, fill = ecotype))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS classification")+ scale_fill_manual(values= c("#006837", "tan","#c2e699"))+ coord_equal()+theme_bw()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                                                                                                                                                        panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1), legend.title = element_blank())+ annotate("text", x=-90000, y=1486000,label= "A", size = 5)+ggtitle("")

png(width = 8, height = 4, units = 'in', res = 300, filename = 'outputs/paper_figs/binimial_prob_forest_pls.png')
grid.arrange(pls.map, p.forest, nrow = 1, ncol=2)
dev.off()

# save the file with p(forest):

write.csv(pls[,c("x", "y", "cell", "ecotype", "pforest", "prob_forest")], "outputs/posterior_prob_forest_pls.csv", row.names = FALSE)



# probability of bimodality for each grid cell:
library(modes)

# read in results from crc_sample_bimodality_probability.R (run on crc):
pls.b <- read.csv("outputs/posterior_prob_bimodal_pls.csv")
pls.b$pbimodal <- cut(pls.b$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

cbpaletteb <- c('#ca0020',
  '#f4a582',
  '#f7f7f7',
  '#92c5de',
  '#0571b0')
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

pls.b$pbimodal <- as.character(pls.b$pbimodal)
#ggplot(full, aes(x, y, color = ypreddiscrete)) + geom_point()


# plot out the probability of forests in the pls region:
png("outputs/preliminary_posterior_prob_bimodality_pls_15.png")
p.bimodal <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.b, aes(x=x, y=y, fill = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+ scale_fill_manual(values= rev(cbpaletteb), labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.bimodal
dev.off()

# for bins of 0.20:

pls.b20 <- read.csv("outputs/posterior_prob_bimodal_pls_20bins.csv")
pls.b20$pbimodal <- cut(pls.b20$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

cbpaletteb <- c('#ca0020',
                '#f4a582',
                '#f7f7f7',
                '#92c5de',
                '#0571b0')
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

pls.b20$pbimodal <- as.character(pls.b20$pbimodal)
#ggplot(full, aes(x, y, color = ypreddiscrete)) + geom_point()


# plot out the probability of forests in the pls region:
png("outputs/preliminary_posterior_prob_bimodality_pls_20.png")
p.bimodal20 <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.b20, aes(x=x, y=y, fill = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+ scale_fill_manual(values= rev(cbpaletteb), labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")

p.bimodal20
dev.off()


# for bins of 0.25 PC1 units
# read in results from crc_sample_bimodality_probability.R (run on crc):
pls.b25 <- read.csv("outputs/posterior_prob_bimodal_pls_25bins.csv")
pls.b25$pbimodal <- cut(pls.b25$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

cbpaletteb <- c('#ca0020',
                '#f4a582',
                '#f7f7f7',
                '#92c5de',
                '#0571b0')
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

pls.b25$pbimodal <- as.character(pls.b25$pbimodal)
#ggplot(full, aes(x, y, color = ypreddiscrete)) + geom_point()


# plot out the probability of forests in the pls region:
png("outputs/preliminary_posterior_prob_bimodality_pls_25.png")
p.bimodal25 <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.b25, aes(x=x, y=y, fill = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+ scale_fill_manual(values= rev(cbpaletteb), labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")

p.bimodal25
dev.off()

# for bins of +/- 0.5 PC1 bins:

pls.b50 <- read.csv("outputs/posterior_prob_bimodal_pls_50bins.csv")
pls.b50$pbimodal <- cut(pls.b50$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

cbpaletteb <- c('#ca0020',
                '#f4a582',
                '#f7f7f7',
                '#92c5de',
                '#0571b0')
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

pls.b50$pbimodal <- as.character(pls.b50$pbimodal)
#ggplot(full, aes(x, y, color = ypreddiscrete)) + geom_point()


# plot out the probability of forests in the pls region:
png("outputs/preliminary_posterior_prob_bimodality_pls_50.png")
p.bimodal50 <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.b50, aes(x=x, y=y, fill = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+ scale_fill_manual(values= rev(cbpaletteb), labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")

p.bimodal50
dev.off()


# for bins of +/- 0.75 bins:
pls.b75 <- read.csv("outputs/posterior_prob_bimodal_pls_75bins.csv")
pls.b75$pbimodal <- cut(pls.b75$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

cbpaletteb <- c('#ca0020',
                '#f4a582',
                '#f7f7f7',
                '#92c5de',
                '#0571b0')
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

pls.b75$pbimodal <- as.character(pls.b75$pbimodal)
#ggplot(full, aes(x, y, color = ypreddiscrete)) + geom_point()


# plot out the probability of forests in the pls region:
png("outputs/preliminary_posterior_prob_bimodality_pls_75.png")
p.bimodal75 <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pls.b75, aes(x=x, y=y, fill = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+ scale_fill_manual(values= rev(cbpaletteb), labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")

p.bimodal75
dev.off()



#------------------------------p(forest) for FIA-----------------------------------------
fia <- read.csv("data/PLS_FIA_density_climate_full.csv")
fia <- fia[! is.na(fia$FIAdensity),]

fia$ecotype <- ifelse(fia$FIAdensity > 47, "Forest", ifelse(fia$FIAdensity > 0.5, "Savanna","Prairie"))

# dummyvariables for logistic regression:
fia$ecocode <- 0
fia[fia$ecotype %in% 'Forest', ]$ecocode <- 1
fia <- fia[!fia$ecotype %in% 'Prairie',]


# get posterior mean probability of forest for each grid cell, base on climate space +/- 0.15 PC1 away from grid cell:

fia$prob_forest <- NA


# this for loop is not ideal, but it works:
for(i in 1:length(fia$prob_forest)){
  
  x <- fia[i,]
  low <- x$PC1fia - 0.15
  high <- x$PC1fia + 0.15
  # sample the number of forests and savannas in each climate range, with replacement:
  forest.num <- sample(fia[fia$PC1fia >= low  & fia$PC1fia < high,]$ecocode, size = 500, replace = TRUE)
  
  
  N = length(forest.num) # sample size should be 500
  nForest = sum(forest.num == 1) # number of forests
  nSav = sum(forest.num== 0 ) # number of savannas
  
  theta = seq(from=1/(N+1), to=N/(N+1), length=N) # theta 
  
  ### prior distribution
  
  #pTheta = pmin(theta, 1-theta) # beta prior with mean = .5
  # beta prior with mean = .5
  pTheta = dbeta(theta, 10, 10)
  pTheta = pTheta/sum(pTheta) # Normalize so sum to 1
  
  # calculate the likelihood given theta
  pDataGivenTheta = choose(N, nForest) * theta^nForest * (1-theta)^nSav
  
  
  # calculate the denominator from Bayes theorem; this is the marginal # probability of y
  pData = sum(pDataGivenTheta*pTheta)
  pThetaGivenData = pDataGivenTheta*pTheta / pData # Bayes theorem
  
  # prints out df
  round(data.frame(theta, prior=pTheta, likelihood=pDataGivenTheta, posterior=pThetaGivenData), 3)
  
  # get the posterior mean probability of forest given data
  posteriorMean = sum(pThetaGivenData*theta) 
  
  fia[i,]$prob_forest <- posteriorMean
}

# plot out the probability of forests in the pls region:
ggplot(fia, aes(x,y, fill = prob_forest)) + geom_raster()


# create discrete probability cuts
label.breaks <- function(beg, end, splitby){
  labels.test <- data.frame(first = seq(beg, end, by = splitby), second = seq((beg + splitby), (end + splitby), by = splitby))
  labels.test <- paste (labels.test$first, '-' , labels.test$second)
  labels.test
}


fia$pforest <- cut(fia$prob_forest, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))



cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")
fia$pforest <- as.character(fia$pforest)





# plot the discrete probability of forest 
p.forest.f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fia, aes(x=x, y=y, fill = pforest))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= cbpalette, labels=c("0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.forest.f


# plot PLS forests
fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fia, aes(x=x, y=y, fill = ecotype))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS classification")+ scale_fill_manual(values= c("#006837", "tan","#c2e699"))+ coord_equal()+theme_bw()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                                                                                                                                                        panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1), legend.title = element_blank())+ annotate("text", x=-90000, y=1486000,label= "A", size = 5)+ggtitle("")

png(width = 8, height = 4, units = 'in', res = 300, filename = 'outputs/paper_figs/binomial_prob_forest_fia.png')
grid.arrange(fia.map, p.forest, nrow = 1, ncol=2)
dev.off()

# save the FIA file with p(forest):
write.csv(fia[,c("x", "y", "cell", "ecotype", "pforest", "prob_forest")], "outputs/posterior_prob_forest_fia.csv", row.names = FALSE)


# ---------------------probability bimodal from FIA:

# scripts run on the crc for looking at p(bimodality) in density over different environmental binwidths:
# read in results from crc_sample_bimodality_probability.R (run on crc):
fia.b <- read.csv("outputs/posterior_prob_bimodal_fia.csv")
fia.b$pbimodal <- cut(fia.b$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

cbpaletteb <- c('#ca0020',
                '#f4a582',
                '#f7f7f7',
                '#92c5de',
                '#0571b0')
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

fia.b$pbimodal <- as.character(fia.b$pbimodal)
#ggplot(full, aes(x, y, color = ypreddiscrete)) + geom_point()


# plot out the probability of forests in the fia region:
png("outputs/preliminary_posterior_prob_bimodality_fia_15.png")
p.bimodalf <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fia.b, aes(x=x, y=y, fill = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+ scale_fill_manual(values= rev(cbpaletteb), labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")

p.bimodalf
dev.off()

# for bins of 0.20:

fia.b20 <- read.csv("outputs/posterior_prob_bimodal_fia_20.csv")
fia.b20$pbimodal <- cut(fia.b20$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

cbpaletteb <- c('#ca0020',
                '#f4a582',
                '#f7f7f7',
                '#92c5de',
                '#0571b0')
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

fia.b20$pbimodal <- as.character(fia.b20$pbimodal)
#ggplot(full, aes(x, y, color = ypreddiscrete)) + geom_point()


# plot out the probability of forests in the fia region:
png("outputs/preliminary_posterior_prob_bimodality_fia_20.png")
p.bimodal20f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fia.b20, aes(x=x, y=y, fill = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+ scale_fill_manual(values= rev(cbpaletteb), labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")

p.bimodal20f
dev.off()


# for bins of 0.25 PC1 units
# read in results from crc_sample_bimodality_probability.R (run on crc):
fia.b25 <- read.csv("outputs/posterior_prob_bimodal_fia_25.csv")
fia.b25$pbimodal <- cut(fia.b25$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

cbpaletteb <- c('#ca0020',
                '#f4a582',
                '#f7f7f7',
                '#92c5de',
                '#0571b0')
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

fia.b25$pbimodal <- as.character(fia.b25$pbimodal)
#ggplot(full, aes(x, y, color = ypreddiscrete)) + geom_point()


# plot out the probability of forests in the fia region:
png("outputs/preliminary_posterior_prob_bimodality_fia_25.png")
p.bimodal25f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fia.b25, aes(x=x, y=y, fill = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+ scale_fill_manual(values= rev(cbpaletteb), labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")

p.bimodal25f
dev.off()

# for bins of +/- 0.5 PC1 bins:

fia.b50 <- read.csv("outputs/posterior_prob_bimodal_fia_50.csv")
fia.b50$pbimodal <- cut(fia.b50$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

cbpaletteb <- c('#ca0020',
                '#f4a582',
                '#f7f7f7',
                '#92c5de',
                '#0571b0')
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

fia.b50$pbimodal <- as.character(fia.b50$pbimodal)
#ggplot(full, aes(x, y, color = ypreddiscrete)) + geom_point()


# plot out the probability of forests in the fia region:
png("outputs/preliminary_posterior_prob_bimodality_fia_50.png")
p.bimodal50f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fia.b50, aes(x=x, y=y, fill = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+ scale_fill_manual(values= rev(cbpaletteb), labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")

p.bimodal50f
dev.off()


# for bins of +/- 0.75 bins:
fia.b75 <- read.csv("outputs/posterior_prob_bimodal_fia_75bins.csv")
fia.b75$pbimodal <- cut(fia.b75$prob_bimodal, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))

cbpaletteb <- c('#ca0020',
                '#f4a582',
                '#f7f7f7',
                '#92c5de',
                '#0571b0')
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")

fia.b75$pbimodal <- as.character(fia.b75$pbimodal)
#ggplot(full, aes(x, y, color = ypreddiscrete)) + geom_point()


# plot out the probability of forests in the fia region:
png("outputs/preliminary_posterior_prob_bimodality_fia_75.png")
p.bimodal75f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fia.b75, aes(x=x, y=y, fill = pbimodal))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(bimodal)")+ scale_fill_manual(values= rev(cbpaletteb), labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (bimodal)")+ggtitle("")

p.bimodal75f
dev.off()


# combine all pls and fia plots together in one big figure:

grid.arrange(p.bimodal20f, p.bimodal25f, p.bimodal50f, p.bimodal75f, ncol = 4)