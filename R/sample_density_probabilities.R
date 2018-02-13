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


dens.samp <- read.csv("outputs/density_100samples_by_cell.csv")

# probability of bimodality for each grid cell:
library(modes)
dens.samp <- merge(dens.samp, pls[,c("x", "y", "cell", "PC1")], by = "cell")
#dens.test <- dens.samp[na.omit(dens.samp)]

dens.samp$prob_bimodal <- NA
dens.samp$bimodal <- NA

BC <- apply(dens.samp[,3:102], MARGIN = 1, FUN = bimodality_coefficient)

dipP <- apply(dens.samp[,3:102], MARGIN = 1, FUN = function(x){diptest::dip.test(na.omit(density(x)$y))$p})
dens.samp$dipP <- dipP
dens.samp$BC <- BC
dens.samp$bimodal <- ifelse(dens.samp$BC >= 0.55 & dens.samp$dipP < 0.05, "Bimodal", "Stable")

ggplot(dens.samp, aes(x,y, fill = bimodal))+geom_raster()


pls$prob_bimodal <- NA

# this loops through each grid cell estimates probability of bimodality in each grid cell:
# for each grid cell, find all grid cells within +/- 0.15 PC1 units 
# sample 100 random w/replacement grid cells, evaluate wither or not the density distn is bimodal
# repeat the random sampling + bimodality evaluation 500 times

# then you have a designation of "bimodal" or "unimodal" for 100 distributions randomly sampled from within similar envrionmental space
# we then estimate the posterior mean probability of bimodality based on the sampled data
# 
for(i in 1:length(pls$prob_bimodal)){
  
  x <- pls[pls$cell %in% 28558,]
  x <- pls[i,]
  l <- x$PC1 - 0.15
  h <- x$PC1 + 0.15
  
  # sample the number of forests and savannas in each climate range, with replacement:
  BC <- vector()
  dipP <- vector()
  forest.num <- vector()
 
  # estimate unimodal vs. bimodal based on 100 random draws, 500 times
  #for(j in 1:100){
    
  getBCdipP <- function(data, low, high){
      forest.cell <- sample(data[data$PC1 >= low  & data$PC1 < high,]$cell, size = 100, replace = TRUE)
      #forest.num <- pls[pls$cell %in% forest.cell, ]$ecocode
      forest.dens <- data[data$cell %in% forest.cell, ]$PLSdensity
      
      BC <- bimodality_coefficient(forest.dens)
      dipP <- diptest::dip.test(na.omit(density(forest.dens)$y))$p
      forest.num <- ifelse(BC >= 0.55 & dipP <= 0.05, 1, 0) # 1 is bimodal 2 is unimodal
     forest.num 
  }
  
  #test <- data.frame(index = 1:100, 
   #                  forest.num = NA)
 #forest.num <- rep(getBCdipP(data = pls, low = l, high = h), 1, 100)
  
  forest.num <- replicate(100, {
    forest.cell <- sample(data[data$PC1 >= l  & data$PC1 < h,]$cell, size = 100, replace = TRUE)
    forest.num <- pls[pls$cell %in% forest.cell, ]$ecocode
    forest.dens <- data[data$cell %in% forest.cell, ]$PLSdensity
    
    BC <- bimodality_coefficient(forest.dens)
    dipP <- diptest::dip.test(na.omit(density(forest.dens)$y))$p
    forest.num <- ifelse(BC >= 0.55 & dipP <= 0.05, 1, 0) # 1 is bimodal 2 is unimodal
    forest.num 
  })
  
      
  
  N = length(forest.num) # sample size should be 500
  nForest = sum(forest.num == 1) # number of forests
  nSav = sum(forest.num== 0 ) # number of savannas
  
  theta = seq(from=1/(N+1), to=N/(N+1), length=N) # theta 
  
  ### prior distribution
  
 #pTheta = pmin(theta, 1-theta) 
  pTheta = dbeta(theta, 10, 10)# beta prior with mean = .5
  pTheta = pTheta/sum(pTheta) # Normalize so sum to 1
  
  # calculate the likelihood given theta
  pDataGivenTheta = choose(N, nForest) * theta^nForest * (1-theta)^nSav
  
  
  # calculate the denominator from Bayes theorem; this is the marginal # probability of y
  pData = sum(pDataGivenTheta*pTheta)
  pThetaGivenData = pDataGivenTheta*pTheta / pData # Bayes theorem
  
  # prints out df
  #round(data.frame(theta, prior=pTheta, likelihood=pDataGivenTheta, posterior=pThetaGivenData), 3)
  
  # get the posterior mean probability of bimodality given data
  posteriorMean = sum(pThetaGivenData*theta) 
  
  pls[i,]$prob_bimodal <- posteriorMean
}

# plot out the probability of forests in the pls region:
png("outputs/preliminary_posterior_prob_bimodality_pls.png")
ggplot(pls, aes(x,y, fill = prob_bimodal))+geom_raster()
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
