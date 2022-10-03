library(ggplot2)
library(modes)
library(diptest)

#------------------------------p(bimodality) for PLS-----------------------------------------
pls <- read.csv("data/PLS_FIA_density_climate_full.csv")


pls$ecotype <- ifelse(pls$PLSdensity > 47, "Forest", ifelse(pls$PLSdensity > 0.5, "Savanna","Prairie"))

# dummyvariables for logistic regression:
pls$ecocode <- 0
pls[pls$ecotype %in% 'Forest', ]$ecocode <- 1
pls <- pls[!pls$ecotype %in% 'Prairie',]




pls$prob_bimodal <- NA

# this loops through each grid cell estimates probability of bimodality in each grid cell:
# for each grid cell, find all grid cells within +/- 0.15 PC1 units 
# sample 100 random w/replacement grid cells, evaluate wither or not the density distn is bimodal
# repeat the random sampling + bimodality evaluation 500 times

# then you have a designation of "bimodal" or "unimodal" for 100 distributions randomly sampled from within similar envrionmental space
# we then estimate the posterior mean probability of bimodality based on the sampled data
# 
for(i in 1:length(pls$prob_bimodal)){
  
  #x <- pls[pls$cell %in% 28558,]
  x <- pls[i,]
  l <- x$PC1 - 0.20
  h <- x$PC1 + 0.20
  
  # sample the number of forests and savannas in each climate range, with replacement:
  BC <- vector()
  dipP <- vector()
  forest.num <- vector()
  
  # estimate unimodal vs. bimodal based on 100 random draws, 500 times
  #for(j in 1:100){
  
  #getBCdipP <- function(data, low, high){
   # forest.cell <- sample(data[data$PC1 >= low  & data$PC1 < high,]$cell, size = 100, replace = TRUE)
    #forest.num <- pls[pls$cell %in% forest.cell, ]$ecocode
    #forest.dens <- data[data$cell %in% forest.cell, ]$PLSdensity
    
    #BC <- bimodality_coefficient(forest.dens)
    #dipP <- diptest::dip.test(na.omit(density(forest.dens)$y))$p
    #forest.num <- ifelse(BC >= 0.55 & dipP <= 0.05, 1, 0) # 1 is bimodal 2 is unimodal
    #forest.num 
  #}
  
  #test <- data.frame(index = 1:100, 
  #                  forest.num = NA)
  #forest.num <- rep(getBCdipP(data = pls, low = l, high = h), 1, 100)
  
  forest.num <- replicate(100, {
    forest.cell <- sample(pls[pls$PC1 >= l  & pls$PC1 < h,]$cell, size = 100, replace = TRUE)
    forest.num <- pls[pls$cell %in% forest.cell, ]$ecocode
    forest.dens <- pls[pls$cell %in% forest.cell, ]$PLSdensity
    
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
png("outputs/preliminary_posterior_prob_bimodality_pls_20bins.png")
ggplot(pls, aes(x,y, fill = prob_bimodal))+geom_raster()
dev.off()


write.csv(pls[,c("x", "y", "cell", "ecotype", "prob_bimodal")], "outputs/posterior_prob_bimodal_pls_20bins.csv", row.names = FALSE)
