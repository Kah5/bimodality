library(ggplot2)
library(modes)
library(diptest)

#------------------------------p(bimodality) for PLS-----------------------------------------



# for PC1:
pls <- read.csv("data/PLS_FIA_density_climate_full.csv")


pls$ecotype <- ifelse(pls$PLSdensity > 47, "Forest", ifelse(pls$PLSdensity > 0.5, "Savanna",ifelse(is.na(pls$PLSdensity),"NA", "Prairie")))

# dummyvariables for logistic regression:
pls$ecocode <- NA
pls[pls$ecotype %in% 'Forest', ]$ecocode <- 1
pls[pls$ecotype %in% 'Savanna', ]$ecocode <- 0
pls[pls$ecotype %in% 'Prairie',]$ecocode <- 0


# get posterior mean probability of forest for each grid cell, base on climate space +/- 0.15 PC1 away from grid cell:


pls <- pls[!is.na(pls$PC1), ]

pls.density <- pls[!is.na(pls$PLSdensity) & ! is.na(pls$ecocode),]
pls$prob_bimodal <- NA
# this loops through each grid cell estimates probability of bimodality in each grid cell:
# for each grid cell, find all grid cells within +/- 0.15 PC1 units 
# sample 100 random w/replacement grid cells, evaluate wither or not the density distn is bimodal
# repeat the random sampling + bimodality evaluation 500 times

# then you have a designation of "bimodal" or "unimodal" for 100 distributions randomly sampled from within similar envrionmental space
# we then estimate the posterior mean probability of bimodality based on the sampled data
# 

for(i in 1:length(pls$prob_bimodal)){
  l <- pls[i,]$PC1 - 0.1
  h <- pls[i,]$PC1 + 0.1
  
  # sample the number of forests and savannas in each climate range, with replacement:
  BC <- vector()
  dipP <- vector()
  forest.num <- vector()
  
  # estimate unimodal vs. bimodal based on 100 random draws, 500 times
  
  #
  data.distn <-  pls.density[pls.density$PC1 >= l  & pls.density$PC1 < h & pls.density$PLSdensity < 650 ,]$PLSdensity
  #data.distn <- data.distn[!is.na(data.distn)]
  if(length(data.distn >= 2)){
  forest.num <- replicate(500,
                          {
                            
                            # crate a function that approximates density, so we can estimate probability at any value
                            dens.func <- approxfun(density(data.distn,na.rm=TRUE))
                            #
                            prob.q <- dens.func(0:600)
                            prob.q[is.na(prob.q)] <- 0
                            bimo <- ifelse(as.numeric(dip.test(sample(0:600, 1000, replace=TRUE, prob=prob.q))$p) <= 0.05,1, 0)
                            bimo  })
  
  N = length(forest.num) # sample size should be 500
  nForest = sum(forest.num == 1) # number of bimodal
  nSav = sum(forest.num== 0 ) # number of unimodal
  
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
  }else{
    pls[i,]$prob_bimodal <- NA
  }
  
}


# plot out the probability of forests in the pls region:
png("outputs/preliminary_posterior_prob_bimodality_pls_0.1bins_dipP_only_PC1_by_distn.png")
ggplot(pls, aes(x,y, fill = prob_bimodal))+geom_raster()
dev.off()


write.csv(pls[,c("x", "y", "cell", "ecotype", "prob_bimodal")], "outputs/posterior_prob_bimodal_pls_0.1bins_dipP_only_PC1_by_distn.csv", row.names = FALSE)
