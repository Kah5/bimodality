#logit or probit regression models for FIA data
#Kelly Heilman
#November 26th, 2016
library(plyr)
library(raster)
library(data.table)
library(rgdal)

FIAgrids <- read.csv("outputs/FIA_plot_agg_grid_alb.csv")
FIAplots <- read.csv("outputs/FIA_plot_agg_fuzzed_alb.csv")

#extract prism data for each plot
# read in and average prism data
prism<- raster("C:/Users/JMac/Documents/Kelly/biomodality/data/PRISM_ppt_30yr_normal_4kmM2_all_bil/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
prism.alb<- projectRaster(prism, crs='+init=epsg:3175')
#spec.table<- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_alb.csv")
#spec.table <- data.frame(spec.table)
FIAplots$pr30yr <- extract(prism.alb, FIAplots[,c("x","y")])

write.csv(FIAplots[,c('x', 'y', 'coverscenter','pctcover', 'pr30yr')], 'C:/Users/JMac/Documents/Kelly/biomodality/data/FIA_plot_cover_prism.csv')

plot(FIAplots$pr30yr, FIAplots$pctcover)

mylogit <- glm(pctcover ~ pr30yr, data = FIAplots, family = "binomial")

cat(
  "model {
  for( i in 1 : N ) {
  FIA[i] ~ dbern(p[i])

  p[i] <- 1 / (1 + exp(-z[i]))
	z[i] <- b0 + b1 * pr30yr[i]
  }             
  b0 ~ dnorm( 0 , 1.0E-12 )
  b1 ~ dnorm( 0 , 1.0E-12 )
  }", file="logistic.bug"
    )


dat1<-list(FIA=FIAplots$pctcover, pr30yr=FIAplots$pr30yr, N=nrow(FIAplots))

#to find the initial parameter estimates, use coeff from mLE logistic reg

estInits<-with(FIAplots, glm(pctcover~pr30yr, family=binomial(logit)))
estInits

inits<-list(
  list(b0=estInits$coef[1]-1.2, b1=estInits$coef[2]+0.001),
  list(b0=estInits$coef[1]+1.2, b1=estInits$coef[2]-0.001)        
)

parameters<- c("b0", "b1")

library('R2jags')


logmod<- jags(data = dat1,
              inits = inits,
              parameters.to.save = parameters,
              model.file = "logistic.jag",
              n.chains = 2,
              n.iter = 5000,
              n.burnin = 2000,
              n.thin = 1)

logmod

plot(as.mcmc(logmod))
logmod.vars <- c( "b0", "b1")

# Sampling from the posterior --still having issues with this
logmod.res <-  coda.samples( model = logmod,
                            variable.names = c('p'),
                           n.iter= 100,
                           thin = 1, na.rm=TRUE)
summary( logmod )

