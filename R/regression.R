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
  logit(p[i]) <- b0 + b1*pr30yr[i]
  #p[i] <- 1 / (1 + exp(-z[i]))
	#z[i] <- b0 + b1 * pr30yr[i]
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

parameters<- c("b0", "b1", 'p')

library('R2jags')


logmod<- jags(data = dat1,
              inits = inits,
              parameters.to.save = parameters,
              model.file = "logistic.bug",
              n.chains = 2,
              n.iter = 5000,
              n.burnin = 2000,
              n.thin = 1)

logmod

plot(as.mcmc(logmod))
logmod.vars <- c( "b0", "b1")

plot(dat1$pr30yr,dat1$FIA, pch=20)
points(dat1$pr30yr,logmod$BUGSoutput$mean$p,col="red", pch=20)
#reg<-lm(m1$BUGSoutput$mean$mu~MAN)
#abline(mylogit, col="red")
#low.ci<-logmod$BUGSoutput$summary[6:28,3]
#high.ci<-logmod$BUGSoutput$summary[6:28,7]
#reg2<-lm(low.ci~MAN)
#abline(reg2, col="red", lty="dashed")
#reg3<-lm(high.ci~MAN)
#abline(reg3, col="red", lty="dashed")



# Sampling from the posterior --still having issues with this
logmod.res <-  jags.samples( model = logmod,
                            variable.names = c('p'),
                           n.iter= 100,
                           thin = 1, na.rm=TRUE)
summary( logmod )



#for PLS plots:
PLSpoints <- read.csv("outputs/species_table_pls_coverscenter.csv")

#extract prism data for each plot
# read in and average prism data
prism<- raster("C:/Users/JMac/Documents/Kelly/biomodality/data/PRISM_ppt_30yr_normal_4kmM2_all_bil/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
prism.alb<- projectRaster(prism, crs='+init=epsg:3175')
#spec.table<- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_alb.csv")
#spec.table <- data.frame(spec.table)

PLSpoints.agg <- dcast(PLSpoints, Pointx + Pointy~., sum, na.rm=TRUE, value.var = 'coverscenter')
write.csv(PLSpoints.agg, "C:/Users/JMac/Documents/Kelly/biomodality/data/PLSpoints.agg.csv")
avg_hist_ppt <- read.csv("data/PLSpoints.agg.1900_1910prismppt.csv") # read in extract precip data

PLSpoints$pr <- avg_hist_ppt$total_.

write.csv(PLSpoints[,c('x', 'y', 'coverscenter', 'pr')], 'C:/Users/JMac/Documents/Kelly/biomodality/data/PLS_point_cover_prism.csv')

plot(PLSpoints$pr, PLSpoints$coverscenter)

PLSpoints <- PLSpoints[!is.na(PLSpoints$pr),]

mylogitpls <- glm(coverscenter ~ pr, data = PLSpoints, family = "binomial")

#need to reduce the size of pls data to train on since R couldn't handle that large of a vector

require(caTools)
set.seed(101) 
sample = sample.split(PLSpoints$coverscenter, SplitRatio = .10)
train = subset(PLSpoints, sample == TRUE) # use 10% of original PLS data
test = subset(PLSpoints, sample == FALSE)


cat(
  "model {
  for( i in 1 : N ) {
  PLS[i] ~ dbern(p[i])
  #logit(p[i]) <- b0 + b1*pr[i]
  p[i] <- 1 / (1 + exp(-z[i]))
  z[i] <- b0 + b1 * pr30yr[i]
  }             
  
  b0 ~ dnorm(0, .0001)
  b1 ~ dnorm(0, .0001)
  }", file="logisticpls.bug"
    )


dat2<-list(PLS=train$coverscenter, pr30yr=train$pr, N=nrow(train))

#to find the initial parameter estimates, use coeff from mLE logistic reg

estInits<-with(train, glm(coverscenter~pr30yr, family=binomial(logit)))
estInits

inits<-list(
  list(b0=estInits$coef[1]-1.2, b1=estInits$coef[2]+0.001),
  list(b0=estInits$coef[1]+1.2, b1=estInits$coef[2]-0.001)        
)

parameters<- c("b0", "b1", "p")

library('R2jags')


logmodpls<- jags(data = dat2,
              #inits = inits,
              parameters.to.save = parameters,
              model.file = "logisticpls.bug",
              n.chains = 2,
              n.iter = 500,
              n.burnin = 200,
              n.thin = 1)

logmodpls

plot(as.mcmc(logmodpls))
logmod.vars <- c( "b0", "b1")

plot(dat2$pr30yr,dat2$PLS, pch=20)
points(dat2$pr30yr,logmodpls$BUGSoutput$mean$p,col="red", pch=20)
#reg<-lm(m1$BUGSoutput$mean$mu~MAN)
abline(mylogit, col="red")
low.ci<-logmod$BUGSoutput$summary[6:28,3]
high.ci<-logmod$BUGSoutput$summary[6:28,7]
reg2<-lm(low.ci~MAN)
abline(reg2, col="red", lty="dashed")
reg3<-lm(high.ci~MAN)
abline(reg3, col="red", lty="dashed")



#example logit from online
N <- 1000
x <- 1:N
z <- 0.01 * x - 5
y <- sapply(1 / (1 + exp(-z)), function(p) {rbinom(1, 1, p)})

write.table(data.frame(X = x, Z = z, Y = y),
            file = 'example3.data',
            row.names = FALSE,
            col.names = TRUE)

plot(x, y)
cat('model {
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    p[i] <- 1 / (1 + exp(-z[i]))
    z[i] <- a + b * x[i]
  }
  a ~ dnorm(0, .0001)
  b ~ dnorm(0, .0001)
}', file="example3.bug")

library('rjags')

data1 = list('x' = x,
            'y' = y,
            'N' = N)

jagstest <- jags(data = data1,
                  model.file= 'example3.bug', 
                   parameters.to.save = c('a', 'b', 'p'),
                 n.chains = 2,
                 n.iter = 5000,
                 n.burnin = 2000,
                 n.thin = 1)

#update(jags, 1000)

#jags.samples(jags,
 #            c('a', 'b'),
  #           1000)

plot(as.mcmc(jagstest))
plot(data1$x, data1$y)
points(data1$x, jagstest$BUGSoutput$mean$p, col = 'red')
