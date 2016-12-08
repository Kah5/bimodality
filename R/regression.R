#logit or probit regression models for FIA data
#Kelly Heilman
#November 26th, 2016
library(plyr)
library(raster)
library(data.table)
library(rgdal)

setwd("C:/Users/JMac/Documents/Kelly/biomodality/")
FIAgrids <- read.csv("outputs/FIA_plot_agg_grid_alb.csv")
FIAplots <- read.csv("outputs/FIA_plot_agg_fuzzed_alb.csv")

#read in GHCN precipitation data

#extract prism data for each plot
# read in and average prism data
#prism<- raster("C:/Users/JMac/Documents/Kelly/biomodality/data/PRISM_ppt_30yr_normal_4kmM2_all_bil/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
#prism.alb<- projectRaster(prism, crs='+init=epsg:3175')
#spec.table<- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_alb.csv")
#spec.table <- data.frame(spec.table)
modprecip <- read.csv("data/FIAplots_pr_alb_1980-2011_GHCN.csv")

FIAplots <- merge(FIAplots, modprecip, by = c('x','y'))

write.csv(FIAplots[,c('x', 'y', 'coverscenter','pctcover', 'total_.')], 'C:/Users/JMac/Documents/Kelly/biomodality/data/FIA_plot_cover_prism.csv')

plot(FIAplots$total_., FIAplots$pctcover)

mylogit <- glm(pctcover ~ total_., data = FIAplots, family = "binomial")

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


dat1<-list(FIA=FIAplots$pctcover, pr30yr=FIAplots$total_., N=nrow(FIAplots))

#to find the initial parameter estimates, use coeff from mLE logistic reg

estInits<-with(FIAplots, glm(pctcover~pr30yr, family=binomial(logit)))
estInits

inits<-list(
  list(b0=estInits$coef[1]-1.2, b1=estInits$coef[2]+0.001),
  list(b0=estInits$coef[1]+1.2, b1=estInits$coef[2]-0.001)        
)

parameters<- c("b0", "b1",'p')

library('R2jags')


logmod<- jags(data = dat1,
             # inits = inits,
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
PLSpoints <- read.csv("data/PLSpoints_sand_soils.csv")

#extract prism data for each plot
# read in and average prism data
#prism<- raster("C:/Users/JMac/Documents/Kelly/biomodality/data/PRISM_ppt_30yr_normal_4kmM2_all_bil/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
#prism.alb<- projectRaster(prism, crs='+init=epsg:3175')
#spec.table<- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_alb.csv")
#spec.table <- data.frame(spec.table)

#PLSpoints.agg <- dcast(PLSpoints, Pointx + Pointy~., sum, na.rm=TRUE, value.var = 'coverscenter')
#write.csv(PLSpoints.agg, "C:/Users/JMac/Documents/Kelly/biomodality/data/PLSpoints.agg.csv")
avg_hist_ppt <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/data/PLSpoints_pr_alb_1950_2000_GHCN.csv') # read in extract precip data
#avg_hist_ppt <- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/outputs/")
#PLS.ppt.merge <- merge(PLSpoints ,avg_hist_ppt, by = c('Pointx', 'Pointy'))
PLSpoints$pr <- avg_hist_ppt$total_.
avg_PPET <- read.csv(("C:/Users/JMac/Documents/Kelly/biomodality/data/PLSpoints_PET_alb_1900_1950_GHCN.csv"))
PLSpoints$PET <- avg_PPET$total_.

avg_TEMP <- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/data/PLSpoints_air_temp_alb_1900_1950_GHCN.csv")
PLSpoints <- merge(PLSpoints, avg_TEMP[,c('x', "y", "annual_.", "range_.")], by = c("x", "y"))


plot(PLSpoints$annual_., PLSpoints$pct.cov)
plot(PLSpoints$range_., PLSpoints$pct.cov)

write.csv(PLSpoints[,c('x', 'y', 'pct.cov', 'pr')], 'C:/Users/JMac/Documents/Kelly/biomodality/data/PLS_point_cover_prism.csv')



plot(PLSpoints$pr, PLSpoints$pct.cov)
hist(PLSpoints$pct.cov)
hist(PLSpoints$pr)
#plot denisity histograms binned by precipitation amount

PLSpoints$plsprbins <- cut(PLSpoints$pr, labels = c('350-500mm', '500-650mm', '650-700mm', '700-850mm', '850-1000mm', '1000-1150mm', '1150-1300mm', ">1300mm"),breaks=c(350, 500, 650, 700, 850, 1000, 1150, 1300, 2000))
#dens.pr$fiaprbins <- cut(dens.pr$MAP2011, labels = c('350-500mm', '500-650mm', '650-700mm', '700-850mm', '850-1000mm', '1000-1150mm', '1150-1300mm', ">1300mm),breaks=c(350, 500, 650, 700, 850, 1000, 1150, 1300))
melted <- melt(PLSpoints[, c("Pointx", 'Pointy', 'cell', 'plsprbins','pr', 'pct.cov')], id.vars = c("Pointx", 'Pointy', 'cell', 'plsprbins','pr')) 

#plot by precipitation bins
ggplot(melted, aes(value, fill = variable)) +geom_density(alpha = 0.3)  +xlim(0, 1.5)+ facet_wrap(~plsprbins)+scale_fill_brewer(palette = "Set1")
ggplot(PLSpoints, aes(x=x, y=y, color = pct.cov))+geom_point()

PLSpoints <- PLSpoints[!is.na(PLSpoints$pr),]

#data_balanced_over <- ovun.sample(pct.cov ~ ., data = PLSpoints, method = "both",N = 10090)$data
#table(data_balanced_over$pct.cov)

mylogitpls <- glm(pct.cov ~ pr +PET +annual_.+sandpct, data = PLSpoints, family = binomial (link = "logit"))
summary(mylogitpls)

newdata <- data.frame(pr = 1300, PET=700, total_. = )
predict(mylogitpls, newdata, type="response")
 

xweight <- seq(550, 2000, 0.01)


yweight <- predict(mylogitpls, list(pr = xweight),type="response")

lines(xweight, yweight)
plot(PLSpoints$pr, PLSpoints$pct.cov)

require(popbio)
logi.hist.plot(data_balanced_over$pr, data_balanced_over$pct.cov,boxp=FALSE,type="hist",col="gray")
#need to reduce the size of pls data to train on since R couldn't handle that large of a vector

require(caTools)
set.seed(101) 
sample = sample.split(PLSpoints$pct.cov, SplitRatio = .05)
train = subset(PLSpoints, sample == TRUE) # use 10% of original PLS data
test = subset(PLSpoints, sample == FALSE)

#train <- train[train$pr >= 600,]
#train <- train[train$pr < 750,]
cat(
  "model {
  for( i in 1 : N ) {
  PLS[i] ~ dbern(p[i])
  logit(p[i]) <- b0 + b1*pr30yr[i]
  #p[i] <- 1 / (1 + exp(-z[i]))
  #z[i] <- b0 + b1 * pr30yr[i]
  }             
  
  b0 ~ dnorm(0, 0.0001)
  b1 ~ dnorm(0, 0.0001)
  }", file="logisticpls.bug"
    )


dat2<-list(PLS=train$pct.cov, pr30yr=train$pr, N=nrow(train))

#to find the initial parameter estimates, use coeff from mLE logistic reg

estInits<-with(train, glm(pct.cov~pr, family=binomial(logit)))
estInits

inits<-list(
  list(b0=estInits$coef[1]-1.2, b1=estInits$coef[2]+0.001),
  list(b0=estInits$coef[1]+1.2, b1=estInits$coef[2]-0.001)        
)

parameters<- c("b0", "b1" , "p")

library('R2jags')


logmodpls<- jags(data = dat2,
              #inits = inits,
              parameters.to.save = parameters,
              model.file = "logisticpls.bug",
              n.chains = 4,
              n.iter = 1000,
              n.burnin = 200,
              n.thin = 1)

logmodpls

plot(as.mcmc(logmodpls))
logmod.vars <- c( "b0", "b1")

plot(dat2$pr30yr, dat2$PLS, pch=20)
points(dat2$pr30yr,logmodpls$BUGSoutput$mean$p,col="red", pch=20)
#reg<-lm(m1$BUGSoutput$mean$mu~MAN)
abline(mylogitpls, col="blue")
low.ci<-logmodpls$BUGSoutput$summary[4:1348,3]
high.ci<-logmodpls$BUGSoutput$summary[4:1348,7]
reg2<-glm(low.ci~dat2$pr30yr, family = 'binomial')
abline(reg2, col="red", lty="dashed")
reg3<-glm(high.ci~dat2$pr30yr)
abline(reg3, col="red", lty="dashed")

mylogitpls <- glm(pct.cov ~ pr, data = PLSpoints, family = "binomial")
plot(PLSpoints$pr, PLSpoints$pct.cov)
curve(predict(mylogitpls,data.frame(pr=x),type="resp"),add=TRUE, col = 'red') # draws a curve based on prediction from logistic regression model

#read in P-PET for pls
PPET <- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/data/PLSpoints_PET_alb_1900_1950_GHCN.csv")
plot(PPET$total_., PLSpoints$pct.cov)
ET <- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/data/FIAplotsPLSpoints_E150_alb_1900_1905_GHCN.csv")
plot(ET$total_., PLSpoints$pct.cov)
temp <- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/data/PLSpoints_air_temp_alb_1900_1950_GHCN.csv")
temp <- merge(temp, PLSpoints, by = c('x', 'y'))
plot(temp$total_., PLSpoints$pct.cov)

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
