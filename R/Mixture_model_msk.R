library(nimble)
library(ggplot2)
library(dplyr)
library(ggridges)

# this script does the same as the mixture model in mixture_model_msk.R, but on data that masks out the grid cells without FIA plots
pls <- read.csv("outputs/density_full_unc.csv")


pls.na <- is.na(pls$PC1)
pls.nona <- pls[!pls.na,]
pls.nona <- pls.nona[!is.na(pls.nona$FIAdensity),]

dmixgamma <- nimbleFunction(
  run = function(x = double(), p = double(0), alpha1 = double(0), beta1 = double(0), 
                 alpha2 =  double(0), beta2 = double(0), log = logical(0, default = 0)) {
    returnType(double())
    tmp = rep(0, 2)
    tmp[1] = log(p) + dgamma(x, alpha1, beta1, log = TRUE) 
    tmp[2] = log(1.0 - p) + dgamma(x, alpha2, beta2, log = TRUE) 
    a = max(tmp[1:2])
    out = a + log(sum(exp(tmp[1:2] - a)))
    if (log) {
      return(out)
    } else {
      return(exp(out))
    }
  }
)

rmixgamma <- nimbleFunction(
  run = function(n = double(), p = double(0), alpha1 = double(0), beta1 = double(0), 
                   alpha2 =  double(0), beta2 = double(0)) {
   returnType(double())
    out = rep(0, 2)
    idx = rbinom(1, 1, prob = 1.0 - p)
    out[1] = rgamma(1, alpha1, beta1)
    out[2] = rgamma(1, alpha2, beta2)
    return(out[idx+1])
  })

registerDistributions(list(
  dmixgamma = list(
    BUGSdist = "dmixgamma(p, alpha1, beta1, alpha2, beta2)",
    discrete = FALSE,
    range = c(0, Inf),
    types = c('value = double()', 'p = double(0)', 'alpha1 = double(0)', 'beta1 = double(0)', 'alpha2 = double(0)', 'beta2 = double(0)')
)))

mixturemod <- nimbleCode({
  
  for (i in 1:n){
    ## no covariate model 
    # y[i] ~ dmixgamma(p, alpha1, beta1, alpha2, beta2)
    
    ## covariate model
    y[i] ~ dmixgamma(p[i], alpha1, beta1, alpha2, beta2)
    logit(p[i]) <- sum(X[i, 1:d] %*% beta[1:d])
    
  }
  
  ## no covaraiate model
  # p ~ dunif(0, 1)
  
  for (j in 1:d) {
    beta[j] ~ dnorm(mean=0, sd = 1)
  }
  alpha1 ~ dgamma(1, 1)
  alpha2 ~ dgamma(1, 1)
  
  beta1 ~ dgamma(1, 1)
  beta2 ~ dgamma(1, 1)
}) 

## keep the missing values
options(na.action='na.pass')
data <- list(
  y = pls.nona$mean_dens,
  X = model.matrix(~ PC1, data = pls.nona)
)
options(na.action='na.omit')

constants <- list(
  n = nrow(pls.nona),
  d = ncol(data$X)
)

#inits <- list(
  #z = sample(c(0, 1), nrow(pls.nona), replace = TRUE)
#)

# dimensions <- list(
#   z = nrow(pls)
# )


model <- nimbleModel(mixturemod, constants = constants, data = data) #, inits = inits)#, dimensions = dimensions )



n_thin <- 5
n_mcmc <- 5000
n_burn <- 5000

spec <- configureMCMC(
  model, 
  thin = n_thin, 
  enableWAIC = TRUE
)

spec$addMonitors('p')

Rmcmc <- buildMCMC(spec)

Cmodel <- compileNimble(model)
Cmcmc <- compileNimble(Rmcmc, project = model)
Cmcmc$run(n_mcmc + n_burn)
Cmcmc$calculateWAIC(nburnin = 100) # calculate WAIC for mixture model


samples.fit <- as.matrix(Cmcmc$mvSamples)[(n_burn/n_thin+1):((n_mcmc+n_burn)/n_thin), ]

#p_post <- samples.fit[, "p"]
p_post <- matrix(0, nrow(pls.nona), nrow(samples.fit))
for(i in 1:nrow(pls.nona)){
    p_post[ i,]  <-    samples.fit[, paste("p[", i, "]", sep = "")]

  }

plot(pls.nona$PC1, apply(p_post, 1, mean))

# plot(p_post)
alpha1_post <- samples.fit[, "alpha1"]
alpha2_post <- samples.fit[, "alpha2"]
beta_int <- samples.fit[,"beta[1]"]
beta_env <- samples.fit[,"beta[2]"]
beta1_post <- samples.fit[, "beta1"]
beta2_post <- samples.fit[, "beta2"]

plot(beta_dens, type = "l")
plot(beta_env, type = "l")
plot(alpha1_post, type = "l")
plot(alpha2_post, type = "l")
plot(beta1_post, type = "l")
plot(beta2_post, type = "l")


x <- seq(0, 600, length.out = 10000)
hist(pls$mean_dens, freq = FALSE, breaks = 200)
curve(mean(p_post) * dgamma(x, mean(alpha1_post), mean(beta1_post)), from = 0, to = 600, add = TRUE)
curve(mean(1 - p_post) * dgamma(x, mean(alpha2_post), mean(beta2_post)), from = 0, to = 600, add = TRUE)

z <- matrix(0, nrow(samples.fit), nrow(pls.nona))

for(i in 1:nrow(samples.fit)){
  if (i %% 250 == 0) {
    message("On iteration ", i, " out of ", nrow(samples.fit))
  }
  # for(j in 1:2){
  # for(j in 1:nrow(pls)){
  #   A <- log(p_post[i]) + dgamma(pls$mean_dens[j], alpha1_post[i], beta1_post[i], log = TRUE)
  #   B <- log(1.0 - p_post[i]) * dgamma(pls$mean_dens[j], alpha2_post[i], beta2_post[i], log = TRUE)
  #   ptilde <- 1 / (1 + exp(B - A))
  #   z[i, j] <- rbinom(1, 1, ptilde)
  # }
  A <- log(p_post[, i]) + dgamma(pls.nona$mean_dens, alpha1_post[i], beta1_post[i], log = TRUE)
  B <- log(1.0 - p_post[, i]) + dgamma(pls.nona$mean_dens, alpha2_post[i], beta2_post[i], log = TRUE)
  ptilde <- 1 / (1 + exp(B - A))
  z[i, ] <- rbinom(nrow(pls.nona), 1, ptilde) 
  # }
}


str(apply(z, 2, mean))# probablity of being in large cluster




pls.nona$prob <- apply(z, 2, mean)
plot(pls.nona$PC1, pls.nona$prob, col = adjustcolor("black", alpha.f = 0.1))

ggplot(pls.nona, aes(PC1, prob, color = mean_dens))+geom_point(size = 0.5)+ylim(1,0)+ylab("Probability Forest mode (0 == forest)")

ggplot(pls.nona, aes(PC1, mean_dens, color = prob))+geom_point()
ggplot(data = pls.nona, aes(mean_dens, fill = prob > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))#+geom_line(data = datcurve, aes(x,y, color = as.factor(zval)))+theme_bw()+
  scale_color_manual(values = c("red", "blue"))+theme(legend.title = element_blank())+xlab("Mean Tree Density (stems/ha)")+ylab("density")



pls$zval <- ifelse(pls$prob_large > 0.5, 0, 1)
x = seq(0, 800, length.out = 1000)



datcurve <- data.frame(x = x, 
                       y = c(dgamma(x, mean(alpha1_post), mean(beta1_post)), dgamma(x, mean(alpha2_post), mean(beta2_post))),
                       zval = c(rep(0, 1000), rep(1, 1000))
                       )


png("outputs/mixture_model_msk/basic_mixture_model_msk_histogram.png")
ggplot(data = pls, aes(mean_dens, fill = zval > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))+geom_line(data = datcurve, aes(x,y, color = as.factor(zval)))+theme_bw()+
  scale_color_manual(values = c("red", "blue"))+theme(legend.title = element_blank())+xlab("Mean Tree Density (stems/ha)")+ylab("density")
dev.off()


# for each bin of climate bins, find the ratio of low to high probability:

ggplot(data = pls.nona, aes(mean_dens, fill = prob > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))#+geom_line(data = datcurve, aes(x,y, color = as.factor(zval)))+theme_bw()+
  #scale_color_manual(values = c("red", "blue"))+theme(legend.title = element_blank())+xlab("Mean Tree Density (stems/ha)")+ylab("density")
pls.nona$pc1_bins <- cut(pls.nona$PC1, breaks=seq(-5.5, 4.5, by = 0.25))
ggplot(data = pls.nona, aes(mean_dens, fill = prob > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))+facet_wrap(~pc1_bins)

ordered.cuts <- data.frame(pc1_bins = levels(cut(pls.nona[order(pls.nona$PC1),]$PC1, breaks=seq(-5.5, 4.5, by = 0.25))),
                           mids = seq(-5.375, 4.5, by = 0.25))

pls.nona <- merge(pls.nona, ordered.cuts, by = "pc1_bins")
pls.nona$mode <- ifelse(pls.nona$prob > 0.5, "Savanna", "Forest")

density.plots.pls.pc1 <- ggplot(data = pls.nona, aes(x = mean_dens, y = as.factor(pc1_bins), fill = mode), alpha = 0.5)+geom_density_ridges()+xlim(0,550)+ylab("PC1 value")+xlab("Tree Density (stems/ha)")+coord_flip()+theme_bw(base_size = 15)+
  theme(panel.grid.major = element_blank())+scale_fill_manual(values = c('#005a32', '#8c510a'))

png(height = 6, width = 12, units = "in", res = 300, "outputs/mixture_model_msk/PC1_mixture_mode_densityplot_by_pc1.png")
density.plots.pls.pc1
dev.off()


# for pls, lets plot points of the mean tree density in each mode & plot the point with equal probability of low and high modes:
mid.summary.pc1 <- pls.nona %>% group_by(mode, pc1_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                       ci.low = quantile(mean_dens,0.025),
                                                                                       ci.high = quantile(mean_dens, 0.975))

mid.summary.lowprob.pc1 <- pls.nona %>% group_by(prob > 0.49 & prob < 0.51 , pc1_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                                                ci.low = quantile(mean_dens,0.025),
                                                                                                                                ci.high = quantile(mean_dens, 0.975),
                                                                                                           ncell = length(mean_dens))
mid.summary.lowprob.pc1<- mid.summary.lowprob.pc1[mid.summary.lowprob.pc1$ncell >=1 ,]

hysteresis.pc1.pls <- ggplot(mid.summary.pc1, aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.pc1, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  theme_bw()+scale_fill_manual(values = c('#005a32', '#8c510a'))+scale_color_manual(values = c('#005a32', '#8c510a'))+geom_smooth(data = mid.summary.lowprob.pc1[mid.summary.lowprob.pc1$`prob > 0.49 & prob < 0.51` %in% T,], aes(mids, mean), color = "black", linetype = "dashed",se = FALSE)+
  ylab("Mean Tree Density (stems/ha)")+xlab("PC1")+theme(panel.grid.major = element_blank())

png(height = 6, width = 6, units = "in", res = 300, "outputs/mixture_model_msk/hysteresis_pc1_pls.png")
hysteresis.pc1.pls
dev.off()

write.csv(pls.nona, "outputs/mixture_model_msk/pls_pc1_mixture_mode_estimates.csv", row.names = FALSE)

# ------------------------------- run the mixture model for fia PC1 ------------------
library(nimble)
fia <- read.csv("outputs/density_full_FIA_PLS_unc.csv")


fia.na <- is.na(fia$PC1fia)
fia.nona <- fia[!fia.na,]
fia.nona <- fia.nona[!is.na(fia.nona$FIAdensity),]

dmixgamma <- nimbleFunction(
  run = function(x = double(), p = double(0), alpha1 = double(0), beta1 = double(0), 
                 alpha2 =  double(0), beta2 = double(0), log = logical(0, default = 0)) {
    returnType(double())
    tmp = rep(0, 2)
    tmp[1] = log(p) + dgamma(x, alpha1, beta1, log = TRUE) 
    tmp[2] = log(1.0 - p) + dgamma(x, alpha2, beta2, log = TRUE) 
    a = max(tmp[1:2])
    out = a + log(sum(exp(tmp[1:2] - a)))
    if (log) {
      return(out)
    } else {
      return(exp(out))
    }
  }
)

rmixgamma <- nimbleFunction(
  run = function(n = double(), p = double(0), alpha1 = double(0), beta1 = double(0), 
                 alpha2 =  double(0), beta2 = double(0)) {
    returnType(double())
    out = rep(0, 2)
    idx = rbinom(1, 1, prob = 1.0 - p)
    out[1] = rgamma(1, alpha1, beta1)
    out[2] = rgamma(1, alpha2, beta2)
    return(out[idx+1])
  })

registerDistributions(list(
  dmixgamma = list(
    BUGSdist = "dmixgamma(p, alpha1, beta1, alpha2, beta2)",
    discrete = FALSE,
    range = c(0, Inf),
    types = c('value = double()', 'p = double(0)', 'alpha1 = double(0)', 'beta1 = double(0)', 'alpha2 = double(0)', 'beta2 = double(0)')
  )))

mixturemod <- nimbleCode({
  
  for (i in 1:n){
    ## no covariate model 
    # y[i] ~ dmixgamma(p, alpha1, beta1, alpha2, beta2)
    
    ## covariate model
    y[i] ~ dmixgamma(p[i], alpha1, beta1, alpha2, beta2)
    logit(p[i]) <- sum(X[i, 1:d] %*% beta[1:d])
    
  }
  
  ## no covaraiate model
  # p ~ dunif(0, 1)
  
  for (j in 1:d) {
    beta[j] ~ dnorm(mean=0, sd = 1)
  }
  alpha1 ~ dgamma(1, 1)
  alpha2 ~ dgamma(1, 1)
  
  beta1 ~ dgamma(1, 1)
  beta2 ~ dgamma(1, 1)
}) 

## keep the missing values
options(na.action='na.pass')
data <- list(
  y = fia.nona$mean_dens_fia,
  X = model.matrix(~ PC1fia, data = fia.nona)
)
options(na.action='na.omit')

constants <- list(
  n = nrow(fia.nona),
  d = ncol(data$X)
)

#inits <- list(
#z = sample(c(0, 1), nrow(fia.nona), replace = TRUE)
#)

# dimensions <- list(
#   z = nrow(fia)
# )


model <- nimbleModel(mixturemod, constants = constants, data = data) #, inits = inits)#, dimensions = dimensions )

n_thin <- 5
n_mcmc <- 5000
n_burn <- 5000

spec <- configureMCMC(
  model, 
  thin = n_thin
)

spec$addMonitors('p')

Rmcmc <- buildMCMC(spec)

Cmodel <- compileNimble(model)
Cmcmc <- compileNimble(Rmcmc, project = model)
Cmcmc$run(n_mcmc + n_burn)

samples.fit <- as.matrix(Cmcmc$mvSamples)[(n_burn/n_thin+1):((n_mcmc+n_burn)/n_thin), ]

#p_post <- samples.fit[, "p"]
p_post <- matrix(0, nrow(fia.nona), nrow(samples.fit))
for(i in 1:nrow(fia.nona)){
  p_post[ i,]  <-    samples.fit[, paste("p[", i, "]", sep = "")]
  
}

plot(fia.nona$PC1fia, apply(p_post, 1, mean))

# plot(p_post)
alpha1_post <- samples.fit[, "alpha1"]
alpha2_post <- samples.fit[, "alpha2"]
beta_int <- samples.fit[,"beta[1]"]
beta_env <- samples.fit[,"beta[2]"]
beta1_post <- samples.fit[, "beta1"]
beta2_post <- samples.fit[, "beta2"]

plot(beta_dens, type = "l")
plot(beta_env, type = "l")
plot(alpha1_post, type = "l")
plot(alpha2_post, type = "l")
plot(beta1_post, type = "l")
plot(beta2_post, type = "l")


x <- seq(0, 600, length.out = 10000)
hist(fia$mean_dens_fia, freq = FALSE, breaks = 200)
curve(mean(p_post) * dgamma(x, mean(alpha1_post), mean(beta1_post)), from = 0, to = 600, add = TRUE)
curve(mean(1 - p_post) * dgamma(x, mean(alpha2_post), mean(beta2_post)), from = 0, to = 600, add = TRUE)

z <- matrix(0, nrow(samples.fit), nrow(fia.nona))

for(i in 1:nrow(samples.fit)){
  if (i %% 250 == 0) {
    message("On iteration ", i, " out of ", nrow(samples.fit))
  }
  # for(j in 1:2){
  # for(j in 1:nrow(fia)){
  #   A <- log(p_post[i]) + dgamma(fia$mean_dens[j], alpha1_post[i], beta1_post[i], log = TRUE)
  #   B <- log(1.0 - p_post[i]) * dgamma(fia$mean_dens[j], alpha2_post[i], beta2_post[i], log = TRUE)
  #   ptilde <- 1 / (1 + exp(B - A))
  #   z[i, j] <- rbinom(1, 1, ptilde)
  # }
  A <- log(p_post[, i]) + dgamma(fia.nona$mean_dens, alpha1_post[i], beta1_post[i], log = TRUE)
  B <- log(1.0 - p_post[, i]) + dgamma(fia.nona$mean_dens, alpha2_post[i], beta2_post[i], log = TRUE)
  ptilde <- 1 / (1 + exp(B - A))
  z[i, ] <- rbinom(nrow(fia.nona), 1, ptilde) 
  # }
}


str(apply(z, 2, mean))# probablity of being in large cluster


fia.nona$prob <- apply(z, 2, mean)
plot(fia.nona$PC1fia, fia.nona$prob, col = adjustcolor("black", alpha.f = 0.1))

ggplot(fia.nona, aes(PC1fia, prob, color = mean_dens_fia))+geom_point(size = 0.5)+ylim(1,0)+ylab("Probability Forest mode (0 == forest)")


fia.nona$zval <- ifelse(fia.nona$prob > 0.5, 0, 1)
x = seq(0, 800, length.out = 1000)



datcurve <- data.frame(x = x, 
                       y = c(dgamma(x, mean(alpha1_post), mean(beta1_post)), dgamma(x, mean(alpha2_post), mean(beta2_post))),
                       zval = c(rep(0, 1000), rep(1, 1000))
)


png("outputs/mixture_model_msk/basic_mixture_model_msk_histogram_fia_pc1.png")
ggplot(data = fia.nona, aes(mean_dens_fia, fill = zval > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))+geom_line(data = datcurve, aes(x,y, color = as.factor(zval)))+theme_bw()+
  scale_color_manual(values = c("red", "blue"))+theme(legend.title = element_blank())+xlab("Mean Tree Density (stems/ha)")+ylab("density")
dev.off()


# for each bin of climate bins, find the ratio of low to high probability:

ggplot(data = fia.nona, aes(mean_dens_fia, fill = prob > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))#+geom_line(data = datcurve, aes(x,y, color = as.factor(zval)))+theme_bw()+
#scale_color_manual(values = c("red", "blue"))+theme(legend.title = element_blank())+xlab("Mean Tree Density (stems/ha)")+ylab("density")
fia.nona$pc1_bins_fia <- cut(fia.nona$PC1fia, breaks=seq(-5.5, 4.5, by = 0.25))
ggplot(data = fia.nona, aes(mean_dens_fia, fill = prob > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))+facet_wrap(~pc1_bins_fia)

ordered.cuts <- data.frame(pc1_bins_fia = levels(cut(fia.nona[order(fia.nona$PC1),]$PC1, breaks=seq(-5.5, 4.5, by = 0.25))),
                           mids = seq(-5.375, 4.5, by = 0.25))

fia.nona <- merge(fia.nona, ordered.cuts, by = "pc1_bins_fia")
fia.nona$mode <- ifelse(fia.nona$prob > 0.5, "Forest", "Low Density Forest")

density.plots.fia.pc1 <- ggplot(data = fia.nona, aes(x = mean_dens_fia, y = as.factor(pc1_bins_fia), fill = mode), alpha = 0.5)+geom_density_ridges()+xlim(0,550)+ylab("PC1 value")+xlab("Tree Density (stems/ha)")+coord_flip()+theme_bw(base_size = 15)+
  theme(panel.grid.major = element_blank())+scale_fill_manual(values = c('#005a32', '#8c510a'))

png(height = 6, width = 12, units = "in", res = 300, "outputs/mixture_model_msk/PC1_mixture_mode_densityplot_by_pc1_FIA.png")
density.plots.fia.pc1
dev.off()

fia.nona %>% group_by(pc1_bins_fia, mode) %>% dplyr::summarise(mean = mean(mean_dens_fia), 
                                                        low = min(mean_dens_fia), 
                                                        high = max(mean_dens_fia))
# for fia, lets plot points of the mean tree density in each mode & plot the point with equal probability of low and high modes:
mid.summary.pc1 <- fia.nona %>% group_by(mode, pc1_bins_fia, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                             ci.low = quantile(mean_dens_fia,0.025),
                                                                             ci.high = quantile(mean_dens_fia, 0.975))

mid.summary.lowprob.pc1 <- fia.nona %>% group_by(prob > 0.49 & prob < 0.51 , pc1_bins_fia, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                           ci.low = quantile(mean_dens_fia,0.025),
                                                                                                           ci.high = quantile(mean_dens_fia, 0.975),
                                                                                                           ncell = length(mean_dens_fia))
mid.summary.lowprob.pc1<- mid.summary.lowprob.pc1[mid.summary.lowprob.pc1$ncell > 1,]

hysteresis.pc1.fia <- ggplot(mid.summary.pc1, aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.pc1, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  theme_bw()+scale_fill_manual(values = c('#005a32', '#8c510a'))+scale_color_manual(values = c('#005a32', '#8c510a'))+geom_smooth(data = mid.summary.lowprob.pc1[mid.summary.lowprob.pc1$`prob > 0.49 & prob < 0.51` %in% T,], aes(mids, mean), color = "black", linetype = "dashed",se = FALSE)+
  ylab("Mean Tree Density (stems/ha)")+xlab("PC1")+theme(panel.grid.major = element_blank())+ylim(0, 600)

png(height = 6, width = 6, units = "in", res = 300, "outputs/mixture_model_msk/hysteresis_pc1_fia.png")
hysteresis.pc1.fia
dev.off()

write.csv(fia.nona, "outputs/mixture_model_msk/fia_pc1_mixture_mode_estimates.csv", row.names = FALSE)


#------------------------ mixture model for PLS P-PET --------------------------------

pls <- read.csv("outputs/density_full_unc.csv")


pls.na <- is.na(pls$GS_ppet)
pls.nona <- pls[!pls.na,]
pls.nona <- pls.nona[!is.na(pls.nona$FIAdensity),]

dmixgamma <- nimbleFunction(
  run = function(x = double(), p = double(0), alpha1 = double(0), beta1 = double(0), 
                 alpha2 =  double(0), beta2 = double(0), log = logical(0, default = 0)) {
    returnType(double())
    tmp = rep(0, 2)
    tmp[1] = log(p) + dgamma(x, alpha1, beta1, log = TRUE) 
    tmp[2] = log(1.0 - p) + dgamma(x, alpha2, beta2, log = TRUE) 
    a = max(tmp[1:2])
    out = a + log(sum(exp(tmp[1:2] - a)))
    if (log) {
      return(out)
    } else {
      return(exp(out))
    }
  }
)

rmixgamma <- nimbleFunction(
  run = function(n = double(), p = double(0), alpha1 = double(0), beta1 = double(0), 
                 alpha2 =  double(0), beta2 = double(0)) {
    returnType(double())
    out = rep(0, 2)
    idx = rbinom(1, 1, prob = 1.0 - p)
    out[1] = rgamma(1, alpha1, beta1)
    out[2] = rgamma(1, alpha2, beta2)
    return(out[idx+1])
  })

registerDistributions(list(
  dmixgamma = list(
    BUGSdist = "dmixgamma(p, alpha1, beta1, alpha2, beta2)",
    discrete = FALSE,
    range = c(0, Inf),
    types = c('value = double()', 'p = double(0)', 'alpha1 = double(0)', 'beta1 = double(0)', 'alpha2 = double(0)', 'beta2 = double(0)')
  )))

mixturemod <- nimbleCode({
  
  for (i in 1:n){
    ## no covariate model 
    # y[i] ~ dmixgamma(p, alpha1, beta1, alpha2, beta2)
    
    ## covariate model
    y[i] ~ dmixgamma(p[i], alpha1, beta1, alpha2, beta2)
    logit(p[i]) <- sum(X[i, 1:d] %*% beta[1:d])
    
  }
  
  ## no covaraiate model
  # p ~ dunif(0, 1)
  
  for (j in 1:d) {
    beta[j] ~ dnorm(mean=0, sd = 1)
  }
  alpha1 ~ dgamma(1, 1)
  alpha2 ~ dgamma(1, 1)
  
  beta1 ~ dgamma(1, 1)
  beta2 ~ dgamma(1, 1)
}) 

## keep the missing values
options(na.action='na.pass')
data <- list(
  y = pls.nona$mean_dens,
  X = model.matrix(~ GS_ppet, data = pls.nona)
)
options(na.action='na.omit')

constants <- list(
  n = nrow(pls.nona),
  d = ncol(data$X)
)

#inits <- list(
#z = sample(c(0, 1), nrow(pls.nona), replace = TRUE)
#)

# dimensions <- list(
#   z = nrow(pls)
# )


model <- nimbleModel(mixturemod, constants = constants, data = data) #, inits = inits)#, dimensions = dimensions )

n_thin <- 5
n_mcmc <- 5000
n_burn <- 5000

spec <- configureMCMC(
  model, 
  thin = n_thin
)

spec$addMonitors('p')

Rmcmc <- buildMCMC(spec)

Cmodel <- compileNimble(model)
Cmcmc <- compileNimble(Rmcmc, project = model)
Cmcmc$run(n_mcmc + n_burn)

samples.fit <- as.matrix(Cmcmc$mvSamples)[(n_burn/n_thin+1):((n_mcmc+n_burn)/n_thin), ]

#p_post <- samples.fit[, "p"]
p_post <- matrix(0, nrow(pls.nona), nrow(samples.fit))
for(i in 1:nrow(pls.nona)){
  p_post[ i,]  <-    samples.fit[, paste("p[", i, "]", sep = "")]
  
}

plot(pls.nona$GS_ppet, apply(p_post, 1, mean))

# plot(p_post)
alpha1_post <- samples.fit[, "alpha1"]
alpha2_post <- samples.fit[, "alpha2"]
beta_int <- samples.fit[,"beta[1]"]
beta_env <- samples.fit[,"beta[2]"]
beta1_post <- samples.fit[, "beta1"]
beta2_post <- samples.fit[, "beta2"]

plot(beta_dens, type = "l")
plot(beta_env, type = "l")
plot(alpha1_post, type = "l")
plot(alpha2_post, type = "l")
plot(beta1_post, type = "l")
plot(beta2_post, type = "l")


x <- seq(0, 600, length.out = 10000)
hist(pls$mean_dens, freq = FALSE, breaks = 200)
curve(mean(p_post) * dgamma(x, mean(alpha1_post), mean(beta1_post)), from = 0, to = 600, add = TRUE)
curve(mean(1 - p_post) * dgamma(x, mean(alpha2_post), mean(beta2_post)), from = 0, to = 600, add = TRUE)

z <- matrix(0, nrow(samples.fit), nrow(pls.nona))

for(i in 1:nrow(samples.fit)){
  if (i %% 250 == 0) {
    message("On iteration ", i, " out of ", nrow(samples.fit))
  }
  # for(j in 1:2){
  # for(j in 1:nrow(pls)){
  #   A <- log(p_post[i]) + dgamma(pls$mean_dens[j], alpha1_post[i], beta1_post[i], log = TRUE)
  #   B <- log(1.0 - p_post[i]) * dgamma(pls$mean_dens[j], alpha2_post[i], beta2_post[i], log = TRUE)
  #   ptilde <- 1 / (1 + exp(B - A))
  #   z[i, j] <- rbinom(1, 1, ptilde)
  # }
  A <- log(p_post[, i]) + dgamma(pls.nona$mean_dens, alpha1_post[i], beta1_post[i], log = TRUE)
  B <- log(1.0 - p_post[, i]) + dgamma(pls.nona$mean_dens, alpha2_post[i], beta2_post[i], log = TRUE)
  ptilde <- 1 / (1 + exp(B - A))
  z[i, ] <- rbinom(nrow(pls.nona), 1, ptilde) 
  # }
}


str(apply(z, 2, mean))# probablity of being in large cluster


pls.nona$prob_ppet <- apply(z, 2, mean)
plot(pls.nona$GS_ppet, pls.nona$prob_ppet, col = adjustcolor("black", alpha.f = 0.1))

ggplot(pls.nona, aes(GS_ppet, prob_ppet, color = mean_dens))+geom_point(size = 0.5)+ylim(1,0)+ylab("Probability Forest mode (0 == forest)")

ggplot(pls.nona, aes(GS_ppet, mean_dens, color = prob_ppet))+geom_point()
ggplot(data = pls.nona, aes(mean_dens, fill = prob_ppet > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))#+geom_line(data = datcurve, aes(x,y, color = as.factor(zval)))+theme_bw()+
#scale_color_manual(values = c("red", "blue"))+theme(legend.title = element_blank())+xlab("Mean Tree Density (stems/ha)")+ylab("density")



# for each bin of climate bins, find the ratio of low to high probability:

ggplot(data = pls.nona, aes(mean_dens, fill = prob_ppet > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))#+geom_line(data = datcurve, aes(x,y, color = as.factor(zval)))+theme_bw()+
#scale_color_manual(values = c("red", "blue"))+theme(legend.title = element_blank())+xlab("Mean Tree Density (stems/ha)")+ylab("density")


ordered.cuts <- data.frame(ppet_bins = unique(cut(pls.nona[order(pls.nona$GS_ppet),]$GS_ppet, breaks=seq(-170, 205, by = 15))),
                           mids = seq(-167.5, 205, by = 15))
pls.nona$GS_ppet_bins <-cut(pls.nona$GS_ppet, breaks=seq(-170, 205, by = 15))
ggplot(data = pls.nona, aes(mean_dens, fill = prob_ppet > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))+facet_wrap(~GS_ppet_bins)

ordered.cuts <- data.frame(GS_ppet_bins = levels(cut(pls.nona[order(pls.nona$GS_ppet),]$GS_ppet, breaks=seq(-170, 205, by = 15))),
                           mids_ppet = seq(-167.5, 205, by = 15))

pls.nona <- merge(pls.nona, ordered.cuts, by = "GS_ppet_bins")
pls.nona$mode <- ifelse(pls.nona$prob_ppet < 0.5, "Forest", "Savanna")

density.plots.pls.GS_ppet <- ggplot(data = pls.nona, aes(x = mean_dens, y = as.factor(GS_ppet_bins), fill = mode), alpha = 0.5)+geom_density_ridges()+xlim(0,550)+ylab("GS_ppet value")+xlab("Tree Density (stems/ha)")+coord_flip()+theme_bw(base_size = 15)+
  theme(panel.grid.major = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_manual(values = c('#005a32', '#8c510a'))

png(height = 6, width = 12, units = "in", res = 300, "outputs/mixture_model_msk/GS_ppet_mixture_mode_densityplot_by_GS_ppet.png")
density.plots.pls.GS_ppet
dev.off()


# for pls, lets plot points of the mean tree density in each mode & plot the point with equal probability of low and high modes:
mid.summary.ppet <- pls.nona %>% group_by(mode, GS_ppet_bins, mids_ppet) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                    ci.low = quantile(mean_dens,0.025),
                                                                                    ci.high = quantile(mean_dens, 0.975))

mid.summary.lowprob <- pls.nona %>% group_by(prob_ppet >= 0.499 & prob_ppet <= 0.509 , GS_ppet_bins, mids_ppet) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                       ci.low = quantile(mean_dens,0.025),
                                                                                       ci.high = quantile(mean_dens, 0.975),
                                                                                       ncell = length(mean_dens))


hysteresis.ppet.pls <- ggplot(mid.summary.ppet, aes(mids_ppet, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.ppet, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  theme_bw()+scale_fill_manual(values = c('#005a32', '#8c510a'))+scale_color_manual(values = c('#005a32', '#8c510a'))+geom_smooth(data = mid.summary.lowprob[mid.summary.lowprob$`prob_ppet >= 0.499 & prob_ppet <= 0.509` %in% T & mid.summary.lowprob$ci.high < 200,], aes(mids_ppet, mean), color = "black", linetype = "dashed",se = FALSE)+
  ylab("Mean Tree Density (stems/ha)")+xlab("growing season P-PET")+theme(panel.grid.major = element_blank())

png(height = 6, width = 6, units = "in", res = 300, "outputs/mixture_model_msk/hysteresis_ppet_pls.png")
hysteresis.ppet.pls
dev.off()

write.csv(pls.nona, "outputs/mixture_model_msk/pls_ppet_mixture_mode_estimates.csv", row.names = FALSE)
#pls.nona <- read.csv("outputs/mixture_model_msk/pls_ppet_mixture_mode_estimates.csv")
# ---------------------------- mixture model for FIA P-PET-------------------------------


fia <- read.csv("outputs/density_full_FIA_PLS_unc.csv")


fia.nona <- fia[!is.na(fia$GS_ppet_mod),]
fia.nona <- fia.nona[!is.na(fia.nona$FIAdensity),]

dmixgamma <- nimbleFunction(
  run = function(x = double(), p = double(0), alpha1 = double(0), beta1 = double(0), 
                 alpha2 =  double(0), beta2 = double(0), log = logical(0, default = 0)) {
    returnType(double())
    tmp = rep(0, 2)
    tmp[1] = log(p) + dgamma(x, alpha1, beta1, log = TRUE) 
    tmp[2] = log(1.0 - p) + dgamma(x, alpha2, beta2, log = TRUE) 
    a = max(tmp[1:2])
    out = a + log(sum(exp(tmp[1:2] - a)))
    if (log) {
      return(out)
    } else {
      return(exp(out))
    }
  }
)

rmixgamma <- nimbleFunction(
  run = function(n = double(), p = double(0), alpha1 = double(0), beta1 = double(0), 
                 alpha2 =  double(0), beta2 = double(0)) {
    returnType(double())
    out = rep(0, 2)
    idx = rbinom(1, 1, prob = 1.0 - p)
    out[1] = rgamma(1, alpha1, beta1)
    out[2] = rgamma(1, alpha2, beta2)
    return(out[idx+1])
  })

registerDistributions(list(
  dmixgamma = list(
    BUGSdist = "dmixgamma(p, alpha1, beta1, alpha2, beta2)",
    discrete = FALSE,
    range = c(0, Inf),
    types = c('value = double()', 'p = double(0)', 'alpha1 = double(0)', 'beta1 = double(0)', 'alpha2 = double(0)', 'beta2 = double(0)')
  )))

mixturemod <- nimbleCode({
  
  for (i in 1:n){
    ## no covariate model 
    # y[i] ~ dmixgamma(p, alpha1, beta1, alpha2, beta2)
    
    ## covariate model
    y[i] ~ dmixgamma(p[i], alpha1, beta1, alpha2, beta2)
    logit(p[i]) <- sum(X[i, 1:d] %*% beta[1:d])
    
  }
  
  ## no covaraiate model
  # p ~ dunif(0, 1)
  
  for (j in 1:d) {
    beta[j] ~ dnorm(mean=0, sd = 1)
  }
  alpha1 ~ dgamma(1, 1)
  alpha2 ~ dgamma(1, 1)
  
  beta1 ~ dgamma(1, 1)
  beta2 ~ dgamma(1, 1)
}) 

## keep the missing values
options(na.action='na.pass')
data <- list(
  y = fia.nona$mean_dens_fia,
  X = model.matrix(~ GS_ppet_mod, data = fia.nona)
)
options(na.action='na.omit')

constants <- list(
  n = nrow(fia.nona),
  d = ncol(data$X)
)




model <- nimbleModel(mixturemod, constants = constants, data = data) #, inits = inits)#, dimensions = dimensions )

n_thin <- 5
n_mcmc <- 5000
n_burn <- 5000

spec <- configureMCMC(
  model, 
  thin = n_thin
)

spec$addMonitors('p')

Rmcmc <- buildMCMC(spec)

Cmodel <- compileNimble(model)
Cmcmc <- compileNimble(Rmcmc, project = model)
Cmcmc$run(n_mcmc + n_burn)

samples.fit <- as.matrix(Cmcmc$mvSamples)[(n_burn/n_thin+1):((n_mcmc+n_burn)/n_thin), ]

#p_post <- samples.fit[, "p"]
p_post <- matrix(0, nrow(fia.nona), nrow(samples.fit))
for(i in 1:nrow(fia.nona)){
  p_post[ i,]  <-    samples.fit[, paste("p[", i, "]", sep = "")]
  
}

plot(fia.nona$GS_ppet_mod, apply(p_post, 1, mean))

# plot(p_post)
alpha1_post <- samples.fit[, "alpha1"]
alpha2_post <- samples.fit[, "alpha2"]
beta_int <- samples.fit[,"beta[1]"]
beta_env <- samples.fit[,"beta[2]"]
beta1_post <- samples.fit[, "beta1"]
beta2_post <- samples.fit[, "beta2"]

plot(beta_dens, type = "l")
plot(beta_env, type = "l")
plot(alpha1_post, type = "l")
plot(alpha2_post, type = "l")
plot(beta1_post, type = "l")
plot(beta2_post, type = "l")


x <- seq(0, 600, length.out = 10000)
hist(fia$mean_dens_fia, freq = FALSE, breaks = 200)
curve(mean(p_post) * dgamma(x, mean(alpha1_post), mean(beta1_post)), from = 0, to = 600, add = TRUE)
curve(mean(1 - p_post) * dgamma(x, mean(alpha2_post), mean(beta2_post)), from = 0, to = 600, add = TRUE)

z <- matrix(0, nrow(samples.fit), nrow(fia.nona))

for(i in 1:nrow(samples.fit)){
  if (i %% 250 == 0) {
    message("On iteration ", i, " out of ", nrow(samples.fit))
  }
  # for(j in 1:2){
  # for(j in 1:nrow(fia)){
  #   A <- log(p_post[i]) + dgamma(fia$mean_dens_fia[j], alpha1_post[i], beta1_post[i], log = TRUE)
  #   B <- log(1.0 - p_post[i]) * dgamma(fia$mean_dens_fia[j], alpha2_post[i], beta2_post[i], log = TRUE)
  #   ptilde <- 1 / (1 + exp(B - A))
  #   z[i, j] <- rbinom(1, 1, ptilde)
  # }
  A <- log(p_post[, i]) + dgamma(fia.nona$mean_dens_fia, alpha1_post[i], beta1_post[i], log = TRUE)
  B <- log(1.0 - p_post[, i]) + dgamma(fia.nona$mean_dens_fia, alpha2_post[i], beta2_post[i], log = TRUE)
  ptilde <- 1 / (1 + exp(B - A))
  z[i, ] <- rbinom(nrow(fia.nona), 1, ptilde) 
  # }
}


str(apply(z, 2, mean))# probablity of being in large cluster


fia.nona$prob_ppet <- apply(z, 2, mean)
plot(fia.nona$GS_ppet_mod, fia.nona$prob_ppet, col = adjustcolor("black", alpha.f = 0.1))

ggplot(fia.nona, aes(GS_ppet_mod, prob_ppet, color = mean_dens_fia))+geom_point(size = 0.5)+ylim(1,0)+ylab("Probability Forest mode (0 == forest)")

ggplot(fia.nona, aes(GS_ppet_mod, mean_dens_fia, color = prob_ppet))+geom_point()
ggplot(data = fia.nona, aes(mean_dens_fia, fill = prob_ppet > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))#+geom_line(data = datcurve, aes(x,y, color = as.factor(zval)))+theme_bw()+
#scale_color_manual(values = c("red", "blue"))+theme(legend.title = element_blank())+xlab("Mean Tree Density (stems/ha)")+ylab("density")



# for each bin of climate bins, find the ratio of low to high probability:

ggplot(data = fia.nona, aes(mean_dens_fia, fill = prob_ppet > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))#+geom_line(data = datcurve, aes(x,y, color = as.factor(zval)))+theme_bw()+
#scale_color_manual(values = c("red", "blue"))+theme(legend.title = element_blank())+xlab("Mean Tree Density (stems/ha)")+ylab("density")


#ordered.cuts <- data.frame(ppet_bins = unique(cut(fia.nona[order(fia.nona$GS_ppet_mod),]$GS_ppet_mod, breaks=seq(-170, 205, by = 15))),
 #                          mids = seq(-167.5, 205, by = 15))
fia.nona$GS_ppet_mod_bins <-cut(fia.nona$GS_ppet_mod, breaks=seq(-170, 205, by = 15))
ggplot(data = fia.nona, aes(mean_dens_fia, fill = prob_ppet > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))+facet_wrap(~GS_ppet_mod_bins)

ordered.cuts <- data.frame(GS_ppet_mod_bins = levels(cut(fia.nona[order(fia.nona$GS_ppet_mod),]$GS_ppet_mod, breaks=seq(-170, 205, by = 15))),
                           mids = seq(-167.5, 205, by = 15))

fia.nona <- merge(fia.nona, ordered.cuts, by = "GS_ppet_mod_bins")
fia.nona$mode <- ifelse(fia.nona$prob_ppet < 0.5,  "Low Density Forest", "Forest")

density.plots.fia.GS_ppet_mod <- ggplot(data = fia.nona, aes(x = mean_dens_fia, y = as.factor(GS_ppet_mod_bins), fill = mode), alpha = 0.5)+geom_density_ridges()+xlim(0,550)+ylab("GS_ppet_mod value")+xlab("Tree Density (stems/ha)")+coord_flip()+theme_bw(base_size = 15)+
  theme(panel.grid.major = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_manual(values = c('#005a32', '#8c510a'))

png(height = 6, width = 12, units = "in", res = 300, "outputs/mixture_model_msk/GS_ppet_mod_mixture_mode_densityplot_by_GS_ppet_mod.png")
density.plots.fia.GS_ppet_mod
dev.off()

# for pls, lets plot points of the mean tree density in each mode & plot the point with equal probability of low and high modes:
mid.summary.ppet <- fia.nona %>% group_by(mode, GS_ppet_mod_bins, mids) %>%dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                       ci.low = quantile(mean_dens_fia,0.025),
                                                                                       ci.high = quantile(mean_dens_fia, 0.975))

mid.summary.lowprob <- fia.nona %>% group_by(prob_ppet >= 0.4999 & prob_ppet <= 0.5099 , GS_ppet_mod_bins, mids) %>%dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                                                ci.low = quantile(mean_dens_fia,0.025),
                                                                                                                                ci.high = quantile(mean_dens_fia, 0.975))


hysteresis.ppet.fia <- ggplot(mid.summary.ppet, aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.ppet, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  theme_bw()+scale_fill_manual(values = c('#005a32', '#8c510a'))+scale_color_manual(values = c('#005a32', '#8c510a'))+geom_smooth(data = mid.summary.lowprob[mid.summary.lowprob$`prob_ppet >= 0.4999 & prob_ppet <= 0.5099` %in% T,], aes(mids, mean), color = "black", linetype = "dashed",se = FALSE)+
  ylab("Mean Tree Density (stems/ha)")+xlab("growing season P-PET")+theme(panel.grid.major = element_blank())+ylim(0, 600)

png(height = 6, width = 6, units = "in", res = 300, "outputs/mixture_model_msk/hysteresis_ppet_fia.png")
hysteresis.ppet.fia
dev.off()

write.csv(fia.nona, "outputs/mixture_model_msk/fia_ppet_mixture_mode_estimates.csv", row.names = FALSE)
#fia.nona <- read.csv("outputs/mixture_model_msk/fia_ppet_mixture_mode_estimates.csv")
# ---------------------------- mixture model for PLS soil moisture:-------------------------------

pls <- read.csv("outputs/density_full_unc.csv")



pls.na <- is.na(pls$mean_GS_soil)
pls.nona <- pls[!pls.na,]
pls.nona <- pls.nona[!is.na(pls.nona$FIAdensity),]


dmixgamma <- nimbleFunction(
  run = function(x = double(), p = double(0), alpha1 = double(0), beta1 = double(0), 
                 alpha2 =  double(0), beta2 = double(0), log = logical(0, default = 0)) {
    returnType(double())
    tmp = rep(0, 2)
    tmp[1] = log(p) + dgamma(x, alpha1, beta1, log = TRUE) 
    tmp[2] = log(1.0 - p) + dgamma(x, alpha2, beta2, log = TRUE) 
    a = max(tmp[1:2])
    out = a + log(sum(exp(tmp[1:2] - a)))
    if (log) {
      return(out)
    } else {
      return(exp(out))
    }
  }
)

rmixgamma <- nimbleFunction(
  run = function(n = double(), p = double(0), alpha1 = double(0), beta1 = double(0), 
                 alpha2 =  double(0), beta2 = double(0)) {
    returnType(double())
    out = rep(0, 2)
    idx = rbinom(1, 1, prob = 1.0 - p)
    out[1] = rgamma(1, alpha1, beta1)
    out[2] = rgamma(1, alpha2, beta2)
    return(out[idx+1])
  })

registerDistributions(list(
  dmixgamma = list(
    BUGSdist = "dmixgamma(p, alpha1, beta1, alpha2, beta2)",
    discrete = FALSE,
    range = c(0, Inf),
    types = c('value = double()', 'p = double(0)', 'alpha1 = double(0)', 'beta1 = double(0)', 'alpha2 = double(0)', 'beta2 = double(0)')
  )))

mixturemod <- nimbleCode({
  
  for (i in 1:n){
    ## no covariate model 
    # y[i] ~ dmixgamma(p, alpha1, beta1, alpha2, beta2)
    
    ## covariate model
    y[i] ~ dmixgamma(p[i], alpha1, beta1, alpha2, beta2)
    logit(p[i]) <- sum(X[i, 1:d] %*% beta[1:d])
    
  }
  
  ## no covaraiate model
  # p ~ dunif(0, 1)
  
  for (j in 1:d) {
    beta[j] ~ dnorm(mean=0, sd = 1)
  }
  alpha1 ~ dgamma(1, 1)
  alpha2 ~ dgamma(1, 1)
  
  beta1 ~ dgamma(1, 1)
  beta2 ~ dgamma(1, 1)
}) 

## keep the missing values
options(na.action='na.pass')
data <- list(
  y = pls.nona$mean_dens,
  X = model.matrix(~ mean_GS_soil, data = pls.nona)
)
options(na.action='na.omit')

constants <- list(
  n = nrow(pls.nona),
  d = ncol(data$X)
)

#inits <- list(
#z = sample(c(0, 1), nrow(pls.nona), replace = TRUE)
#)

# dimensions <- list(
#   z = nrow(pls)
# )


model <- nimbleModel(mixturemod, constants = constants, data = data) #, inits = inits)#, dimensions = dimensions )

n_thin <- 5
n_mcmc <- 5000
n_burn <- 5000

spec <- configureMCMC(
  model, 
  thin = n_thin
)

spec$addMonitors('p')

Rmcmc <- buildMCMC(spec)

Cmodel <- compileNimble(model)
Cmcmc <- compileNimble(Rmcmc, project = model)
Cmcmc$run(n_mcmc + n_burn)

samples.fit <- as.matrix(Cmcmc$mvSamples)[(n_burn/n_thin+1):((n_mcmc+n_burn)/n_thin), ]

#p_post <- samples.fit[, "p"]
p_post <- matrix(0, nrow(pls.nona), nrow(samples.fit))
for(i in 1:nrow(pls.nona)){
  p_post[ i,]  <-    samples.fit[, paste("p[", i, "]", sep = "")]
  
}

plot(pls.nona$mean_GS_soil, apply(p_post, 1, mean))

# plot(p_post)
alpha1_post <- samples.fit[, "alpha1"]
alpha2_post <- samples.fit[, "alpha2"]
beta_int <- samples.fit[,"beta[1]"]
beta_env <- samples.fit[,"beta[2]"]
beta1_post <- samples.fit[, "beta1"]
beta2_post <- samples.fit[, "beta2"]

plot(beta_dens, type = "l")
plot(beta_env, type = "l")
plot(alpha1_post, type = "l")
plot(alpha2_post, type = "l")
plot(beta1_post, type = "l")
plot(beta2_post, type = "l")


x <- seq(0, 600, length.out = 10000)
hist(pls$mean_dens, freq = FALSE, breaks = 200)
curve(mean(p_post) * dgamma(x, mean(alpha1_post), mean(beta1_post)), from = 0, to = 600, add = TRUE)
curve(mean(1 - p_post) * dgamma(x, mean(alpha2_post), mean(beta2_post)), from = 0, to = 600, add = TRUE)

z <- matrix(0, nrow(samples.fit), nrow(pls.nona))

for(i in 1:nrow(samples.fit)){
  if (i %% 250 == 0) {
    message("On iteration ", i, " out of ", nrow(samples.fit))
  }
  # for(j in 1:2){
  # for(j in 1:nrow(pls)){
  #   A <- log(p_post[i]) + dgamma(pls$mean_dens[j], alpha1_post[i], beta1_post[i], log = TRUE)
  #   B <- log(1.0 - p_post[i]) * dgamma(pls$mean_dens[j], alpha2_post[i], beta2_post[i], log = TRUE)
  #   ptilde <- 1 / (1 + exp(B - A))
  #   z[i, j] <- rbinom(1, 1, ptilde)
  # }
  A <- log(p_post[, i]) + dgamma(pls.nona$mean_dens, alpha1_post[i], beta1_post[i], log = TRUE)
  B <- log(1.0 - p_post[, i]) + dgamma(pls.nona$mean_dens, alpha2_post[i], beta2_post[i], log = TRUE)
  ptilde <- 1 / (1 + exp(B - A))
  z[i, ] <- rbinom(nrow(pls.nona), 1, ptilde) 
  # }
}


str(apply(z, 2, mean))# probablity of being in large cluster


pls.nona$prob_soil <- apply(z, 2, mean)
plot(pls.nona$mean_GS_soil, pls.nona$prob_soil, col = adjustcolor("black", alpha.f = 0.1))

ggplot(pls.nona, aes(mean_GS_soil, prob_soil, color = mean_dens))+geom_point(size = 0.5)+ylim(1,0)+ylab("Probability Forest mode (0 == forest)")

ggplot(pls.nona, aes(mean_GS_soil, mean_dens, color = prob_soil))+geom_point()
ggplot(data = pls.nona, aes(mean_dens, fill = prob_soil > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))#+geom_line(data = datcurve, aes(x,y, color = as.factor(zval)))+theme_bw()+
#scale_color_manual(values = c("red", "blue"))+theme(legend.title = element_blank())+xlab("Mean Tree Density (stems/ha)")+ylab("density")



# for each bin of climate bins, find the ratio of low to high probability:

ggplot(data = pls.nona, aes(mean_dens, fill = prob_soil > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))#+geom_line(data = datcurve, aes(x,y, color = as.factor(zval)))+theme_bw()+
#scale_color_manual(values = c("red", "blue"))+theme(legend.title = element_blank())+xlab("Mean Tree Density (stems/ha)")+ylab("density")



pls.nona$mean_GS_soil_bins <-cut(pls.nona$mean_GS_soil, breaks=seq(0, 1.8, by = 0.05))
ggplot(data = pls.nona, aes(mean_dens, fill = prob_soil > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))+facet_wrap(~mean_GS_soil_bins)

ordered.cuts <- data.frame(mean_GS_soil_bins = levels(cut(pls.nona[order(pls.nona$mean_GS_soil),]$mean_GS_soil, breaks=seq(0, 1.8, by = 0.05))),
                           mids = seq(0.025, 1.8, by = 0.05))

pls.nona <- merge(pls.nona, ordered.cuts, by = "mean_GS_soil_bins")
pls.nona$mode <- ifelse(pls.nona$prob_soil < 0.5, "Savanna", "Forest")

density.plots.pls.mean_GS_soil <- ggplot(data = pls.nona, aes(x = mean_dens, y = as.factor(mean_GS_soil_bins), fill = mode), alpha = 0.5)+geom_density_ridges()+xlim(0,550)+ylab("mean_GS_soil value")+xlab("Tree Density (stems/ha)")+coord_flip()+theme_bw(base_size = 15)+
  theme(panel.grid.major = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_manual(values = c('#005a32', '#8c510a'))

png(height = 6, width = 12, units = "in", res = 300, "outputs/mixture_model_msk/mean_GS_soil_mixture_mode_densityplot_by_mean_GS_soil.png")
density.plots.pls.mean_GS_soil
dev.off()


# for pls, lets plot points of the mean tree density in each mode & plot the point with equal probability of low and high modes:
mid.summary.soil <- pls.nona %>% group_by(mode, mean_GS_soil_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                ci.low = quantile(mean_dens,0.025),
                                                                ci.high = quantile(mean_dens, 0.975))

ggplot(mid.summary.soil, aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.soil, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)


mid.summary.lowprob <- pls.nona %>% group_by(prob_soil >= 0.4999 & prob_soil <= 0.5099 , mean_GS_soil_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                                                                ci.low = quantile(mean_dens,0.025),
                                                                                                                                ci.high = quantile(mean_dens, 0.975))


hysteresis.soil.pls <- ggplot(data = mid.summary.soil, aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.soil, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  theme_bw()+scale_fill_manual(values = c('#005a32', '#8c510a'))+scale_color_manual(values = c('#005a32', '#8c510a'))+geom_smooth(data = mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.4999 & prob_soil <= 0.5099` %in% T,], aes(mids, mean), color = "black", linetype = "dashed",se = FALSE)+
  ylab("Mean Tree Density (stems/ha)")+xlab("growing season soil moisture")+theme(panel.grid.major = element_blank())

png(height = 6, width = 6, units = "in", res = 300, "outputs/mixture_model_msk/hysteresis_soil_pls.png")
hysteresis.soil.pls
dev.off()

write.csv(pls.nona, "outputs/mixture_model_msk/pls_soil_mixture_mode_estimates.csv", row.names = FALSE)


# ---------------------------- mixture model for FIA soil moisture:-------------------------------


fia <- read.csv("outputs/density_full_FIA_PLS_unc.csv")


fia.na <- is.na(fia$mean_GS_soil_m)
fia.nona <- fia[!fia.na,]
fia.nona <- fia.nona[!is.na(fia.nona$FIAdensity),]

dmixgamma <- nimbleFunction(
  run = function(x = double(), p = double(0), alpha1 = double(0), beta1 = double(0), 
                 alpha2 =  double(0), beta2 = double(0), log = logical(0, default = 0)) {
    returnType(double())
    tmp = rep(0, 2)
    tmp[1] = log(p) + dgamma(x, alpha1, beta1, log = TRUE) 
    tmp[2] = log(1.0 - p) + dgamma(x, alpha2, beta2, log = TRUE) 
    a = max(tmp[1:2])
    out = a + log(sum(exp(tmp[1:2] - a)))
    if (log) {
      return(out)
    } else {
      return(exp(out))
    }
  }
)

rmixgamma <- nimbleFunction(
  run = function(n = double(), p = double(0), alpha1 = double(0), beta1 = double(0), 
                 alpha2 =  double(0), beta2 = double(0)) {
    returnType(double())
    out = rep(0, 2)
    idx = rbinom(1, 1, prob = 1.0 - p)
    out[1] = rgamma(1, alpha1, beta1)
    out[2] = rgamma(1, alpha2, beta2)
    return(out[idx+1])
  })

registerDistributions(list(
  dmixgamma = list(
    BUGSdist = "dmixgamma(p, alpha1, beta1, alpha2, beta2)",
    discrete = FALSE,
    range = c(0, Inf),
    types = c('value = double()', 'p = double(0)', 'alpha1 = double(0)', 'beta1 = double(0)', 'alpha2 = double(0)', 'beta2 = double(0)')
  )))

mixturemod <- nimbleCode({
  
  for (i in 1:n){
    ## no covariate model 
    # y[i] ~ dmixgamma(p, alpha1, beta1, alpha2, beta2)
    
    ## covariate model
    y[i] ~ dmixgamma(p[i], alpha1, beta1, alpha2, beta2)
    logit(p[i]) <- sum(X[i, 1:d] %*% beta[1:d])
    
  }
  
  ## no covaraiate model
  # p ~ dunif(0, 1)
  
  for (j in 1:d) {
    beta[j] ~ dnorm(mean=0, sd = 1)
  }
  alpha1 ~ dgamma(1, 1)
  alpha2 ~ dgamma(1, 1)
  
  beta1 ~ dgamma(1, 1)
  beta2 ~ dgamma(1, 1)
}) 

## keep the missing values
options(na.action='na.pass')
data <- list(
  y = fia.nona$mean_dens_fia,
  X = model.matrix(~ mean_GS_soil_m, data = fia.nona)
)
options(na.action='na.omit')

constants <- list(
  n = nrow(fia.nona),
  d = ncol(data$X)
)

#inits <- list(
#z = sample(c(0, 1), nrow(fia.nona), replace = TRUE)
#)

# dimensions <- list(
#   z = nrow(fia)
# )


model <- nimbleModel(mixturemod, constants = constants, data = data) #, inits = inits)#, dimensions = dimensions )

n_thin <- 5
n_mcmc <- 5000
n_burn <- 5000

spec <- configureMCMC(
  model, 
  thin = n_thin
)

spec$addMonitors('p')

Rmcmc <- buildMCMC(spec)

Cmodel <- compileNimble(model)
Cmcmc <- compileNimble(Rmcmc, project = model)
Cmcmc$run(n_mcmc + n_burn)

samples.fit <- as.matrix(Cmcmc$mvSamples)[(n_burn/n_thin+1):((n_mcmc+n_burn)/n_thin), ]

#p_post <- samples.fit[, "p"]
p_post <- matrix(0, nrow(fia.nona), nrow(samples.fit))
for(i in 1:nrow(fia.nona)){
  p_post[ i,]  <-    samples.fit[, paste("p[", i, "]", sep = "")]
  
}

plot(fia.nona$mean_GS_soil_m, apply(p_post, 1, mean))

# plot(p_post)
alpha1_post <- samples.fit[, "alpha1"]
alpha2_post <- samples.fit[, "alpha2"]
beta_int <- samples.fit[,"beta[1]"]
beta_env <- samples.fit[,"beta[2]"]
beta1_post <- samples.fit[, "beta1"]
beta2_post <- samples.fit[, "beta2"]

plot(beta_dens, type = "l")
plot(beta_env, type = "l")
plot(alpha1_post, type = "l")
plot(alpha2_post, type = "l")
plot(beta1_post, type = "l")
plot(beta2_post, type = "l")


x <- seq(0, 600, length.out = 10000)
hist(fia$mean_dens_fia, freq = FALSE, breaks = 200)
curve(mean(p_post) * dgamma(x, mean(alpha1_post), mean(beta1_post)), from = 0, to = 600, add = TRUE)
curve(mean(1 - p_post) * dgamma(x, mean(alpha2_post), mean(beta2_post)), from = 0, to = 600, add = TRUE)

z <- matrix(0, nrow(samples.fit), nrow(fia.nona))

for(i in 1:nrow(samples.fit)){
  if (i %% 250 == 0) {
    message("On iteration ", i, " out of ", nrow(samples.fit))
  }
  # for(j in 1:2){
  # for(j in 1:nrow(fia)){
  #   A <- log(p_post[i]) + dgamma(fia$mean_dens_fia[j], alpha1_post[i], beta1_post[i], log = TRUE)
  #   B <- log(1.0 - p_post[i]) * dgamma(fia$mean_dens_fia[j], alpha2_post[i], beta2_post[i], log = TRUE)
  #   ptilde <- 1 / (1 + exp(B - A))
  #   z[i, j] <- rbinom(1, 1, ptilde)
  # }
  A <- log(p_post[, i]) + dgamma(fia.nona$mean_dens_fia, alpha1_post[i], beta1_post[i], log = TRUE)
  B <- log(1.0 - p_post[, i]) + dgamma(fia.nona$mean_dens_fia, alpha2_post[i], beta2_post[i], log = TRUE)
  ptilde <- 1 / (1 + exp(B - A))
  z[i, ] <- rbinom(nrow(fia.nona), 1, ptilde) 
  # }
}


str(apply(z, 2, mean))# probablity of being in large cluster


fia.nona$prob_soil <- apply(z, 2, mean)
plot(fia.nona$mean_GS_soil_m, fia.nona$prob_soil, col = adjustcolor("black", alpha.f = 0.1))

ggplot(fia.nona, aes(mean_GS_soil_m, prob_soil, color = mean_dens_fia))+geom_point(size = 0.5)+ylim(1,0)+ylab("Probability Forest mode (0 == forest)")

ggplot(fia.nona, aes(mean_GS_soil_m, mean_dens_fia, color = prob_soil))+geom_point()
ggplot(data = fia.nona, aes(mean_dens_fia, fill = prob_soil > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))#+geom_line(data = datcurve, aes(x,y, color = as.factor(zval)))+theme_bw()+
#scale_color_manual(values = c("red", "blue"))+theme(legend.title = element_blank())+xlab("Mean Tree Density (stems/ha)")+ylab("density")



# for each bin of climate bins, find the ratio of low to high probability:

ggplot(data = fia.nona, aes(mean_dens_fia, fill = prob_soil > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))#+geom_line(data = datcurve, aes(x,y, color = as.factor(zval)))+theme_bw()+
#scale_color_manual(values = c("red", "blue"))+theme(legend.title = element_blank())+xlab("Mean Tree Density (stems/ha)")+ylab("density")



fia.nona$mean_GS_soil_m_bins <-cut(fia.nona$mean_GS_soil_m, breaks=seq(0, 1.8, by = 0.05))
ggplot(data = fia.nona, aes(mean_dens_fia, fill = prob_soil > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))+facet_wrap(~mean_GS_soil_m_bins)

ordered.cuts <- data.frame(mean_GS_soil_m_bins = levels(cut(fia.nona[order(fia.nona$mean_GS_soil_m),]$mean_GS_soil_m, breaks=seq(0, 1.8, by = 0.05))),
                           mids = seq(0.025, 1.8, by = 0.05))

fia.nona <- merge(fia.nona, ordered.cuts, by = "mean_GS_soil_m_bins")
fia.nona$mode <- ifelse(fia.nona$prob_soil > 0.5, "Low Density Mode", "High Density Mode")

density.plots.fia.mean_GS_soil_m <- ggplot(data = fia.nona, aes(x = mean_dens_fia, y = as.factor(mean_GS_soil_m_bins), fill = mode), alpha = 0.5)+geom_density_ridges()+xlim(0,550)+ylab("mean_GS_soil_m value")+xlab("Tree Density (stems/ha)")+coord_flip()+theme_bw(base_size = 15)+
  theme(panel.grid.major = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_manual(values = c('#005a32', '#8c510a'))

png(height = 6, width = 12, units = "in", res = 300, "outputs/mixture_model_msk/mean_GS_soil_m_mixture_mode_densityplot_by_mean_GS_soil_m.png")
density.plots.fia.mean_GS_soil_m
dev.off()



# for pls, lets plot points of the mean tree density in each mode & plot the point with equal probability of low and high modes:
mid.summary.soil <- fia.nona %>% group_by(mode, mean_GS_soil_m_bins, mids) %>%dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                       ci.low = quantile(mean_dens_fia,0.025),
                                                                                       ci.high = quantile(mean_dens_fia, 0.975),
                                                                                       ncell = length(mean_dens_fia))

ggplot(mid.summary.soil, aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.soil, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)


mid.summary.lowprob <- fia.nona %>% group_by(prob_soil >= 0.4999 & prob_soil <= 0.5099 , mean_GS_soil_m_bins, mids) %>%dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                                                ci.low = quantile(mean_dens_fia,0.025),
                                                                                                                                ci.high = quantile(mean_dens_fia, 0.975))


hysteresis.soil.fia <- ggplot(mid.summary.soil, aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.soil, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)+
  theme_bw(base_size = 18)+scale_fill_manual(values = c('#005a32', '#8c510a'))+scale_color_manual(values = c('#005a32', '#8c510a'))+geom_smooth(data = mid.summary.lowprob[mid.summary.lowprob$`prob_soil >= 0.4999 & prob_soil <= 0.5099` %in% T,], aes(mids, mean), color = "black", linetype = "dashed",se = FALSE)+
  ylab("Mean Tree Density (stems/ha)")+xlab("growing season soil moisture")+theme(panel.grid.major = element_blank())+ylim(0, 600)

png(height = 6, width = 6, units = "in", res = 300, "outputs/mixture_model_msk/hysteresis_soil_fia.png")
hysteresis.soil.fia
dev.off()

write.csv(fia.nona, "outputs/mixture_model_msk/fia_soil_mixture_mode_estimates.csv", row.names = FALSE)



# ------------- plot all hysetersis figures together -------------
library(cowplot)

mode.legend <- get_legend(hysteresis.soil.fia)

png(height = 12, width = 13, units = "in", res = 300, "outputs/mixture_model_msk/hysteresis_allvars_summary_new.png")
plot_grid(plot_grid(hysteresis.pc1.pls+ylim(0,500)+theme_bw(base_size = 18)+theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()), 
          hysteresis.pc1.fia+ylim(0,500)+theme_bw(base_size = 18)+theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()), 
          hysteresis.ppet.pls+ylim(0,500)+theme_bw(base_size = 18)+theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+xlim(-200, 200), 
          hysteresis.ppet.fia+ylim(0,500)+theme_bw(base_size = 18)+theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+xlim(-200, 200), 
          hysteresis.soil.pls+ylim(0,500)+theme_bw(base_size = 18)+theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+xlim(0,2), 
          hysteresis.soil.fia+ylim(0,500)+theme_bw(base_size = 18)+theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+xlim(0,2),  align = "hv",ncol = 2, labels = "AUTO"),
          mode.legend, ncol = 2, rel_widths = c(1, 0.25))
dev.off()


#----------- plot all the binned histograms together ---------------

mode.legend.fill <- get_legend(density.plots.fia.mean_GS_soil_m)

png(height = 12, width = 24, units = "in", res = 300, "outputs/mixture_model_msk/density_bins_allvars_summary.png")
plot_grid(plot_grid(density.plots.pls.pc1+theme_bw(base_size = 18)+theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))+ylab("PLS PC1"), 
                    density.plots.fia.pc1+theme_bw(base_size = 18)+theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))+ylab("FIA PC1"), 
                    density.plots.pls.GS_ppet+theme_bw(base_size = 18)+theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))+ylab("PLS P-PET"), 
                    density.plots.fia.GS_ppet_mod+theme_bw(base_size = 18)+theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))+ylab("FIA P-PET"), 
                    density.plots.pls.mean_GS_soil+theme_bw(base_size = 18)+theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))+ylab("PLS Soil Moisture"), 
                    density.plots.fia.mean_GS_soil_m+theme_bw(base_size = 18)+theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))+ylab("FIA Soil Moisture"), ncol = 2, align = "hv"),
          mode.legend.fill,ncol = 2, rel_widths = c(1, 0.1))
dev.off()


#------------------------code from simple mixture model:

x <- seq(0, 600, length.out = 10000)
hist(fia$mean_dens_fia, freq = FALSE, breaks = 200)
curve(mean(p_post) * dgamma(x, mean(alpha1_post), mean(beta1_post)), from = 0, to = 600, add = TRUE)
curve(mean(1 - p_post) * dgamma(x, mean(alpha2_post), mean(beta2_post)), from = 0, to = 600, add = TRUE)

z <- matrix(0, nrow(samples.fit), nrow(fia))

for(i in 1:nrow(samples.fit)){
  if (i %% 250 == 0) {
    message("On iteration ", i, " out of ", nrow(samples.fit))
  }
  # for(j in 1:nrow(fia)){
  #   A <- log(p_post[i]) + dgamma(fia$mean_dens[j], alpha1_post[i], beta1_post[i], log = TRUE)
  #   B <- log(1.0 - p_post[i]) * dgamma(fia$mean_dens[j], alpha2_post[i], beta2_post[i], log = TRUE)
  #   ptilde <- 1 / (1 + exp(B - A))
  #   z[i, j] <- rbinom(1, 1, ptilde)
  # }
  A <- log(p_post[i]) + dgamma(fia$mean_dens, alpha1_post[i], beta1_post[i], log = TRUE)
  B <- log(1.0 - p_post[i]) + dgamma(fia$mean_dens, alpha2_post[i], beta2_post[i], log = TRUE)
  ptilde <- 1 / (1 + exp(B - A))
  z[i, ] <- rbinom(nrow(fia), 1, ptilde) 
}
str(apply(z, 2, mean))# probablity of being in large cluster


fia$prob_large <- apply(z, 2, mean)
fia$zval <- ifelse(fia$prob_large > 0.5, 0, 1)
x = seq(0, 800, length.out = 1000)

datcurve <- data.frame(x = x, 
                       y = c(dgamma(x, mean(alpha1_post), mean(beta1_post)), dgamma(x, mean(alpha2_post), mean(beta2_post))),
                       zval = c(rep(0, 1000), rep(1, 1000))
)


png("outputs/mixture_model_msk/basic_mixture_model_msk_histogram.png")
ggplot(data = fia, aes(mean_dens, fill = zval > 0.5), alpha = 0.5)+geom_histogram(position = "dodge", aes(y =..density..))+geom_line(data = datcurve, aes(x,y, color = as.factor(zval)))+theme_bw()+
  scale_color_manual(values = c("red", "blue"))+theme(legend.title = element_blank())+xlab("Mean Tree Density (stems/ha)")+ylab("density")
dev.off()




