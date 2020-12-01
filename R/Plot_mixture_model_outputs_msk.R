## Extract MCMC output
# code for extracting mixture model output to make the hysteresis figure 3 msked hysteresis figures
library(tidyverse)
library(here)
library(splines)

generate_plot <- function(data_of_interest, var_of_interest, mixture_type) {
  
  ##
  ## Load data
  ##
  
  dat <- NULL
  data_variable <- NULL
  if (data_of_interest == "fia") {
    dat <- read.csv(here::here("data", "density_full_FIA_PLS_unc.csv"))
    dat <- dat[!is.na(dat$FIAdensity),]
    data_variable <- "mean_dens_fia"
  } else if (data_of_interest == "pls") {
    dat <- read.csv(here::here("data", "density_full_unc_v1.0.csv"))
    dat <- dat[!is.na(dat$FIAdensity),]
    data_variable <- "mean_dens"
  } else {
    stop("Only datasets available are fia and pls")
  }
  
  dat_na <- dat[!is.na(dat %>% select(var_of_interest)), ]
  
  ## Center and scale the covariate
  
  y <- dat_na %>% select(data_variable) %>% pull()
  X <- dat_na %>% select(var_of_interest) %>% pull()
  mu_X <- mean(X)
  sd_X <- sd(X)
  X_center <- (X - mu_X) / sd_X
  
  ##
  ## Load the fitted data
  ##
  
  if (mixture_type == "mixture") {
    load(here::here("fit", paste0("b-spline-", var_of_interest, "-", 
                                  data_of_interest, "-constrained-fia-only.RData")))
  } else if (mixture_type == "single") {
    load(here::here("fit", paste0("b-spline-", var_of_interest, "-", 
                                  data_of_interest, "-single-fia-only.RData")))    
  } else {
    stop('The only possible values for mixture_type are "mixture" and "single"')
  }
  
  ##
  ## Extract the MCMC parameters from the nimble output
  ##
  
  data <- list(
    y = y,
    X = bs(X_center, df = 5, intercept = TRUE),
    constraint_data = rep(1, length(y))
  )
  
  # ## extract posterior samples
  # mu1_post <- matrix(0, nrow(dat_na), nrow(samples.fit))
  # mu2_post <- matrix(0, nrow(dat_na), nrow(samples.fit))
  # sigma1_post  <- matrix(0, nrow(dat_na), nrow(samples.fit))
  # sigma2_post  <- matrix(0, nrow(dat_na), nrow(samples.fit))
  # p_post       <- matrix(0, nrow(dat_na), nrow(samples.fit))
  # 
  # alpha_post <- matrix(0, nrow(dat_na), nrow(samples.fit))
  # beta_post  <- matrix(0, nrow(dat_na), nrow(samples.fit))
  # 
  # message("Extracting parameters for each sample")
  # if (mixture_type == "mixture") {
  #   for(i in 1:nrow(dat_na)){
  #     if (i %% 500 == 0) {
  #       message("On iteration ", i, " out of ", nrow(dat_na))
  #     }
  #     mu1_post[i, ]    <- samples.fit[, paste("mu1[", i, "]", sep = "")]
  #     mu2_post[i, ]    <- samples.fit[, paste("mu2[", i, "]", sep = "")]
  #     sigma1_post[i, ] <- samples.fit[, paste("sigma1[", i, "]", sep = "")]
  #     sigma2_post[i, ] <- samples.fit[, paste("sigma2[", i, "]", sep = "")]
  #     p_post[i, ]      <- samples.fit[, paste("p[", i, "]", sep = "")]
  #   }
  # }
  
  # if (mixture_type == "single") {
  #   for(i in 1:nrow(dat_na)){
  #     if (i %% 500 == 0) {
  #       message("On iteration ", i, " out of ", nrow(dat_na))
  #     }
  #     alpha_post[i, ] <- samples.fit[, paste("alpha[", i, "]", sep = "")]
  #     beta_post[i, ] <- samples.fit[, paste("beta[", i, "]", sep = "")]
  #   }
  # }
  
  beta_post    <- matrix(0, ncol(data$X), nrow(samples.fit))
  gamma1_post  <- matrix(0, ncol(data$X), nrow(samples.fit))
  gamma2_post  <- matrix(0, ncol(data$X), nrow(samples.fit))
  lambda1_post <- matrix(0, ncol(data$X), nrow(samples.fit))
  lambda2_post <- matrix(0, ncol(data$X), nrow(samples.fit))
  
  gamma_post  <- matrix(0, ncol(data$X), nrow(samples.fit))
  lambda_post <- matrix(0, ncol(data$X), nrow(samples.fit))
  
  if (mixture_type == "mixture") {
    for (j in 1:ncol(data$X)) {
      beta_post[j, ]    <- samples.fit[, paste("beta[", j, "]", sep = "")]    
      gamma1_post[j, ]  <- samples.fit[, paste("gamma1[", j, "]", sep = "")]    
      gamma2_post[j, ]  <- samples.fit[, paste("gamma2[", j, "]", sep = "")]    
      lambda1_post[j, ] <- samples.fit[, paste("lambda1[", j, "]", sep = "")]    
      lambda2_post[j, ] <- samples.fit[, paste("lambda2[", j, "]", sep = "")]    
    }
    sigma_beta_post    <- samples.fit[, "sigma_beta"]
    sigma_gamma1_post  <- samples.fit[, "sigma_gamma1"]
    sigma_gamma2_post  <- samples.fit[, "sigma_gamma2"]
    sigma_lambda1_post <- samples.fit[, "sigma_lambda1"]
    sigma_lambda2_post <- samples.fit[, "sigma_lambda2"]
  } 
  
  if (mixture_type == "single") {
    for (j in 1:ncol(data$X)) {
      gamma_post[j, ]  <- samples.fit[, paste("gamma[", j, "]", sep = "")]    
      lambda_post[j, ] <- samples.fit[, paste("lambda[", j, "]", sep = "")]    
    }
    sigma_gamma_post  <- samples.fit[, "sigma_gamma"]
    sigma_lambda_post <- samples.fit[, "sigma_lambda"]
  }
  
  ## 
  ## Generate a posterior predictive distribution across the climate gradient
  ## 
  
  n_climate <- 500
  X_rep <- base::seq(min(X_center), max(X_center), length.out = n_climate)
  
  ## for b-spline model
  Xbs_rep <- bs(X_rep, knots = attr(data$X, "knots"), intercept = TRUE)
  
  ## for linear model
  # Xbs_rep <- model.matrix(~ X_rep)
  
  ## increase n_rep if there is interest in smoothly estimating extremely high/low quantiles
  n_rep <- 100
  post_pred1  <- array(0, dim = c(n_climate, nrow(samples.fit), n_rep)) # rows are climate, # cols are the fit samples
  post_pred2  <- array(0, dim = c(n_climate, nrow(samples.fit), n_rep))
  post_pred  <- array(0, dim = c(n_climate, nrow(samples.fit), n_rep))
  
  expit <- function(x) {
    1 / (1 + exp(-x))    
  }
  
  if (mixture_type == "mixture") {
    for (j in 1:n_climate) {
      if (j %% 50 == 0) {
        message("Simulating posterior predictitve distribution ", j, " out of ", n_climate)
      }
      
      mu1_tmp    <- exp(Xbs_rep[j, ] %*% gamma1_post)
      sigma1_tmp <- exp(Xbs_rep[j, ] %*% lambda1_post)
      mu2_tmp    <- exp(Xbs_rep[j, ] %*% gamma2_post)
      sigma2_tmp <- exp(Xbs_rep[j, ] %*% lambda2_post)
      p_tmp      <- expit(Xbs_rep[j, ] %*% beta_post)
      
      post_pred1[j, , ] <- replicate(
        n_rep, 
        rgamma(
          nrow(samples.fit), 
          shape = mu1_tmp^2 / sigma1_tmp^2, 
          scale = sigma1_tmp^2 / mu1_tmp
        ), 
        simplify = "matrix"
      )
      
      post_pred2[j, , ] <- replicate(
        n_rep, 
        rgamma(
          nrow(samples.fit), 
          shape = mu2_tmp^2 / sigma2_tmp^2, 
          scale = sigma2_tmp^2 / mu2_tmp
        ),
        simplify = "matrix"
      )
      # post_pred_p[j, , ] <- replicate(n_rep, expit(Xbs_rep[j, ] %*% beta_post), simplify = "matrix")
    }
  }
  
  if (mixture_type == "single") {
    for (j in 1:n_climate) {
      if (j %% 50 == 0) {
        message("Simulating posterior predictitve distribution ", j, " out of ", n_climate)
      }
      
      alpha_tmp <- exp(Xbs_rep[j, ] %*% gamma_post)
      beta_tmp  <- exp(Xbs_rep[j, ] %*% lambda_post)
      
      post_pred[j, , ] <- replicate(
        n_rep, 
        rgamma(
          nrow(samples.fit), 
          shape = alpha_tmp,
          rate = beta_tmp
        ), 
        simplify = "matrix"
      )
    }
  }
  
  ## define function to construct dataframe that is a repetition of ribbon widths
  ## https://stackoverflow.com/questions/37326686/ggplot2-geom-ribbon-with-alpha-dependent-on-data-density-along-y-axis-for-each
  
  ## start of shaded figures
  ## number of quantiles
  nq = 50
  
  ## define the quantile boundaries of interest
  # qq = seq(0, 1, length.out = nq)
  # qq = seq(0.025, 0.975, length.out = nq)
  qq = seq(0.05, 0.95, length.out = nq)     ## widest interval is 90%
  
  message("Generating data.frame for plotting \n This can take a couple minutes")
  if (mixture_type == "mixture") {
    dat_ribbon <- data.frame(
      x = rep(X_rep * sd_X + mu_X, times = 2),
      component = rep(factor(c(0, 1)), each = n_climate),
      # prob      = rep(apply(post_pred_p, 1, mean), times = 2),
      sapply(1:nq, function (i) {
        c(
          apply(post_pred1, 1, quantile, prob = qq[i]),
          apply(post_pred2, 1, quantile, prob = qq[i])
        )
      })
    )
    # names(dat_ribbon) = c("x", "component", "prob", paste0("p", 100 * qq))
    names(dat_ribbon) = c("x", "component", paste0("p", 100 * qq))
    
    
    library(reshape2)
    ## convert from wide to tall data
    ## drop the last quantile
    dat_ribbon1 = dat_ribbon[, -length(dat_ribbon)]
    ## drop the first quantile
    dat_ribbon2 = dat_ribbon[, -3]
    
    ## reshape the datasets
    # dat_ribbon1 = melt(dat_ribbon1, id.var = c("x", "component", "prob"))
    dat_ribbon1 = melt(dat_ribbon1, id.var = c("x", "component"))
    # names(dat_ribbon1) = c("x", "component", "prob", "group", "min")
    names(dat_ribbon1) = c("x", "component", "group", "min")
    # dat_ribbon2 = melt(dat_ribbon2, id.var = c("x", "component", "prob"))
    dat_ribbon2 = melt(dat_ribbon2, id.var = c("x", "component"))
    # names(dat_ribbon2) = c("x", "component", "prob", "group", "max")
    names(dat_ribbon2) = c("x", "component", "group", "max")
    dat_ribbon = bind_cols(dat_ribbon1, dat_ribbon2)
    colnames(dat_ribbon) <-c("x", "component", "group", "min", "x1",
                             "component1", "group1", "max")
  }  
  
  if (mixture_type == "single") {
    dat_ribbon <- data.frame(
      x = X_rep * sd_X + mu_X, 
      sapply(1:nq, function (i) {
        apply(post_pred, 1, quantile, prob = qq[i])
      })
    )
    # names(dat_ribbon) = c("x", "component", "prob", paste0("p", 100 * qq))
    names(dat_ribbon) = c("x", paste0("p", 100 * qq))
    
    library(reshape2)
    ## convert from wide to tall data
    ## drop the last quantile
    dat_ribbon1 = dat_ribbon[, -length(dat_ribbon)]
    ## drop the first quantile
    dat_ribbon2 = dat_ribbon[, -2]
    
    ## reshape the datasets
    # dat_ribbon1 = melt(dat_ribbon1, id.var = c("x", "prob"))
    dat_ribbon1 = melt(dat_ribbon1, id.var = c("x"))
    # names(dat_ribbon1) = c("x", "prob", "group", "min")
    names(dat_ribbon1) = c("x", "group", "min")
    # dat_ribbon2 = melt(dat_ribbon2, id.var = c("x", "prob"))
    dat_ribbon2 = melt(dat_ribbon2, id.var = c("x"))
    # names(dat_ribbon2) = c("x", "prob", "group", "max")
    names(dat_ribbon2) = c("x", "group", "max")
    dat_ribbon = bind_cols(dat_ribbon1, dat_ribbon2)
    colnames(dat_ribbon) <-c("x", "group", "min", "x1",
                             "group1", "max")
  }  
  
  
  ##
  ## Generate the plot -- Kelly, make changes to format the graphic here
  ##
  
  
  p_plot <- ggplot(dat_ribbon, aes(x = x)) +
    ylim(c(0, 500)) +
    scale_alpha_manual(
      values = c(
        seq(0.05, 0.9, length.out = floor(0.5 * length(qq)))^(1.5),
        seq(0.9, 0.05, length.out = floor(0.5 * length(qq)))^(1.5)
      )
    ) +
    theme_bw() +
    theme(legend.position = "none") +
    ## we could add points shaded based on posterior predictive density
    geom_point(
      data = dat_na,
      aes_string(x = var_of_interest, y = data_variable),
      color = "black",
      inherit.aes = FALSE,
      alpha = 0.05
    ) 
  
  if (mixture_type == "mixture") {
    p_plot <- p_plot +     
      geom_ribbon(data = subset(dat_ribbon, component == "1"),
                  aes(x = x, ymin = min, ymax = max, group = group, alpha = group), fill = "#8c510a") +
      geom_ribbon(data = subset(dat_ribbon, component == "0"),
                  aes(x = x, ymin = min, ymax = max, group = group, alpha = group), fill = "#005a32")
  }
  
  if (mixture_type == "single") {
    p_plot <- p_plot + 
      geom_ribbon(data = dat_ribbon,
                  aes(x = x, ymin = min, ymax = max, group = group, alpha = group), fill = "#005a32") 
  }
  return(p_plot)
}


## 
## 
##

## which of the variables are we considering
# data_of_interest <- "pls"
# data_of_interest <- "fia"

## which of the variables are we considering
# var_of_interest <- "GS_ppet"                   ## pls P-PET
# var_of_interest <- "GS_ppet_mod"               ## modern P-PET
# var_of_interest <- "mean_GS_soil"              ## pls soil moisture
# var_of_interest <- "mean_GS_soil_m"            ## modern soil moisture
# var_of_interest <- "PC1"                       ## pls PC1
# var_of_interest <- "PC1fia"                     ## modern PC1

data_of_interest <- c("pls", "fia", "pls", "fia", "pls", "fia")
var_of_interest <- c(
  "GS_ppet", "GS_ppet_mod", "mean_GS_soil", 
  "mean_GS_soil_m", "PC1", "PC1fia"
)
mixture_type = c("mixture", "single")

i <- 1
j <- 1
## Using the above options, you can generate the plots using the function
# for the mixture model:
p_plot <- list()
j <- 1
for (i in 1:6) {
  #for (j in 1:2) {
  if(!file.exists(here::here("figures", 
                             paste0(
                               data_of_interest[i],
                               "-", var_of_interest[i],
                               "-", mixture_type[1],
                               ".png"
                             )))) {
    p_plot[[i]] <- generate_plot(
      data_of_interest = data_of_interest[i], 
      var_of_interest  = var_of_interest[i],
      mixture_type     = mixture_type[1]
      
    )
  }    
  ## save this plot
  png(file = here::here("figures", 
                        paste0(
                          data_of_interest[i],
                          "-", var_of_interest[i],
                          "-", mixture_type[1],
                          ".png"
                        )), 
      width = 16, height = 9, units = "in", res = 400)
  print(p_plot[[i]])
  dev.off()
}
#}


# by hand:
pls.mix.gsppt <- generate_plot(
  data_of_interest = data_of_interest[1], 
  var_of_interest  = var_of_interest[1],
  mixture_type     = mixture_type[1]
  
)

pls.mix.ppet.clean <- pls.mix.gsppt + ylab("Tree density (stems/ha)")+xlab("Growing season P-PET") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(-170, 300)+ylim(0, 500)

cowplot::save_plot("pls.mix.gs.ppt.msk.png", pls.mix.ppet.clean, base_height = NULL, base_aspect_ratio = 1.618, 
                   base_width = 6)

saveRDS(pls.mix.ppet.clean, "pls.mix.ppet.clean.msk.RDS")


pls.mix.soilm <- generate_plot(
  data_of_interest = data_of_interest[3], 
  var_of_interest  = var_of_interest[3],
  mixture_type     = mixture_type[1]
  
)


pls.mix.soilm.clean <- pls.mix.soilm + ylab("Tree density (stems/ha)")+xlab("Growing season soil moisture") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(0, 1.6)+ylim(0, 500)

cowplot::save_plot("pls.mix.gs.soil.msk.png", pls.mix.soilm.clean, base_height = NULL, base_aspect_ratio = 1.618, 
                   base_width = 6)
saveRDS(pls.mix.soilm.clean, "pls.mix.gs.soil.msk.RDS")

pls.mix.pc1 <- generate_plot(
  data_of_interest = data_of_interest[5], 
  var_of_interest  = var_of_interest[5],
  mixture_type     = mixture_type[1]
  
)

pls.mix.pc1.clean <- pls.mix.pc1 + ylab("Tree density (stems/ha)")+xlab("Principal Component 1") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(-5.5, 4.1)+ylim(0, 500)

cowplot::save_plot("pls.mix.gs.pc1.msk.png",pls.mix.pc1.clean, base_height = NULL, base_aspect_ratio = 1.618, 
                   base_width = 6)
saveRDS(pls.mix.pc1.clean, "pls.mix.gs.pc1.msk.RDS")


fia.mix.gsppt <- generate_plot(
  data_of_interest = data_of_interest[2], 
  var_of_interest  = var_of_interest[2],
  mixture_type     = mixture_type[1]
  
)

fia.mix.soilm <- generate_plot(
  data_of_interest = data_of_interest[4], 
  var_of_interest  = var_of_interest[4],
  mixture_type     = mixture_type[1]
  
)

fia.mix.plsm <- generate_plot(
  data_of_interest = data_of_interest[6], 
  var_of_interest  = var_of_interest[6],
  mixture_type     = mixture_type[1]
  
)

# clean up axes and align:
pls.mix.pc1.clean <- pls.mix.pc1 + ylab("Tree density (stems/ha)")+xlab("Principal Component 1") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(-5.5, 4.1)+ylim(0, 500)

fia.mix.pc1.clean <- fia.mix.pc1 + ylab("Tree density (stems/ha)")+xlab("Principal Component 1") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(-5.5, 4.1)+ylim(0, 500)


cowplot::save_plot("fia.mix.gs.pc1.msk.png",fia.mix.pc1.clean, base_height = NULL, base_aspect_ratio = 1.618, 
                   base_width = 6)
saveRDS(fia.mix.pc1.clean , "fia.mix.gs.pc1.msk.RDS")

pls.mix.ppet.clean <- pls.mix.gsppt + ylab("Tree density (stems/ha)")+xlab("Growing season P-PET") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(-170, 300)+ylim(0, 500)


fia.mix.ppet.clean <- fia.mix.gsppt + ylab("Tree density (stems/ha)")+xlab("Growing season P-PET") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(-170, 300)+ylim(0, 500)


cowplot::save_plot("fia.mix.gs.ppet.msk.png",fia.mix.ppet.clean, base_height = NULL, base_aspect_ratio = 1.618, 
                   base_width = 6)
saveRDS(fia.mix.ppet.clean , "fia.mix.ppet.msk.RDS")



pls.mix.soilm.clean <- pls.mix.soilm + ylab("Tree density (stems/ha)")+xlab("Growing season soil moisture") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(0, 1.6)+ylim(0, 500)

fia.mix.soilm.clean <- fia.mix.soilm + ylab("Tree density (stems/ha)")+xlab("Growing season soil moisture") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(0, 1.6)+ylim(0, 500)

cowplot::save_plot("fia.mix.gs.soilm.msk.png",fia.mix.soilm.clean, base_height = NULL, base_aspect_ratio = 1.618, 
                   base_width = 6)
saveRDS(fia.mix.soilm.clean , "fia.mix.soilm.msk.RDS")

save.image(file = "Hysteresis_plots_temp.RData")
load(file = "Hysteresis_plots_temp.RData")
# started over with fresh rstudio because of vector memory issue:
fia.mix.pc1 <- generate_plot(
  data_of_interest = data_of_interest[6], 
  var_of_interest  = var_of_interest[6],
  mixture_type     = mixture_type[1]
  
)

fia.mix.pc1.clean <- fia.mix.pc1 + ylab("Tree density (stems/ha)")+xlab("Principal Component 1") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(-5.5, 4.1)+ylim(0, 500)

cowplot::save_plot("fia.mix.pc1.msk.png",fia.mix.pc1.clean, base_height = NULL, base_aspect_ratio = 1.618, 
                   base_width = 6)


# generate non mixture (single component models for FIA):

fia.single.gsppt <- generate_plot(
  data_of_interest = data_of_interest[2], 
  var_of_interest  = var_of_interest[2],
  mixture_type     = mixture_type[2]
  
)

fia.single.ppet.clean <- fia.single.gsppt + ylab("Tree density (stems/ha)")+xlab("Growing season P-PET") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(-170, 300)+ylim(0, 500)


cowplot::save_plot("fia.single.ppet.msk.png",fia.single.ppet.clean, base_height = NULL, base_aspect_ratio = 1.618, 
                   base_width = 6)
saveRDS(fia.single.ppet.clean , "fia.single.ppet.msk.RDS")

fia.single.soilm <- generate_plot(
  data_of_interest = data_of_interest[4], 
  var_of_interest  = var_of_interest[4],
  mixture_type     = mixture_type[2]
  
)

fia.single.soilm.clean <- fia.single.soilm + ylab("Tree density (stems/ha)")+xlab("Growing season soil moisture") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(0, 1.6)+ylim(0, 500)

cowplot::save_plot("fia.single.soilm.msk.png",fia.single.soilm.clean, base_height = NULL, base_aspect_ratio = 1.618, 
                   base_width = 6)

saveRDS(fia.single.soilm.clean , "fia.single.soilm.msk.RDS")

fia.single.pc1 <- generate_plot(
  data_of_interest = data_of_interest[6], 
  var_of_interest  = var_of_interest[6],
  mixture_type     = mixture_type[2]
  
)

fia.single.pc1.clean <- fia.single.pc1 +scale_fill_manual(values =  "#005a32")+ ylab("Tree density (stems/ha)")+xlab("Principal Component 1") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(-5.5, 4.1)+ylim(0, 500)

cowplot::save_plot("fia.single.pc1.msk.png",fia.single.pc1.clean, base_height = NULL, base_aspect_ratio = 1.618, 
                   base_width = 6)

saveRDS(fia.single.pc1.clean , "fia.single.pc1.msk.RDS")

# 
# load("Hysteresis_plots_temp_fiamsk.RData")
library(cowplot)

png(height = 12, width = 8, units = "in", res = 300, "figures/clean_all_hysteresis_plots_fia_msk.png")
plot_grid(pls.mix.ppet.clean, fia.mix.ppet.clean, 
          pls.mix.soilm.clean, fia.mix.soilm.clean, 
          pls.mix.pc1.clean, fia.mix.pc1.clean, 
          align = "hv", labels = "AUTO", ncol = 2)
dev.off()


# do the same but now plot the FIA single component model:


fia.single.pc1.clean <- fia.single.pc1 +scale_fill_manual(values =  "#005a32")+ ylab("Tree density (stems/ha)")+xlab("Principal Component 1") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(-5.5, 4.1)+ylim(0, 500)


fia.single.ppet.clean <- fia.single.gsppt + ylab("Tree density (stems/ha)")+xlab("Growing season P-PET") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(-170, 300)+ylim(0, 500)


fia.single.soilm.clean <- fia.single.soilm + ylab("Tree density (stems/ha)")+xlab("Growing season soil moisture") + 
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(), legend.position = "none")+xlim(0, 1.6)+ylim(0, 500)


png(height = 12, width = 8, units = "in", res = 300, "figures/clean_all_hysteresis_plots_fia_single_fia_msk.png")
plot_grid(pls.mix.ppet.clean, fia.single.ppet.clean, 
          pls.mix.soilm.clean, fia.single.soilm.clean, 
          pls.mix.pc1.clean, fia.single.pc1.clean, 
          align = "hv", labels = "AUTO", ncol = 2)
dev.off()


# okay now load the bimodality arrow plots to make the figure 3:
# readin the saved arrow plots from bimodality working directory--this is messy, but for now it will work:
pct.inc.ppet.10 <- readRDS("/Users/kah/Documents/bimodality/outputs/pct.inc.ppet.1980.2000.arrowplot.rds")
pct.inc.soil.10 <- readRDS("/Users/kah/Documents/bimodality/outputs/pct.inc.soil.1980.2000.arrowplot.rds")
pct.inc.pc1.10 <- readRDS( "/Users/kah/Documents/bimodality/outputs/pct.inc.pc1.1980.2000.arrowplot.rds")



# also read in the saved maps--these were generated from the previous mixture model...but for now we will just use this:
three.color.bimodal.plots.fia.nolabs <- readRDS("three.color.bimodal.plots.fia.msk.rds")
three.color.bimodal.plots.nolabs <- readRDS("three.color.bimodal.plots.pls.msk.rds")


# same figure but with the mixture model:

# read in the ggplots saved as RDS
fia.single.pc1.clean <- readRDS( "fia.single.pc1.msk.RDS")
fia.single.soilm.clean <- readRDS( "fia.single.soilm.msk.RDS")
fia.single.ppet.clean <- readRDS( "fia.single.ppet.msk.RDS")

pls.mix.pc1.clean <- readRDS( "pls.mix.gs.pc1.msk.RDS")
pls.mix.ppet.clean <- readRDS( "pls.mix.ppet.clean.msk.RDS")
pls.mix.soilm.clean <- readRDS( "pls.mix.gs.soil.msk.RDS")

library(cowplot)
# make a plot legend:
legend.df <- data.frame(Mode = c("Low Tree Density", "High Tree Density"),
                        class2 = c("Savanna", "Forest"), 
                        dummy = 1:2)

mode.legend <- get_legend(ggplot(legend.df, aes(Mode, dummy, fill = Mode))+geom_bar(stat = "identity")+scale_fill_manual(values = c("#005a32", "#8c510a")) + theme_bw(base_size = 12)+ theme(legend.position = "top", legend.direction = "horizontal", legend.title = element_blank()))


# now plot out the full thing with all the data
png(height = 12, width = 10, units = "in", res = 300, "figures/clean_all_hysteresis_plots_fia_single_1980_2000_arrows_fia_msk.png")

plot_grid( mode.legend, 
           plot_grid(pls.mix.ppet.clean, fia.single.ppet.clean, pct.inc.ppet.10 + theme_bw(base_size = 12)+theme(panel.grid = element_blank())+xlim(-170, 300)+ylim(0, 500),
                     pls.mix.soilm.clean, fia.single.soilm.clean, pct.inc.soil.10 + theme_bw(base_size = 12)+theme(panel.grid = element_blank())+xlim(0, 1.6)+ylim(0, 500),
                     pls.mix.pc1.clean, fia.single.pc1.clean, pct.inc.pc1.10 + theme_bw(base_size = 12)+theme(panel.grid = element_blank())+xlim(-5.5, 4.1)+ylim(0, 500),
                     three.color.bimodal.plots.nolabs + theme(legend.title = element_blank()), three.color.bimodal.plots.fia.nolabs + theme(legend.title = element_blank()),
                     align = "hv", labels = c("A", "E", "I", 
                                              "B", "F", "J",
                                              "C", "G", "K",
                                              "D", "H") ,  ncol = 3),
           
           nrow = 2, rel_heights = c(0.1, 1))
dev.off()



# now plot out the full thing with all the data
png(height = 12, width = 10, units = "in", res = 300, "figures/clean_all_hysteresis_plots_fia_mixture_arrows_fia_msk.png")
plot_grid(pls.mix.ppet.clean, fia.mix.ppet.clean, pct.inc.ppet.10 + theme_bw(base_size = 12)+theme(panel.grid = element_blank())+xlim(-170, 300)+ylim(0, 500),
          pls.mix.soilm.clean, fia.mix.soilm.clean, pct.inc.soil.10 + theme_bw(base_size = 12)+theme(panel.grid = element_blank())+xlim(0, 1.6)+ylim(0, 500),
          pls.mix.pc1.clean, fia.mix.pc1.clean, pct.inc.pc1.10 + theme_bw(base_size = 12)+theme(panel.grid = element_blank())+xlim(-5.5, 4.1)+ylim(0, 500),
          align = "hv", labels = "AUTO", ncol = 3)
dev.off()
