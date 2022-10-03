# exploratory data analysis--GAM models predicting p(forest) and density from climate + environmental data
# migrated over from the Bimodality_draft_3.1.17.rmd
library(caTools)
library(mgcv)
library(grid)
library(gridExtra)

#----------------------------------------------binomial GAMs for  PLS era p(forest) predictions-------------------------------
dens.pr <- read.csv("data/PLS_FIA_density_climate_full.csv")

dens.pr$ecotype <- ifelse(dens.pr$PLSdensity > 47, "Forest", ifelse(dens.pr$PLSdensity > 0.5, "Savanna","Prairie"))

# dummyvariables for logistic regression:
dens.pr$ecocode <- 0
dens.pr[dens.pr$ecotype %in% 'Forest', ]$ecocode <- 1
dens.pr<- dens.pr[!dens.pr$ecotype %in% 'Prairie',]


pls.inil <- read.csv(paste0('outputs/biomass_no_na_pointwise.ests_inilmi_v',version, '.csv'))

so.mi.cells <- unique(pls.inil[pls.inil$state %in% "MI", ]$cell)

dens.full.df <- dens.pr
dens.pr <- dens.pr[!dens.pr$cell %in% so.mi.cells,]

#split training and testing data 
Y <- dens.pr$ecotype
msk <- sample.split( Y, SplitRatio = 3/4, group = NULL)


train <- dens.pr[msk,]
test <- dens.pr[!msk,]


#plot(dens.pr$sandpct, dens.pr$ecotype)


PLS.lgr1 <-gam(ecocode ~  s(MAP1910), family = 'binomial',data = train)
PLS.lgr1L <-gam(ecocode ~  MAP1910, family = 'binomial',data = train)


PLS.lgr2 <- gam(ecocode ~ s(pasttmean) , family = 'binomial',data = train)
PLS.lgr2L <- gam(ecocode ~ pasttmean , family = 'binomial',data = train)
#summary(PLS.lgr2) #explains 15.8% deviance
#plot(PLS.lgr2)

PLS.lgr3 <- gam(ecocode ~ s(pastdeltaP), family = 'binomial',data = train)
PLS.lgr3L <- gam(ecocode ~ pastdeltaP, family = 'binomial',data = train)
#summary(PLS.lgr3) #explains 6.07% deviance

PLS.lgr4 <- gam(ecocode ~ s(deltaT), family = 'binomial',data = train)
PLS.lgr4L <- gam(ecocode ~ deltaT, family = 'binomial',data = train)
#summary(PLS.lgr4) #explains 6.07% deviance

PLS.lgr5 <- gam(ecocode ~ s(awc), family = 'binomial',data = train)
PLS.lgr5L <- gam(ecocode ~ awc, family = 'binomial',data = train)
#summary(PLS.lgr5) #explains 12.5% of deviance

PLS.lgr6 <- gam(ecocode ~ s(sandpct), family = 'binomial',data = train)
PLS.lgr6L <- gam(ecocode ~ sandpct, family = 'binomial',data = train)
#summary(PLS.lgr6) #explains 12.5% of deviance



PLS.lgr7 <- gam(ecocode ~ s(pasttmean) + s(MAP1910) ,  family = 'binomial',data = train)
PLS.lgr7L <- gam(ecocode ~ pasttmean + MAP1910 ,  family = 'binomial',data = train)
#summary(PLS.lgr7) #explains 41% deviance
#summary(PLS.lgr7L) # explains 19.6%

PLS.lgr8 <- gam(ecocode ~ s(awc) +s(sandpct), family = 'binomial',data = train )
PLS.lgr8L <- gam(ecocode ~ awc + sandpct, family = 'binomial',data = train )
#summary(PLS.lgr8) #explains 28% of deviance

PLS.lgr9 <- gam(ecocode ~ s(MAP1910) + s(pasttmean) + s(sandpct), family = 'binomial',data = train)
PLS.lgr9L <- gam(ecocode ~ MAP1910 + pasttmean + sandpct, family = 'binomial',data = train)
#summary(PLS.lgr9) #explains 39% deviance
#summary(PLS.lgr9L)
#plot(PLS.lgr6)


PLS.lgr10 <- gam(ecocode ~ s(MAP1910) +s(pasttmean) + s(awc), family = 'binomial',data = train)
PLS.lgr10L <- gam(ecocode ~ MAP1910 + pasttmean + awc, family = 'binomial',data = train)

#summary(PLS.lgr10) #explains 41.3% of deviance
#plot(PLS.lgr10)

PLS.lgr11 <- gam(ecocode ~ s(MAP1910)  +s(pasttmean) +s(sandpct) + s(awc), family = 'binomial',data = train)
PLS.lgr11L <- gam(ecocode ~ MAP1910  + pasttmean + sandpct + awc, family = 'binomial',data = train)

#summary(PLS.lgr8) # explains 41% of deviance
PLS.lgr12 <- gam(ecocode ~ s(MAP1910)  + s(pasttmean) + s(pastdeltaP),family = 'binomial', data = train)
PLS.lgr12L <- gam(ecocode ~ MAP1910  + pasttmean + pastdeltaP, family = 'binomial',data = train)

PLS.lgr13 <- gam(ecocode ~ s(MAP1910)  + s(pasttmean) + s(pastdeltaP), family = 'binomial',data = train)
PLS.lgr13L <- gam(ecocode ~ MAP1910  + pasttmean + pastdeltaP, family = 'binomial',data = train)

#PLS.lgr13 <- gam(ecocode ~ s(MAP1910)  + s(pasttmean) + s(deltaT), family = 'binomial',data = train)
#PLS.lgr13L <- gam(ecocode ~ MAP1910  + pasttmean + deltaT, family = 'binomial',data = train)

PLS.lgr14 <- gam(ecocode ~ s(MAP1910)  + s(pasttmean) + s(deltaT) + s(pastdeltaP)+s(sandpct)+s(awc), family = 'binomial',data = train)
PLS.lgr14L <- gam(ecocode ~ MAP1910  + pasttmean + deltaT + pastdeltaP, family = 'binomial',data = train)

# model with Just PC1 --we use this to preditct the future
PLS.lgr15 <- gam(ecocode ~ s(PC1), family = 'binomial',data = train)
PLS.lgr15L <- gam(ecocode ~ PC1, family = 'binomial',data = train)

#AIC2.df<- AIC(PLS.lgr1, PLS.lgr2, PLS.lgr3, PLS.lgr4, PLS.lgr5, PLS.lgr6, PLS.lgr7, PLS.lgr8, PLS.lgr9,PLS.lgr10,PLS.lgr11,PLS.lgr12,PLS.lgr13,PLS.lgr14 , PLS.lgr1L, PLS.lgr2L, PLS.lgr3L, PLS.lgr4L, PLS.lgr5L, PLS.lgr6L, PLS.lgr7L, PLS.lgr8L, PLS.lgr9L, PLS.lgr10L,PLS.lgr11L,PLS.lgr12L,PLS.lgr13L,PLS.lgr14L, PLS.lgr15, PLS.lgr15L)




#AIC2.df$modeltype <- c(rep('smooth', 15), rep('linear', 15))

#AIC2.df$formula <- c(PLS.lgr1$formula, PLS.lgr2$formula, PLS.lgr3$formula, PLS.lgr4$formula, PLS.lgr5$formula, PLS.lgr6$formula, PLS.lgr7$formula, PLS.lgr8$formula, PLS.lgr9$formula,PLS.lgr10$formula,PLS.lgr11$formula,PLS.lgr12$formula,PLS.lgr13$formula,PLS.lgr14$formula , PLS.lgr1L$formula, PLS.lgr2L$formula, PLS.lgr3L$formula, PLS.lgr4L$formula, PLS.lgr5L$formula, PLS.lgr6L$formula, PLS.lgr7L$formula, PLS.lgr8L$formula, PLS.lgr9L$formula, PLS.lgr10L$formula,PLS.lgr11L$formula,PLS.lgr12L$formula,PLS.lgr13L$formula, PLS.lgr14L$formula , PLS.lgr15$formula, PLS.lgr15L$formula)

#AIC2.df<- AIC2.df[order(AIC2.df$AIC),]
#library(knitr)
#library(pander)
#AIC2.df$model <- rownames(AIC2.df)

#calculate prediction error:
#msqe.code<- function(model, testdata){
#ypred <- predict(model, newdata = testdata, type="response")
#testdata$ypred <- ypred

#predicted<- testdata

#predicted$sqerr <- (predicted$ecocode - predicted$ypred)^2
#mean(predicted$sqerr, na.rm = TRUE)
#}

#AIC2.df$MSPE <- 1

#add prediction error ot AIC dataframe

#for (i in 1: length(AIC2.df$model)){
#AIC2.df[i,'MSPE'] <- msqe.code(get(AIC2.df[i,"model"]), test)
#}
#pander(AIC2.df[,c("model","modeltype", "formula", "df", "AIC", "MSPE")], split.cells = 30)


full <- dens.pr
#full <- test
logsample <- predict(PLS.lgr15, full, type="response")
full$ypred <- as.numeric(logsample)


# create discrete probability cuts
label.breaks <- function(beg, end, splitby){
  labels.test <- data.frame(first = seq(beg, end, by = splitby), second = seq((beg + splitby), (end + splitby), by = splitby))
  labels.test <- paste (labels.test$first, '-' , labels.test$second)
  labels.test
}


full$ypreddiscrete <- cut(full$ypred, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))



cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")
full$ypreddiscrete <- as.character(full$ypreddiscrete)
#ggplot(full, aes(x, y, color = ypreddiscrete)) + geom_point()


fullnona <- full[!is.na(full$ypreddiscrete),]
# plot the discrete probability of forest 
p.forest <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fullnona, aes(x=x, y=y, fill = ypreddiscrete))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Climate Predicted Prob(forest)")+ scale_fill_manual(values= cbpalette, labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ annotate("text", x=-90000, y=1486000,label= "B", size = 5)+ggtitle("")

p.forest

# plot PLS forests
pls <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=full, aes(x=x, y=y, fill = ecotype))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS classification")+ scale_fill_manual(values= c("#006837", "tan","#c2e699"))+ coord_equal()+theme_bw()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                                                                                                                                                        panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1), legend.title = element_blank())+ annotate("text", x=-90000, y=1486000,label= "A", size = 5)+ggtitle("")

png(width = 8, height = 4, units = 'in', res = 300, filename = '/outputs/paper_figs/logistic_pred_prob_testdata_pls_no_so_mi.png')
grid.arrange(pls, p.forest, nrow = 1, ncol=2)
dev.off()



# now make predictions for southern michigan:

#full <- test
logsamplefull <- predict(PLS.lgr15L, dens.full.df, type="response")
dens.full.df$ypred <- as.numeric(logsamplefull)


# use same probability cuts as above
dens.full.df$ypreddiscrete <- cut(dens.full.df$ypred, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))



cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")
dens.full.df$ypreddiscrete <- as.character(dens.full.df$ypreddiscrete)



dens.fullnona <- dens.full.df[!is.na(dens.full.df$ypreddiscrete),]

# plot the discrete probability of forest 
p.forest.f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.fullnona, aes(x=x, y=y, fill = ypreddiscrete))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Climate Predicted Prob(forest)")+ scale_fill_manual(values= cbpalette, labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ annotate("text", x=-90000, y=1486000,label= "B", size = 5)+ggtitle("")

p.forest.f

# plot PLS forests
pls.full <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.full.df, aes(x=x, y=y, fill = ecotype))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS classification")+ scale_fill_manual(values= c("#006837", "tan","#c2e699"))+ coord_equal()+theme_bw()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                                                                                                                                                        panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1), legend.title = element_blank())+ annotate("text", x=-90000, y=1486000,label= "A", size = 5)+ggtitle("")

png(width = 8, height = 4, units = 'in', res = 300, 'outputs/paper_figs/log_pred_prob_f_so_mi.png')
grid.arrange(pls.full, p.forest.f, nrow = 1, ncol=2)
dev.off()


# now lets make the gam prediction using southern michigan in the test data:

#split training and testing data 
Y <- dens.full.df$ecotype
msk <- sample.split( Y, SplitRatio = 3/4, group = NULL)


train <- dens.full.df[msk,]
test <- dens.full.df[!msk,]


PLS.lgr15.f <- gam(ecocode ~ s(PC1), family = 'binomial',data = train)
PLS.lgr15L.f <- gam(ecocode ~ PC1, family = 'binomial',data = train)




logsample <- predict(PLS.lgr15.f, dens.full.df, type="response")
dens.full.df$ypred <- as.numeric(logsample)


# use same probability cuts as above
dens.full.df$ypreddiscrete <- cut(dens.full.df$ypred, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))



cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")
dens.full.df$ypreddiscrete <- as.character(dens.full.df$ypreddiscrete)



dens.fullnona <- dens.full.df[!is.na(dens.full.df$ypreddiscrete),]

# plot the discrete probability of forest 
p.forest.f <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.fullnona, aes(x=x, y=y, fill = ypreddiscrete))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Climate Predicted Prob(forest)")+ scale_fill_manual(values= cbpalette, labels=c("0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1")) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ annotate("text", x=-90000, y=1486000,label= "B", size = 5)+ggtitle("")

p.forest.f

# plot PLS forests
pls.full <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.full.df, aes(x=x, y=y, fill = ecotype))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS classification")+ scale_fill_manual(values= c("#006837", "tan","#c2e699"))+ coord_equal()+theme_bw()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                                                                                                                                                        panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1), legend.title = element_blank())+ annotate("text", x=-90000, y=1486000,label= "A", size = 5)+ggtitle("")

png(width = 8, height = 4, units = 'in', res = 300, 'outputs/paper_figs/log_pred_prob_forests_full.png')
grid.arrange(pls.full, p.forest.f, nrow = 1, ncol=2)
dev.off()

#-------------------------------------binomial GAMs for  FIA era p(forest) predictions----------------------
fiadens <- read.csv("outputs/cluster/bimodal_widths/FIA_Dens_Bimodal_width_0.25.csv")
# dummyvariables for logistic regression:

fiadens$ecocode <- 0
fiadens[fiadens$ecotype %in% 'Forest', ]$ecocode <- 1
#fiadens<- fiadens[!fiadens$ecotype %in% 'prairie',]

#split trainffing and testfing
Y <- fiadens$ecotype
msk <- sample.split( Y, SplitRatio = 1/4, group = NULL )
#table(Y,msk)

trainf <- fiadens[msk,]
testf <- fiadens[!msk,]


#plot(fiadens$sandpct, fiadens$ecotype)


FIA.lgr1 <-gam(ecocode ~  s(MAP1910), family = 'binomial',data = trainf)
FIA.lgr1L <-gam(ecocode ~  MAP1910, family = 'binomial',data = trainf)


FIA.lgr2 <- gam(ecocode ~ s(pasttmean) , family = 'binomial',data = trainf)
FIA.lgr2L <- gam(ecocode ~ pasttmean , family = 'binomial',data = trainf)
#summary(FIA.lgr2) #explains 15.8% deviance
#plot(FIA.lgr2)

FIA.lgr3 <- gam(ecocode ~ s(pastdeltaP), family = 'binomial',data = trainf)
FIA.lgr3L <- gam(ecocode ~ pastdeltaP, family = 'binomial',data = trainf)
#summary(FIA.lgr3) #explains 6.07% deviance

FIA.lgr4 <- gam(ecocode ~ s(deltaT), family = 'binomial',data = trainf)
FIA.lgr4L <- gam(ecocode ~ deltaT, family = 'binomial',data = trainf)
#summary(FIA.lgr4) #explains 6.07% deviance

FIA.lgr5 <- gam(ecocode ~ s(awc), family = 'binomial',data = trainf)
FIA.lgr5L <- gam(ecocode ~ awc, family = 'binomial',data = trainf)
#summary(FIA.lgr5) #explains 12.5% of deviance

FIA.lgr6 <- gam(ecocode ~ s(sandpct), family = 'binomial',data = trainf)
FIA.lgr6L <- gam(ecocode ~ sandpct, family = 'binomial',data = trainf)
#summary(FIA.lgr6) #explains 12.5% of deviance



FIA.lgr7 <- gam(ecocode ~ s(pasttmean) + s(MAP1910) ,  family = 'binomial',data = trainf)
FIA.lgr7L <- gam(ecocode ~ pasttmean + MAP1910 ,  family = 'binomial',data = trainf)
#summary(FIA.lgr7) #explains 41% deviance
#summary(FIA.lgr7L) # explains 19.6%

FIA.lgr8 <- gam(ecocode ~ s(awc) +s(sandpct), family = 'binomial',data = trainf )
FIA.lgr8L <- gam(ecocode ~ awc + sandpct, family = 'binomial',data = trainf )
#summary(FIA.lgr8) #explains 28% of deviance

FIA.lgr9 <- gam(ecocode ~ s(MAP1910) + s(pasttmean) + s(sandpct), family = 'binomial',data = trainf)
FIA.lgr9L <- gam(ecocode ~ MAP1910 + pasttmean + sandpct, family = 'binomial',data = trainf)
#summary(FIA.lgr9) #explains 39% deviance
#summary(FIA.lgr9L)
#plot(FIA.lgr6)


FIA.lgr10 <- gam(ecocode ~ s(MAP1910) +s(pasttmean) + s(awc), family = 'binomial',data = trainf)
FIA.lgr10L <- gam(ecocode ~ MAP1910 + pasttmean + awc, family = 'binomial',data = trainf)

#summary(FIA.lgr10) #explains 41.3% of deviance
#plot(FIA.lgr10)

FIA.lgr11 <- gam(ecocode ~ s(MAP1910)  +s(pasttmean) +s(sandpct) + s(awc), family = 'binomial',data = trainf)
FIA.lgr11L <- gam(ecocode ~ MAP1910  + pasttmean + sandpct + awc, family = 'binomial',data = trainf)

#summary(FIA.lgr8) # explains 41% of deviance
FIA.lgr12 <- gam(ecocode ~ s(MAP1910)  + s(pasttmean) + s(pastdeltaP),family = 'binomial', data = trainf)
FIA.lgr12L <- gam(ecocode ~ MAP1910  + pasttmean + pastdeltaP, family = 'binomial',data = trainf)

FIA.lgr13 <- gam(ecocode ~ s(MAP1910)  + s(pasttmean) + s(pastdeltaP), family = 'binomial',data = trainf)
FIA.lgr13L <- gam(ecocode ~ MAP1910  + pasttmean + pastdeltaP, family = 'binomial',data = trainf)

#FIA.lgr13 <- gam(ecocode ~ s(MAP1910)  + s(pasttmean) + s(deltaT), family = 'binomial',data = trainf)
#FIA.lgr13L <- gam(ecocode ~ MAP1910  + pasttmean + deltaT, family = 'binomial',data = trainf)

FIA.lgr14 <- gam(ecocode ~ s(MAP1910)  + s(pasttmean) + s(deltaT) + s(pastdeltaP)+s(sandpct)+s(awc), family = 'binomial',data = trainf)
FIA.lgr14L <- gam(ecocode ~ MAP1910  + pasttmean + deltaT + pastdeltaP, family = 'binomial',data = trainf)

FIA.lgr15 <- gam(ecocode ~ s(PC1), family = 'binomial',data = trainf)
FIA.lgr15L <- gam(ecocode ~ PC1, family = 'binomial',data = trainf)

#AIC2.df<- AIC(FIA.lgr1, FIA.lgr2, FIA.lgr3, FIA.lgr4, FIA.lgr5, FIA.lgr6, FIA.lgr7, FIA.lgr8, FIA.lgr9,FIA.lgr10,FIA.lgr11,FIA.lgr12,FIA.lgr13,FIA.lgr14 , FIA.lgr1L, FIA.lgr2L, FIA.lgr3L, FIA.lgr4L, FIA.lgr5L, FIA.lgr6L, FIA.lgr7L, FIA.lgr8L, FIA.lgr9L, FIA.lgr10L,FIA.lgr11L,FIA.lgr12L,FIA.lgr13L,FIA.lgr14L)

logsample <- predict(FIA.lgr15, fiadens, type="response")
fiadens$ypred <- as.numeric(logsample)
summary(logsample)
hist(fiadens$ypred)



#AIC2.df$modeltype <- c(rep('smooth', 14), rep('linear', 14))

#AIC2.df$formula <- c(FIA.lgr1$formula, FIA.lgr2$formula, FIA.lgr3$formula, FIA.lgr4$formula, FIA.lgr5$formula, FIA.lgr6$formula, FIA.lgr7$formula, FIA.lgr8$formula, FIA.lgr9$formula,FIA.lgr10$formula,FIA.lgr11$formula,FIA.lgr12$formula,FIA.lgr13$formula,FIA.lgr14$formula , FIA.lgr1L$formula, FIA.lgr2L$formula, FIA.lgr3L$formula, FIA.lgr4L$formula, FIA.lgr5L$formula, FIA.lgr6L$formula, FIA.lgr7L$formula, FIA.lgr8L$formula, FIA.lgr9L$formula, FIA.lgr10L$formula,FIA.lgr11L$formula,FIA.lgr12L$formula,FIA.lgr13L$formula, FIA.lgr14L$formula )

#AIC2.df<- AIC2.df[order(AIC2.df$AIC),]

#AIC2.df$model <- rownames(AIC2.df)

#calculate prediction error:

#msqe.code<- function(model, testfdata){
#ypred <- predict(model, newdata = testfdata, type="response")
#testfdata$ypred <- ypred

#predicted<- testfdata

#predicted$sqerr <- (predicted$ecocode - predicted$ypred)^2
#mean(predicted$sqerr, na.rm = TRUE)
#}

#AIC2.df$MSPE <- 1

#add prediction error ot AIC dataframe

#for (i in 1: length(AIC2.df$model)){
#AIC2.df[i,'MSPE'] <- msqe.code(get(AIC2.df[i,"model"]), test)
#}
#pander(AIC2.df[,c("model","modeltype", "formula", "df", "AIC", "MSPE")], #split.cells = 30)

fiadens$ypred <- as.numeric(logsample)
#summary(logsample)
#hist(test$ypred)

#ggplot(full, aes(x, y, color = ypred)) + geom_point()
#ggplot(test, aes(x, y, color = ecocode)) + geom_point()+coord_equal()

ggplot(fiadens, aes(ypred, Density))+geom_point()
ggplot(fiadens, aes(ypred, ecotype))+geom_point()
#ggplot(test, aes (ypred, MAP1910))+geom_point()



# create discrete probability cuts
label.breaks <- function(beg, end, splitby){
  labels.test <- data.frame(first = seq(beg, end, by = splitby), second = seq((beg + splitby), (end + splitby), by = splitby))
  labels.test <- paste (labels.test$first, '-' , labels.test$second)
  labels.test
}


fiadens$ypreddiscrete <- cut(fiadens$ypred, breaks = seq(0,1, by = 0.2), labels = label.breaks(0,0.8, 0.2))



cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
names(cbpalette) <- c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")
fiadens$ypreddiscrete <- as.character(fiadens$ypreddiscrete)
#ggplot(fiadens, aes(x, y, color = ypreddiscrete)) + geom_point()


fiadensnona <- fiadens[!is.na(fiadens$ypreddiscrete),]
# plot the discrete probability of forest 
p.forest <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fiadensnona, aes(x=x, y=y, fill = ypreddiscrete))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Climate Predicted Prob(forest)")+ scale_fill_manual(values = cbpalette) +
  coord_equal()+theme_bw()+ theme()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0)),
                                          panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ annotate("text", x=-90000, y=1486000,label= "B", size = 5)+ggtitle("")

p.forest

# plot PLS forests
fia <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fiadens, aes(x=x, y=y, fill = ecotype))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS classification")+ scale_fill_manual(values= c("#006837", "tan","#c2e699"))+ coord_equal()+theme_bw()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                                                                                                                                                        panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1), legend.title = element_blank())+ annotate("text", x=-90000, y=1486000,label= "A", size = 5)+ggtitle("")

#png(width = 8, height = 4, units = 'in', res = 300, filename = '/outputs/paper_figs/logistic_pred_prob_testdata_fia.png')
#grid.arrange(fia, p.forest, nrow = 1, ncol=2)
#dev.off()

png(width = 8, height = 4, units = 'in', res = 300, filename = 'outputs/paper_figs/logistic_pred_prob_testdata_fia.png')
grid.arrange(fia, p.forest, nrow = 1, ncol=2)
dev.off()




#----------------------------------------------GAMs for  PLS era Desnity predictions-------------------------------
suppressMessages(library(mgcv))
suppressMessages(library(caTools))
suppressMessages(library(ggplot2))
suppressMessages(library(MASS))

#dens.pr <- read.csv("data/midwest_pls_density_pr_alb1.6-5.csv") # just with grid cells that have both pls & FIA
#dens.pr <- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_pr_alb1.6-5.csv") # full set of PLS data
#hist(dens.pr$PLSdensity, breaks = 50)
#dens.pr <- read.csv("data/PLS_full_dens_pr_with_bins.csv")

dens.pr <- read.csv("outputs/cluster/bimodal_widths/PLS_Dens_Bimodal_width_0.25.csv") 

pls.new <- dens.pr

#pls.new <- dens.pr[dens.pr$bimodal == "Unimodal",] # remove all bimodal places for now
#pls.bimodal <- dens.pr[dens.pr$bimodal == "Bimodal",]

pls.new <- pls.new[pls.new$PLSdensity > 0.5, ]# remove all zero places
dens.pr <- pls.new
dens.pr <- pls.new

# split into test and training datasets:
Y <- dens.pr$PLSdensity
msk <- sample.split( Y, SplitRatio = 1/4, group = NULL )
#table(Y,msk)

train <- dens.pr[msk,]
test <- dens.pr[!msk,]


# basic climate plots of data with only "stable forests/savannas"
PLS.gam1 <- gam(PLSdensity ~ MAP1910, data = train)
PLS.gam1L <- gam(PLSdensity ~  MAP1910, data = train)

pr <- predict(PLS.gam1L, test)

PLS.gam2 <- gam(PLSdensity ~ s(pasttmean) , data = train)
PLS.gam2L <- gam(PLSdensity ~ pasttmean , data = train)
#summary(PLS.gam2) #explains 15.8% deviance
#plot(PLS.gam2)

PLS.gam3 <- gam(PLSdensity ~ s(pastdeltaP),data = train)
PLS.gam3L <- gam(PLSdensity ~ pastdeltaP,data = train)
#summary(PLS.gam3) #explains 6.07% deviance

PLS.gam4 <- gam(PLSdensity ~ s(deltaT), data = train)
PLS.gam4L <- gam(PLSdensity ~ deltaT, data = train)
#summary(PLS.gam4) #explains 6.07% deviance

PLS.gam5 <- gam(PLSdensity ~ s(awc),data = train)
PLS.gam5L <- gam(PLSdensity ~ awc, data = train)
#summary(PLS.gam5) #explains 12.5% of deviance

PLS.gam6 <- gam(PLSdensity ~ s(sandpct), data = train)
PLS.gam6L <- gam(PLSdensity ~ sandpct, data = train)
#summary(PLS.gam6) #explains 12.5% of deviance



PLS.gam7 <- gam(PLSdensity ~ s(pasttmean) + s(MAP1910) , data = train)
PLS.gam7L <- gam(PLSdensity ~ pasttmean + MAP1910 ,  data = train)
#summary(PLS.gam7) #explains 41% deviance


PLS.gam8 <- gam(PLSdensity ~ s(awc) +s(sandpct), data = train )
PLS.gam8L <- gam(PLSdensity ~ awc + sandpct, data = train )
#summary(PLS.gam8) #explains 28% of deviance

PLS.gam9 <- gam(PLSdensity ~ s(MAP1910) + s(pasttmean) + s(sandpct), data = train)
PLS.gam9L <- gam(PLSdensity ~ MAP1910 + pasttmean + sandpct, data = train)
#summary(PLS.gam9) #explains 39% deviance
#summary(PLS.gam9L)
#plot(PLS.gam6)


PLS.gam10 <- gam(PLSdensity ~ s(MAP1910) +s(pasttmean) + s(awc), data = train)
PLS.gam10L <- gam(PLSdensity ~ MAP1910 + pasttmean + awc, data = train)

#summary(PLS.gam10) #explains 41.3% of deviance
#plot(PLS.gam10)

PLS.gam11 <- gam(PLSdensity ~ s(MAP1910)  +s(pasttmean) +s(sandpct) + s(awc), data = train)
PLS.gam11L <- gam(PLSdensity ~ MAP1910  + pasttmean + sandpct + awc, data = train)

#summary(PLS.gam8) # explains 41% of deviance
PLS.gam12 <- gam(PLSdensity ~ s(MAP1910)  + s(pasttmean) + s(pastdeltaP), data = train)
PLS.gam12L <- gam(PLSdensity ~ MAP1910  + pasttmean + pastdeltaP, data = train)

PLS.gam13 <- gam(PLSdensity ~ s(MAP1910)  + s(pasttmean) + s(pastdeltaP), data = train)
PLS.gam13L <- gam(PLSdensity ~ MAP1910  + pasttmean + pastdeltaP, data = train)

PLS.gam13 <- gam(PLSdensity ~ s(MAP1910)  + s(pasttmean) + s(deltaT), data = train)
PLS.gam13L <- gam(PLSdensity ~ MAP1910  + pasttmean + deltaT, data = train)

PLS.gam14 <- gam(PLSdensity ~ s(MAP1910)  + s(pasttmean) + s(deltaT) + s(pastdeltaP)+s(sandpct), data = train)
PLS.gam14L <- gam(PLSdensity ~ MAP1910  + pasttmean + deltaT + pastdeltaP + sandpct, data = train)

PLS.gam15 <- gam(PLSdensity ~ s(MAP1910)  + s(pasttmean) + s(deltaT) + s(pastdeltaP)+s(sandpct)+s(awc), data = train, family = gaussian(link = 'identity'))
PLS.gam15L <- gam(PLSdensity ~ MAP1910  + pasttmean + deltaT + pastdeltaP +sandpct + awc, data = train)

t <- predict(PLS.gam15, test[,c("PLSdensity", "MAP1910", "pasttmean", "deltaT", 'pastdeltaP', 'sandpct', 'awc')])
summary(t)

plot(t, test$PLSdensity)
abline (a = 0, b = 1, col = 'red')
hist(t)
hist(test$PLSdensity)

PLS.gam16 <- gam(PLSdensity ~ s(MAP1910)  + s(pasttmean) + s(deltaT) + s(pastdeltaP), data = train)
PLS.gam16L <- gam(PLSdensity ~ MAP1910  + pasttmean + deltaT + pastdeltaP, data = train)


#calculate prediction error:
msqe<- function(model, testdata){
  ypred <- predict(model, newdata = testdata, type="response")
  testdata$ypred <- ypred
  
  predicted<- testdata
  
  predicted$sqerr <- (predicted$PLSdensity - predicted$ypred)^2
  mean(predicted$sqerr, na.rm = TRUE)
}

AIC.df$dev.expl <- 1

#add prediction error & Deviance Explained to the AIC dataframe

for (i in 1: length(AIC.df$model)){
  AIC.df[i,'MSPE'] <- msqe(get(AIC.df[i,"model"]), test)
  AIC.df[i,"dev.expl"] <- summary(get(AIC.df[i,"model"]))$dev.expl * 100
}


print(AIC.df)


#------------------------------------Fit GAMs for FIA era--------------------------

suppressMessages(library(mgcv))
suppressMessages(library(caTools))
suppressMessages(library(ggplot2))

#dens.pr <- read.csv("data/midwest_pls_density_pr_alb1.6-5.csv") # just with grid cells that have both pls & FIA
#dens.pr <- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_pr_alb1.6-5.csv") # full set of PLS data
#hist(dens.pr$PLSdensity, breaks = 50)
#dens.pr <- read.csv("data/PLS_full_dens_pr_with_bins.csv")

fiadens <- read.csv("outputs/cluster/bimodal_widths/FIA_Dens_Bimodal_width_0.25.csv")

fiadens <- fiadens[fiadens$FIAdensity > 47, ]
fiadens$ecoclass <- ifelse(fiadens$ecotype == "Forest", 1, 0)
#split FIA data into test and trainfing datasets
Y <- fiadens$ecoclass
msk <- sample.split( Y, SplitRatio = 1/4, group = fiadens$ecoclass )
#table(Y,msk)

trainf <- fiadens[msk,]
testf <- fiadens[!msk,]


FIA.gam1 <-gam(ecoclass ~  s(MAP2011), data = trainf)
FIA.gam1L <-gam(ecoclass ~  MAP2011, data = trainf)


FIA.gam2 <- gam(ecoclass ~ s(modtmean) , data = trainf)
FIA.gam2L <- gam(ecoclass ~ modtmean , data = trainf)
#summary(FIA.gam2) #explains 5.6% deviance
#plot(FIA.gam2)

FIA.gam3 <- gam(ecoclass ~ s(moderndeltaP), data = trainf)
FIA.gam3L <- gam(ecoclass ~ moderndeltaP, data = trainf)
#summary(FIA.gam3) #explains 6.07% deviance

FIA.gam4 <- gam(ecoclass ~ s(moddeltaT), data = trainf)
FIA.gam4L <- gam(ecoclass ~ moddeltaT, data = trainf)
#summary(FIA.gam4) #explains 6.07% deviance

FIA.gam5 <- gam(ecoclass ~ s(awc),data = trainf)
FIA.gam5L <- gam(ecoclass ~ awc, data = trainf)
#summary(FIA.gam5) #explains 12.5% of deviance

FIA.gam6 <- gam(ecoclass ~ s(sandpct), data = trainf)
FIA.gam6L <- gam(ecoclass ~ sandpct, data = trainf)
#summary(FIA.gam6) #explains 12.5% of deviance



FIA.gam7 <- gam(ecoclass ~ s(modtmean) + s(MAP2011) , data = trainf, family = quasipoisson())
FIA.gam7L <- gam(ecoclass ~ modtmean + MAP2011 ,  data = trainf)
#summary(FIA.gam7) #explains 41% deviance
#summary(FIA.gam7L) # explains 19.6%

FIA.gam8 <- gam(ecoclass ~ s(awc) +s(sandpct), data = trainf )
FIA.gam8L <- gam(ecoclass ~ awc + sandpct, data = trainf )
#summary(FIA.gam8) #explains 28% of deviance

FIA.gam9 <- gam(ecoclass ~ s(MAP2011) + s(modtmean) + s(sandpct), data = trainf)
FIA.gam9L <- gam(ecoclass ~ MAP2011 + modtmean + sandpct, data = trainf)
#summary(FIA.gam9) #explains 39% deviance
#summary(FIA.gam9L)
#plot(FIA.gam6)


FIA.gam10 <- gam(ecoclass ~ s(MAP2011) +s(modtmean) + s(awc), data = trainf)
FIA.gam10L <- gam(ecoclass ~ MAP2011 + modtmean + awc, data = trainf)

#summary(FIA.gam10) #explains 41.3% of deviance
#plot(FIA.gam10)

FIA.gam11 <- gam(ecoclass ~ s(MAP2011)  +s(modtmean) +s(sandpct) + s(awc), data = trainf)
FIA.gam11L <- gam(ecoclass ~ MAP2011  + modtmean + sandpct + awc, data = trainf)

#summary(FIA.gam8) # explains 41% of deviance
FIA.gam12 <- gam(ecoclass ~ s(MAP2011)  + s(modtmean) + s(moderndeltaP), data = trainf)
FIA.gam12L <- gam(ecoclass ~ MAP2011  + modtmean + moderndeltaP, data = trainf)

FIA.gam13 <- gam(ecoclass ~ s(MAP2011)  + s(modtmean) + s(moderndeltaP), data = trainf)
FIA.gam13L <- gam(ecoclass ~ MAP2011  + modtmean + moderndeltaP, data = trainf)

FIA.gam13 <- gam(ecoclass ~ s(MAP2011)  + s(modtmean) + s(moddeltaT), data = trainf)
FIA.gam13L <- gam(ecoclass ~ MAP2011  + modtmean + moddeltaT, data = trainf)

FIA.gam14 <- gam(ecoclass ~ s(MAP2011)  + s(modtmean) + s(moddeltaT) + s(moderndeltaP)+s(sandpct), data = trainf)
FIA.gam14L <- gam(ecoclass ~ MAP2011  + modtmean + moddeltaT + moderndeltaP, data = trainf)

FIA.gam15 <- gam(ecoclass ~ s(MAP2011)  + s(modtmean) + s(moddeltaT) + s(moderndeltaP)+s(sandpct)+s(awc), data = trainf)
FIA.gam15L <- gam(ecoclass ~ MAP2011  + modtmean + moddeltaT + moderndeltaP, data = trainf)

FIA.gam16 <- gam(ecoclass ~ s(MAP2011)  + s(modtmean) + s(moddeltaT) + s(moderndeltaP), data = trainf)
FIA.gam16L <- gam(ecoclass ~ MAP2011  + modtmean + moddeltaT + moderndeltaP, data = trainf)

AICf.df<- AIC(FIA.gam1, FIA.gam2, FIA.gam3, FIA.gam4, FIA.gam5, FIA.gam6, FIA.gam7, FIA.gam8, FIA.gam9,FIA.gam10,FIA.gam11,FIA.gam12,FIA.gam13,FIA.gam14 ,FIA.gam15,FIA.gam16, FIA.gam1L, FIA.gam2L, FIA.gam3L, FIA.gam4L, FIA.gam5L, FIA.gam6L, FIA.gam7L, FIA.gam8L, FIA.gam9L, FIA.gam10L,FIA.gam11L,FIA.gam12L,FIA.gam13L,FIA.gam14L,FIA.gam15L, FIA.gam16L)

#AIC.df$modeltype <- c(rep('smooth', 14), rep('linear', 14))

AICf.df$formula <- c(FIA.gam1$formula, FIA.gam2$formula, FIA.gam3$formula, FIA.gam4$formula, FIA.gam5$formula, FIA.gam6$formula, FIA.gam7$formula, FIA.gam8$formula, FIA.gam9$formula,FIA.gam10$formula,FIA.gam11$formula,FIA.gam12$formula,FIA.gam13$formula,FIA.gam14$formula ,FIA.gam15$formula , FIA.gam16L$formula, FIA.gam1L$formula, FIA.gam2L$formula, FIA.gam3L$formula, FIA.gam4L$formula, FIA.gam5L$formula, FIA.gam6L$formula, FIA.gam7L$formula, FIA.gam8L$formula, FIA.gam9L$formula, FIA.gam10L$formula,FIA.gam11L$formula,FIA.gam12L$formula,FIA.gam13L$formula, FIA.gam14L$formula, FIA.gam15L$formula, FIA.gam16L$formula )

AICf.df<- AICf.df[order(AICf.df$AIC),]
library(knitr)
library(pander)
AICf.df$model <- as.character(rownames(AICf.df))

#calculate prediction error:
msqe.f<- function(model, testdata){
  ypred <- predict(model, newdata = testdata, type="response")
  testdata$ypred <- ypred
  
  predicted<- testdata
  
  predicted$sqerr <- (predicted$FIAdensity - predicted$ypred)^2
  mean(predicted$sqerr, na.rm = TRUE)
}

AICf.df$MSPE <- 1
AICf.df$dev.expl <- 1

#add prediction error of AIC dataframe

for (i in 1: length(AICf.df$model)){
  AICf.df[i,'MSPE'] <- msqe.f(get(AICf.df[i,"model"]), testf)
  AICf.df[i,"dev.expl"] <- summary(get(AICf.df[i,"model"]))$dev.expl * 100
}

print(AICf.df)
