#this script investigates different Crown area DBH relationships
#utlizing the Forest Health Monitoring 1991-1999 datasets
#this is different than Bechtold allometries because wer
#are using open and stand grown trees.
library(plyr)
library(ggplot2)
library(Hmisc)

#load in data from 1999 (last) Forest Health Monitoring plot surveys
#these were the last surveys where DBH & crown width were both measured

#here we use only those surveys in IL, MN, IN, WI, and MI, since this is where the PLS data cover

IL1999<- read.csv("./data/FHM_data/IL_TREE_1999.CSV")
MN1999<- read.csv("./data/FHM_data/MN_TREE_1999.CSV")
IN1999 <- read.csv("./data/FHM_data/IN_TREE_1999.CSV")
WI1999 <- read.csv("./data/FHM_data/WI_TREE_1999.CSV")
MI1999 <- read.csv("./data/FHM_data/MI_TREE_1999.CSV")

#only keep relevant columns
keeps <- c("FHM_SPECIES", "SPECIES_COMMON_NAME", "DBH", "DBH_CHECK", "CROWN_DIAMETER_90", 
           "CROWN_DIAMETER_WIDE", "FOREST_TYPE")


IL1999 <- IL1999[,keeps]
IN1999 <- IN1999[,keeps]
MN1999 <- MN1999[,keeps]
WI1999 <- WI1999[,keeps]
MI1999 <- MI1999[,keeps]
#merge all datasets together
mw1999 <- rbind(IL1999, 
                IN1999, 
                MN1999, 
                WI1999, 
                MI1999)
#look at species
summary(mw1999$SPECIES_COMMON_NAME)
#mw1999<- mw1999[!summary(mw1999$SPECIES_COMMON_NAME)<2,]

#capitalize the first character in the name
mw1999$SPECIES_COMMON_NAME<- capitalize(as.character(mw1999$SPECIES_COMMON_NAME))
#double check the FHM- paleon taxa conversion
FHM_spec2 <- data.frame(unique(mw1999$SPECIES_COMMON_NAME))
#colnames(FHM_spec2) <- "SPECIES_COMMON_NAME"
#write.csv(FHM_spec2, "FHM_spec2.csv")

#created a lookup table converting FHM species to paleon species
lookup <- read.csv("FHM_spec2.csv")
mw1999<- join(mw1999, lookup, by= 'SPECIES_COMMON_NAME')
mw1999$CROWN_DIAMETER_90 <- mw1999$CROWN_DIAMETER_90*0.3048 #convert ft to m
mw1999$CROWN_DIAMETER_WIDE <- mw1999$CROWN_DIAMETER_WIDE*0.3048
mw1999$AVG_CROWN_DIAMETER <- (mw1999$CROWN_DIAMETER_90 + mw1999$CROWN_DIAMETER_WIDE)/2
mw1999$DBH <- mw1999$DBH*2.54 #convert in to cm
#need to create a lookup table converting FHM spcies to paleon


#Fit a simple crown model for the allometry first, not species specific
#LCW = b0 + b1* DBH
full.lm <- lm(mw1999$AVG_CROWN_DIAMETER ~ mw1999$DBH)

#remove NAs for DBH and CROWn diameters
mw1999 <- mw1999[complete.cases(mw1999),]
library("data.table")

mw1999 <- data.table(mw1999, key = "Paleon")

#################
#create generic model function to create the linear model allometries
model <- function(x){
  lm(AVG_CROWN_DIAMETER ~ DBH, data=x)
}

#example of allometry for single species type
fit <- model(mw1999[mw1999$Paleon=="Oak" ,])

#loop over all subsets of species
fitted.crown.allom <- dlply(mw1999, .(Paleon), model)

#gives the coefficients, fstat, sigma, and r-squared for the simple models
species.coef <- ldply(fitted.crown.allom, coef)
species.fstat <- ldply(fitted.crown.allom, function(x) summary(x)$fstatistic )
colnames(species.fstat) <- c("Paleon", "F-stat", "numdf", "dendf")
species.sigma <- ldply(fitted.crown.allom, function(x) summary(x)$sigma)

#r-squared value for the model
species.rsq <- ldply(fitted.crown.allom, function(x) summary(x)$r.squared)

spec.lms <- cbind(species.coef, 
                  r.squared = species.rsq[,2], 
                  species.fstat[,2:4], 
                  sigma = species.sigma[,2])

#save to csv
write.csv(spec.lms, "data/outputs/Species_crown_allometry.csv")
##plot the crown diameters using ggplot

#save just the coefficients
write.csv(species.coef, "data/FHM_paleon_crown_allometry_coeff.csv")



#plot each species specific relationship with the linear model:
crown.allom <- ggplot(data = mw1999, aes(x = DBH, y = AVG_CROWN_DIAMETER, color = Paleon))+geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)+
  ylim (0, 25)+ xlab('DBH (cm)') + ylab('largest crown diameter (m)')+
  facet_wrap(~Paleon) 


ggplot(data = mw1999, aes(x = DBH, y = AVG_CROWN_DIAMETER, color = Paleon))+geom_point()+
  geom_smooth(method = "lm", se=FALSE,  formula = y ~ x)+
  ylim (0, 75)+ xlab('DBH (cm)') + ylab('largest crown diameter (m)')

#save to pdf
pdf("Species_crown_allom.pdf")
crown.allom
dev.off()


#crown model from summer 2015 savanna trees
trees <- read.csv("tree_cores_data_sheet.csv", stringsAsFactors = FALSE)
trees <- trees[,c("Species", "CW.1..m.", "CW.2..m.", "DBH..cm.")]
trees<- trees[complete.cases(trees),]
trees$DBH <- as.numeric(trees$DBH..cm.)
#take mean crown area of the two crown measurements
trees$CA <- rowMeans(trees[,c("CW.1..m.", "CW.2..m.")], na.rm=TRUE)

#need to make a funciton that has same column names as our data
model2 <- function(x){
  lm(CA ~ DBH, data=x)
}

fit2 <- model2(trees)

#loop over all subsets of species
fitted.crown.allom.sav <- dlply(trees, .(Species), model2)

#gives the coefficients, fstat, sigma, and r-squared for the simple models
species.coef.sav <- ldply(fitted.crown.allom.sav, coef)
species.fstat.sav <- ldply(fitted.crown.allom.sav, function(x) summary(x)$fstatistic )
colnames(species.fstat.sav) <- c("Species", "F-stat", "numdf", "dendf")
species.sigma.sav <- ldply(fitted.crown.allom.sav, function(x) summary(x)$sigma)

#r-squared value for the model
species.rsq.sav <- ldply(fitted.crown.allom.sav, function(x) summary(x)$r.squared)

crown.allom.sav <- ggplot(data = trees, aes(x = DBH, y = CA, color = Species))+stat_summary()+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)+
  ylim (0, 40)+ xlab('DBH (cm)') + ylab('largest crown diameter (m)')+
  facet_wrap(~Species) 



compare.allom <- ggplot()+stat_smooth(data = trees, aes(x = DBH, y = CA, color = "black"))+
  #geom_smooth(method = "lm", se=FALSE,  formula = y ~ x)+
  ylim (0, 25)+ xlab('DBH (cm)') + ylab('largest crown diameter (m)')+
  stat_smooth( data =mw1999, aes(x = DBH, y = CROWN_DIAMETER_90, color = "red") )+
scale_colour_manual(name = 'Allometry', 
                    values =c('black'='black','red'='red'), labels = c('Field','FHM'))
  #geom_smooth(method = "lm", se=FALSE,  formula = y ~ x)

#+
  facet_wrap(~Species) 

#this doesnt work for some reason
spec.lms.sav <- cbind(species.coef.sav, 
                  r.squared = species.rsq.sav[,2], 
                  species.fstat.sav[,2:4], 
                  sigma = species.sigma.sav[,2])

#save to csv
write.csv(spec.lms, "data/outputs/Species_crown_allometry.csv")
