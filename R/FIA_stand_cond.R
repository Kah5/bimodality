# This script is looking at stand history in the FIA dataset:
# Author: kheilman

#-----------------------------------------------------------
# Does stand history explain low density sites in the FIA?
#-----------------------------------------------------------

# investigating plot condition for FIA datapoints:
FIA <- read.csv('data/FIA_species_plot_parameters_paleongrid.csv')
speciesconversion <- read.csv('data/FIA_conversion-SGD_remove_dups.csv')

FIA.pal <- merge(FIA, speciesconversion, by = 'spcd' )
FIA.by.paleon <- dcast(FIA.pal, x + y+ cell+ plt_cn ~ PalEON, sum, na.rm=TRUE, value.var = 'density') #sum all species in common taxa in FIA grid cells
FIA.by.paleon$FIAdensity <- rowSums(FIA.by.paleon[,6:25], na.rm = TRUE) # sum the total density in each plot
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y', 'cell', 'plt_cn', 'Var.5')) # melt the dataframe
fia.by.cell <- dcast(fia.melt, x + y+ cell ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell


# read in in condition tables from each state
IN <- read.csv('data/FIA_cond/IN_COND.csv')
IL <- read.csv('data/FIA_cond/IL_COND.csv')
MI <- read.csv('data/FIA_cond/MI_cond.csv')
MN <- read.csv('data/FIA_cond/MN_cond.csv')
WI <- read.csv('data/FIA_cond/WI_cond.csv')


cond <- rbind(IN, IL, MI, MN, WI)
# merge with FIA estimated densities
mergedin<- merge(FIA.by.paleon, cond[,c('PLT_CN', 'STDAGE','DSTRBCD1','STDORGCD', 'FLDSZCD','TRTCD1')], by.x = 'plt_cn', by.y = 'PLT_CN')

# ensity is somewhat correlated with stand age
ggplot(mergedin, aes(x=STDAGE, y = FIAdensity))+geom_point()
ggplot(mergedin, aes(x=FLDSZCD, y = FIAdensity))+geom_point()
ggplot(mergedin, aes(x=STDORGCD, y = FIAdensity))+geom_point()

ggplot(mergedin, aes(x=x, y = y, color = STDORGCD))+geom_point()
ggplot(mergedin, aes(x=x, y = y, color = FIAdensity))+geom_point()
ggplot(mergedin, aes(x=x, y = y, color = DSTRBCD1))+geom_point()

unique(mergedin$STDAGE)

# add fia classificaitons:
#define ecotype for modern landscape
mergedin$fiaecotype<- 'test'
mergedin[mergedin$FIAdensity > 47, ]$fiaecotype <-  "Forest"
mergedin[mergedin$FIAdensity < 47, ]$fiaecotype <-  "Savanna" 
mergedin[mergedin$FIAdensity < 0.5, ]$fiaecotype <-  "prairie"

#-----------------------------------------------------
# Do savannas have lower stand age than modern forest?
#-----------------------------------------------------

#historgrams of stand ages by forest class:
ggplot(mergedin, aes(x = STDAGE)) + geom_histogram()+facet_wrap(~fiaecotype)
# note that most savannas cluster at lower standages than forest:

ggplot(mergedin, aes(x= fiaecotype, y = STDAGE))+geom_boxplot()

# forest stand ages are significantly higher than savanna standages
t.test(mergedin$STDAGE ~ mergedin$fiaecotype)

#--------------------------------------------------------
# Are they lower ages and density due to fire or harvest?
#--------------------------------------------------------

# want TRTCD1:
#00 No observable treatment.
#10 Cutting - The removal of one or more trees from a stand.
#20 Site preparation - Clearing, slash burning, chopping, disking, bedding, or other
#practices clearly intended to prepare a site for either natural or artificial
#regeneration.
#30 Artificial regeneration - Following a disturbance or treatment (usually cutting), a new
#stand where at least 50 percent of the live trees present resulted from planting or
#direct seeding.
#40 Natural regeneration - Following a disturbance or treatment (usually cutting), a new
#stand where at least 50 percent of the live trees present (of any size) were
#established through the growth of existing trees and/or natural seeding or
#sprouting.
#50 Other silvicultural treatment - The use of fertilizers, herbicides, girdling, pruning, or
#other activities (not covered by codes 10-40) designed to improve the commercial
#value of the residual stand; or chaining, which is a practice used on woodlands to
#encourage wildlife forage.


ggplot(mergedin, aes(x = FIAdensity)) + geom_histogram()+facet_wrap(~TRTCD1)

ggplot(mergedin, aes(x = STDAGE)) + geom_histogram()+facet_wrap(~TRTCD1)

ggplot(mergedin, aes(x= TRTCD1, y = FIAdensity))+geom_boxplot()

# most places document no treatement, but some are cut...stdage an dfia dneisty dont seem to be driving this

#-------------------------------------------------------------
#Does stand origin dictate density or stand age?
#-------------------------------------------------------------

#0 = no evidence of artificial regen
#1 = evidence for artificial regenreation (planting/artificial seeding)
ggplot(mergedin, aes(x = FIAdensity)) + geom_histogram()+facet_wrap(~STDORGCD)

ggplot(mergedin, aes(x = STDAGE)) + geom_histogram()+facet_wrap(~STDORGCD)


storigin <- ddply(mergedin[, c("x", "y", "cell", "STDAGE", "STDORGCD", 'DSTRBCD1', 'FIAdensity', "TRTCD1")],~STDORGCD,summarise,meandens=mean(FIAdensity),sddens=sd(FIAdensity), meanstdage = mean(STDAGE), sdstdage = sd(STDAGE))
dstrub <- ddply(mergedin[, c("x", "y", "cell", "STDAGE", "STDORGCD", 'DSTRBCD1', 'FIAdensity', "TRTCD1")],~DSTRBCD1,summarise,meandens=mean(FIAdensity),sddens=sd(FIAdensity), meanstdage = mean(STDAGE), sdstdage = sd(STDAGE))
TRT <- ddply(mergedin[, c("x", "y", "cell", "STDAGE", "STDORGCD", 'DSTRBCD1', 'FIAdensity', "TRTCD1")],~TRTCD1,summarise,meandens=mean(FIAdensity),sddens=sd(FIAdensity), meanstdage = mean(STDAGE), sdstdage = sd(STDAGE))



ggplot(mergedin, aes(x= STDORGCD, y = FIAdensity)) + geom_bar()
