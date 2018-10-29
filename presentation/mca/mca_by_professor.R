#_____________________________________________________________________
# Part 1 of class 10
# create nominal variables
# Illustrate with the iris data set.
#
#_____________________________________________________________________
# Preamble ----
# created October 11, 2018.
# 
# Last edit: HA October 18, 2018.
#_____________________________________________________________________
# clean start ----
rm(list = ls())
graphics.off()
# Remember: Get there with an Rproject
#_____________________________________________________________________
# Libraries ----
library(ExPosition)
library(InPosition)
# use the last version of PTCA
# devtools::install_github('HerveAbdi/PTCA4CATA')
library(PTCA4CATA)
library(corrplot)
library(ggplot2)
# install.packages('gplots')
# also install data4PCCAR last version
devtools::install_github('HerveAbdi/data4PCCAR')
library(data4PCCAR)
#
#_____________________________________________________________________
# here we load the classic iris data set
data(iris)
# The iris data set ----
#_____________________________________________________________________
# Have a look at the iris data set
summary(iris)
#_____________________________________________________________________
#_____________________________________________________________________
# separate the measurements from the factor (design)
# resIris ----
mesIris <- iris[,1:4] # measures: SL SW PL PW
grIris <- iris[,5]    # Three types pf irises
#_____________________________________________________________________
# PCA ----
resPCA <- epPCA(DATA = mesIris,
                scale = 'SS1', # Make to use 'SS1' rather than TRUE
                DESIGN = grIris,
                graphs =  FALSE # TRUE first pass only
)
#_____________________________________________________________________
#  MCA ----
#_____________________________________________________________________
## Look at the variables ----
hist.SL <- hist(mesIris[,1], breaks = 20) # 4
hist.SW <- hist(mesIris[,2], breaks = 20) # 3
hist.PL <- hist(mesIris[,3], breaks = 20) # 3
hist.PW <- hist(mesIris[,4], breaks = 20) # 3
#_____________________________________________________________________
# recode for  MCA ----
# Initialized recoded df
mesIrisRecoded <- data.frame(row.names = row.names(mesIris))
#_____________________________________________________________________
# Sepal Length: Recode as 4 quartiles
irec = which(colnames(mesIris)=='Sepal.Length')
mesIrisRecoded[,colnames(mesIris)[irec]] <- BinQuant(
  mesIris[,irec], nClass = 4, stem = '')

irec = which(colnames(mesIris)=='Sepal.Width')
mesIrisRecoded[,colnames(mesIris)[irec]] <- BinQuant(
  mesIris[,irec], nClass = 3, stem = '')

irec = which(colnames(mesIris)=='Petal.Length')
mesIrisRecoded[,colnames(mesIris)[irec]] <- BinQuant(
  mesIris[,irec], nClass = 3, stem = '')

irec = which(colnames(mesIris)=='Petal.Width')
mesIrisRecoded[,colnames(mesIris)[irec]] <- BinQuant(
  mesIris[,irec], nClass = 3, stem = '')
#_____________________________________________________________________
# Run the MCA
#_____________________________________________________________________
# PCA ----
resMCA <- epMCA(DATA = mesIrisRecoded,
                DESIGN = grIris,
                graphs = FALSE # TRUE first pass only
)
#_____________________________________________________________________
# ctr Variables ----
# contributions for variables
ctrK <- ctr4Variables(resMCA$ExPosition.Data$cj)
#_____________________________________________________________________
# Do it ctr graph ----
# Exercise: Make a graph for the variable contributions
#_____________________________________________________________________
# Inference battery ----
resMCA.inf <- InPosition::epMCA.inference.battery(DATA = mesIris,
                                                  DESIGN = grIris,
                                                  graphs =  FALSE # TRUE first pass only
)
#_____________________________________________________________________

#_____________________________________________________________________
#  GRAPHS -----
#_____________________________________________________________________
# Pseudo Heat Map ----
corrMatBurt.list <- phi2Mat4BurtTable(mesIrisRecoded)
corr4MCA <- corrplot.mixed(as.matrix(corrMatBurt.list$phi2.mat,
                                     title = "Phi2: (squared) Correlation Map for MCA"))
a0001a.corMat.phi2 <- recordPlot()
#_____________________________________________________________________
# Pseudo Heat Map. Correlation ----
# We need correlation to compare with PCA
corrMatBurt.list <- phi2Mat4BurtTable(mesIrisRecoded)
corr4MCA.r <- corrplot.mixed(as.matrix(sqrt(corrMatBurt.list$phi2.mat),
                                       title = "Phi: Correlation Map for MCA"))
a0001b.corMat.phi <- recordPlot()
#_____________________________________________________________________
# Scree ----
PlotScree(ev = resMCA$ExPosition.Data$eigs)
a001a.screePlot <- recordPlot()
#_____________________________________________________________________
# I-set map ----
# a graph of the observations
iris.Imap <- PTCA4CATA::createFactorMap(
  title = 'MCA: Iris Data Set',
  resMCA$ExPosition.Data$fi,
  col.points = resMCA$Plotting.Data$fi.col,
  display.labels = FALSE,
  alpha.points = .5
)
#_____________________________________________________________________
# make labels ----
label4Map <- createxyLabels.gen(1,2,
                                lambda = resMCA$ExPosition.Data$eigs,
                                tau = resMCA$ExPosition.Data$t)
#_____________________________________________________________________
a002.Map.I <- iris.Imap$zeMap + label4Map
#_____________________________________________________________________
#_____________________________________________________________________
# J-set Map ----
#_____________________________________________________________________
# color 4 J ----
# create a color scheme for the variables
# Width is darker than length: Sepal is orange. Petal is red
# Variables
col4Var <- c('orange','orange4','red','red4')
# Levels
col4Levels <-  coloringLevels(rownames(resMCA$ExPosition.Data$fj), 
                              col4Var)   
#_____________________________________________________________________
axis1 = 1
axis2 = 2
# to save typing
Fj <- resMCA$ExPosition.Data$fj
#_____________________________________________________________________
# generate the set of maps
BaseMap.Fj <- createFactorMap(X = Fj , # resMCA$ExPosition.Data$fj,
                              axis1 = axis1, 
                              axis2 = axis2,
                              title = 'MCA. Variables', 
                              col.points = col4Levels$color4Levels, 
                              cex = 1,
                              col.labels = col4Levels$color4Levels, 
                              text.cex = 2.5,
                              force = 2)
#_____________________________________________________________________
# make the J-maps ----
b001.BaseMap.Fj <- BaseMap.Fj$zeMap + label4Map 
b002.BaseMapNoDot.Fj  <- BaseMap.Fj$zeMap_background +
  BaseMap.Fj$zeMap_text + label4Map 
# add Lines ----
lines4J <- addLines4MCA(Fj, col4Var = col4Var)
b003.MapJ <-  b001.BaseMap.Fj + lines4J
#_____________________________________________________________________
# Save the graphics  ----
saving.pptx <-  saveGraph2pptx(file2Save.pptx = 'Class8-9_MCA', 
                               title = "The Iris Set as MCA" , 
                               addGraphNames = TRUE)
#_____________________________________________________________________

