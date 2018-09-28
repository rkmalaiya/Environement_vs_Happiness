# Class 4 & 5: playing with the IRIS data set
# Factor Maps with Groups:
# Compute and display group means, confidence intervals ,and convex hulls
# Current version is September 25,  2018.
## clean start

rm(list = ls())
graphics.off()
# Get there with an Rproject
# if not an Rproject set the directory here
# e.g.,
# setwd(~/Box Sync/RM3-CurrentYear/R4RM3/r-Class04/'')

library(ExPosition)
library(InPosition)
# use the last version of PTCA
# devtools::install_github('HerveAbdi/PTCA4CATA')

library(PTCA4CATA)
library(corrplot)
library(ggplot2)
# install.packages('gplots')
# also install data4PCCAR
# devtools::install_github('HerveAbdi/data4PCCAR')
library(data4PCCAR)

#
#here we load the classic iris data set
data(iris)
# The iris data set ----
#_____________________________________________________________________
# Have a look at the iris data set
summary(iris)
#_____________________________________________________________________
# separate the measurements from the factor (design)
# resIris ----
mesIris <- iris[,1:4] # measures: SL SW PL PW
grIris <- iris[,5] # Three types pf irises
#_____________________________________________________________________
# PCA ----
resPCA <- epPCA(DATA = mesIris,
                scale = 'SS1', # Make to use 'SS1' rather than TRUE
                DESIGN = grIris,
                graphs =  FALSE # TRUE first pass only
)
#_____________________________________________________________________
# Varimax here ----
testVari    <- data4PCCAR::epVari(resPCA)
#_____________________________________________________________________
# Inference battery ----
resPCA.inf <- InPosition::epPCA.inference.battery(DATA = mesIris,
                                                  scale = 'SS1', # Make sure to use 'SS1' rather than TRUE
                                                  DESIGN = grIris,
                                                  graphs =  FALSE # TRUE first pass only
)
#_____________________________________________________________________
# Group Analysis ----
# Bootstrap ----
# Confidence Intervals ----
# Bootstrap for CI:
BootCube.Gr <- PTCA4CATA::Boot4Mean(resPCA$ExPosition.Data$fi, 
                                    design = grIris,
                                    niter = 100,
                                    suppressProgressBar = TRUE)
#_____________________________________________________________________
# Bootstrap ratios ----
bootRatios.Gr <- boot.ratio.test(BootCube.Gr$BootCube)

#*********************************************************************
# eigenvalues: MonteCarlo Approach ----
# 
random.eigen <- data4PCCAR::monteCarlo.eigen(X = mesIris, nIter = 100)
#
# eigenvalues: Bootstrap approach
#
bootstrap.eigen <- data4PCCAR::boot.eigen(mesIris, nIter = 100)
# End of computation ---- 
#_____________________________________________________________________
# Graphics star here
#_____________________________________________________________________
# corrplot ----
# First a correlation plot
cor.iris <- cor(mesIris)
corrplot.mixed(round(cor.iris, 2), lower = 'number',
               upper = 'ellipse', )
a001.corrMap <- recordPlot()
#_____________________________________________________________________
# Scree ----
PlotScree(ev = resPCA$ExPosition.Data$eigs)
a001a.screePlot <- recordPlot()
#_____________________________________________________________________
# I-set map ----
# a graph of the observations
iris.Imap <- PTCA4CATA::createFactorMap(
  resPCA$ExPosition.Data$fi,
  col.points = resPCA$Plotting.Data$fi.col,
  display.labels = FALSE,
  alpha.points = .5
)

#_____________________________________________________________________
# make labels ----
label4Map <- createxyLabels.gen(1,2,
                                lambda =resPCA$ExPosition.Data$eigs,
                                tau = resPCA$ExPosition.Data$t)
#_____________________________________________________________________
a002.Map.I <- iris.Imap$zeMap + label4Map
#_____________________________________________________________________
# J-set Map ----
#_____________________________________________________________________
# color 4 J
# create a color scheme for the variables
# Width is darker than length: Sepal is orange. Petal is red
col4Var <- c('orange','orange4','red','red4')
#_____________________________________________________________________
# A graph for the J set ----
baseMap.j <- PTCA4CATA::createFactorMap(resPCA$ExPosition.Data$fj,
                                        col.points   = col4Var,
                                        alpha.points =  .3,
                                        col.labels   = col4Var)
#_____________________________________________________________________
# arrows
zeArrows <- addArrows(resPCA$ExPosition.Data$fj, color = col4Var)
# A graph for the J-set
b000.aggMap.j <- baseMap.j$zeMap_background + # background layer
  baseMap.j$zeMap_dots + baseMap.j$zeMap_text +  # dots & labels
  label4Map + zeArrows
# We print this Map with the following code
dev.new()
print(b000.aggMap.j)
#_____________________________________________________________________
# Contribution Plots ----
# get the Contributions and make a plot.
#_____________________________________________________________________
# Here we look only at the (signed) contributions for the variables
# compute the signed contributions
signed.ctrJ <- resPCA$ExPosition.Data$cj * sign(resPCA$ExPosition.Data$fj)
# Contribution # 1 & 2
#_____________________________________________________________________
#
b003.ctrJ.s.1 <- PrettyBarPlot2(signed.ctrJ[,1],
                                threshold = 1 / NROW(signed.ctrJ),
                                font.size = 5,
                                color4bar = gplots::col2hex(col4Var), # we need hex code
                                main = 'PCA on the Iris Set: Variable Contributions (Signed)',
                                ylab = 'Contributions',
                                ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)
print(b003.ctrJ.s.1)
# 
b004.ctrJ.s.2 <- PrettyBarPlot2(signed.ctrJ[,2],
                                threshold = 1 / NROW(signed.ctrJ),
                                font.size = 5,
                                color4bar = gplots::col2hex(col4Var), # we need hex code
                                main = 'PCA on the Iris Set: Variable Contributions (Signed)',
                                ylab = 'Contributions',
                                ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)
print(b004.ctrJ.s.2)
#_____________________________________________________________________
#
#_____________________________________________________________________
# Scree + Inference----
PlotScree(ev = resPCA$ExPosition.Data$eigs, 
          p.ev =  resPCA.inf$Inference.Data$components$p.vals,
          title = 'Iris Set. Eigenvalues Inference',
          plotKaiser = TRUE
)
b004a.screePlot.Inf <- recordPlot()
#_____________________________________________________________________
# I-set map ----
# Permutation test for eigen-values
# Permutation tests graph ----
# Look at first eigenvalue
zeDim = 1
pH1 <- prettyHist(
  distribution = resPCA.inf$Inference.Data$components$eigs.perm[,zeDim], 
  observed = resPCA.inf$Fixed.Data$ExPosition.Data$eigs[zeDim], 
  xlim = c(0, 4.5), # needs to be set by hand
  breaks = 20,
  border = "white", 
  main = paste0("Permutation Test for Eigenvalue ",zeDim),
  xlab = paste0("Eigenvalue ",zeDim), 
  ylab = "", 
  counts = FALSE, 
  cutoffs = c( 0.975))
b005.PermTest <- recordPlot()
# # Test prettyHist2
# source('~/Dropbox/SortingWithDistatisR/ptca/test4ptca/prettyHist2.R')
# pH1.2 <- prettyHist2(
#   distribution = as.data.frame(resPCA.inf$Inference.Data$components$eigs.perm[,zeDim]), 
#   observed = resPCA.inf$Fixed.Data$ExPosition.Data$eigs[zeDim], 
#   xlim = c(0, 4.5), # needs to be set by hand
#   breaks = 20,
#   border = "white", 
#   main = paste0("Permutation Test for Eigenvalue ",zeDim),
#   xlab = paste0("Eigenvalue ",zeDim), 
#   ylab = "", 
#   counts = FALSE, 
#   cutoffs = c( 0.975))
# dev.new()
# print(pH1.2)
##_____________________________________________________________________
# Parallel test for eigen-values
# Parallel tests graph ----
# Look at first eigenvalue
pH1.p <- prettyHist(random.eigen$rand.eigs[,zeDim], 
                    observed = random.eigen$fixed.eigs[zeDim], 
                    xlim = c(0, 4.5), # needs to set by hand
                    breaks = 20,
                    border = "white", 
                    main = paste0("Monte Carlo (Parallel) Test for Eigenvalue ",zeDim),
                    xlab = paste0("Eigenvalue ",zeDim), 
                    ylab = "", 
                    counts = FALSE, 
                    cutoffs = c( 0.975))
#
b006.ParalTest <- recordPlot()
##_____________________________________________________________________
# Bootstrap test for eigen-values
# Bootstrap eigen ----
# Look at first eigenvalue
pH1.p <- prettyHist(bootstrap.eigen$boot.eigs[,zeDim], 
                    observed = random.eigen$fixed.eigs[zeDim], 
                    xlim = c(0, 4.5), # needs to set by hand
                    breaks = 20,
                    border = "white", 
                    main = paste0("Bootstrapped distribution for Eigenvalue ",zeDim),
                    xlab = paste0("Eigenvalue ",zeDim), 
                    ylab = "", 
                    counts = FALSE, 
                    cutoffs = c(0.025, 0.975))
b007.BootTest <- recordPlot()
#
#_____________________________________________________________________
# bootstrap ratios ----
#
BR <- resPCA.inf$Inference.Data$fj.boots$tests$boot.ratios
laDim = 1
ba001.BR1 <- PrettyBarPlot2(BR[,laDim],
                            threshold = 2,
                            font.size = 5,
                            color4bar = gplots::col2hex(col4Var), # we need hex code
                            main = paste0(
                              'PCA on the Iris Set: Bootstrap ratio ',laDim),
                            ylab = 'Bootstrap ratios'
                            #ylim = c(1.2*min(BR[,laDim]), 1.2*max(BR[,laDim]))
)
print(ba001.BR1)
#
laDim = 2
ba002.BR2 <- PrettyBarPlot2(BR[,laDim],
                            threshold = 2,
                            font.size = 5,
                            color4bar = gplots::col2hex(col4Var), # we need hex code
                            main = paste0(
                              'PCA on the Iris Set: Bootstrap ratio ',laDim),
                            ylab = 'Bootstrap ratios'
)
print(ba002.BR2)
# Still to be done 
# Scree with parallel, permutation, bootstrap
#


#_____________________________________________________________________
# Varimax graphs-----
#_____________________________________________________________________

labels4Vari <- PTCA4CATA::createxyLabels.gen(1,2,
                                             lambda = testVari$rotated.eigs,
                                             tau = testVari$rotated.t)
baseMap.j.rot <- PTCA4CATA::createFactorMap(testVari$rotated.J,
                                            col.points   = col4Var,
                                            alpha.points =  .3,
                                            col.labels   = col4Var,
                                            title = 'Loadings Post Varimax')
# arrows
zeArrows.rot <- addArrows(testVari$rotated.J, color = col4Var)
# A graph for the J-set
c001.aggMap.J.rot <- baseMap.j.rot$zeMap_background + # background layer
  baseMap.j.rot$zeMap_dots +
  baseMap.j.rot$zeMap_text +  # dots & labels
  zeArrows.rot + labels4Vari
dev.new()
print(c001.aggMap.J.rot)

# Plot the Rotated observations too
# a graph of the observations
iris.Imap.rot <- PTCA4CATA::createFactorMap(
  testVari$rotated.I,
  col.points = resPCA$Plotting.Data$fi.col,
  display.labels = FALSE,
  alpha.points = .5,
  title = 'Factor Scores Post Varimax'
)
c002.Map.I.rot <- iris.Imap.rot$zeMap + labels4Vari
dev.new()

print(c002.Map.I.rot)
#_____________________________________________________________________
#_____________________________________________________________________
# Use the factor scores from resPCA. 
# The groups for the irises is in grIris
#_____________________________________________________________________
# Mean Map
#  create the map for the means
#  get the means by groups
IrisMeans <- PTCA4CATA::getMeans(resPCA$ExPosition.Data$fi, grIris)
# a vector of color for the means
col4Iris <- resPCA$Plotting.Data$fi.col
col4Means <- unique(col4Iris)
# the map
MapGroup <- PTCA4CATA::createFactorMap(IrisMeans,
                                       # use the constraint from the main map
                                       constraints = iris.Imap$constraints,
                                       col.points = col4Means,
                                       cex = 7,  # size of the dot (bigger)
                                       col.labels = col4Means,
                                       text.cex = 6)
# The map with observations and group means
a003.Map.I.withMeans <- a002.Map.I +
  MapGroup$zeMap_dots + MapGroup$zeMap_text
print(a003.Map.I.withMeans)
#_____________________________________________________________________
# Create the ellipses
# Bootstrapped CI ----
#_____________________________________________________________________
# Create Confidence Interval Plots
# use function MakeCIEllipses from package PTCA4CATA
GraphElli <- PTCA4CATA::MakeCIEllipses(BootCube.Gr$BootCube[,1:2,],
                                       names.of.factors = c("Dimension 1","Dimension 2"),
                                       col = col4Means,
                                       p.level = .95
)
#_____________________________________________________________________
# create the I-map with Observations, means and confidence intervals
#
a004.Map.I.withCI <-  a002.Map.I + MapGroup$zeMap_text +  GraphElli
#_____________________________________________________________________
# plot it!
dev.new()
print(a004.Map.I.withCI)
#_____________________________________________________________________
# Tolerance Intervals ----
# use function MakeToleranceIntervals from package PTCA4CATA
GraphTI.Hull <- PTCA4CATA::MakeToleranceIntervals(resPCA$ExPosition.Data$fi,
                                                  design = as.factor(grIris),
                                                  # line below is needed
                                                  names.of.factors =  c("Dim1","Dim2"), # needed 
                                                  col = col4Means,
                                                  line.size = .50, 
                                                  line.type = 3,
                                                  alpha.ellipse = .2,
                                                  alpha.line    = .4,
                                                  p.level       = .75)
#_____________________________________________________________________
# Create the map:
a005.Map.I.withTIHull <-a002.Map.I  +
  GraphTI.Hull + MapGroup$zeMap_dots +
  MapGroup$zeMap_text + MapGroup$zeMap_dots
#_____________________________________________________________________
# plot it
dev.new()
print(a005.Map.I.withTIHull)
#_____________________________________________________________________
# End of graphs ----
#_____________________________________________________________________
# Save as Powerpoint ----
#_____________________________________________________________________
#  
listSaved <- saveGraph2pptx(
  file2Save.pptx = 'PCA4TheIrisDataSet.pptx', 
  title = "The Iris data set: PCA, confidence and tolerance intervals", 
  addGraphNames = TRUE)

