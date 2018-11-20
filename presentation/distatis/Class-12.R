#=====================================================================
# Script for R-RM3: Class 12. Part 1.
#  11/13/2017
# Program:
# DISTATIS etc.
# kmeans and cluster analysis with graphs
#
#=====================================================================

# A clean start
rm(list = ls())
graphics.off()

# You will need to install the package called PTCA4CATA
# which is currently in development.
# The last version of packages or packages in development
# live in Github rather than in the CRAN.
# To install a package that lives in Github
# you need to used the function install_github wehich is part of
# the package devtools()
# if devtools is not installed on your machine,
# you first need to install it with
# install.packages(devtools)
# then you need to decomment the following line and execute it:
# devtools::install_github('HerveAbdi/PTCA4CATA', dependencies = TRUE)
# this will install the PTCA4CATA package
# This line install the last version of flextable, (table for officer)
# devtools::install_github("davidgohel/flextable")
# devtools::install_github("davidgohel/officer")
# devtools::install_github("davidgohel/rvg")
# install.packages('TInPosition')

# If DistatisR is not yet installed, you will need to install it
install.packages('DistatisR')

file2Save <- 'BeersWithDistatis.pptx'
#       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#        You need to change this line
# load the libraries that we will need
# some of them maybe new: if so use install.packages())
suppressMessages(library(PTCA4CATA))
# PTCA4CATA should first to avoid conflict with TInPosition
suppressMessages(library(ExPosition))
#suppressMessages(library(InPosition))
#suppressMessages(library(TInPosition))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(officer))
suppressMessages(library(flextable))
suppressMessages(library(rvg))
suppressMessages(library(useful))
suppressMessages(library(DistatisR))
# other libraries here


# Today Distatis
# Beer Example Today
#---------------------------------------------------------------------
#  1. Get the data from the 2007 sorting example
#     from  Abdi et al. (2007). ref: A.59
data("beers2007")
BoysGirls = c('f','m','f','f','m','m','m','m','f','m')
Judges <- paste0(BoysGirls,1:length(BoysGirls))
Sort <- beers2007$data

#---------------------------------------------------------------------
# 2. Create the set of distance matrices
#  (one distance matrix per assessor)
#    (use the function DistanceFromSort)
DistanceCube <- DistatisR::DistanceFromSort(Sort)


#---------------------------------------------------------------------
# 3. Call the DISTATIS routine with the cube of distance as parameter
testDistatis <- DistatisR::distatis(DistanceCube)
# The factor scores for the beers are in
# testDistatis$res4Splus$F
# the factor scores for the assessors are in (RV matrice)
#  testDistatis$res4Cmat$G


#---------------------------------------------------------------------
# 4. Inferences on the beers obtained via bootstrap
# 4.1 Get the bootstrap factor scores (with default 1000 iterations)
BootF <- BootFactorScores(testDistatis$res4Splus$PartialF)
#
#---------------------------------------------------------------------
#*********************************************************************
# End of computations
# We have now in testDistatis
#1 "$res4Cmat"  "Results from the C matrix (see notation)"
#2 "$res4Splus" "Results from the S+ matrix (see notation)"
#

#---------------------------------------------------------------------
# 5. The graphs
#  5.1 First the C matrix
# Look at the scree to start with
#
ev4C <- testDistatis$res4Cmat$eigValues
Scree.1 <-  PlotScree(ev = ev4C,
          p.ev = NULL, max.ev = NULL, alpha = 0.05,
          col.ns = "#006D2C", col.sig = "#54278F",
          title = "RV-mat: Explained Variance per Dimension")
zeScree.Rv <- recordPlot()
# Plot the assessor matrix
G <- testDistatis$res4Cmat$G
# Create a color scheme for the judges
col4B <- 'turquoise3'
col4G <- 'thistle3'
col4Judges <- rep(col4G,length(BoysGirls))
col4Judges[BoysGirls == 'm'] <- col4B
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# A graph for the Judges set
baseMap.j <- PTCA4CATA::createFactorMap(G,
                                        title = 'The Rv map',
                                        col.points   = col4Judges,
                                        alpha.points =  .3,
                                        col.labels = col4Judges)
# A graph for the J-set
aggMap.j <- baseMap.j$zeMap_background + # background layer
           baseMap.j$zeMap_dots + baseMap.j$zeMap_text # dots & labels
# We print this Map with the following code
dev.new()
print(aggMap.j)

#---------------------------------------------------------------------
# Means Boys and Girls
#---------------------------------------------------------------------
# Add a convexHull
#---------------------------------------------------------------------
# Create 100% Tolerance interval polygons
#
GraphTJ.Hull.100 <- MakeToleranceIntervals(G,
                                  as.factor(BoysGirls),
                                  names.of.factors = c("Dim1","Dim2"),
                                  col = unique(col4Judges),
                                          line.size = .5,
                                          line.type = 3,
                                          alpha.ellipse = .1,
                                          alpha.line = .4,
                                          p.level = 1, # full Hulls
                                          type = 'hull' #
                                          # use 'hull' for convex hull
)
#---------------------------------------------------------------------
# Create the map
aggMap.j.withHull <- baseMap.j$zeMap_background + # background layer
                           baseMap.j$zeMap_dots + GraphTJ.Hull.100
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Plot it!
dev.new()
print(aggMap.j.withHull)
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Plot for the group means computed on the factor scores
#---------------------------------------------------------------------
# First compute the means
JudgesMeans.tmp <- aggregate(G, list(BoysGirls), mean) # compute the means
JudgesMeans <- JudgesMeans.tmp[,2:ncol(JudgesMeans.tmp )] # drop var 1
rownames(JudgesMeans) <- JudgesMeans.tmp[,1] # use var 1 to name the groups
#---------------------------------------------------------------------
# a vector of color for the means
col4Means <- unique(col4Judges)
#---------------------------------------------------------------------
# create the map for the means
MapGroup    <- PTCA4CATA::createFactorMap(JudgesMeans,
                                  axis1 = 1, axis2 = 2,
                                  constraints = baseMap.j$constraints,
                                          title = NULL,
                                          col.points = col4Means,
                                          display.points = TRUE,
                                          pch = 19, cex = 5,
                                          display.labels = TRUE,
                                          col.labels = col4Means,
                                          text.cex = 4,
                                          font.face = "bold",
                                          font.family = "sans",
                                          col.axes = "darkorchid",
                                          alpha.axes = 0.2,
                                          width.axes = 1.1,
                          col.background = adjustcolor("lavender",
                                             alpha.f = 0.2),
                                          force = 1, segment.size = 0)
# The map with observations and group means
aggMap.j.withMeans <- aggMap.j.withHull +
                    MapGroup$zeMap_dots + MapGroup$zeMap_text
#---------------------------------------------------------------------
# plot it!
dev.new()
print(aggMap.j.withMeans)
#---------------------------------------------------------------------
# Go for the beers now
# First we fix a bit of shamefull absentmindness:
#   The eigenvalues of the compromise matrix are not available
#   in DistatisR.
#   So we recompute them here
ev4S <- eigen(testDistatis$res4Splus$Splus,
                symmetric = TRUE, only.values = TRUE)$values
# A scree for the compromise
Scree.S <-  PlotScree(ev = ev4S,
                   p.ev = NULL, max.ev = NULL, alpha = 0.05,
                   col.ns = "#006D2C", col.sig = "#54278F",
                   title = "S-mat: Explained Variance per Dimension")
zeScree.S <- recordPlot()
#---------------------------------------------------------------------
Fi <- testDistatis$res4Splus$F
col4Beers <- prettyGraphsColorSelection(nrow(Fi))
#  Use colors from prettyGraphs
#---------------------------------------------------------------------
# Graphs for the I set
#---------------------------------------------------------------------
# Create the base map
constraints4Fi <- lapply(minmaxHelper(Fi),'*',1.2)
baseMap.i <- PTCA4CATA::createFactorMap(Fi,
                                        col.points = col4Beers,
                                        col.labels = col4Beers,
                        constraints = constraints4Fi,
                                        alpha.points =  .4)
#---------------------------------------------------------------------
# We are  interested about the labels here
# so we will use dots and labels
#---------------------------------------------------------------------
# Plain map with color for the I-set
aggMap.i <- baseMap.i$zeMap_background + baseMap.i$zeMap_dots +
                                         baseMap.i$zeMap_text
#---------------------------------------------------------------------
# print this Map
dev.new()
print(aggMap.i)
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Create Confidence Interval Plots
# use function MakeCIEllipses from package PTCA4CATA
#
constraints4Fi <- lapply(minmaxHelper(Fi),'*',1.2)
GraphElli <- MakeCIEllipses(BootF[,1:2,],
                      names.of.factors = c("Factor 1","Factor 2"),
                      alpha.line = .5,
                      alpha.ellipse = .3,
                      line.size = .5,
                      line.type = 3,
                      col = col4Beers,
                      p.level = .95 )
#---------------------------------------------------------------------
# create the I-map with Observations and their confidence intervals
#
aggMap.i.withCI <-  aggMap.i +  GraphElli + MapGroup$zeMap_text
#---------------------------------------------------------------------
# plot it!
dev.new()
print(aggMap.i.withCI)
#---------------------------------------------------------------------
# Old graph with links to partial factor scores
# Not that informative for sorting tasks
# Change names of the assessors
partF <- testDistatis$res4Splus$PartialF
dimnames(partF)[[3]] <- as.character(1:dim(partF)[3])
PartialF <- GraphDistatisPartial(FS = testDistatis$res4Splus$F,
                     PartialFS = partF,
                     axis1 = 1, axis2 = 2, constraints = NULL,
                     item.colors = col4Beers,
                     participant.colors = NULL,
                     ZeTitle = "Distatis-Partial",
                     Ctr=NULL, color.by.observations = TRUE,
                     nude = FALSE, lines = TRUE)
# save the graphs in F.and.PartialF
F.and.PartialF <- recordPlot()
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#*********************************************************************
# Some classification now
# First plain k-means
set.seed(42)
beers.kMeans <- kmeans(x = Fi , centers = 3)
#---------------------------------------------------------------------
# Now to get a map by cluster:
col4Clusters  <- createColorVectorsByDesign(
              makeNominalData(
              as.data.frame(beers.kMeans$cluster)  ))

#=====================================================================
#---------------------------------------------------------------------
# Graphs for the I set
#---------------------------------------------------------------------
# Create the base map
# constraints4Fi <- lapply(minmaxHelper(Fi),'*',1.2)
baseMap.i.km <- PTCA4CATA::createFactorMap(Fi,
                                        col.points = col4Clusters$oc,
                                        col.labels = col4Clusters$oc,
                                        constraints = constraints4Fi,
                                        alpha.points =  .4)
#---------------------------------------------------------------------
# We are  interested about the labels here
# so we will use dots and labels
#---------------------------------------------------------------------
# Plain map with color for the I-set
aggMap.i.km <- baseMap.i.km$zeMap_background +
  baseMap.i.km$zeMap_dots + baseMap.i.km$zeMap_text
# print
dev.new()
print(aggMap.i.km)
#---------------------------------------------------------------------
# Add the cluster names
#---------------------------------------------------------------------
# get the color order in the c=good order
col4C <- col4Clusters$gc[sort(rownames(col4Clusters$gc),
                                     index.return = TRUE)$ix]
# create the map for the means
map4Clusters    <- PTCA4CATA::createFactorMap(beers.kMeans$centers,
                                          axis1 = 1, axis2 = 2,
                                          constraints = constraints4Fi,
                                          title = NULL,
                                          col.points = col4C,
                                          display.points = TRUE,
                                          pch = 19, cex = 5,
                                          display.labels = TRUE,
                                          col.labels = col4C,
                                          text.cex = 6,
                                          font.face = "bold",
                                          font.family = "sans",
                                          col.axes = "darkorchid",
                                          alpha.axes = 0.2,
                                          width.axes = 1.1,
                                          col.background =
                          adjustcolor("lavender", alpha.f = 0.2),
                                          force = 1, segment.size = 0)
# The map with observations and group means
aggMap.i.withCenters <- aggMap.i.km  +
               map4Clusters$zeMap_dots + map4Clusters$zeMap_text
#
dev.new()
print(aggMap.i.withCenters)
#---------------------------------------------------------------------
#*********************************************************************
# A cluster analysis
beer.hc <- hclust(d = dist(Fi),
                  method = 'ward.D2' )

plot.tree <- plot(beer.hc,  main = "Beers. Ward's method")
hc.tree <- recordPlot()
dev.new()
print(hc.tree)
#---------------------------------------------------------------------
# Use three clusters too
hc.3.cl <- rect.hclust(beer.hc, k = 3,
               border = c('darkorchid',
                          'darkolivegreen4','darkgoldenrod3')
                          )
hc.tree.3c <- recordPlot()
dev.new()
print(hc.tree.3c)

# Optimal number of clustera
#  install.packages('useful')
best.beers <- useful::FitKMeans(Fi, max.clusters = 5,
                                seed = 314)
print(best.beers) # when Hartigan parameter > 10 => add a cluster
dev.new()
plot.harti <- useful::PlotHartigan(best.beers)
print(plot.harti)
# here effect of small N. go for 4 clusters ?

#---------------------------------------------------------------------
# Below we illustrate the use of the officer package
#  to save results in a powerpoint file (with editable graphs)
#  we also need the rvg and the flextable packages (see preamble)
#  These pacakges are very new and  rapidly evolving
#  so it is better to install them from git_hub
#

#---------------------------------------------------------------------
doc <- read_pptx() # Create the pptx file
#---------------------------------------------------------------------
# create a General Title
doc <- add_slide(doc, layout = "Title Only", master = "Office Theme")
doc <- ph_with_text(doc, type = 'title',
                    str =  'Nice Graphs with DisTATIS')
# #---------------------------------------------------------------------
# # A new Slide with text and a table
# # Prepare the table to be stored in the pptx file
# # We need to add a column with the rownames
# # because flextable does not print it
# tabIris <- cbind(rownames(IrisMeans.var ), IrisMeans.var )
# colnames(tabIris)[1] <- 'Species'
# # create the pptx table
# tabIris4pptx <- theme_booktabs(autofit(regulartable(tabIris) ))
# # alternative style: theme_vanilla(regulartable(tabIris) )
# # store the table
# doc <- add_slide(doc, layout = "Title and Content",
#                  master = "Office Theme")
# doc <- ph_with_text(doc, type = 'title',
#                     str =  'Mean Values per Species') # The title
# doc <- ph_with_flextable(doc, value = tabIris4pptx, type = "body")
# #---------------------------------------------------------------------
# A new Slide with text and a prettyPlot figure
doc <- add_slide(doc, layout = "Title and Content",
                 master = "Office Theme")
doc <- ph_with_text(doc, type = 'title',
                    str =  'The Scree for Rv') # The title
doc <- ph_with_vg(doc, code = print(zeScree.Rv),
                  type = "body") # The ggplot2 picture
#---------------------------------------------------------------------
# A new Slide with text and  a ggplot2 figure
doc <- add_slide(doc, layout = "Title and Content",
                 master = "Office Theme")
doc <- ph_with_text(doc, type = 'title',
                    str =  'Judges Map (Rv-Map)') # The title
doc <- ph_with_vg(doc, code = print(aggMap.j.withHull),
                  type = "body") # The ggplot2 picture

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# A new Slide with text and  a ggplot2 figure
doc <- add_slide(doc, layout = "Title and Content",
                 master = "Office Theme")
doc <- ph_with_text(doc, type = 'title',
                    str =  'Beer Map (S-Map)') # The title
doc <- ph_with_vg(doc, code = print(aggMap.i.withCI) ,
                  type = "body") # The ggplot2 picture

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Save the powerpoint Presentation
print(doc, target = "file2Save.pptx" )
# et voila
#---------------------------------------------------------------------



