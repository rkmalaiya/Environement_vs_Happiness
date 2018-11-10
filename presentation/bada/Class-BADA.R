#=====================================================================
# Script for R-RM3: Class 10. Part 1.
#  10/30/2017
# Program: Getting data from packages, today iris dataset
# graph with PTCA4CATA
# MCA and cut variablesggplots2::cut_number() + line
# Add line to a graph. Plot a polygon.
# Compute and plot a convex Hull
# Perform the same tasks with the alternate ggplot2 approach
# as implemented in the PTCA4CATA package under development
# Next BADA and LDA with the same data set.
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
#install.packages('TInPosition')


# setwd('~/Box Sync/RM3-2017/R-Script/Class-10/')
#       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#        You need to change this line
# load the libraries that we will need
suppressMessages(library(PTCA4CATA))
# PTCA4CATA should first to avoid conflict with TInPosition
suppressMessages(library(ExPosition))
suppressMessages(library(InPosition))
suppressMessages(library(TInPosition))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(officer))
suppressMessages(library(flextable))
suppressMessages(library(rvg))
# other libraries here



#here we load the iris data set
data(iris)
#---------------------------------------------------------------------
# We have a look at the iris data set
summary(iris)
#---------------------------------------------------------------------
# separate the measurements from the factor (design)
mesIris <- iris[,1:4] # measures: SL SW PL PW
grIris <- iris[,5] # Three types pf irises
#---------------------------------------------------------------------
resBADA <- tepBADA(DATA = mesIris,
                   scale = 'SS1', center = TRUE,
                   DESIGN = grIris,
                   make_design_nominal = TRUE,
                   group.masses = NULL,
                   weights = NULL, graphs =  FALSE,
                   k = 0)
#---------------------------------------------------------------------
# A look at the scree
dev.new()
PlotScree(ev = resBADA$TExPosition.Data$eigs,
          p.ev = NULL, max.ev = NULL, alpha = 0.05,
          col.ns = "#006D2C", col.sig = "#54278F",
          title = "Explained Variance per Dimension")
zeScree <- recordPlot()

#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# To minimize typing:
Fk <- resBADA$TExPosition.Data$fi
Fi <- resBADA$TExPosition.Data$fii
Fj <- resBADA$TExPosition.Data$fj
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Color for the three irises (from TExPosition)
col4Iris <- resBADA$Plotting.Data$fii.col
#
# create a color scheme for the variables
# Width is darker than length:` `
# Sepal is orange
# Petal is red
col4Var <- c('orange','orange4','red','red4')
#
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# The maps are below
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# A graph for the J set
baseMap.j <- PTCA4CATA::createFactorMap(Fj,
                            col.points   = col4Var,
                            alpha.points =  .3,
                            col.labels   = col4Var)
# A graph for the J-set
aggMap.j <- baseMap.j$zeMap_background + # background layer
  baseMap.j$zeMap_dots + baseMap.j$zeMap_text # dots & labels
# We print this Map with the following code
dev.new()
print(aggMap.j)
#---------------------------------------------------------------------
# De we want to add lines?
# PTCA4CATA does not have arrowed lines,
#    but this line of code create them
zeLines <- ggplot2::annotate("segment", x = c(0), y = c(0),
                    xend = Fj[,1],
                    yend = Fj[,2],
                    color = col4Var,
                    alpha = .5,
                    arrow = arrow(length = unit(.3, "cm") ) )
# Create the map by adding background, labels, and arrows:
aggMap.j.arrows <- baseMap.j$zeMap_background +
                                      zeLines + baseMap.j$zeMap_text
dev.new()
print(aggMap.j.arrows)
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Graphs for the I set
#---------------------------------------------------------------------
# Create the base map
baseMap.i <- PTCA4CATA::createFactorMap(Fi,
                                        col.points   = col4Iris,
                                        alpha.points =  .3)
#---------------------------------------------------------------------
# We are not interested about the labels here
#  so we will use only the dots
#  in all I-set graph.
#---------------------------------------------------------------------
# Plain map with color for the I-set
aggMap.i <- baseMap.i$zeMap_background + baseMap.i$zeMap_dots
#---------------------------------------------------------------------
# print this Map
dev.new()
print(aggMap.i)
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# a vector of color for the means
col4Means <- unique(col4Iris)
#---------------------------------------------------------------------
# create the map for the means
MapGroup    <- PTCA4CATA::createFactorMap(Fk,
                                axis1 = 1, axis2 = 2,
                                constraints = baseMap.i$constraints,
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
aggMap.i.withMeans <- aggMap.i+
  MapGroup$zeMap_dots + MapGroup$zeMap_text
#---------------------------------------------------------------------
# plot it!
dev.new()
print(aggMap.i.withMeans)
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Create 75% Tolerance interval polygons
#
GraphTI.Hull.90 <- MakeToleranceIntervals(Fi,
                                  as.factor(grIris),
                                  names.of.factors = c("Dim1","Dim2"),
                                  col = unique(col4Iris),
                                  line.size = .5, line.type = 3,
                                  alpha.ellipse = .2,
                                  alpha.line = .4,
                                  p.level = .75, # 75% TI
                                  type = 'hull' #
                                          # use 'hull' for convex hull
)
#---------------------------------------------------------------------
# Create the map
aggMap.i.withHull <- aggMap.i +
  GraphTI.Hull.90 + MapGroup$zeMap_dots +
  MapGroup$zeMap_text +  MapGroup$zeMap_dots
#---------------------------------------------------------------------
# Plot it!
dev.new()
print(aggMap.i.withHull)
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Inferences
#
resBADA.inf <- tepBADA.inference.battery(DATA = mesIris,
                   scale = 'SS1', center = TRUE,
                   DESIGN = grIris  ,
                   make_design_nominal = TRUE,
                   group.masses = NULL,
                   weights = NULL,
                   graphs = FALSE,
                   k = 2,
                   test.iters = 100,
                   critical.value = 2)
#---------------------------------------------------------------------
# Confusion matrices
# To be saved as table
fixedCM   <-   resBADA.inf$Inference.Data$loo.data$fixed.confuse
looedCM   <- resBADA.inf$Inference.Data$loo.data$loo.confuse

#---------------------------------------------------------------------
# Create Confidence Interval Plots
BootCube <- resBADA.inf$Inference.Data$boot.data$fi.boot.data$boots
dimnames(BootCube)[[2]] <- c("Dimension 1","Dimension 2")
# use function MakeCIEllipses from package PTCA4CATA
GraphElli <- MakeCIEllipses(BootCube[,1:2,],
                  names.of.factors = c("Dimension 1","Dimension 2"),
                  col = col4Means,
                  p.level = .95
)
#---------------------------------------------------------------------
# create the I-map with Observations, means and confidence intervals
#
aggMap.i.withCI <-  aggMap.i +  GraphElli + MapGroup$zeMap_text
#---------------------------------------------------------------------
# plot it!
dev.new()
print(aggMap.i.withCI)
#---------------------------------------------------------------------
#---------------------------------------------------------------------
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
                    str =  'BADA with the Iris data-set')
#---------------------------------------------------------------------
# A new Slide with text and  a table
# Prepare the table to be stored in the pptx file
# We need to add a column with the rownames
# because flextable does not print it
#tabIris <- cbind(rownames(IrisMeans.var ), IrisMeans.var )
#colnames(tabIris)[1] <- 'Species'
# create the pptx table
IrisNames <- c("Setosa", "Vesricolor", "Virginia")
fixedCM.tmp <-    as.data.frame(cbind( (IrisNames),fixedCM ) )
colnames(fixedCM.tmp)[1] <- 'Species'
FixedCM4pptx <- theme_booktabs(autofit(regulartable(
                       as.data.frame(fixedCM.tmp)  ) ))
# alternative style: theme_vanilla(regulartable(tabIris) )
# store the table
doc <- add_slide(doc, layout = "Title and Content",
                 master = "Office Theme")
doc <- ph_with_text(doc, type = 'title',
                    str =  'Fixed Confusion Matrix') # The title
doc <- ph_with_flextable(doc, value = FixedCM4pptx, type = "body")
#---------------------------------------------------------------------
# The random effect CM

looedCM.tmp <-    as.data.frame(cbind( (IrisNames),looedCM ) )
colnames(fixedCM.tmp)[1] <- 'Species'
LooedCM4pptx <- theme_booktabs(autofit(regulartable(
   looedCM.tmp )  ))
# alternative style: theme_vanilla(regulartable(tabIris) )
# store the table
doc <- add_slide(doc, layout = "Title and Content",
                 master = "Office Theme")
doc <- ph_with_text(doc, type = 'title',
                    str =  'Loo Confusion Matrix') # The title
doc <- ph_with_flextable(doc, value = LooedCM4pptx, type = "body")
#---------------------------------------------------------------------
# A new Slide with text and a prettyPlot figure
doc <- add_slide(doc, layout = "Title and Content",
                 master = "Office Theme")
doc <- ph_with_text(doc, type = 'title',
                    str =  'The Scree') # The title
doc <- ph_with_vg(doc, code = print(zeScree),
                  type = "body") # The ggplot2 picture
#---------------------------------------------------------------------
# A new Slide with text and  a ggplot2 figure
doc <- add_slide(doc, layout = "Title and Content",
                 master = "Office Theme")
doc <- ph_with_text(doc, type = 'title',
                    str =  'The loadings') # The title
doc <- ph_with_vg(doc, code = print(aggMap.j),
                  type = "body") # The ggplot2 picture


#---------------------------------------------------------------------
# Save the powerpoint Presentation
print(doc, target = "my_plot_class10.pptx")
# et voila
#---------------------------------------------------------------------

