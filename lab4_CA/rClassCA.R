rm(list = ls())
graphics.off()
# Get there with an Rproject
# if not an Rproject set the directory here
# e.g.,
# setwd(~/Box Sync/RM3-CurrentYear/R4RM3/r-Class04/'')

library(ExPosition)
library(InPosition)
# use the last version of PTCA
devtools::install_github('HerveAbdi/PTCA4CATA')
library(PTCA4CATA)
library(corrplot)
library(ggplot2)
# install.packages('gplots')
# also install data4PCCAR
devtools::install_github('HerveAbdi/data4PCCAR')
library(data4PCCAR)


# Get the data ----

data("sixAuthorsPunctuated", package = 'data4PCCAR')
# The active data set
X <- as.matrix(sixAuthorsPunctuated$df.active)
# A supplementary points: Abdi
HApunct <-sixAuthorsPunctuated$supplementary.observation  
# The supplementary variables
otherPunct <- sixAuthorsPunctuated$supplementary.variables


# CA analysis ----

#
resCA.sym  <- epCA(X, symmetric = TRUE)
# to run a plain CA but asymetric
resCA.asym <- epCA(X, symmetric = FALSE)
# HA as sup ----
HA.sup <- supplementaryRows(SUP.DATA = HApunct, res = resCA.sym)
# Other punct ----
punct.sup <- supplementaryCols(SUP.DATA = otherPunct, res = resCA.sym)


# CA graphs ----
# to make life easier ----
Fj.a <- resCA.asym$ExPosition.Data$fj
Fi   <- resCA.sym$ExPosition.Data$fi
Fj   <- resCA.sym$ExPosition.Data$fj

# constraints -----
# first get the constraints correct
constraints.sym <- minmaxHelper(mat1 = Fi, mat2  = Fj)
constraints.asym <- minmaxHelper(mat1 = Fi, mat2  = Fj.a)
constraints.sup <- minmaxHelper(mat1 = rbind(Fi, HA.sup$fii), 
                                mat2  = rbind(Fj, punct.sup$fjj) )

# Get some colors ----
color4Authors <-prettyGraphsColorSelection(n.colors = nrow(Fi))
# baseMaps ----
baseMap.i <- createFactorMap(Fi, constraints = constraints.sym,
                             col.points = color4Authors,
                             col.labels = color4Authors)
print(baseMap.i$zeMap)
baseMap.j <- createFactorMap(Fj, constraints = constraints.sym,
                             color.points = 'darkorchid4')
print(baseMap.j$zeMap)
print(baseMap.i$zeMap + baseMap.j$zeMap_dots + baseMap.j$zeMap_text)

symMap  <- createFactorMapIJ(Fi,Fj,
                             col.points.i = color4Authors,
                             col.labels.i = color4Authors)

asymMap  <- createFactorMapIJ(Fi,Fj.a,
                              col.points.i = color4Authors,
                              col.labels.i = color4Authors)

mapSup <- createFactorMapIJ(as.data.frame(HA.sup$fii), 
                            as.data.frame(punct.sup$fjj)  ,
                            col.points.i = "Orange",
                            col.labels.i = 'Orange' ,
                            col.points.j = 'Pink',
                            col.labels.j = 'Pink',
                            constraints = constraints.sup
)

labels4CA <- createxyLabels(resCA = resCA.sym)

# draw the maps ----
map.IJ.sym <- symMap$baseMap + symMap$I_labels + symMap$I_points +
  symMap$J_labels + symMap$J_points + labels4CA
print(map.IJ.sym)
map.IJ.asym <- asymMap$baseMap + asymMap$I_labels + 
  asymMap$I_points + asymMap$J_labels + 
  asymMap$J_points + labels4CA
print(map.IJ.asym)

# create an asymetric map with a supplementary row
map.I.sup.asym <- asymMap$baseMap + asymMap$I_labels + 
  asymMap$I_points +
  asymMap$J_labels + asymMap$J_points + 
  mapSup$I_labels + mapSup$I_points +
  labels4CA
print(map.I.sup.asym)

# Create a symmetric map with sup and correct constraints
map.IJ.sup.sym <- mapSup$baseMap + 
  symMap$I_labels + symMap$I_points +
  symMap$J_labels + symMap$J_points + 
  mapSup$I_labels + mapSup$I_points + 
  mapSup$J_labels + mapSup$J_points +
  ggtitle('Symmetric Map with Supplementary Elements') + 
  labels4CA
print(map.IJ.sup.sym)



