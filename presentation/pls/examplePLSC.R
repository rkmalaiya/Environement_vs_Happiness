# An Example of running a PLSC
# with TExPosition and TinPosition
# Script for RM3 2016
# Hervé Abdi
#--------------------------------------------------------------------
# In this script we run a plain PLSC 
#  using tepPLS() from TeXPosition 
# and use ad-hoc code to perform standard inference:
# permutation and bootstrap test.
# We use data downloaded from the net
#--------------------------------------------------------------------
# First: A clean start.
#
rm(list = ls())
graphics.off()
# source the file with the current version of the
file4PLSCfunctions <- 'pls/inferences4PLSC.R'
# the functions are in ^^^^^^^^^^^^^^^^^^^
# This is the name of the file that stores the inference functions
# make sure that you saved these functions in this file
#
# Source the function file:
source(file4PLSCfunctions)
#
install.packages('TExPosition')
library(TExPosition)


# get the data for the example
# this exmaple for originaly designed for
# Canonical Correlation Analysis
# The CCA analysis is explained in this page:
# http://www.ats.ucla.edu/stat/r/dae/canonical.htm/
# (reading is highly recommended) 
# Load the Data
Psy.Ach <- read.csv("https://stats.idre.ucla.edu/stat/data/mmreg.csv")
colnames(Psy.Ach) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math", 
                  "Science", "Sex")
All.Psy <- Psy.Ach[,1:3]
All.Ach <- Psy.Ach[,4:7]
All.Design.PsyAch <- Psy.Ach[,8]
##
## make a small scale example out of the data
## Here we take the first 30 observations
nN <- 30
Psy <- All.Psy[1:nN,]
Ach <- All.Ach[1:nN,]
Design.PsyAch <- All.Design.PsyAch[1:nN]

## heatmap
corrplot::corrplot(cor(Psy,Ach))

#--------------------------------------------------------------------
# Run a PLSC with the epPLS() function
resPLSC <- tepPLS(Psy,Ach,DESIGN = Design.PsyAch,graphs = FALSE)
# No graph we will need to make them explicitly as an exercise

#--------------------------------------------------------------------
#!!!__ Here are the 4 types of factor scores maps you need to include !!!
# Exercise: Make the graphs for PLSC
#plots|__X__|__Y__
# 1a: | Lx1 | Ly1
# 1b: | Lx2 | Ly2
# 2a: | Fi1 | Fi2
# 2b: | Fj1 | Fj2




# Eigenv
EigVal <- resPLSC$TExPosition.Data$eigs
# How many eigen-value do we have here
nL <- min(ncol(Psy),ncol(Ach))
#--------------------------------------------------------------------
# First: Go for a permutation test
#
resPerm4PLSC <- perm4PLSC(Psy, # First Data matrix 
                          Ach, # Second Data matrix
                          nIter = 1000 # How mny iterations
                          )
# to see what results we have
print(resPerm4PLSC)
#
# Exercise: Plot the histogram of the permuted omnibus values
# along with the critical value. 
# Question what do we conclude?
# Exercise: How many latent variables are significant?
#--------------------------------------------------------------------
# Second: Compute the Bootstrap ratios
#
#' 
resBoot4PLSC <- Boot4PLSC(Psy, # First Data matrix 
                          Ach, # Second Data matrix
                          nIter = 1000, # How many iterations
                      Fi = resPLSC$TExPosition.Data$fi,
                      Fj = resPLSC$TExPosition.Data$fj,
                      nf2keep = 3,
                      critical.value = 2,
                      # To be implemented later
                      # has no effect currently
                      alphaLevel = .05)
#
# to see what results we have
print(resBoot4PLSC)
# Exercise: Plot the Bootstrap ratios
# along with their critical value. 
# Question what do we conclude?
#--------------------------------------------------------------------


