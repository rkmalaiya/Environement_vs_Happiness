install.packages("devtools")
install.packages("ExPosition")
install.packages("ggplot2")
install.packages("corrplot")
install.packages('ggrepel')

library("devtools")
library("ExPosition")
library("ggplot2")
library("corrplot")

devtools::install_github('HerveAbdi/data4PCCAR')
library(data4PCCAR)
data()
data('twentyWines')
twentyWines$df.active
