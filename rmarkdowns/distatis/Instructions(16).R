
#Load the data
setwd(your_file_path)
Raw_Data <- read.csv('natural36_constrained_forR.csv', row.names = 1)
Sorting_Data <- Raw_Data[-38,]
Design_Data <- Raw_Data[38,]

#Load (and run) DiSTATIS
library(DistatisR)

#...