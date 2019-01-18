
# Multi Variate Analysis

## Data set: Environmental Readings per Country

### ***Ritesh Malaiya***
*For the full version of cookbook, please visit [link](https://rkmalaiya.github.io/Environement_vs_Happiness/)*
# Overview

# Dataset
* Data: Measurements of environment conditions in Countries
* Rows: There are 137 observations, 1 for each country.
* Columns: Total 29 variables
    + Qualitative: Country (nominal), Happiness (Ordinal).
    + Quantitative: Aspect, Slope Crop Land, Tree Canopy Wind Cloud & Multiple variables for Temp & Rain
    

![image1.png](pptimages/image1.png)

# Methods

## Quantitative

* PCA

## Qualitative

* MCA

* DiCA

## Grouping variables to observe relative effect among groups

* PLS-C

* MFA

* Cluster Analysis (DiSTATIS)

# Correlation Plot 

![image3.png](pptimages/image3.png)

# Heat plot
![image2.png](pptimages/image2.png)

# Quantitative Analysis
## PCA
![image4.png](pptimages/image4.png)
![image5.png](pptimages/image5.png)


| **Methods** | **Unhappy**               | **Normal**             | **Very Happy**                | **Reliability**  |
|------------------|-------------------------|-----------------------|---------------------------|------------|
| **PCA**     | Others                                      | Temp & Rain                         | N/A                     | Components have significant contribution but convex hull has overlapping areas and Component 2 & 7 contradicts |


# Qualitative Analysis
## Data Binning
![image6.png](pptimages/image6.png)

## MCA
![image10.png](pptimages/image10.png)

![image11.png](pptimages/image11.png)

| **Methods** | **Unhappy**               | **Normal**             | **Very Happy**                | **Reliability**  |
|------------------|-------------------------|-----------------------|---------------------------|------------|
| **PCA**     | Others                                      | Temp & Rain                         | N/A                     | Components have significant contribution but convex hull has overlapping areas and Component 2 & 7 contradicts |
| **MCA**     | warm summers, cold winters, high rain | N/A     | Warm winter, cold summer, low rain    | Components have significant contribution but convex hull has overlapping areas                                 |


### MCA Inference

![image30.png](pptimages/image30.png)

![image31.png](pptimages/image31.png)

### Discriminant Correspondence Analysis (DiCA)

![image33.png](pptimages/image33.png)

![image51.png](pptimages/image51.png)

| **Methods** | **Unhappy**               | **Normal**             | **Very Happy**                | **Reliability**  |
|------------------|-------------------------|-----------------------|---------------------------|------------|
| **PCA**     | Others                                      | Temp & Rain                         | N/A                     | Components have significant contribution but convex hull has overlapping areas and Component 2 & 7 contradicts |
| **MCA**     | warm summers, cold winters, high rain | N/A     | Warm winter, cold summer, low rain    | Components have significant contribution but convex hull has overlapping areas                                 |
| **DiCA**    | warm summers, cold winters, high rain | Higher variation in temperature is correlated with lower happiness | Warm winter, cold summer, low rain, windy | Convex hulls are separeted but second component only has temp variables as significant                         |


### DiCA Inference

![image52.png](pptimages/image52.png)
![image53.png](pptimages/image53.png)

Grouping variables to observe relative effect among variables

## PLS-C

![image60.png](pptimages/image60.png)
![image61.png](pptimages/image61.png)
![image62.png](pptimages/image62.png)


| **Methods** | **Unhappy**               | **Normal**             | **Very Happy**                | **Reliability**  |
|------------------|-------------------------|-----------------------|---------------------------|------------|
| **PCA**     | Others                                      | Temp & Rain                         | N/A                     | Components have significant contribution but convex hull has overlapping areas and Component 2 & 7 contradicts |
| **MCA**     | warm summers, cold winters, high rain | N/A     | Warm winter, cold summer, low rain    | Components have significant contribution but convex hull has overlapping areas                                 |
| **DiCA**    | warm summers, cold winters, high rain | Higher variation in temperature is correlated with lower happiness | Warm winter, cold summer, low rain, windy | Convex hulls are separeted but second component only has temp variables as significant                         |
| **PLS-C**   | Rain                                      | Temp                         | Temp                                      | Second component has more rain variables as significant than temp variables                                    |


## Multi Factor Analysis

![image63.png](pptimages/image63.png)

| **Methods** | **Unhappy**               | **Normal**             | **Very Happy**                | **Reliability**  |
|------------------|-------------------------|-----------------------|---------------------------|------------|
| **PCA**     | Others                                      | Temp & Rain                         | N/A                     | Components have significant contribution but convex hull has overlapping areas and Component 2 & 7 contradicts |
| **MCA**     | warm summers, cold winters, high rain | N/A     | Warm winter, cold summer, low rain    | Components have significant contribution but convex hull has overlapping areas                                 |
| **DiCA**    | warm summers, cold winters, high rain | Higher variation in temperature is correlated with lower happiness | Warm winter, cold summer, low rain, windy | Convex hulls are separeted but second component only has temp variables as significant                         |
| **PLS-C**   | Rain                                      | Temp                         | Temp                                      | Second component has more rain variables as significant than temp variables                                    |
| **MFA**     |  Partial factors dominated by Temp, then rain and other variables   | Neither of partial factors seems to have sufficient effect       |  Partial factors dominated by Temp and other variables, lesser effect of rain         | Convex hull has overlapping areas                                                                              |


## Cluster Analysis - DiSTATIS

### Kmeans

![image64.png](pptimages/image64.png)
![image65.png](pptimages/image65.png)
![image66.png](pptimages/image66.png)

# Conclusion

* **MCA** and **DiCA** agrees: 
  - Warmer winter, colder summer, low rain, windy cities makes people *happy*
  - Colder Winter, warmer summers, high rain, less windy makes people *unhappy*


However, even though **MCA** shows that most the variables has high contribution for the strongest signal in the data - **DiCA** shows that temp, rain and wind variables contributes significantly.

*Hence*, 

* Happiness doesn’t seem to be highly correlated to environmental conditions
* Temperature, rain and wind seem to be slightly correlated with happiness.
* Cluster Analysis doesn’t seem to show any patterns in the data.
