# Scientific Programming Project (MSB1015)
For the Scientific Programming (MSB1015) course, the *Breast Cancer Wisconsin (Diagnostic) Data Set* was analysed. This repository contains all the scripts used for this analysis.

##Data
The original Breast Cancer Wisconsin (Diagnostic) Data Set can be downloaded on [Kaggle](https://www.kaggle.com/datasets/uciml/breast-cancer-wisconsin-data). However, for the current analysis a modified version of this data set was used.

The data set includes the sample ID, the sample diagnosis (M: Malignant; B: Beneign), as well as 30 variables. These 30 variables encompasses the **mean**, standard error (**SE**), and the mean of the three largest values (**worst**) of the following 10 characteristics:
1. **Radius:** mean of distances from center to points on the perimeter
2. **Texture:** standard deviation of gray-scale values
3. **Perimeter**
4. **Area**
5. **Smoothness:** local variation in radius lengths
6. **Compactness:** Perimeter^2 / Area - 1.0
7. **Concavity:** Severity of concave portions of the contour
8. **Concave points:** number of concave portions of the contour
9. **Symmetry**
10. **Fractal dimension:** "coastline approximation" - 1

Before starting.....
-Make sure to set the working directory to the Scientific Programming folder.
-Analyse the data in the following order:
1) Pre-processing
2) Classification
3) Clustering
