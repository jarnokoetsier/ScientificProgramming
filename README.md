# Scientific Programming Project (MSB1015)
For the Scientific Programming (MSB1015) course, the *Breast Cancer Wisconsin (Diagnostic) Data Set* was analysed. This repository contains all the scripts used for this analysis.

## Data
The original Breast Cancer Wisconsin (Diagnostic) Data Set can be downloaded from [Kaggle](https://www.kaggle.com/datasets/uciml/breast-cancer-wisconsin-data). However, for the current analysis a modified version of this data set was used.

The data set consist of 569 samples and includes the sample ID, the sample diagnosis (M: Malignant; B: Beneign), as well as 30 variables computed from a digitized image of a fine needle aspirate (FNA) of a breast mass. These 30 variables describe features from the cell nuclei in these images and encompasses the **mean**, standard error (**SE**), and the mean of the three largest values (**worst**) of the following 10 characteristics:
1. **Radius:** The mean of distances from center to points on the border of the cell nucleus.
2. **Texture:** The standard deviation of gray-scale values of the digitalized image.
3. **Perimeter:** The total length of the border of the cell nucleus.
4. **Area:** The size of the surface of the cell nucleus.
5. **Smoothness:** The local variation in radius lengths.
6. **Compactness:** Perimeter<sup>2</sup> / Area - 1.0
7. **Concavity:** The severity of concave portions of the contour of the cell nucleus.
8. **Concave points:** The number of concave portions of the contour of the cell nucleus.
9. **Symmetry:** Similarity of the radius length on both sides of the diameter.
10. **Fractal dimension:** "coastline approximation" - 1

More information about the variables can be found on page 8 in this [paper by Westerdijk (2018)](https://www.math.vu.nl/~sbhulai/papers/paper-westerdijk.pdf).

## Research aim
The aim of the analysis is three-fold:
1. Construct a robust classifier to distinguish malignant from beneign samples (**Classification**).
2. Identify subclasses within the maligant samples (**Clustering**).
3. Create an app for the prediction and visualization of new samples (**App**).

## Analysing the data
Before analysing the data, be aware of the following:

1. Put the data file (Data.xlsx) into the main folder (..PATH../ScientificProgramming/).
2. Furthermore, it is important to run the scripts in the following order:
    * Pre-processing
    * Classification
    * Clustering
    * App
3. Finally, please follow the instructions in the scripts carefully to ensure a successfull analysis.


## App
To run the app in RStudio, click on "Run App" in the top right corner when having either the ui.R, server.R, or global.R file open in the RStudio window.

If this is not possible, run the following commands:

`install.packages("shiny")`

`library(shiny)`

`runApp(..PATH../ScientificProgramming/App)`

