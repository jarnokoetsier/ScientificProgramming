# Scientific Programming Project (MSB1015)
![licence](https://badgen.net/badge/licence/MIT/green)
![status](https://badgen.net/badge/status/building/orange)

For the Scientific Programming (MSB1015) course, an adjusted version of the *Breast Cancer Wisconsin (Diagnostic) Data Set* was analysed. This repository contains all the scripts that were used for this analysis.

1. [Data](#Data)
2. [Research aim](#Research-aim)
3. [Analysing the data](#Analysing-the-data)
4. [App](#App)
5. [Contact](#Contact)

## Data
The original Breast Cancer Wisconsin (Diagnostic) Data Set can be downloaded from [Kaggle](https://www.kaggle.com/datasets/uciml/breast-cancer-wisconsin-data). However, for the current analysis a modified version of this data set was used. [Contact me](#Contact) to access the adjusted data set.

The data set consist of 569 samples and includes the sample ID, the sample diagnosis (Malignant(M): 212 and Benign (B): 357), as well as 30 variables computed from a digitized image of a fine needle aspirate (FNA) of a breast mass. These 30 variables describe features from the cell nuclei in these images and encompasses the **mean**, standard error (**SE**), and the mean of the three largest values (**worst**) of the following 10 characteristics:
1. **Radius:** The mean of distances from center to points on the border of the cell nucleus.
2. **Texture:** The standard deviation of gray-scale values of the digitalized image.
3. **Perimeter:** The total length of the border of the cell nucleus.
4. **Area:** The size of the surface of the cell nucleus.
5. **Smoothness:** The local variation in radius lengths.
6. **Compactness:** Perimeter<sup>2</sup> / Area - 1.0
7. **Concavity:** The severity of concave portions of the contour of the cell nucleus.
8. **Concave points:** The number of concave portions of the contour of the cell nucleus.
9. **Symmetry:** Similarity of the radius length on both sides of the diameter.
10. **Fractal dimension:** *Coastline approximation* - 1

More information about the variables can be found on page 8 in this [paper by Westerdijk (2018)](https://www.math.vu.nl/~sbhulai/papers/paper-westerdijk.pdf).

## Research aim
The aim of the analysis is three-fold:
1. Construct a robust classifier to distinguish malignant from benign samples (**Classification**).
2. Identify subclasses within the maligant samples (**Clustering**).
3. Create an app for the prediction and visualization of new samples (**App**).

## Analysing the data
Before analysing the data, be aware of the following:

1. Put the data file (`Data.xlsx`) into the main folder (`..PATH../ScientificProgramming/`).
2. Furthermore, it is important to run the scripts in the following order:
    * `Pre-processing/Preprocessing.R`
    * `Classification/Classification.R`
    * `Clustering/Clustering.R`
    * [App](#App)
3. Finally, please follow the instructions in the scripts carefully to ensure a successfull analysis.


## App
To run the app in RStudio, click on "Run App" in the top right corner when having either the `App/ui.R`, `App/server.R`, or `App/global.R` file open in the RStudio window.

![Start App](/Figures/StartApp.JPG?raw=true "Start App")

If this is not possible, run the following commands:
```r
# Install the shiny package
install.packages("shiny")

# Load the shiny package
library(shiny)

# Run the shiny app
runApp(..PATH../ScientificProgramming/App)
```
 ### Now you can use the classification model to predict the class of new samples!
 
![App Demo](/Figures/AppDemo.gif?raw=true "App Demo")


## Contact
Feel free to contact me via email: j.koetsier@student.maastrichtuniversity.nl
