#=============================================================================#
# File: global.R
# Date: October 15, 2022										                                      
# Author: Jarno Koetsier                                                      
# Data: 'trainingData_filtered.RData', 'trainingData.RData', trainingClass.Rdata'
# 'testData.RData', "finalModel.RData', 'sampleInfo_filtered.RData', and
# 'featureInfo.RData'
#
# R version: 4.2.1 (getRversion())
# RStudio version: 2022.7.1.544 (RStudio.Version())
#=============================================================================#

# DISCLAIMER: The code has been developed using R version 4.2.1. Code 
# functionality for other R versions can not be guaranteed. 

# Clear working environment
rm(list = ls())

# Install (if needed) and load "devtools" package (version 2.4.4)
# This package is needed to install the correct versions of the packages
if (!requireNamespace("devtools", quietly = TRUE))
  install.packages("devtools", ask = FALSE)
require(as.character("devtools"), character.only = TRUE)

# Required packages
CRANpackages <- c("tidyverse",         # Data formatting and plotting
                  "shiny",             # Make App
                  "shinyWidgets",      # Widgets for app
                  "shinycssloaders",   # Loading figure
                  "shinythemes",       # Layout theme for app
                  "caret",             # Machine learning workflow
                  "glmnet",            # Elastic net
                  "plotly")            # Interactive plots

# Versions of required packages
versions <- c("1.3.2",
              "1.7.2",
              "0.7.3",
              "1.0.0",
              "1.2.0",
              "6.0.93",
              "4.1.4",
              "4.10.0")

# Install (if not yet installed) and load the required CRAN packages:
for (pkg in 1:length(CRANpackages)) {
  
  # Install package if not installed yet
  if (!requireNamespace(CRANpackages[pkg], quietly = TRUE)){
    install_version(CRANpackages[pkg], version = versions[pkg], 
                    repos = "http://cran.us.r-project.org")
  }
  # Install package if correct version is not installed yet
  if (packageVersion(CRANpackages[pkg]) != versions[pkg]){
    install_version(CRANpackages[pkg], version = versions[pkg], 
                    repos = "http://cran.us.r-project.org")
  }
  # Load package
  require(as.character(CRANpackages[pkg]), character.only = TRUE)
}

# Set working directory
wd <- getwd()
homeDir <- str_remove(wd, "/App")
homeDir <- str_remove(homeDir, "/Pre-Processing")
homeDir <- str_remove(homeDir, "/Classification")
homeDir <- str_remove(homeDir, "/Clustering")

# Load data
load(paste0(homeDir, "/Classification/trainingData_filtered.RData"))
load(paste0(homeDir, "/Classification/trainingData.RData"))
load(paste0(homeDir, "/Classification/trainingClass.RData"))
load(paste0(homeDir, "/Classification/testData.RData"))
load(paste0(homeDir, "/Classification/finalModel.RData"))
load(paste0(homeDir, "/Pre-Processing/sampleInfo_filtered.RData"))
load(paste0(homeDir, "/Pre-Processing/featureInfo.RData"))

