CRANpackages <- c("tidyverse",
                  "shiny",
                  "shinyWidgets",
                  "shinycssloaders",
                  "shinythemes",
                  "caret",
                  "glmnet",
                  "plotly")

#Install (if not yet installed) and load the required packages: 
for (pkg in CRANpackages) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, ask = FALSE)
  require(as.character(pkg), character.only = TRUE)
}

wd <- getwd()
homeDir <- str_remove(wd, "/App")
load(paste0(homeDir, "/Classification/trainingData_filtered.RData"))
load(paste0(homeDir, "/Classification/trainingData.RData"))
load(paste0(homeDir, "/Classification/trainingClass.RData"))
load(paste0(homeDir, "/Classification/finalModel.RData"))
load(paste0(homeDir, "/Pre-Processing/sampleInfo_filtered.RData"))
load(paste0(homeDir, "/Pre-Processing/featureInfo.RData"))