
#=============================================================================#
# File: Preprocessing.R
# Date: October 5, 2022										                                      
# Author: Jarno Koetsier                                                      
# Data: 'Data.xlsx' 
#=============================================================================#



###############################################################################

# 0. Preparation

###############################################################################

# Clear working environment
rm(list = ls())

# Set path to the project folder
homeDir <- "C:/Users/Gebruiker/Documents/GitHub/ScientificProgramming"

# Required packages
CRANpackages <- c("tidyverse",     # Data formatting and plotting
                  "readxl",        # Read excel files
                  "tidyverse",     # Data formatting and plotting
                  "caret",         # Needed for imputation
                  "ggridges",      # Create ridge plot
                  "isotree",       # Isolation forest
                  "gridExtra",     # Combine multiple plots into single image
                  "grid",          # Add objects to combined plots
                  "ggrepel")       # Add labels in plot        

# Install (if not yet installed) and load the required CRAN packages: 
for (pkg in CRANpackages) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, ask = FALSE)
  require(as.character(pkg), character.only = TRUE)
}

# Set working directory to the pre-processing folder
setwd(paste0(homeDir, "/Pre-processing"))


################################################################################

# 1.. Data cleaning

################################################################################


#******************************************************************************#
#* 2.1. Prepare data matrix
#******************************************************************************#

# Read data
dataset <- read_excel(paste0(homeDir,"/Data.xlsx"))

# Select the features (column 3-32)
dataMatrix <- data.frame(dataset[,3:32])
rownames(dataMatrix) <- dataset$id

# Replace NaN with NA
dataMatrix[dataMatrix == "NaN"] <- NA

# Replace comma with dot
for (j in 1:ncol(dataMatrix)) {
  dataMatrix[,j] <- as.numeric(str_replace_all(as.character(dataMatrix[,j]), ",", "\\."))
}
colnames(dataMatrix) <- str_replace_all(colnames(dataset)[3:32], " ", "_")

#******************************************************************************#
#* 2.2. Prepare sample Information object
#******************************************************************************#

# Get sample ID + label from the dataset
sampleInfo <- dataset[,1:2]

# Convert ID from numeric to character
sampleInfo$id <- as.character(sampleInfo$id)

# Change B to Beneign and M to Malignant
sampleInfo$diagnosis[sampleInfo$diagnosis == "B"] <- "Beneign"
sampleInfo$diagnosis[sampleInfo$diagnosis == "M"] <- "Malignant"


#******************************************************************************#
#* 2.3. Prepare feature Information object
#******************************************************************************#

# Make a featureInfo data frame
featureInfo <- data.frame(Name = colnames(dataMatrix),
                          Statistic = colnames(dataMatrix),
                          Feature = colnames(dataMatrix))

# Statistic: mean, SE, or worst
featureInfo$Statistic[str_detect(featureInfo$Statistic, "_mean")] <- "Mean"
featureInfo$Statistic[str_detect(featureInfo$Statistic, "_se")] <- "SE"
featureInfo$Statistic[str_detect(featureInfo$Statistic, "_worst")] <- "Worst"

# Feature: radius, area, perimeter, etc.
featureInfo$Feature <- str_remove_all(featureInfo$Feature, "_mean")
featureInfo$Feature <- str_remove_all(featureInfo$Feature, "_se")
featureInfo$Feature <- str_remove_all(featureInfo$Feature, "_worst")
featureInfo$Feature <- str_replace_all(featureInfo$Feature, "_", " ")
featureInfo$Feature <- str_to_title(featureInfo$Feature)

# Name1: proper names that will be used for plotting
featureInfo$Name1 <- paste(featureInfo$Feature, featureInfo$Statistic, sep = " ")

# Save data
save(dataMatrix, file = "dataMatrix.RData")
save(featureInfo, file = "featureInfo.RData")
save(sampleInfo, file = "sampleInfo.RData")




################################################################################

# 2. Data pre-processing

################################################################################


#******************************************************************************#
#* 2.1. Data imputation
#******************************************************************************#

# Load data (if needed)
load("dataMatrix.RData")
load("featureInfo.RData")
load("sampleInfo.RData")

# Impute missing values using KNN
imputeModel <- preProcess(dataMatrix,
                          method = c("knnImpute"),
                          k = 10,
                          knnSummary = mean)

dataMatrix_imputed <- predict(imputeModel, dataMatrix, na.action = na.pass)

# The output values are unit scaled: reverse this
for(j in 1:ncol(dataMatrix_imputed)){
  dataMatrix_imputed[,j] <- dataMatrix_imputed[,j]*imputeModel$std[j]+imputeModel$mean[j] 
}

# Save imputed data
save(dataMatrix_imputed, file = "dataMatrix_imputed.RData")


#******************************************************************************#
#* 2.2. Visualize imputation
#******************************************************************************#

# Load data (if needed)
load("dataMatrix_imputed.RData")
load("featureInfo.RData")
load("sampleInfo.RData")

# Unit scale the data (mean = 0, sd = 0)
dataMatrix_scaled <- t((t(dataMatrix_imputed) - rowMeans(t(dataMatrix_imputed)))/(apply(t(dataMatrix_imputed),1,sd)))

# Prepare data for plotting
plotData <- gather(data.frame(dataMatrix_scaled))
plotData <- inner_join(plotData, featureInfo, by = c("key" = "Name"))

# Retrieve values that were imputed
imputedValues <- gather(data.frame(dataMatrix_scaled[which(is.na(dataMatrix))]))
imputedValues$key <- colnames(dataMatrix)[which(is.na(dataMatrix), arr.ind = TRUE)[,2]]
imputedValues <- inner_join(imputedValues, featureInfo, by = c("key" = "Name"))

# Make boxplots
knnPlot <- ggplot(plotData, aes(x = Name1, y = value, fill = Statistic)) +
  geom_boxplot() +
  geom_point(data = imputedValues, aes(x = Name1, y = value), color = "red", shape = 18, size = 3) +
  xlab(NULL) +
  ylab("Unit scaled value") +
  labs(title = "kNN imputation", 
       caption = "NOTE: The imputed values are indicated by the red dots.") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.caption = element_text(hjust = 0.5,
                                    size = 10,
                                    face = "italic")) + 
  scale_fill_manual(values = c("#293462", "#1CD6CE", "#FEDB39"))

# Save plots
ggsave(plot = knnPlot, filename = "knnPlot.png", width = 12, height = 8)


#******************************************************************************#
#* 2.3. Transformation
#******************************************************************************#

# Load data (if needed)
load("dataMatrix_imputed.RData")
load("featureInfo.RData")
load("sampleInfo.RData")

# Perform log transformation (add 0.5 to avoid zero values)
dataMatrix_log <- log(dataMatrix_imputed + 0.5)

# Unit scale the data (mean = 0, sd = 0)
dataMatrix_scaled <- t((t(dataMatrix_imputed) - rowMeans(t(dataMatrix_imputed)))/(apply(t(dataMatrix_imputed),1,sd)))
dataMatrix_log_scaled <- t((t(dataMatrix_log) - rowMeans(t(dataMatrix_log)))/(apply(t(dataMatrix_log),1,sd)))


#==============================================================================#
# 1) Ridge plot
#==============================================================================#

# Prepare data for plotting
plotData <- gather(data.frame(dataMatrix_scaled))
plotData <- inner_join(plotData, featureInfo, by = c("key" = "Name"))

plotData_log <- gather(data.frame(dataMatrix_log_scaled))
plotData_log <- inner_join(plotData_log, featureInfo, by = c("key" = "Name"))

# Ridge plot
ridgePlot <- ggplot(plotData, aes(x = value, y = Feature, fill = Feature)) +
  geom_density_ridges(scale = 2, rel_min_height = 0.01) +
  xlab("Unit scaled value") +
  ylab(NULL) +
  ggtitle("No Transformation") +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16)) + 
  facet_wrap(~Statistic, ncol = 3)

ridgePlot_log <- ggplot(plotData_log, aes(x = value, y = Feature, fill = Feature)) +
  geom_density_ridges(scale = 2, rel_min_height = 0.01) +
  xlab("Unit scaled value") +
  ylab(NULL) +
  ggtitle("Log Transformation") +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16)) + 
  facet_wrap(~Statistic, ncol = 3)

# Save ridge plots
ggsave("ridgePlot.png", 
       grid.arrange(ridgePlot, 
                    ridgePlot_log, 
                    ncol = 2, 
                    nrow = 1,
                    top = textGrob("Feature Distributions",gp=gpar(fontsize=20,font=2))), 
       width = 12, height = 8)


#==============================================================================#
# 2) PCA plot
#==============================================================================#

# Construct a PCA model using the filtered data

#No tranformation:
pcaList <-  prcomp(dataMatrix_scaled,        
                   retx = TRUE,
                   center = FALSE,
                   scale = FALSE)
#Log tranformation:
pcaList_log <-  prcomp(dataMatrix_log_scaled,        
                   retx = TRUE,
                   center = FALSE,
                   scale = FALSE)


# Explained variance

#No tranformation:
explVar <- round(((pcaList$sdev^2)/sum(pcaList$sdev^2))*100,2)
#Log tranformation:
explVar_log <- round(((pcaList_log$sdev^2)/sum(pcaList_log$sdev^2))*100,2)


# Retrieve scores from pcaList object

#No tranformation:
PCAscores <- as.data.frame(pcaList$x)
PCAscores$ID <- rownames(PCAscores)
PCAscores <- inner_join(PCAscores, sampleInfo, by = c("ID" = "id"))
#Log tranformation:
PCAscores_log <- as.data.frame(pcaList_log$x)
PCAscores_log$ID <- rownames(PCAscores_log)
PCAscores_log <- inner_join(PCAscores_log, sampleInfo, by = c("ID" = "id"))


# Make PCA score plot

#No tranformation:
scorePlot <- ggplot(data = PCAscores, aes(x = PC1, y = PC2, color = diagnosis)) +
  geom_point(alpha = 0.9, size = 2) +
  ggtitle(label = "No Transformation") +
  xlab(paste0("PC1 (", explVar[1],"%)")) +
  ylab(paste0("PC2 (", explVar[2],"%)")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 14),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(breaks = c("Malignant", "Beneign"),
                     values=c("#D61C4E", "#293462")) +
  scale_fill_manual(breaks = c("Malignant", "Beneign"),
                    values=c("#D61C4E", "#293462"))

#Log tranformation:
scorePlot_log <- ggplot(data = PCAscores_log, aes(x = PC1, y = PC2, color = diagnosis)) +
  geom_point(alpha = 0.9, size = 2) +
  ggtitle(label = "Log Transformation") +
  xlab(paste0("PC1 (", explVar_log[1],"%)")) +
  ylab(paste0("PC2 (", explVar_log[2],"%)")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 14),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(breaks = c("Malignant", "Beneign"),
                     values=c("#D61C4E", "#293462")) +
  scale_fill_manual(breaks = c("Malignant", "Beneign"),
                    values=c("#D61C4E", "#293462"))


# Save plots
ggsave("PCA_ScorePlot_Transformation.png", 
       grid.arrange(scorePlot, 
                    scorePlot_log, 
                    ncol = 2, 
                    nrow = 1,
                    top = textGrob("PCA score plot",gp=gpar(fontsize=20,font=2))), 
       width = 12, height = 6)


#.............................................................................#
# NOTE: Looking at the plots, the log-transformation seems to make the data
# more normally distributed. So, we will work with the log transformed data 
# for now.
#.............................................................................#

# Save log-tranformed data
save(dataMatrix_log, file = "dataMatrix_log.RData")


#******************************************************************************#
#* 2.4. Outlier detection
#******************************************************************************#

#==============================================================================#
# 1) Isolation forest-based outlier detection
#==============================================================================#

# Load (if needed) log-transformed data
load("dataMatrix_log.RData")
load("featureInfo.RData")
load("sampleInfo.RData")

# Make isolation forest model with 1000 trees
set.seed(123)
iforest_1000 <- isolation.forest(dataMatrix_log, 
                                ndim=1, 
                                ntrees=1000,
                                missing_action="fail")
# Calculate anomaly scores
anomalyScore_1000 <- predict(iforest_1000, dataMatrix_log, type = "score")

# Check whether samples are in the same order (this should be TRUE)
all(sampleInfo$id == names(anomalyScore_1000))

# Add the anomaly scores to the sampleInfo object
sampleInfo$AnomalyScore1000 <- as.vector(anomalyScore_1000)

# Make isolation forest model with 500 trees 
# (This will be used to check for convergence)
set.seed(123)
iforest_500 <- isolation.forest(dataMatrix_log, 
                                ndim=1, 
                                ntrees=500,
                                missing_action="fail")
# Calculate anomaly scores
anomalyScore_500 <- predict(iforest_500, dataMatrix_log, type = "score")

# Check whether samples are in the same order (this should be TRUE)
all(sampleInfo$id == names(anomalyScore_500))

# Add the anomaly scores to the sampleInfo object
sampleInfo$AnomalyScore500 <- as.vector(anomalyScore_500)


# Check for convergence
convergencePlot <- ggplot(sampleInfo,aes(AnomalyScore1000, AnomalyScore500)) +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1.5) +
  geom_point(color = "black", alpha = 0.5) +
  theme_classic() +
  xlab("Anomaly score (500 trees)") +
  ylab("Anomaly score (100 trees)") +
  ggtitle("Isolation Forest Covergence Plot") +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "bottom",
        legend.title = element_blank())

# Save plot
ggsave(plot = convergencePlot, filename = "convergencePlot.png", width = 10, height = 8)

# select threshold for outliers
anomalyThreshold <- 0.65 

# Make an histogram of the anomaly scores
anomalyHistogram <- ggplot(sampleInfo, aes(AnomalyScore1000, fill = diagnosis)) + 
  geom_histogram(alpha = 0.5, bins = 100, position = "identity") +
  geom_vline(xintercept = anomalyThreshold, color="black", 
              linetype="dashed", size=1.5) +
  xlab("Anomaly score") +
  ylab("Count") +
  ggtitle("Histogram of Anomaly Scores") +
  theme_classic()  +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(breaks = c("Malignant", "Beneign"),
                     values=c("#D61C4E", "#293462")) +
  scale_fill_manual(breaks = c("Malignant", "Beneign"),
                    values=c("#D61C4E", "#293462"))

# Save plot
ggsave(plot = anomalyHistogram, filename = "anomalyHistogram.png", width = 10, height = 8)

#==============================================================================#
# 2) PCA-based outlier detection
#==============================================================================#

# Unit scale the data (mean = 0, sd = 0)
dataMatrix_log_scaled <- t((t(dataMatrix_log) - rowMeans(t(dataMatrix_log)))/(apply(t(dataMatrix_log),1,sd)))

# Construct a PCA model using the filtered data
pcaList <-  prcomp(dataMatrix_log_scaled,        
                   retx = TRUE,
                   center = FALSE,
                   scale = FALSE)

# Explained variance
explVar <- round(((pcaList$sdev^2)/sum(pcaList$sdev^2))*100,2)

# Retrieve scores from pcaList object
PCAscores <- as.data.frame(pcaList$x)
PCAscores$ID <- rownames(PCAscores)
PCAscores <- inner_join(PCAscores, sampleInfo, by = c("ID" = "id"))

# Make PCA score plot

# 1) colored by anomaly score
PCA_ScorePlot_Outliers1 <- ggplot(data = PCAscores, aes(x = PC1, y = PC2, color = AnomalyScore500)) +
  geom_point(alpha = 0.9, size = 2) +
  geom_point(data = PCAscores[PCAscores$AnomalyScore1000 > anomalyThreshold,], aes(x = PC1, y = PC2), shape = 1, size = 5, color = "red") +
  ggtitle(label = "Anomaly Score") +
  xlab(paste0("PC1 (", explVar[1],"%)")) +
  ylab(paste0("PC2 (", explVar[2],"%)")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 14),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "right",
        legend.title = element_blank()) +
  scale_color_viridis_c()

# 2) colored by diagnosis
PCA_ScorePlot_Outliers2 <- ggplot(data = PCAscores, aes(x = PC1, y = PC2, color = diagnosis)) +
  geom_point(alpha = 0.9, size = 2) +
  geom_point(data = PCAscores[PCAscores$AnomalyScore1000 > anomalyThreshold,], aes(x = PC1, y = PC2), shape = 1, size = 5, color = "red") +
  ggtitle(label = "Diagnosis") +
  xlab(paste0("PC1 (", explVar[1],"%)")) +
  ylab(paste0("PC2 (", explVar[2],"%)")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 14),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "right",
        legend.title = element_blank()) +
  scale_color_manual(breaks = c("Malignant", "Beneign"),
                     values=c("#D61C4E", "#293462"))

ggsave("PCA_ScorePlot_Outliers.png", 
       grid.arrange(PCA_ScorePlot_Outliers1, 
                    PCA_ScorePlot_Outliers2, 
                    ncol = 2, 
                    nrow = 1,
                    top = textGrob("PCA Score Plot",gp=gpar(fontsize=20,font=2))), 
       width = 14, height = 6)


# Split data into reconstruction and residuals based on the first two PC's
reconstruction <- as.matrix(PCAscores[,1:2]) %*% t(as.matrix(PCAscores[,1:2]))
residuals <- as.matrix(PCAscores[,3:30]) %*% t(as.matrix(PCAscores[,3:30]))

# Calculate the orthogonal distances
ortDist<- sqrt(rowSums(residuals^2))

# Calculate the mahalanobis distances
coVar <- cov(PCAscores[,1:2])
center <- colMeans(PCAscores[,1:2])
mahalanobisDist <- mahalanobis(as.matrix(PCAscores[,1:2]), center = center, cov = coVar)

# Save distance metrics into dataframe
distanceDF <- data.frame(OD = ortDist,
                         MD = mahalanobisDist,
                         AnomalyScore = PCAscores$AnomalyScore1000,
                         Diagnosis = PCAscores$diagnosis)

# Distance-distance plot, colored by anomaly score
DDplot1 <- ggplot(distanceDF, aes(x = MD, y = OD, color = AnomalyScore)) +
  geom_point(alpha = 0.9, size = 2) +
  geom_point(data = distanceDF[distanceDF$AnomalyScore > anomalyThreshold,], 
             aes(x = MD, y = OD), shape = 1, size = 5, color = "red") +
  xlab("Score distance") + 
  ylab("Orthogonal distance") +
  labs(color = "Anomaly Score") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 14),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "right") +
  scale_color_viridis_c()

# Distance-distance plot, colored by diagnosis
DDplot2 <- ggplot(distanceDF, aes(x = MD, y = OD, color = Diagnosis)) +
  geom_point(alpha = 0.9, size = 2) +
  geom_point(data = distanceDF[distanceDF$AnomalyScore > anomalyThreshold,], 
             aes(x = MD, y = OD), shape = 1, size = 5, color = "red") +
  xlab("Score distance") + 
  ylab("Orthogonal distance") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 14),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "right",
        legend.title = element_blank()) +
  scale_color_manual(breaks = c("Malignant", "Beneign"),
                     values=c("#D61C4E", "#293462"))

# Distance-distance plot on a logarithmic scale, colored by anomaly score
logDDplot1 <- ggplot(distanceDF, aes(x = log(MD), y = log(OD), color = AnomalyScore)) +
  geom_point(alpha = 0.9, size = 2) +
  geom_point(data = distanceDF[distanceDF$AnomalyScore > anomalyThreshold,], 
             aes(x = log(MD), y = log(OD)), shape = 1, size = 5, color = "red") +
  xlab("log Score distance") + 
  ylab("log Orthogonal distance") +
  labs(color = "Anomaly Score") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 14),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "right") +
  scale_color_viridis_c()

# Distance-distance plot on a logarithmic scale, colored by diagnosis
logDDplot2 <- ggplot(distanceDF, aes(x = log(MD), y = log(OD), color = Diagnosis)) +
  geom_point(alpha = 0.9, size = 2) +
  geom_point(data = distanceDF[distanceDF$AnomalyScore > anomalyThreshold,], 
             aes(x = log(MD), y = log(OD)), shape = 1, size = 5, color = "red") +
  xlab("log Score distance") + 
  ylab("log Orthogonal distance") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 14),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "right",
        legend.title = element_blank()) +
  scale_color_manual(breaks = c("Malignant", "Beneign"),
                     values=c("#D61C4E", "#293462"))

# Save plots
ggsave("Distance-DistancePlot.png", 
       grid.arrange(DDplot1, 
                    logDDplot1,
                    DDplot2,
                    logDDplot2,
                    ncol = 2, 
                    nrow = 2,
                    top = textGrob("Distance-Distance Plot",gp=gpar(fontsize=20,font=2))), 
       width = 14, height = 10)


# Filter data matrix for outliers
dataMatrix_filtered <- dataMatrix_log[anomalyScore_1000 < anomalyThreshold,]

# Save data matrix
save(dataMatrix_filtered, file = "dataMatrix_filtered.RData")

# Filter sample information for outliers
sampleInfo_filtered <- sampleInfo[sampleInfo$id %in% rownames(dataMatrix_filtered),1:2]

# Save sample information
save(sampleInfo_filtered, file = "sampleInfo_filtered.RData")


################################################################################

# End of the script

################################################################################