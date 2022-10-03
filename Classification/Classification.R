#=============================================================================#
# File: Classification.R
# Date: October 5, 2022										                                      
# Author: Jarno Koetsier                                                      
# Data: 'dataMatrix_filtered.RData','featureInfo.Rdata','sampleInfo_filtered.RData'
#=============================================================================#



###############################################################################

# 0. Preparation

###############################################################################

# Clear working environment
rm(list = ls())

# Set path to the project folder
homeDir <- "C:/Users/Gebruiker/OneDrive/MSB year 2/MSB1015 Scientific Programming/Project"

# Required packages
CRANpackages <- c("tidyverse",     # Data formatting and plotting
                  "prospectr",     # Kennard-Stone algorithm
                  "caret",         # Create machine learning workflow
                  "glmnet",        # Construct elastic net model
                  "gridExtra",     # Combine images in single plot
                  "grid",          # Add elements to the combined image
                  "UBL")           # Adasyn algorithm for class imbalance


# Install (if not yet installed) and load the required CRAN packages: 
for (pkg in CRANpackages) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, ask = FALSE)
  require(as.character(pkg), character.only = TRUE)
}

# Set working directory to the pre-processing folder
setwd(paste0(homeDir, "/Classification"))

# Load data
load(paste0(homeDir, "/Pre-processing/", "dataMatrix_filtered.RData"))
load(paste0(homeDir, "/Pre-processing/", "featureInfo.Rdata"))
load(paste0(homeDir, "/Pre-processing/", "sampleInfo_filtered.RData"))


################################################################################

# 1. Make test and training set

################################################################################


#*****************************************************************************#
# 1.1. Select representative training set using Kennard-Stone algorithm
#*****************************************************************************#

# Check if samples in data matrix and sample information object are in the same order
all(rownames(dataMatrix_filtered) == sampleInfo_filtered$id)

# Select representative subset from the beneign samples:

# Select beneign samples from the data
dataMatrix_B <- dataMatrix_filtered[sampleInfo_filtered$diagnosis == "Beneign",]
sampleInfo_B <- sampleInfo_filtered[sampleInfo_filtered$diagnosis == "Beneign",]

# Select training set (80% of the samples) using Kennard-stone algorithm
selectedSamples_B <- prospectr::kenStone(
  X = dataMatrix_B, 
  k = round(0.8*nrow(dataMatrix_B))
  )


# Select representative subset from the malignant samples

# Select beneign samples from the data
dataMatrix_M <- dataMatrix_filtered[sampleInfo_filtered$diagnosis == "Malignant",]
sampleInfo_M <- sampleInfo_filtered[sampleInfo_filtered$diagnosis == "Malignant",]

# Select training set (80% of the samples) using Kennard-stone algorithm
selectedSamples_M <- prospectr::kenStone(
  X = dataMatrix_M, 
  k = round(0.8*nrow(dataMatrix_M))
)


# Make the training data
trainingData <- rbind(dataMatrix_B[selectedSamples_B$model,], dataMatrix_M[selectedSamples_M$model,])
trainingSamples <- rbind.data.frame(sampleInfo_B[selectedSamples_B$model,], sampleInfo_M[selectedSamples_M$model,])

# Check if samples are in correct order
all(trainingSamples$id == rownames(trainingData))

# Make a seperate object for the training labels/classes
trainingClass <- as.factor(trainingSamples$diagnosis)

# Check the number of samples per class in the training data
table(trainingClass)

# Make the test data
testData <- rbind(dataMatrix_B[selectedSamples_B$test,], dataMatrix_M[selectedSamples_M$test,])
testSamples <- rbind.data.frame(sampleInfo_B[selectedSamples_B$test,], sampleInfo_M[selectedSamples_M$test,])

# Check if samples are in correct order
all(testSamples$id == rownames(testData))

# Make a seperate object for the test labels/classes
testClass <- as.factor(testSamples$diagnosis)

# Check the number of samples per class in the test data
table(testClass)


#******************************************************************************#
# 1.2. Visualize test and training set with PCA
#******************************************************************************#

# Unit scale the training Data
trainingData_scaled <- t((t(trainingData) - rowMeans(t(trainingData)))/(apply(t(trainingData),1,sd)))

# Make PCA model
pcaList <-  prcomp(trainingData_scaled,        
                   retx = TRUE,
                   center = FALSE,
                   scale = FALSE)

# Scores training data
scores_train <- as.data.frame(pcaList$x)
scores_train$ID <- rownames(scores_train)
scores_train <- inner_join(scores_train, sampleInfo_filtered, by = c("ID" = "id"))

# Explained variance
explVar <- round(((pcaList$sdev^2)/sum(pcaList$sdev^2))*100,2)

# Scale test data (using the sd and mean of the training data)
testData_scaled <- t((t(testData) - rowMeans(t(trainingData)))/(apply(t(trainingData),1,sd)))

# Scores test data
scores_test <- as.data.frame(as.matrix(testData_scaled) %*% as.matrix(pcaList$rotation))
scores_test$ID <- rownames(scores_test)
scores_test <- inner_join(scores_test, sampleInfo_filtered, by = c("ID" = "id"))

# Combine scores of test and training data in a single data frame
scores_all <- rbind.data.frame(scores_train, scores_test)
scores_all$Train <- c(rep("Training", nrow(scores_train)), rep("Test", nrow(scores_test)))
scores_all$Group <- paste0(scores_all$Train, ": ", scores_all$diagnosis)

# Plot the scores of the training data and the project scores of the test data 
PCAtrain <- ggplot()+
  geom_point(data = scores_all, aes(x = PC1, y = PC2, shape = Group, color = Group), size = 2, alpha = 0.9) +
  scale_shape_manual(values = c(15,17,0,2)) +
  scale_color_brewer(palette = "Dark2") +
  xlab(paste0("PC1 (", explVar[1], "%)")) +
  ylab(paste0("PC2 (", explVar[2], "%)")) +
  labs(title = "PCA Score Plot", 
       caption = "NOTE: The PCA model is constructed using the training data only. The test data is projected.") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.caption = element_text(hjust = 0.5,
                                     size = 10,
                                    face = "italic"))

# Save plot
ggsave(PCAtrain, file = "PCA_TrainingAndTest.png", width = 10, height = 8)

#******************************************************************************#
# 1.3. Save training and test set
#******************************************************************************#

save(trainingData, file = "trainingData.RData")
save(trainingClass, file = "trainingClass.RData")
save(testData, file = "testData.RData")
save(testClass, file = "testClass.RData")


################################################################################

# 2. Construct classification models

################################################################################

# Load data (if needed)
load("trainingData.RData")
load("trainingClass.RData")
load("testData.RData")
load("testClass.RData")

# Load function in MLfunction script
source("MLfunction.R")

# Scale training data
trainingData_scaled <- t((t(trainingData) - rowMeans(t(trainingData)))/(apply(t(trainingData),1,sd)))

# Test for the following alpha values:
alpha <- seq(0,1,0.2)

# Test for the following lambda values:
lambda <- 10^seq(-4,-2,length.out = 5)

# Make a parameter grid for each combination of alpha and lambda:
parameterGrid <- expand.grid(alpha, lambda)

# Number of repeats (nrep) and CV folds (nfolds)
nrep = 10
nfold = 5

# Make an empty object for saving values of the models:
accuracy <- matrix(NA, nrow = nrep*nfold, ncol = (ncol(trainingData_scaled)-1))
bestAlpha <- rep(NA, (ncol(trainingData_scaled)-1))
bestLambda <- rep(NA, (ncol(trainingData_scaled)-1))
removedFeature <- rep(NA, (ncol(trainingData_scaled)-1))

# Train model using all features
set.seed(123)
testModel <- logisticRegression(trainingData = trainingData_scaled,
                                trainingClass = trainingClass,
                                nfold = nrep,
                                nrep = nfold,
                                alpha = alpha,
                                lambda = lambda)

# The best parameter combination (alpha & lambda) for which the mean accuracy is the highest:
bestParameters <- which.max(colMeans(testModel$Accuracy))

# The worst feature has the lowest absolute average coefficient:
#worstFeature <- which.min(abs(rowMeans(testModel$Coefficients[[bestParameters]])))
worstFeature <- which.min(rowMeans(abs(testModel$Coefficients[[bestParameters]])))

# Collect values of model
accuracy[,1] <- testModel$Accuracy[,bestParameters]
removedFeature[1] <- colnames(trainingData_scaled)[which.min(rowMeans(abs(testModel$Coefficients[[bestParameters]])))]
bestAlpha[1] <- parameterGrid[bestParameters,1]
bestLambda[1] <- parameterGrid[bestParameters,2]

# Make copy of scaled data
trainingData_scaled1 <- trainingData_scaled

# Recursive feature elimination:
for (q in 1:(ncol(trainingData_scaled)-2)) {
  
  # remove worst feature
  trainingData_scaled1 <- trainingData_scaled1[,-worstFeature]
  
  # Train model
  set.seed(123)
  testModel <- logisticRegression(trainingData = trainingData_scaled1,
                                  trainingClass = trainingClass,
                                  nfold = nrep,
                                  nrep = nfold,
                                  alpha = alpha,
                                  lambda = lambda)
  
  # The best parameter combination (alpha & lambda) for which the mean accuracy is the highest:
  bestParameters <- which.max(colMeans(testModel$Accuracy))
  
  # The worst feature has the lowest absolute average coefficient:
  #worstFeature <- which.min(abs(rowMeans(testModel$Coefficients[[bestParameters]])))
  worstFeature <- which.min(rowMeans(abs(testModel$Coefficients[[bestParameters]])))
  
  # Collect values
  accuracy[,q + 1] <- testModel$Accuracy[,bestParameters]
  removedFeature[q + 1] <- colnames(trainingData_scaled1)[which.min(rowMeans(abs(testModel$Coefficients[[bestParameters]])))]
  bestAlpha[q + 1] <- parameterGrid[bestParameters,1]
  bestLambda[q + 1] <- parameterGrid[bestParameters,2]
}

# Combine values into a single list object
modelInfo <- list(accuracy,
                  removedFeature,
                  bestAlpha,
                  bestLambda)
names(modelInfo) <- c("accuracy", "removedFeature", "bestAlpha", "bestLambda")

# Save model information
save(modelInfo, file = "modelInfo.RData")


################################################################################

# 3. Evaluate the classification models

################################################################################


#******************************************************************************#
# 3.1. Evaluate the accuracy for the different number of features
#******************************************************************************#

# Load (if needed) modelInfo data object
load("modelInfo.Rdata")

# Get accuracy for all repeated cross-validations
plotAccuracy <- gather(as.data.frame(modelInfo$accuracy), value = "Accuracy")

# Add number of features to the data frame
plotAccuracy$nFeatures <- 30 - (as.numeric(str_remove_all(plotAccuracy$key, "V")) - 1)

# Get mean accuracy for the different number of features
plotMeanAccuracy <- data.frame(Accuracy = colMeans(modelInfo$accuracy),
                               nFeatures = rev(2:30))

# Combine both data frames
plotAccuracy <- inner_join(plotAccuracy, plotMeanAccuracy, by = c("nFeatures" = "nFeatures"))

# Plot CV accuracy vs number of features in model
n_features = 4
accuracy_plot <- ggplot(plotAccuracy, aes(x = nFeatures, y = Accuracy.x, fill = Accuracy.y)) +
  geom_vline(xintercept = n_features, linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(x = n_features + 6, y = 0.82), label = paste0("Number of Features: ", n_features,
                                                 "\nAlpha: ", modelInfo$bestAlpha[31 - n_features],
                                                 "\nLambda: ", round(modelInfo$bestLambda[31 - n_features],3)), color = "red") +
  geom_boxplot(aes(group = nFeatures), alpha = 0.5) +
  geom_jitter(aes(group = nFeatures, color = Accuracy.y)) +
  #geom_point(data = plotMeanAccuracy, aes(x = nFeatures, y = Accuracy), size = 4, alpha = 0.5, shape = 17, color = "red") +
  xlab("Number of features") +
  ylab("CV accuracy") +
  labs(fill = "Mean accuracy", color = "Mean accuracy") +
  theme_classic() +
  scale_fill_viridis_c() +
  scale_color_viridis_c()

# Save plot
ggsave(plot = accuracy_plot, filename = "accuracyPlot.png", width = 10, height = 8)


#******************************************************************************#
# 3.2. Evaluate the stability of the coefficients
#******************************************************************************#

# Stability of coefficients

#Exclude the 27 worst features: after removing more than 24 features the CV accuracy decreases
excludedFeatures <- modelInfo$removedFeature[1:(30 - n_features)]
trainingData_filtered <- trainingData_scaled[,!(colnames(trainingData_scaled) %in% excludedFeatures)]

# Train model
set.seed(123)
testModel <- logisticRegression(trainingData = trainingData_filtered,
                                trainingClass = trainingClass,
                                nfold = 5,
                                nrep = 10,
                                alpha = modelInfo$bestAlpha[31-n_features],
                                lambda = modelInfo$bestLambda[31-n_features])

# Get coefficients in the repeated CV
coeffs <- as.data.frame(t(testModel$Coefficients[[1]]))
colnames(coeffs) <- colnames(trainingData_filtered)
coeffPlot <- gather(coeffs)
coeffPlot <- inner_join(coeffPlot, featureInfo, by = c("key" = "Name"))

# Construct final model
finalModel <- glmnet(x = trainingData_scaled[,!(colnames(trainingData_scaled) %in% excludedFeatures)], 
                   y = trainingClass, 
                   alpha = bestAlpha[31-n_features], 
                   lambda = bestLambda[31-n_features],
                   family = "binomial",
                   standardize = FALSE)

# save final model
save(finalModel, file = "finalModel.RData")

# Get coefficients from final model
finalCoeffs <- data.frame(finalModel$beta[,1],
                          rownames(finalModel$beta))
colnames(finalCoeffs) <- c("value", "key")
finalCoeffs <- inner_join(finalCoeffs, featureInfo, by = c("key" = "Name"))


# Make plot
coeff_plot <- ggplot(coeffPlot, aes(x = Name1, y = value)) + 
  geom_violin(aes(fill = Name1), alpha = 0.5) +
  geom_boxplot(width=0.1, fill="white")+
  geom_jitter(aes(color = Name1),position=position_jitter(0.2)) +
  geom_point(data = finalCoeffs, aes(x = Name1, y = value), fill = "black", shape = 23, size = 3, alpha = 0.8) +
  labs(caption = "NOTE: The large black diamant in the violin plot indicates the regression coefficient in the final model.") +
  xlab(NULL) +
  ylab("Regression Coefficient") +
  #ylim(c(0,2)) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.caption = element_text(hjust = 0.5,
                                    size = 10,
                                    face = "italic")) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2")

# Save plot
ggsave(plot = coeff_plot, filename = "coefficientPlot.png", width = 10, height = 8)



#******************************************************************************#
# 3.2. Evaluate the performance on the test set
#******************************************************************************#

# If needed, load the final model
load("finalModel.RData")

# Scale the test set
testData_scaled <- t((t(testData) - rowMeans(t(trainingData)))/(apply(t(trainingData),1,sd)))

# Remove exluded features from the data
testData_filtered <- testData_scaled[,!(colnames(testData_scaled) %in% excludedFeatures)]

# Make a prediction on the test set
pred_test <- predict(finalModel, testData_filtered, type = "class")

# Make confusion Matrix
cm_test <- confusionMatrix(as.factor(pred_test), testClass)
cm_test

# Make a prediction on the training set
pred_train <- predict(finalModel, trainingData_filtered, type = "class")

# Make confusion Matrix
cm_train <- confusionMatrix(as.factor(pred_train), trainingClass)
cm_train


# Get class probability for each test sample
pred_prob_test <- predict(finalModel, testData_filtered, type = "response")
testProb <- data.frame(Class = testClass,
                       Probability = pred_prob_test[,1])


# Make probability plot
testProb_plot <- ggplot(testProb, aes(x = Class, y = Probability)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(aes(color = Probability), position=position_jitter(0.2), size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", size = 1) +
  xlab(NULL) +
  ylab("Class Probability") +
  ggtitle(label = "Test data", subtitle = paste0("Accuracy: ", round(cm_test$overall,2),
                                                 ", Sensitivity: ", round(cm_test$byClass[1],2),
                                                 ", Specificity: ",  round(cm_test$byClass[2],2))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "none",
        legend.title = element_blank()) +
  scale_color_viridis_c()


# Get class probability for each training sample
pred_prob_train <- predict(finalModel, trainingData_filtered, type = "response")
trainingProb <- data.frame(Class = trainingClass,
                       Probability = pred_prob_train[,1])


# Make probability plot
trainingProb_plot <- ggplot(trainingProb, aes(x = Class, y = Probability)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(aes(color = Probability), position=position_jitter(0.2), size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", size = 1) +
  xlab(NULL) +
  ylab("Class Probability") +
  ggtitle("Training data", subtitle = paste0("Accuracy: ", round(cm_train$overall,2),
                                             ", Sensitivity: ", round(cm_train$byClass[1],2),
                                             ", Specificity: ",  round(cm_train$byClass[2],2))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "none",
        legend.title = element_blank()) +
  scale_color_viridis_c()

# Combine and save the plots
ggsave("probabilityPlot.png", 
       grid.arrange(trainingProb_plot, 
                    testProb_plot, 
                    ncol = 2, 
                    nrow = 1,
                    top = textGrob("Class Probabilities",gp=gpar(fontsize=20,font=2))), 
       width = 12, height = 6)





