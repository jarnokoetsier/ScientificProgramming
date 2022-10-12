#=============================================================================#
# File: MLfunction.R
# Date: October 5, 2022										                                      
# Author: Jarno Koetsier                                                      
# Data: NA
#
# R version: 4.2.1 (getRversion())
# RStudio version: 2022.7.1.544 (RStudio.Version())
#=============================================================================#


################################################################################

# logisticRegression_par() [logistic regression with parallel computing]

################################################################################

# Train logistic regression model with parallelization implemented
logisticRegression_par <- function(trainingData, trainingClass, nfold, nrep, alpha, lambda, nCores, seed){
  
  #============================================================================#
  
  # Write a function that performs repeated cross validation
  LR_par1 <- function(trainingData, trainingClass, nfold, nrep, alpha, lambda){
    
    # Empty matrix to collect model accuracy
    accuracy <- rep(NA,nrep*nfold)
    
    # Make matrix for regression coefficients
    coeffs <- matrix(NA, nrow = ncol(trainingData), ncol = nrep*nfold)
    count = 0
    
    # For each repeat.....
    for(r in 1:nrep){
      
      # Combine class and data into single data frame
      trainingData_combined <- cbind.data.frame(trainingData, trainingClass)
      
      # Use ADASYN algorithm to create synthetic data samples to deal with
      # class imbalance:
      ada_train <- AdasynClassif(trainingClass ~ ., dat = trainingData_combined)
      trainingData1 <- as.matrix(ada_train[,1:(ncol(ada_train)-1)])
      trainingClass1 <- ada_train[,ncol(ada_train)]
      
      # Create folds (seperate for beneign and malignant to ensure an even distribution among the folds)
      fold_B <- createFolds(which(trainingClass1 == "Benign"), k = nfold)
      fold_M <- createFolds(which(trainingClass1 == "Malignant"), k = nfold)
      
      
      for (i in 1:nfold){
        count = count + 1
        
        # Get folds
        folds <- c(which(trainingClass1 == "Benign")[fold_B[[i]]], 
                   which(trainingClass1 == "Malignant")[fold_M[[i]]])
        
        #split training data in training and validation set
        X_train <- trainingData1[-folds,]
        C_train <- trainingClass1[-folds]
        
        X_val <- trainingData1[folds,]
        C_val <- trainingClass1[folds]
        
        # Fit model
        en_model_cv <- glmnet(x = X_train, 
                              y = C_train, 
                              alpha = alpha, 
                              lambda = lambda,
                              family = "binomial",
                              standardize = FALSE)
        
        # Evaluate model
        pred <- predict(en_model_cv, X_val, type="class")
        pred <- factor(pred[,1], levels = levels(C_val))
        accuracy[count] <- sum(pred == C_val)/length(C_val)
        
        # Variable importance
        coeffs[,count] <- coef(en_model_cv)[-1,1]
      }
      
    }
    
    # Save accuracies and coefficients in a list object
    output <- list(accuracy, coeffs, alpha, lambda)
    names(output) <- c("Accuracy", "Coefficients", "Alpha", "Lambda")
    return(output)
  }
  
  #============================================================================#
  
  # Parameter grid: each combination of alpha and lambda
  parameterGrid <- expand.grid(alpha, lambda)
  
  # Make clusters
  cl <- makeCluster(nCores)
  registerDoParallel(cl)
  
  # For each parameter combination....
  output <- foreach(i =  1:nrow(parameterGrid), .packages = c("glmnet", "UBL", "caret"), .inorder = FALSE) %dopar% {
    
    # set seed
    set.seed(seed)
    
    # Perform repeated cross-validation
    LR_par1(trainingData, trainingClass, nfold, nrep, parameterGrid[i,1], parameterGrid[i,2])
  }
  
  #Stop clusters
  stopCluster(cl)
  
  # Return values
  return(output)
}


################################################################################

# logisticRegression() [logistic regression without parallel computing]

################################################################################


# Train logistic regression model with parallelization implemented
logisticRegression <- function(trainingData, trainingClass, nfold, nrep, alpha, lambda, seed){
  
  #============================================================================#
  
  # Write a function that performs repeated cross validation
  LR_par1 <- function(trainingData, trainingClass, nfold, nrep, alpha, lambda){
    
    # Empty matrix to collect model accuracy
    accuracy <- rep(NA,nrep*nfold)
    
    # Make matrix for regression coefficients
    coeffs <- matrix(NA, nrow = ncol(trainingData), ncol = nrep*nfold)
    count = 0
    
    # For each repeat.....
    for(r in 1:nrep){
      
      # Combine class and data into single data frame
      trainingData_combined <- cbind.data.frame(trainingData, trainingClass)
      
      # Use ADASYN algorithm to create synthetic data samples to deal with
      # class imbalance:
      ada_train <- AdasynClassif(trainingClass ~ ., dat = trainingData_combined)
      trainingData1 <- as.matrix(ada_train[,1:(ncol(ada_train)-1)])
      trainingClass1 <- ada_train[,ncol(ada_train)]
      
      # Create folds (seperate for beneign and malignant to ensure an even distribution among the folds)
      fold_B <- createFolds(which(trainingClass1 == "Benign"), k = nfold)
      fold_M <- createFolds(which(trainingClass1 == "Malignant"), k = nfold)
      
      
      for (i in 1:nfold){
        count = count + 1
        
        # Get folds
        folds <- c(which(trainingClass1 == "Benign")[fold_B[[i]]], 
                   which(trainingClass1 == "Malignant")[fold_M[[i]]])
        
        #split training data in training and validation set
        X_train <- trainingData1[-folds,]
        C_train <- trainingClass1[-folds]
        
        X_val <- trainingData1[folds,]
        C_val <- trainingClass1[folds]
        
        # Fit model
        en_model_cv <- glmnet(x = X_train, 
                              y = C_train, 
                              alpha = alpha, 
                              lambda = lambda,
                              family = "binomial",
                              standardize = FALSE)
        
        # Evaluate model
        pred <- predict(en_model_cv, X_val, type="class")
        pred <- factor(pred[,1], levels = levels(C_val))
        accuracy[count] <- sum(pred == C_val)/length(C_val)
        
        # Variable importance
        coeffs[,count] <- coef(en_model_cv)[-1,1]
      }
      
    }
    
    # Save accuracies and coefficients in a list object
    output <- list(accuracy, coeffs, alpha, lambda)
    names(output) <- c("Accuracy", "Coefficients", "Alpha", "Lambda")
    return(output)
  }
  
  #============================================================================#
  
  # Parameter grid: each combination of alpha and lambda
  parameterGrid <- expand.grid(alpha, lambda)
  

  # For each parameter combination....
  output <- foreach(i =  1:nrow(parameterGrid), .packages = c("glmnet", "UBL", "caret"), .inorder = FALSE) %do% {
    
    # set seed
    set.seed(seed)
    
    # Perform repeated cross-validation
    LR_par1(trainingData, trainingClass, nfold, nrep, parameterGrid[i,1], parameterGrid[i,2])
  }
  
  # Return values
  return(output)
}


################################################################################

# logisticRegression1() [OLD VERSION: do not use!]

################################################################################

# Train logistic regression model
logisticRegression1 <- function(trainingData, trainingClass, nfold, nrep, alpha, lambda){
  
  # Empty list for collected regression coefficients
  coeffs <- list()
  
  # Parameter grid: each combination of alpha and lambda
  parameterGrid <- expand.grid(alpha, lambda)
  
  # Empty matrix to collect model accuracy
  accuracy <- matrix(NA, nrow = nrep*nfold, ncol = nrow(parameterGrid))
  
  # For each parameter combination....
  for (p in 1:nrow(parameterGrid)){
    
    # Make matrix for regression coefficients
    coeffs[[p]] <- matrix(NA, nrow = ncol(trainingData), ncol = nrep*nfold)
    count = 0
    
    # For each repeat.....
    for (r in 1:nrep){
      
      # Combine class and data into single data frame
      trainingData_combined <- cbind.data.frame(trainingData, trainingClass)
      
      # Use ADASYN algorithm to create synthetic data samples to deal with
      # class imbalance:
      ada_train <- AdasynClassif(trainingClass ~ ., dat = trainingData_combined)
      trainingData1 <- as.matrix(ada_train[,1:(ncol(ada_train)-1)])
      trainingClass1 <- ada_train[,ncol(ada_train)]
      
      # ROSE: over- and undersampling (NOT USED)
      #rose_train <- ROSE(trainingClass ~ ., data = trainingData_combined)
      #trainingData1 <- as.matrix(rose_train$data[,1:(ncol(rose_train$data)-1)])
      #trainingClass1 <- rose_train$data[,ncol(rose_train$data)]
      
      # Create folds (seperate for beneign and malignant to ensure an even distribution among the folds)
      fold_B <- createFolds(which(trainingClass1 == "Benign"), k = nfold)
      fold_M <- createFolds(which(trainingClass1 == "Malignant"), k = nfold)
      
      
      for (i in 1:nfold){
        count = count + 1
        
        # Get folds
        folds <- c(which(trainingClass1 == "Benign")[fold_B[[i]]], 
                   which(trainingClass1 == "Malignant")[fold_M[[i]]])
        
        #split training data in training and validation set
        X_train <- trainingData1[-folds,]
        C_train <- trainingClass1[-folds]
        
        X_val <- trainingData1[folds,]
        C_val <- trainingClass1[folds]
        
        # Fit model
        en_model_cv <- glmnet(x = X_train, 
                              y = C_train, 
                              alpha = parameterGrid[p,1], 
                              lambda = parameterGrid[p,2],
                              family = "binomial",
                              standardize = FALSE)
        
        # Evaluate model
        pred <- predict(en_model_cv, X_val, type="class")
        pred <- factor(pred[,1], levels = levels(C_val))
        accuracy[count,p] <- sum(pred == C_val)/length(C_val)
        
        # Variable importance
        coeffs[[p]][,count] <- coef(en_model_cv)[-1,1]
      }
    }
    
  }
  
  # Save accuracies and coefficients in a list object
  output <- list(accuracy, coeffs)
  names(output) <- c("Accuracy", "Coefficients")
  return(output)
}