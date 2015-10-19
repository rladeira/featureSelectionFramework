
library(cvTools)
library(foreach)
library(HandTill2001)

# This file cointains functions to perform feature selection, and assess
# the performance using classifiers in a cross validation scenario.

selectFeaturesAndAssess <- 
  function(featureSelectionMethod,
           dataset,
           assessmentClassifiers,
           nFolds = 5,
           runCrossValidationInParallel = TRUE) {
    
    if(is.function(featureSelectionMethod) == FALSE)
      stop("Invalid Argument! searchMethod must be a function")
    
    result <- 
      computeFeatureSelectionResult(
        dataset,
        featureSelectionMethod,
        assessmentClassifiers,
        nFolds,
        runCrossValidationInParallel)
    
    return(result)
  }


computeFeatureSelectionResult <- 
  function(dataset,
           featureSelectionMethod,
           assessmentClassifiers,
           nFolds,
           runCrossValidationInParallel) {
    
    computePerformanceForFold <- function(fold) {
      
      trainIndexes <- folds$subsets[folds$which != fold]
      testIndexes  <- folds$subsets[folds$which == fold]
      trainData    <- dataset$X[trainIndexes,]
      trainLabels  <- dataset$Y[trainIndexes]
      
      foldResult <- list()
      
      selectedFeatures <- featureSelectionMethod(trainData, trainLabels)
      
      for (i in 1:nClassifiers) {
        
        classifier <- assessmentClassifiers[[i]]
        classifierName <- names(assessmentClassifiers)[i]
        
        classifierPredictionInfo <- 
          classifier(trainIndexes,
                     testIndexes,
                     dataset,
                     selectedFeatures)
        
        observedTestLabels <- dataset$Y[testIndexes]
        
        classifierPerformanceInfo <-
          computeClassifierPerformanceInfo(
            observed = observedTestLabels,
            predictedClasses = classifierPredictionInfo$predictedClasses,
            probabilities = classifierPredictionInfo$probabilities)
        
        foldResult[[classifierName]] <- classifierPerformanceInfo
      }
      
      foldResult$selectedFeatures = selectedFeatures
      
      return(foldResult)
    }
    
    extractFeatureSelectionResultFrom <- function(crossValidationResult) {
      
      featureSelectionResult <- list()
      for (classifierName in names(assessmentClassifiers)) {
        
        classifierPerformanceInfo <- 
          lapply(crossValidationResult,
                 function (foldResult) {
                   foldResult[[classifierName]]
                 }) 
        
        accVec <- sapply(classifierPerformanceInfo,
                         function(foldResult) foldResult$acc)
        
        giniVec <- sapply(classifierPerformanceInfo,
                          function(foldResult) foldResult$gini)
        
        classifierPerformance <- 
          list(accMean = mean(accVec),
               accSd  = sd(accVec),
               giniMean = mean(giniVec),
               giniSd = sd(giniVec))
        
        featureSelectionResult[[classifierName]] <- classifierPerformance                    
      }
      
      selectedFeatures <- 
        lapply(crossValidationResult,
               function (foldResult) {
                 
                 featuresSubset <- 
                   extractSelectedFeaturesIndexesFrom(
                     foldResult$selectedFeatures)
                 
                 sets::as.set(featuresSubset)
               })
      
      featureSelectionResult$selectedFeatures <- selectedFeatures
      
      return(featureSelectionResult)
    }
    
    nObservations <- nrow(dataset$X)
    nClassifiers <- length(assessmentClassifiers)
    folds <- cvFolds(n = nObservations,
                     K = nFolds,
                     type = "interleaved")
    
    if (runCrossValidationInParallel) {
      crossValidationResult <- 
        foreach (fold = 1:nFolds) %dopar% {
          computePerformanceForFold(fold)
        }
    } else {
      crossValidationResult <-
        lapply(1:nFolds,
               function (fold) {
                 #print(paste("Executing selection for fold:", fold))
                 computePerformanceForFold(fold)
               })
    }
    
    featureSelectionResult <-
      extractFeatureSelectionResultFrom(
        crossValidationResult)
    
    return(featureSelectionResult)
  }

computeClassifierPerformanceInfo <- 
  function(observed,
           predictedClasses,
           probabilities) {
    
    acc <- mean(observed == predictedClasses)
    gini <- computeGini(observed, probabilities)
    
    return(list(acc = acc,
                gini = gini))
  }

computeGini <- function(observed, probabilities) {
  
  AUC <- computeAUC(observed, probabilities)
  gini <- 2*AUC - 1
  
  return(gini)
}

computeAUC <- function(observed, probabilities) { 
  
  m <- multcap(observed, probabilities)
  
  return(auc(m))
}

extractSelectedFeaturesIndexesFrom <- function(selectedFeatures) {
  
  featuresSubset <-
    sapply(selectedFeatures,
           function(feature) {
             substr(feature, 2, nchar(feature))
           })
  
  names(featuresSubset) <- NULL
  
  return(featuresSubset)
}
