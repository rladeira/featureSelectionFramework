
library(foreach)

# This file cointains functions to perform feature selection, and assess
# the performance using classifiers in a cross validation scenario.

selectFeaturesAndAssess <- 
  function(featureSelectionMethod,
           dataset,
           assessmentClassifiers,
           summaryFunction,
           trainIndexes,
           testIndexes,
           allowParallel = TRUE) {
    
    if(is.function(featureSelectionMethod) == FALSE)
      stop("Invalid Argument! featureSelectionMethod must be a function")
    
    result <- 
      computeFeatureSelectionResult(
        dataset, featureSelectionMethod,
        assessmentClassifiers, summaryFunction,
        trainIndexes, testIndexes, allowParallel)
    
    return(result)
  }


computeFeatureSelectionResult <- 
  function(dataset,
           featureSelectionMethod,
           assessmentClassifiers,
           summaryFunction,
           trainIndexes,
           testIndexes,
           allowParallel = TRUE) {
    
    if (allowParallel) {
      resampleResult <- 
        foreach (resampleIndex = 1:length(trainIndexes)) %dopar% {
          computePerformanceForResampleInstance(
            featureSelectionMethod,
            dataset,
            assessmentClassifiers,
            summaryFunction,
            trainIndexes,
            testIndexes,
            resampleIndex)
        }
    } else {
      resampleResult <-
        lapply(1:length(trainIndexes),
               function (resampleIndex) {
                 computePerformanceForResampleInstance(
                   featureSelectionMethod,
                   dataset,
                   assessmentClassifiers,
                   summaryFunction,
                   trainIndexes,
                   testIndexes,
                   resampleIndex
                 )
               })
    }
    
    featureSelectionResult <-
      extractFeatureSelectionResultFrom(
        resampleResult,
        assessmentClassifiers)
    
    return(featureSelectionResult)
  }

computePerformanceForResampleInstance <- 
  function(featureSelectionMethod,
           dataset,
           assessmentClassifiers,
           summaryFunction,
           trainIndexes,
           testIndexes,
           resampleIndex) {
    
    trainIdx <- trainIndexes[[resampleIndex]]
    testIdx <- testIndexes[[resampleIndex]]
    trainData    <- dataset$X[trainIdx,]
    trainLabels  <- dataset$Y[trainIdx]
    
    resampleResult <- list()
    
    selectedFeatures <- featureSelectionMethod(trainData, trainLabels)
    
    nClassifiers <- length(assessmentClassifiers)
    for (i in 1:nClassifiers) {
      
      classifier <- assessmentClassifiers[[i]]
      classifierName <- names(assessmentClassifiers)[i]
      
      classifierPredictionInfo <- 
        classifier(trainIdx, testIdx,
                   dataset, selectedFeatures)
      
      observedTestLabels <- dataset$Y[testIdx]
      
      classifierPerformanceInfo <-
        summaryFunction(observedTestLabels,
                        classifierPredictionInfo)
      
      resampleResult[[classifierName]] <- classifierPerformanceInfo
    }
    
    resampleResult$selectedFeatures = selectedFeatures
    
    return(resampleResult)
  }

extractFeatureSelectionResultFrom <- 
  function(resampleResult, assessmentClassifiers) {
    
    featureSelectionResult <- list()
    for (classifierName in names(assessmentClassifiers)) {
      
      classifierPerformanceInfo <- 
        t(sapply(resampleResult,
                 function (resampleResult) {
                   resampleResult[[classifierName]]
                 }))
      
      classifierPerformanceInfo <- 
        apply(classifierPerformanceInfo, 2, as.numeric)
      
      classifierPerformance <- 
        list(accMean = mean(classifierPerformanceInfo[, "acc"]),
             accSd  = sd(classifierPerformanceInfo[, "acc"]),
             giniMean = mean(classifierPerformanceInfo[, "gini"]),
             giniSd = sd(classifierPerformanceInfo[, "gini"]))
      
      featureSelectionResult[[classifierName]] <- classifierPerformance                    
    }
    
    selectedFeatures <- 
      lapply(resampleResult,
             function (resampleResult) {
               
               featuresSubset <- 
                 extractSelectedFeaturesIndexesFrom(
                   resampleResult$selectedFeatures)
               
               sets::as.set(featuresSubset)
             })
    
    featureSelectionResult$selectedFeatures <- selectedFeatures
    
    return(featureSelectionResult)
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
