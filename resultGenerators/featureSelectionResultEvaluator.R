
library(foreach)

# This file cointains functions to perform feature selection, and assess
# the performance using classifiers in each resample iteration.

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
    
    selectedFeatures <- featureSelectionMethod(trainData, trainLabels)
    nSelectedFeatures <- length(selectedFeatures)
    nTotalFeatures <- ncol(dataset$X)
    observedTestLabels <- dataset$Y[testIdx]
    
    resampleResult <-
      lapply(assessmentClassifiers,
             function (classifier) {
               
               classifierPredictionInfo <- 
                 classifier(trainIdx, testIdx,
                            dataset, selectedFeatures)
               
               classifierPerformanceInfo <-
                 summaryFunction(nSelectedFeatures,
                                 nTotalFeatures,
                                 observedTestLabels,
                                 classifierPredictionInfo)
               
               return(classifierPerformanceInfo)
             })
    
    resampleResult$selectedFeatures = selectedFeatures
    
    return(resampleResult)
  }

extractFeatureSelectionResultFrom <- 
  function(resampleResult, assessmentClassifiers) {
    
    featureSelectionResult <- list()
    
    featureSelectionResult$performance <-
      t(sapply(names(assessmentClassifiers),
               function (classifierName) {
                 
                 classifierPerformanceInfo <- 
                   t(sapply(resampleResult,
                            function (resampleResult) {
                              metricNames <- names(resampleResult[[classifierName]])
                              metrics <- as.numeric(resampleResult[[classifierName]])
                              names(metrics) <- metricNames
                              metrics
                            }))
                 
                 metricsMean <- colMeans(classifierPerformanceInfo)
                 names(metricsMean) <- paste("mean.", names(metricsMean), sep = "")
                 metricsSd <- apply(classifierPerformanceInfo, 2, sd)
                 names(metricsSd) <- paste("sd.", names(metricsSd), sep = "")
                 
                 c(metricsMean, metricsSd)
               }))
    
    featureSelectionResult$selectedFeatures <- 
      lapply(resampleResult,
             function (resampleResult) {
               
               featuresSubset <- 
                 extractSelectedFeaturesIndexesFrom(
                   resampleResult$selectedFeatures)
               
               checkInstall("sets")
               sets::as.set(featuresSubset)
             })
    
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
