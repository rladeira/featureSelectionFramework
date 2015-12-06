
# This file cointains functions to perform feature selection, and assess
# the performance using classifiers in each resample iteration.

library(foreach)

selectFeaturesAndAssess <- function(featureSelectionMethod,
                                    dataset,
                                    assessmentClassifiers,
                                    summaryFunction,
                                    trainIndexes,
                                    testIndexes,
                                    allowParallel = TRUE) {
  
  if(is.function(featureSelectionMethod) == FALSE)
    stop("Invalid Argument! featureSelectionMethod must be a function")
  
  result <- computeFeatureSelectionResult(
    dataset, featureSelectionMethod,
    assessmentClassifiers, summaryFunction,
    trainIndexes, testIndexes, allowParallel)
  
  return(result)
}


computeFeatureSelectionResult <- function(dataset,
                                          featureSelectionMethod,
                                          assessmentClassifiers,
                                          summaryFunction,
                                          trainIndexes,
                                          testIndexes,
                                          allowParallel = TRUE) {
  
  if (allowParallel) {
    resampleResults <- foreach (
      resampleIndex = 1:length(trainIndexes)) %dopar% {
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
    resampleResults <- lapply(
      1:length(trainIndexes),
      function (resampleIndex) {
        
        print(paste("resample iteration:", resampleIndex))
        
        computePerformanceForResampleInstance(
          featureSelectionMethod,
          dataset,
          assessmentClassifiers,
          summaryFunction,
          trainIndexes,
          testIndexes,
          resampleIndex)
      })
  }
  
  featureSelectionResult <- extractFeatureSelectionResultFrom(
    resampleResults, assessmentClassifiers)
  
  return(featureSelectionResult)
}

computePerformanceForResampleInstance <-  function(featureSelectionMethod,
                                                   dataset,
                                                   assessmentClassifiers,
                                                   summaryFunction,
                                                   trainIndexes,
                                                   testIndexes,
                                                   resampleIndex) {
  
  trainIdx <- trainIndexes[[resampleIndex]]
  testIdx <- testIndexes[[resampleIndex]]
  trainData <- dataset$X[trainIdx,]
  trainLabels <- dataset$Y[trainIdx]
  
  selectedFeatures <- featureSelectionMethod(trainData, trainLabels)
  nSelectedFeatures <- length(selectedFeatures)
  nTotalFeatures <- ncol(dataset$X)
  observedTestLabels <- dataset$Y[testIdx]
  
  resampleResult <- list()
  
  # Evaluate the performance related to each classifier
  # passed as parameter.
  resampleResult$performance <- lapply(
    assessmentClassifiers,
    function (classifier) {
      
      classifierPredictionInfo <- classifier(
        trainIdx, testIdx,
        dataset, selectedFeatures)
      
      classifierPerformanceInfo <- summaryFunction(
        nSelectedFeatures,
        nTotalFeatures,
        observedTestLabels,
        classifierPredictionInfo)
      
      return(classifierPerformanceInfo)
    })
  
  resampleResult$selectedFeatures <- selectedFeatures
  resampleResult$summaryMetrics <- names(resampleResult$performance[[1]])
  
  return(resampleResult)
}

extractFeatureSelectionResultFrom <- function(resampleResults,
                                              assessmentClassifiers) {
  featureSelectionResult <- list()
  
  # extract a matrix containing the performance
  # of the feature selection method from the
  # list of resample results
  featureSelectionResult$performance <- t(sapply(
    names(assessmentClassifiers),
    function (classifierName) {
      
      classifierPerformanceInfo <- t(sapply(
        resampleResults,
        function (resampleResult) {
          metricNames <-  names(resampleResult$performance[[classifierName]])
          metrics <- as.numeric(resampleResult$performance[[classifierName]])
          names(metrics) <- metricNames
          metrics
        }))
      
      metricsMean <- colMeans(classifierPerformanceInfo)
      names(metricsMean) <- paste("mean.", names(metricsMean), sep = "")
      metricsSd <- apply(classifierPerformanceInfo, 2, sd)
      names(metricsSd) <- paste("sd.", names(metricsSd), sep = "")
      
      c(metricsMean, metricsSd)
    }))
  
  # Extract the selected features as set objects.
  featureSelectionResult$selectedFeatures <- lapply(
    resampleResults,
    function (resampleResult) {
      
      featuresSubset <- extractSelectedFeaturesIndexesFrom(
        resampleResult$selectedFeatures)
      
      checkInstall("sets")
      sets::as.set(featuresSubset)
    })
  
  # The evaluation metrics are the same for all resamples, since
  # all of them come from the summary function. So, the first list
  # is stored.
  featureSelectionResult$summaryMetrics <- resampleResults[[1]]$summaryMetrics
  
  return(featureSelectionResult)
}

extractSelectedFeaturesIndexesFrom <- function(selectedFeatures) {
  
  featuresSubset <- sapply(
    selectedFeatures,
    function(feature) {
      substr(feature, 2, nchar(feature))
    })
  
  names(featuresSubset) <- NULL
  
  return(featuresSubset)
}