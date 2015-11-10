
library(dplyr)
library(caret)

source(file.path("resultGenerators", "featureSelectionDatasetResultGenerator.R"))

featureSelectionDatasetsResultGenerator <-
  function(datasets,
           featureSelectionMethods,
           assessmentClassifiers,
           summaryFunction,
           allowParallel = TRUE) {
    
    if (is.list(datasets) == FALSE ||
          length(datasets) < 1)
      stop("datasets must be a list containing at least one element.")
    if (is.list(featureSelectionMethods) == FALSE ||
          length(featureSelectionMethods) < 1)
      stop("featureSelectionMethods must be a list containing at least one element.")
    if (is.list(assessmentClassifiers) == FALSE ||
          length(assessmentClassifiers) < 1)
      stop("assessmentClassifiers must be a list containing at least one element.")
    if (is.logical(allowParallel) == FALSE)
      stop("allowParallel must be a logical value")
    
    giniOrderedDatasetsResults <- list()
    accOrderedDatasetsResults  <- list()
    datasetsResultsOrderedByName <- list()
    selectedFeaturesDataFrame <- list()
    selectedFeaturesSubsets <- list()
    
    for (dataset in datasets) {
      
      trainIndexes <- createResample(dataset$Y, times = 50)
      # trainIndexes <- createMultiFolds(dataset$Y, times = 5, k = 10)
      testIndexes <- lapply(trainIndexes,
                            function(training, allSamples) allSamples[-unique(training)],
                            allSamples = seq(along = dataset$Y))
      
      datasetResult <-
        featureSelectionDatasetResultGenerator(
          dataset, featureSelectionMethods,
          assessmentClassifiers, trainIndexes,
          testIndexes, summaryFunction, allowParallel)
      
      giniOrderedDatasetsResults[[dataset$name]] <- datasetResult$giniOrdered
      accOrderedDatasetsResults[[dataset$name]] <- datasetResult$accOrdered
      
      datasetsResultsOrderedByName[[dataset$name]] <-
        datasetResult$giniOrdered %>% 
          arrange(featureSelectionMethods) 
      
      selectedFeaturesDataFrame[[dataset$name]] <-
        datasetsResultsOrderedByName[[dataset$name]]$selectedFeatures
      
      selectedFeaturesSubsets[[dataset$name]] <- 
        datasetResult$selectedFeatures[order(
          names(datasetResult$selectedFeatures))]
    }
    
    featureSelectionMethods <- 
      datasetsResultsOrderedByName[[1]]$featureSelectionMethods
    
    selectedFeaturesDataFrame <- 
      data.frame(featureSelectionMethods,
                 selectedFeaturesDataFrame)
    
    giniScores <- sapply(datasetsResultsOrderedByName,
                            function(dr) dr$giniScore)
    
    meanGiniScores <- rowMeans(giniScores)
    
    sdGiniScores <- computeSdFor(giniScores)
      
    accScores <- sapply(datasetsResultsOrderedByName,
                           function(dr) dr$accScore)
    
    meanAccScores <- rowMeans(accScores)
    
    sdAccScores <- computeSdFor(accScores)
    
    meanGiniScores <- 
      rowMeans(sapply(datasetsResultsOrderedByName,
                      function(dr) dr$giniMean))
    
    sdGiniScores <- 
      rowMeans(sapply(datasetsResultsOrderedByName,
                      function(dr) dr$giniSd))
    
    meanAccScores <- 
      rowMeans(sapply(datasetsResultsOrderedByName,
                      function(dr) dr$giniMean))
    
    sdAccScores <- 
      rowMeans(sapply(datasetsResultsOrderedByName,
                      function(dr) dr$accSd))
    
    meanFeaturesFractions <-
      rowMeans(sapply(datasetsResultsOrderedByName,
                      function(dr) dr$featuresFraction))
    
    totalElapsedMinutesScores <-
      rowMeans(sapply(datasetsResultsOrderedByName,
                      function(dr) dr$elapsedMinutes))
    
    combinedResult <- data.frame(
      featureSelectionMethod = featureSelectionMethods,
      giniScoreMean = meanGiniScores,
      giniScoresd = sdGiniScores,
      giniMean = meanGiniScores,
      giniSd = sdGiniScores,
      accScoreMean = meanAccScores,
      accScoresd = sdAccScores,
      accMean = meanAccScores,
      accSd = sdAccScores,
      meanFeaturesFraction = meanFeaturesFractions,
      totalElapsedMinutes = totalElapsedMinutesScores)
    
    giniOrderedResult <- list(
      combined = combinedResult %>%
        arrange(desc(giniScoreMean)) %>%
        dplyr::select(featureSelectionMethod,
                      giniScoreMean, giniScoresd,
                      giniMean, giniSd,
                      meanFeaturesFraction, everything()),
      datasetsResults = giniOrderedDatasetsResults)
    
    accOrderedResult <- list(
      combined = combinedResult %>%
        arrange(desc(accScoreMean)) %>%
        dplyr::select(featureSelectionMethod,
                      accScoreMean, accScoresd,
                      accMean, accSd,
                      meanFeaturesFraction, everything()),
      datasetsResults = accOrderedDatasetsResults)
    
    list(orderedByGiniScore = giniOrderedResult,
         orderedByAccScore  = accOrderedResult,
         selectedFeaturesDataFrame = selectedFeaturesDataFrame,
         selectedFeaturesSubsets = selectedFeaturesSubsets)
  }

computeSdFor <- function(distances) {
  
  if (ncol(distances) == 1)
    return(rep(0, nrow(distances)))
  else
    return(apply(distances, 1, sd))
}

