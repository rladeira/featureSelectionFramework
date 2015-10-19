
library(dplyr)

source(file.path("resultGenerators", "featureSelectionDatasetResultGenerator.R"))

featureSelectionDatasetsResultGenerator <-
  function(datasets,
           featureSelectionMethods,
           assessmentClassifiers,
           runFeatureSelectionCVInParallel = TRUE,
           nFolds = 5) {
    
    if (is.list(datasets) == FALSE ||
          length(datasets) < 1)
      stop("datasets must be a list containing at least one element.")
    if (is.list(featureSelectionMethods) == FALSE ||
          length(featureSelectionMethods) < 1)
      stop("featureSelectionMethods must be a list containing at least one element.")
    if (is.list(assessmentClassifiers) == FALSE ||
          length(assessmentClassifiers) < 1)
      stop("assessmentClassifiers must be a list containing at least one element.")
    if (is.numeric(nFolds) == FALSE)
      stop("nFolds must be of numeric type")
    if (length(nFolds) != 1)
      stop("nFolds must a numeric vector containing just one element.")
    if (nFolds < 1)
      stop("nFolds must be a numeric value greater than one.")
    if (is.logical(runFeatureSelectionCVInParallel) == FALSE)
      stop("runFeatureSelectionCVInParallel must be a logical value")
    
    giniOrderedDatasetsResults <- list()
    accOrderedDatasetsResults  <- list()
    datasetsResultsOrderedByName <- list()
    selectedFeaturesDataFrame <- list()
    selectedFeaturesSubsets <- list()
    
    for (dataset in datasets) {
      
      datasetResult <-
        featureSelectionDatasetResultGenerator(
          dataset = dataset,
          featureSelectionMethods = featureSelectionMethods,
          assessmentClassifiers = assessmentClassifiers,
          runFeatureSelectionCVInParallel = runFeatureSelectionCVInParallel,
          nFolds = nFolds)
      
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
    
    giniDistances <- sapply(datasetsResultsOrderedByName,
                            function(dr) dr$giniDistance)
    
    meanGiniDistances <- rowMeans(giniDistances)
    
    sdGiniDistances <- computeSdFor(giniDistances)
      
    accDistances <- sapply(datasetsResultsOrderedByName,
                           function(dr) dr$accDistance)
    
    meanAccDistances <- rowMeans(accDistances)
    
    sdAccDistances <- computeSdFor(accDistances)
    
    meanGiniVector <- 
      rowMeans(sapply(datasetsResultsOrderedByName,
                      function(dr) dr$giniMean))
    
    sdGiniVector <- 
      rowMeans(sapply(datasetsResultsOrderedByName,
                      function(dr) dr$giniSd))
    
    meanAccVector <- 
      rowMeans(sapply(datasetsResultsOrderedByName,
                      function(dr) dr$giniMean))
    
    sdAccVector <- 
      rowMeans(sapply(datasetsResultsOrderedByName,
                      function(dr) dr$accSd))
    
    meanFeaturesFractions <-
      rowMeans(sapply(datasetsResultsOrderedByName,
                      function(dr) dr$featuresFraction))
    
    totalElapsedMinutesVector <-
      rowMeans(sapply(datasetsResultsOrderedByName,
                      function(dr) dr$elapsedMinutes))
    
    combinedResult <- data.frame(
      featureSelectionMethod = featureSelectionMethods,
      giniDistanceMean = meanGiniDistances,
      giniDistanceSd = sdGiniDistances,
      giniMean = meanGiniVector,
      giniSd = sdGiniVector,
      accDistanceMean = meanAccDistances,
      accDistanceSd = sdAccDistances,
      accMean = meanAccVector,
      accSd = sdAccVector,
      meanFeaturesFraction = meanFeaturesFractions,
      totalElapsedMinutes = totalElapsedMinutesVector)
    
    giniOrderedResult <- list(
      combined = combinedResult %>%
        arrange(giniDistanceMean) %>%
        dplyr::select(featureSelectionMethod,
                      giniDistanceMean, giniDistanceSd,
                      giniMean, giniSd,
                      meanFeaturesFraction, everything()),
      datasetsResults = giniOrderedDatasetsResults)
    
    accOrderedResult <- list(
      combined = combinedResult %>%
        arrange(accDistanceMean) %>%
        dplyr::select(featureSelectionMethod,
                      accDistanceMean, accDistanceSd,
                      accMean, accSd,
                      meanFeaturesFraction, everything()),
      datasetsResults = accOrderedDatasetsResults)
    
    list(giniOrdered = giniOrderedResult,
         accOrdered  = accOrderedResult,
         selectedFeaturesDataFrame = selectedFeaturesDataFrame,
         selectedFeaturesSubsets = selectedFeaturesSubsets)
  }

computeSdFor <- function(distances) {
  
  if (ncol(distances) == 1)
    return(rep(0, nrow(distances)))
  else
    return(apply(distances, 1, sd))
}

