
library(dplyr)

source(file.path("resultGenerators", "featureSelectionCombinedResultGenerator.R"))

multipleDatasetsFeatureSelectionResultGenerator <-
  function(datasets,
           searchMethods,
           multivariateCriterions,
           featureSelectionMethods,
           assessmentClassifiers,
           runFeatureSelectionWithCritetionsCVInParallel = TRUE,
           runStandardFeatureSelectionCVInParallel = TRUE,
           runSearchInParallel = TRUE,
           nFolds = 5) {
    
    # TODO: Validar tamanho dos vetores de entrada.
    # Todos devem ter pelo menos um elemento
    
    giniOrderedDatasetsResults <- list()
    accOrderedDatasetsResults  <- list()
    datasetsResultsOrderedByName <- list()
    selectedFeaturesDataFrame <- list()
    selectedFeaturesSubsets <- list()
    
    for (dataset in datasets) {
      
      datasetResult <-
        featureSelectionCombinedResultGenerator(
          dataset = dataset,
          searchMethods = searchMethods,
          multivariateCriterions = multivariateCriterions,
          featureSelectionMethods = featureSelectionMethods,
          assessmentClassifiers = assessmentClassifiers,
          runFeatureSelectionWithCritetionsCVInParallel = 
            runFeatureSelectionWithCritetionsCVInParallel,
          runStandardFeatureSelectionCVInParallel = 
            runStandardFeatureSelectionCVInParallel,
          runSearchInParallel = runSearchInParallel,
          nFolds = nFolds)
      
      giniOrderedDatasetsResults[[dataset$name]] <- datasetResult$giniOrdered
      accOrderedDatasetsResults[[dataset$name]] <- datasetResult$accOrdered
      
      datasetsResultsOrderedByName[[dataset$name]] <-
        datasetResult$giniOrdered %>% 
          arrange(searchMethods, featureSelectionMethods) 
      
      selectedFeaturesDataFrame[[dataset$name]] <-
        datasetsResultsOrderedByName[[dataset$name]]$selectedFeatures
      
      selectedFeaturesSubsets[[dataset$name]] <- 
        datasetResult$selectedFeatures[order(
          names(datasetResult$selectedFeatures))]
    }
    
    searchMethods <- datasetsResultsOrderedByName[[1]]$searchMethods
    
    featureSelectionMethods <- 
      datasetsResultsOrderedByName[[1]]$featureSelectionMethods
    
    selectedFeaturesDataFrame <- 
      data.frame(searchMethods,
                 featureSelectionMethods,
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
      searchMethod = searchMethods,
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
        dplyr::select(searchMethod, featureSelectionMethod,
                      giniDistanceMean, giniDistanceSd,
                      giniMean, giniSd,
                      meanFeaturesFraction, everything()),
      datasetsResults = giniOrderedDatasetsResults)
    
    accOrderedResult <- list(
      combined = combinedResult %>%
        arrange(accDistanceMean) %>%
        dplyr::select(searchMethod, featureSelectionMethod,
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

