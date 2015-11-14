
library(dplyr)
library(caret)

source(file.path("resultGenerators", "featureSelectionResultAggregator.R"))

featureSelectionDatasetsResultGenerator <- function(datasets,
                                                    featureSelectionMethods,
                                                    assessmentClassifiers,
                                                    summaryFunction,
                                                    nResamples = 50,
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
  
  selectedFeaturesDataFrame <- list()
  selectedFeaturesSubsets <- list()
  datasetsResultsOrderedByName <- list()
  datasetsResults <- list()
  
  ## Compute feature selection performance for each of the datasets.
  for (dataset in datasets) {
    
    trainIndexes <- createResample(dataset$Y, times = nResamples)
    testIndexes <- lapply(trainIndexes,
                          function(training, allSamples)
                            allSamples[-unique(training)],
                          allSamples = seq(along = dataset$Y))
    
    datasetResult <- featureSelectionResultAggregator(
      dataset, featureSelectionMethods,
      assessmentClassifiers, trainIndexes,
      testIndexes, summaryFunction, allowParallel)
    
    ## Extract a data.frame ordered by each metric used to assess the solutions.
    orderedByMetrics <- select_vars(names(datasetResult),
                                    -selectedFeatures, -metrics)
    
    for (orderedByMetric in orderedByMetrics) 
      datasetsResults[[orderedByMetric]][[dataset$name]] <-
      datasetResult[[orderedByMetric]]
    
    metrics <- datasetResult$metrics
    
    datasetsResultsOrderedByName[[dataset$name]] <-
      datasetResult[[1]] %>% arrange(featureSelectionMethods) 
    
    selectedFeaturesDataFrame[[dataset$name]] <-
      datasetsResultsOrderedByName[[dataset$name]]$selectedFeatures
    
    selectedFeaturesSubsets[[dataset$name]] <- 
      datasetResult$selectedFeatures[order(
        names(datasetResult$selectedFeatures))]
  }
  
  ## Compute combined metric scores for all the datasets.
  combinedScores <- lapply(
    metrics, 
    function (metric) {
      metricScore <- sapply(datasetsResultsOrderedByName,
                            function(datasetResult) datasetResult[[metric]])
      meanMetric <- rowMeans(metricScore)
      sdMetric <- computeSdFor(metricScore)
      combinedScore <- cbind(meanMetric, sdMetric)
      colnames(combinedScore) <- c(metric, gsub("mean", "sd", metric))
      combinedScore
    })
  
  combinedScores <- Reduce(cbind, combinedScores)
  
  featureSelectionMethods <- 
    datasetsResultsOrderedByName[[1]]$featureSelectionMethods
  
  selectedFeaturesDataFrame <- data.frame(featureSelectionMethods,
                                          selectedFeaturesDataFrame)
  
  totalElapsedMinutes <- rowSums(
    sapply(datasetsResultsOrderedByName,
           function(dr) dr$elapsedMinutes))
  
  combinedResult <- data.frame(
    featureSelectionMethod = featureSelectionMethods,
    combinedScores,
    totalElapsedMinutes = totalElapsedMinutes)
  
  ## Generate a final list containing the combined results 
  ## ordered by each one the evaluation metrics.
  finalResult <- lapply(
    metrics, 
    function (metric) {
      metricDesc <- paste("desc(", metric, ")", sep = "")
      meanMetricIdx <- which(names(combinedResult) == metric)
      sdMetricIdx <- which(names(combinedResult) == gsub("mean", "sd", metric))
      
      orderedCombinedResult <-
        combinedResult %>%
        arrange_(metricDesc) %>%
        dplyr::select(featureSelectionMethod,
                      meanMetricIdx, sdMetricIdx,
                      everything())
      
      list(combined = orderedCombinedResult,
           datasetsResults = datasetsResults[[orderByDescriptionFor(metric)]])
    })
  names(finalResult) <- orderByDescriptionFor(metrics)
  
  ## Add a data.frame ordered by overall time consumption to the result list.
  finalResult$orderedByTotalElapsedMinutes$combined <-
    combinedResult %>%
    arrange(totalElapsedMinutes) %>%
    dplyr::select(featureSelectionMethod,
                  totalElapsedMinutes,
                  everything())
  
  finalResult$selectedFeaturesDataFrame <- selectedFeaturesDataFrame
  finalResult$selectedFeaturesSubsets <- selectedFeaturesSubsets
  
  return(finalResult)
}

computeSdFor <- function(distances) {
  
  if (ncol(distances) == 1)
    return(rep(0, nrow(distances)))
  else
    return(apply(distances, 1, sd))
}

