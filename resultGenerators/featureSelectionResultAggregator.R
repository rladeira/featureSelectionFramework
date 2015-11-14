
library(dplyr)

source(file.path("resultGenerators", "featureSelectionResultGenerator.R"))

featureSelectionResultAggregator <- function(dataset,
                                             featureSelectionMethods,
                                             assessmentClassifiers,
                                             trainIndexes,
                                             testIndexes,
                                             summaryFunction,
                                             allowParallel = TRUE) {
  
  ## format all features names to use feature index as name
  ## the preffix f is used to avoid errors related to formula
  ## parsing.
  colnames(dataset$X) <- paste("f", 1:ncol(dataset$X), sep = "")
  
  featureSelectionDatasetResult <- featureSelectionResultGenerator(
    dataset, featureSelectionMethods,
    assessmentClassifiers, summaryFunction,
    trainIndexes, testIndexes, allowParallel)
  
  aggregatedResult <- list()
  
  columnNames <- colnames(featureSelectionDatasetResult$dataFrame)
  
  ## format informations related to each performance metric in
  ## a separated data.frame ordered by its respective metric.
  for (metric in featureSelectionDatasetResult$metrics) {
    
    metricName <- gsub("mean.", "", metric)
    metricSd <- paste("sd.", metricName, sep = "")
    metricDescription <- paste("orderedBy", R.utils::capitalize(metricName), sep = "")
    metricMeanDesc <- paste("desc(", metric, ")", sep = "")
    
    metricMeanIdx <- which(columnNames == metric)
    metricSdIdx <- which(columnNames == metricSd)
    
    aggregatedResult[[metricDescription]] <-
      featureSelectionDatasetResult$dataFrame %>%
      arrange_(metricMeanDesc) %>%
      dplyr::select(featureSelectionMethods,
                    metricMeanIdx,
                    metricSdIdx,
                    everything())
  }
  
  ## format a data.frame to present the results ordered by 
  ## the time consumtion of each method.
  aggregatedResult$orderedByElapsedMinutes <-
    featureSelectionDatasetResult$dataFrame %>%
    arrange(elapsedMinutes) %>%
    dplyr::select(featureSelectionMethods,
                  elapsedMinutes,
                  everything())
  
  aggregatedResult$selectedFeatures <- featureSelectionDatasetResult$selectedFeatures
  aggregatedResult$metrics <- featureSelectionDatasetResult$metrics
  
  return(aggregatedResult)
}

orderByDescriptionFor <- function(metrics) {
  paste("orderedBy", R.utils::capitalize(gsub("mean.", "", metrics)), sep = "")
}