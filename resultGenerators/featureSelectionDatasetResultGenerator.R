
library(dplyr)
library(desirability)

source(file.path("resultGenerators", "featureSelectionResultGenerator.R"))

featureSelectionDatasetResultGenerator <-
  function(dataset,
           featureSelectionMethods,
           assessmentClassifiers,
           trainIndexes,
           testIndexes,
           summaryFunction,
           allowParallel = TRUE) {

    # format all features names to use feature index as name
    # the preffix f is used to avoid errors related to formula
    # parsing.
    colnames(dataset$X) <- paste("f", 1:ncol(dataset$X), sep = "")
    
    # desirability Function construction
    d_Metric <- dMax(low = 0, high = 1, scale = 2)
    d_subsetSize <- dMin(low = 1, high = ncol(dataset$X))
    overall <- dOverall(d_Metric, d_subsetSize)
    
    featureSelectionDatasetResult <-
      featureSelectionResultGenerator(
        dataset, featureSelectionMethods,
        assessmentClassifiers, summaryFunction,
        trainIndexes, testIndexes, allowParallel)
    
    nFeatures <- ncol(dataset$X)
    
    # function to compute the comparison score between the performance
    # achieved by different feature selection methods, according to
    # some metric passed as paramater
    computeDesirabilityFunctionValue <- function(featuresFraction, metricMean) {
      value <- predict(overall, data.frame(metricMean, featuresFraction))
      return(value)
    }
    
    # use dplyr package to add new columns related to efficiency
    # of each feature selection method
    featureSelectionDatasetResult$dataFrame <- 
      featureSelectionDatasetResult$dataFrame %>%
      mutate(featuresFraction = meanSelectedFeaturesNumber / nFeatures,
             giniScore = computeDesirabilityFunctionValue(meanSelectedFeaturesNumber, giniMean),
             accScore =  computeDesirabilityFunctionValue(meanSelectedFeaturesNumber, accMean))
    
    giniOrderedCombinedResult <- 
      featureSelectionDatasetResult$dataFrame %>%
      arrange(desc(giniScore)) %>%
      dplyr::select(featureSelectionMethods,
                    giniScore, giniMean, giniSd,
                    featuresFraction,
                    accScore, accMean, accSd,
                    everything())
    
    accOrderedCombinedResult <- 
      featureSelectionDatasetResult$dataFrame %>%
      arrange(desc(accScore)) %>%
      dplyr::select(featureSelectionMethods,
                    accScore, accMean, accSd,
                    featuresFraction,
                    giniScore, giniMean, giniSd, 
                    everything())
    
    return(list(giniOrdered = giniOrderedCombinedResult,
                accOrdered = accOrderedCombinedResult,
                selectedFeaturesSubset = 
                  featureSelectionDatasetResult$selectedFeaturesSubset))
  }