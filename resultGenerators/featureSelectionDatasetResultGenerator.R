
library(dplyr)

source(file.path("resultGenerators", "featureSelectionResultGenerator.R"))

featureSelectionDatasetResultGenerator <-
  function(dataset,
           multivariateCriterions,
           featureSelectionMethods,
           assessmentClassifiers,
           runFeatureSelectionCVInParallel = TRUE,
           nFolds = 5) {
    
    # format all features names to use feature index as name
    # the preffix f is used to avoid errors related to formula
    # parsing.
    colnames(dataset$X) <- paste("f", 1:ncol(dataset$X), sep = "")
    
    featureSelectionDatasetResult <-
      featureSelectionResultGenerator(
        featureSelectionMethods = featureSelectionMethods,
        dataset = dataset,
        assessmentClassifiers = assessmentClassifiers,
        runCrossValidationInParallel = runFeatureSelectionCVInParallel,
        nFolds = nFolds)
    
    nFeatures <- ncol(dataset$X)
    
    # function to compute the comparison score between the performance
    # achieved by different feature selection methods, according to
    # some metric passed as paramater
    computeDistance <- function(featuresFraction, metricMean, metricSd) {
      distance <- sqrt(((featuresFraction -  1/nFeatures)^2 +
                          (1 - (metricMean - metricSd))^2))
      return(distance)
    }
    
    # use dplyr package to add new columns related to efficiency
    # of each feature selection method
    featureSelectionDatasetResult$dataFrame <- 
      featureSelectionDatasetResult$dataFrame %>%
      mutate(featuresFraction = meanSelectedFeaturesNumber / nFeatures,
             giniDistance = computeDistance(featuresFraction, giniMean, giniSd),
             accDistance =  computeDistance(featuresFraction, accMean, accSd))
    
    giniOrderedCombinedResult <- 
      featureSelectionDatasetResult$dataFrame %>%
      arrange(giniDistance) %>%
      dplyr::select(featureSelectionMethods,
                    giniDistance, giniMean, giniSd,
                    featuresFraction,
                    accDistance, accMean, accSd,
                    everything())
    
    accOrderedCombinedResult <- 
      featureSelectionDatasetResult$dataFrame %>%
      arrange(accDistance) %>%
      dplyr::select(featureSelectionMethods,
                    accDistance, accMean, accSd,
                    featuresFraction,
                    giniDistance, giniMean, giniSd, 
                    everything())
    
    return(list(giniOrdered = giniOrderedCombinedResult,
                accOrdered = accOrderedCombinedResult,
                selectedFeaturesSubset = 
                  featureSelectionDatasetResult$selectedFeaturesSubset))
  }