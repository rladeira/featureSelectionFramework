
library(dplyr)

source(file.path("resultGenerators", "featureSelectionWithCriterionsResultGenerator.R"))
source(file.path("resultGenerators", "standardFeatureSelectionResultGenerator.R"))

featureSelectionCombinedResultGenerator <-
  function(dataset,
           searchMethods,
           multivariateCriterions,
           featureSelectionMethods,
           assessmentClassifiers,
           runFeatureSelectionWithCritetionsCVInParallel = TRUE,
           runStandardFeatureSelectionCVInParallel = TRUE,
           runSearchInParallel = TRUE,
           nFolds = 5
  ) {
    
    # format all features names to use feature index as name
    # the preffix f is used to avoid errors related to formula
    # parsing.
    colnames(dataset$X) <- paste("f", 1:ncol(dataset$X), sep = "")
    
    featureSelectionWithCriterionsResult <- 
      featureSelectionWithCriterionsResultGenerator(
        dataset = dataset,
        searchMethods = searchMethods,
        multivariateCriterions = clusterQualityIndexes,
        assessmentClassifiers = assessmentClassifiers,
        runCrossValidationInParallel = runFeatureSelectionWithCritetionsCVInParallel,
        runSearchInParallel = runSearchInParallel,
        nFolds = nFolds
      )
    
    standardFeatureSelectionMethodsResult <-
      standardFeatureSelectionResultGenerator(
        featureSelectionMethods = featureSelectionMethods,
        dataset = dataset,
        assessmentClassifiers = assessmentClassifiers,
        runCrossValidationInParallel = runStandardFeatureSelectionCVInParallel,
        nFolds = nFolds
      )
    
    # add column to the standard feature selection result in order
    # to match the criterions result configuration
    standardFeatureSelectionMethodsResult$dataFrame <-
      cbind(searchMethods = rep("-", nrow(standardFeatureSelectionMethodsResult$dataFrame)),
            standardFeatureSelectionMethodsResult$dataFrame)
    
    # edit the second column name to match the standard feature
    # selection result data frame
    names(featureSelectionWithCriterionsResult$dataFrame)[2] <- "featureSelectionMethods"
    
    # combine the generated results
    combinedDatasetResult <- 
      rbind(featureSelectionWithCriterionsResult$dataFrame,
            standardFeatureSelectionMethodsResult$dataFrame)
    
    selectedFeaturesSubset <- 
      c(standardFeatureSelectionMethodsResult$selectedFeaturesSubset,
        featureSelectionWithCriterionsResult$selectedFeaturesSubset)
    
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
    combinedDatasetResult <- combinedDatasetResult %>%
      mutate(featuresFraction = meanSelectedFeaturesNumber / nFeatures,
             giniDistance = computeDistance(featuresFraction, giniMean, giniSd),
             accDistance =  computeDistance(featuresFraction, accMean, accSd))
    
    giniOrderedCombinedResult <- combinedDatasetResult %>%
      arrange(giniDistance) %>%
      dplyr::select(searchMethods, featureSelectionMethods,
                    giniDistance, giniMean, giniSd, featuresFraction,
                    accDistance, accMean, accSd,
                    everything())
    
    accOrderedCombinedResult <- combinedDatasetResult %>%
      arrange(accDistance) %>%
      dplyr::select(searchMethods, featureSelectionMethods,
                    accDistance, accMean, accSd, featuresFraction,
                    giniDistance, giniMean, giniSd, 
                    everything())
    
    return(list(giniOrdered = giniOrderedCombinedResult,
                accOrdered = accOrderedCombinedResult,
                selectedFeaturesSubset = selectedFeaturesSubset))
  }