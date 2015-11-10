
library(dplyr)

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
    
    featureSelectionDatasetResult <-
      featureSelectionResultGenerator(
        dataset, featureSelectionMethods,
        assessmentClassifiers, summaryFunction,
        trainIndexes, testIndexes, allowParallel)
    
    browser()
    
    nFeatures <- ncol(dataset$X)
    
    # use dplyr package to add new columns related to efficiency
    # of each feature selection method
    featureSelectionDatasetResult$dataFrame <- 
      featureSelectionDatasetResult$dataFrame %>%
      mutate(featuresFraction = meanSelectedFeaturesNumber / nFeatures)
    
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