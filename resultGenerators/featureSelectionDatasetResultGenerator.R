
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
    
    resultOrderedByGini <- 
      featureSelectionDatasetResult$dataFrame %>%
      arrange(desc(mean.giniDesirability)) %>%
      dplyr::select(featureSelectionMethods,
                    mean.giniDesirability,
                    sd.giniDesirability,
                    everything())
    
    resultOrderedByAcc <- 
      featureSelectionDatasetResult$dataFrame %>%
      arrange(desc(mean.accDesirability)) %>%
      dplyr::select(featureSelectionMethods,
                    mean.accDesirability,
                    sd.accDesirability,
                    everything())
    
    return(list(orderedByGini = resultOrderedByGini,
                orderedByAcc = resultOrderedByAcc,
                selectedFeaturesSubset = 
                  featureSelectionDatasetResult$selectedFeaturesSubset))
  }