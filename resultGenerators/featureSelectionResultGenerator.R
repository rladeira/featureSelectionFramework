

source(file.path("resultGenerators", "featureSelectionResultEvaluator.R"))
source(file.path("resultGenerators", "featureSelectionResultDataframeGenerator.R"))
source(file.path("utils", "utils.R"))

featureSelectionResultGenerator <- 
  function(dataset,
           featureSelectionMethods,
           assessmentClassifiers,
           summaryFunction,
           trainIndexes,
           testIndexes,
           allowParallel = TRUE) {
    
    featureSelectionMethodsResult <- generateResults(
      dataset, featureSelectionMethods,
      assessmentClassifiers, summaryFunction,
      trainIndexes, testIndexes, allowParallel)
    
    featureSelectionResultDataFrame <-
      extractDataFrameFrom(featureSelectionMethodsResult,
                           assessmentClassifiers)
    
    selectedFeaturesSubsets <- 
      lapply(featureSelectionMethodsResult,
             function(r) r$info$selectedFeatures)
    
    return(list(
      dataFrame = featureSelectionResultDataFrame,
      selectedFeaturesSubsets = selectedFeaturesSubsets))
  }

# function to generate feature selection results
# for all passed methods. The classifiers
# passed as parameters are used to assess
# the quality of each solution.
generateResults <- function(dataset,
                            featureSelectionMethods,
                            assessmentClassifiers,
                            summaryFunction,
                            trainIndexes,
                            testIndexes,
                            allowParallel = TRUE) {
  
  featureSelectionMethodsResult <- list()
  
  # iterate over all feature selection methods
  for (i in 1:length(featureSelectionMethods)) {
    
    featureSelectionMethod <- featureSelectionMethods[[i]]
    featureSelectionMethodName <- names(featureSelectionMethods)[i]
    
    print(paste("Selecting features using",
                formatMethodName(featureSelectionMethodName),
                "for dataset",
                dataset$name))
    
    # perform the feature selection using the current
    # method
    elapsedSeconds <- timeOperation( 
      function() {
        featureSelectionResult <<- 
          selectFeaturesAndAssess(
            featureSelectionMethod, dataset,
            assessmentClassifiers, summaryFunction,
            trainIndexes, testIndexes, allowParallel)
      })
    
    featureSelectionResult[["elapsedMinutes"]] <- elapsedSeconds / 60
    
    # assemble a list wrapping the information about
    # the current computed result
    featureSelectionResultInfo <-
      list(featureSelectionMethod = featureSelectionMethodName,
           info = featureSelectionResult)
    
    # save the current result in the result's list
    featureSelectionMethodsResult[[featureSelectionMethodName]] <-
      featureSelectionResultInfo
  }
  
  return(featureSelectionMethodsResult)
}

# function to extract the generated results, and
# construct a data.frame to summarize and present
# the solutions of feature selection method.
extractDataFrameFrom <- 
  function(featureSelectionMethodsResult, assessmentClassifiers) {

    # extract a list containing information 
    # about the feature selection proccess
    featureSelectionMethodsResultInfo <- 
      lapply(featureSelectionMethodsResult,
             function (individualResult) {
               individualResult$info
             })
    
    resultDataFrame <-
      featureSelectionResultDataframeGenerator(
        featureSelectionMethodsResultInfo,
        assessmentClassifiers)
    
    return(resultDataFrame)
  }

