

source(file.path("resultGenerators", "featureSelectionResultEvaluator.R"))
source(file.path("resultGenerators", "featureSelectionResultDataframeGenerator.R"))
source(file.path("utils", "utils.R"))

featureSelectionResultGenerator <- function(dataset,
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
  
  featureSelectionResultDataFrame <- extractDataFrameFrom(
    featureSelectionMethodsResult, assessmentClassifiers)
  
  selectedFeatures <- lapply(
    featureSelectionMethodsResult,
    function(r) r$info$selectedFeatures)
  
  ## extract the names of the metrics used to assess
  ## feature selection performance
  metrics <- Filter(function (n) grepl("^mean.*", n),
                    names(featureSelectionResultDataFrame))
  
  return(list(
    dataFrame = featureSelectionResultDataFrame,
    selectedFeatures = selectedFeatures,
    metrics = metrics))
}

## function to generate feature selection results
## for all methods passed as parameters. 
## The classifiers are used to assess
## the quality of each solution.
generateResults <- function(dataset,
                            featureSelectionMethods,
                            assessmentClassifiers,
                            summaryFunction,
                            trainIndexes,
                            testIndexes,
                            allowParallel = TRUE) {
  
  featureSelectionMethodsResult <- list()
  
  ## iterate over all feature selection methods
  for (i in 1:length(featureSelectionMethods)) {
    
    featureSelectionMethod <- featureSelectionMethods[[i]]
    featureSelectionMethodName <- names(featureSelectionMethods)[i]
    
    print(paste("Selecting features using",
                featureSelectionMethodName,
                "for dataset",
                dataset$name))
    
    customAllowParallel <- grepl("SFS|SFFS", featureSelectionMethodName) && allowParallel
    
    ## perform the feature selection using the current method
    elapsedSeconds <- timeOperation( 
      function() {
        loopUntilDone( function () {
          featureSelectionResult <<- selectFeaturesAndAssess(
            featureSelectionMethod, dataset,
            assessmentClassifiers, summaryFunction,
            trainIndexes, testIndexes, 
            allowParallel = customAllowParallel)
        })
      })
    
    featureSelectionResult[["elapsedMinutes"]] <- elapsedSeconds / 60
    
    ## assemble a list wrapping the information about
    ## the current computed result
    featureSelectionResultInfo <- list(
      featureSelectionMethod = featureSelectionMethodName,
      info = featureSelectionResult)
    
    featureSelectionMethodsResult[[featureSelectionMethodName]] <-
      featureSelectionResultInfo
  }
  
  return(featureSelectionMethodsResult)
}

## function to extract the generated results, and
## construct a data.frame to summarize and present
## the solutions of feature selection method.
extractDataFrameFrom <- function(featureSelectionMethodsResult,
                                 assessmentClassifiers) {
  
  ## extract a list containing the information 
  ## about the feature selection proccess
  featureSelectionMethodsResultInfo <- lapply(
    featureSelectionMethodsResult,
    function (individualResult) {
      individualResult$info
    })
  
  resultDataFrame <- featureSelectionResultDataframeGenerator(
    featureSelectionMethodsResultInfo, assessmentClassifiers)
  
  return(resultDataFrame)
}

