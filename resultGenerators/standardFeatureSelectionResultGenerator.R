

source(file.path("resultGenerators", "featureSelectionResultEvaluator.R"))
source(file.path("resultGenerators", "featureSelectionResultDataframeGenerator.R"))
source(file.path("utils", "utils.R"))

standardFeatureSelectionResultGenerator <- 
  function(dataset,
           featureSelectionMethods,
           assessmentClassifiers,
           nFolds = 5,
           runCrossValidationInParallel = TRUE) {
    
    # function to generate feature selection results
    # for all passed methods. The classifiers
    # passed as parameters are used to assess
    # the quality of each solution.
    generateResults <- function() {
      
      methodsResult <- list()
      
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
                featureSelectionMethod,
                dataset,
                assessmentClassifiers,
                nFolds,
                runCrossValidationInParallel)
          })
        
        featureSelectionResult[["elapsedMinutes"]] <- elapsedSeconds / 60
        
        # assemble a list wrapping the information about
        # the current computed result
        featureSelectionResultInfo <-
          list(featureSelectionMethod = featureSelectionMethodName,
               info = featureSelectionResult)
        
        # save the current result in the result's list
        methodsResult[[featureSelectionMethodName]] <-
          featureSelectionResultInfo
      }
      
      methodsResult
    }
    
    # function to extract the generated results, and
    # construct a data.frame to summarize and present
    # the solutions of feature selection method.
    extractDataFrameFrom <- 
      function(methodsResult) {
        
        # auxiliary function to extract information
        # from the list containing the results
        extractDataFromResult <- function(extractor) {
          sapply(methodsResult,
                 function(individualResult) {
                   extractor(individualResult)
                 })
        }
        
        names(methodsResult) <- NULL
        
        featureSelectionMethods <- 
          extractDataFromResult(
            function(individualResult) {
              formatMethodName(
                individualResult$featureSelectionMethod)
            })
 
        # extract a list containing information 
        # about the feature selection proccess
        featureSelectionInfo <- 
          lapply(methodsResult,
                 function (individualResult) {
                   individualResult$info
                 })
        
        resultDataFrame <-
          featureSelectionResultDataframeGenerator(
            featureSelectionInfo,
            assessmentClassifiers)
        
        resultDataFrame <- cbind(featureSelectionMethods,
                                 resultDataFrame)
        
        resultDataFrame
      }
    
    featureSelectionResult <- generateResults()
    
    featureSelectionResultDataFrame <-
      extractDataFrameFrom(featureSelectionResult)
    
    selectedFeaturesSubset <- 
      lapply(featureSelectionResult,
             function(r) r$info$selectedFeatures)
    
    return(list(
      dataFrame = featureSelectionResultDataFrame,
      selectedFeaturesSubset = selectedFeaturesSubset))
  }

