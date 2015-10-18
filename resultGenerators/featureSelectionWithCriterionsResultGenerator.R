

source(file.path("resultGenerators", "featureSelectionResultEvaluator.R"))
source(file.path("resultGenerators", "featureSelectionResultDataframeGenerator.R"))
source(file.path("utils", "utils.R"))

featureSelectionWithCriterionsResultGenerator <- 
  function(dataset,
           searchMethods,
           multivariateCriterions,
           assessmentClassifiers,
           nFolds = 5,
           runCrossValidationInParallel = TRUE,
           runSearchInParallel = TRUE) {
    
    # function to generate feature selection results
    # for all combination of search methods and
    # multivariate criterions. The classifiers
    # passed as parameters are used to assess
    # the quality of each solution.
    generateResults <- function() {
      
      featureSelectionWithCriterionsResult <- list()
      
      # iterate over all passed search methods
      for (j in 1:length(searchMethods)) {
        
        searchMethod <- searchMethods[[j]]
        searchMethodName <- names(searchMethods)[j]
        
        # iterate over all multivariate Criterions, which
        # are used as guides in the feature selection proccess
        for (i in 1:length(multivariateCriterions)) {
          
          multivariateCriterion <- multivariateCriterions[[i]]
          multivariateCriterionName <- names(multivariateCriterions)[i]
          
          print(paste("Selecting features using",
                      formatMethodName(multivariateCriterionName),
                      "with search method",
                      searchMethodName,
                      "for dataset",
                      dataset$name))
          
          # perform the feature selection using the current
          # combination
          elapsedSeconds <- timeOperation( 
            function() {
              featureSelectionResult <<- 
                selectFeaturesWithCriterionAndAssess(
                  multivariateCriterion,
                  searchMethod,
                  dataset,
                  assessmentClassifiers,
                  nFolds,
                  runCrossValidationInParallel,
                  runSearchInParallel)
            })
          
          featureSelectionResult[["elapsedMinutes"]] <- elapsedSeconds / 60
          
          # assemble a list wrapping the information about
          # the current computed result
          featureSelectionResultInfo <-
            list(multivariateCriterion = multivariateCriterionName,
                 searchMethod = searchMethodName,
                 info = featureSelectionResult)
          
          resultIndex <- 
            concatCharacter(searchMethodName,
                            multivariateCriterionName)
          
          # save the current result in the result's list
          featureSelectionWithCriterionsResult[[resultIndex]] <-
            featureSelectionResultInfo
        }
      }
      
      featureSelectionWithCriterionsResult
    }
    
    # function to extract the generated results, and
    # construct a data.frame to summarize and present
    # the solutions of each combinations.
    extractDataFrameFrom <- 
      function(featureSelectionWithCriterionsResult) {
        
        # auxiliary function to extract information
        # from the list containing the results
        extractDataFromResult <- function(extractor) {
          sapply(featureSelectionWithCriterionsResult,
                 function(individualResult) {
                   extractor(individualResult)
                 })
        }
        
        names(featureSelectionWithCriterionsResult) <- NULL
        
        searchMethods <- 
          extractDataFromResult(
            function(individualResult) {
              individualResult$searchMethod
            })
        
        multivariateCriterions <- 
          extractDataFromResult(
            function(individualResult) {
              formatMethodName(
                individualResult$multivariateCriterion)
            })
        
        # extract a list containing information 
        # about the feature selection proccess
        featureSelectionInfo <- 
          lapply(featureSelectionWithCriterionsResult,
                 function (individualResult) {
                   individualResult$info
                 })
        
        resultDataFrame <-
          featureSelectionResultDataframeGenerator(
            featureSelectionInfo,
            assessmentClassifiers)
        
        resultDataFrame <- 
          cbind(searchMethods,
                multivariateCriterions,
                resultDataFrame)
        
        resultDataFrame
      }
    
    featureSelectionWithCriterionsResult <- generateResults()
    
    featureSelectionWithCriterionsResultDataFrame <-
      extractDataFrameFrom(featureSelectionWithCriterionsResult)
    
    selectedFeaturesSubset <- 
      lapply(featureSelectionWithCriterionsResult,
             function(r) r$info$selectedFeatures)
    
    return(list(
      dataFrame = featureSelectionWithCriterionsResultDataFrame,
      selectedFeaturesSubset = selectedFeaturesSubset))
  }
