
source(file.path("featureSelectionMethods", "generateFeatureSelectionMethod.R"))

generateFeatureSelectionMethods <- 
  function(searchMethods,
           multivariateCriterions,
           runSearchInParallel = TRUE,
           ...) {
    
    if(is.list(searchMethods) == FALSE ||
         length(searchMethods) < 1)
      stop("searchMethods must be a list containing at least one function.")
    if(is.list(multivariateCriterions) == FALSE ||
         length(multivariateCriterions) < 1)
      stop("multivariateCriterions must be a list containing at least one function.")
    
    searchMethodsNames <- names(searchMethods)
    multivariateCriterionsNames <- names(multivariateCriterions)
    
    combinations <- expand.grid(searchMethodsNames,
                                multivariateCriterionsNames)
    
    colnames(combinations) <-
      c("searchMethod", "multivariateCriterion")
    
    featureSelectionMethods <-
      apply(combinations, 1,
            function(r) {
              
              searchMethod <- searchMethods[[r["searchMethod"]]]
              multivariateCriterion <-
                multivariateCriterions[[r["multivariateCriterion"]]]
              
              featureSelectionMethod <-
                generateFeatureSelectionMethod(
                  searchMethod,
                  multivariateCriterion,
                  runSearchInParallel = runSearchInParallel,
                  ...)
              
              return(featureSelectionMethod)
            })
    
    names(featureSelectionMethods) <-
      apply(combinations, 1, 
            function(r) {
              featureSelectionMethodName <-
                paste(r["searchMethod"],
                      r["multivariateCriterion"],
                      sep = "-")
            })
    
    return(featureSelectionMethods)
  }