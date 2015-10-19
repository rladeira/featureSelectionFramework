
generateFeatureSelectionMethod <-
  function(searchMethod,
           multivariateCriterion,
           runSearchInParallel = TRUE,
           ...) {
    
    if(is.function(multivariateCriterion) == FALSE)
      stop("Invalid Argument! multivariateCriterion must be a function.")
    if(is.function(searchMethod) == FALSE)
      stop("Invalid Argument! searchMethod must be a function.")
    
    featureSelectionMethod <- 
      function(data, labels, ...) {
        
        searchResult <-
          searchMethod(
            featuresNames = colnames(data), 
            multivariateCriterion = multivariateCriterion,
            data = data,
            labels = labels,
            runSearchInParallel = runSearchInParallel,
            ...)
        
        return(searchResult)
      }
    
    return(featureSelectionMethod)
  }