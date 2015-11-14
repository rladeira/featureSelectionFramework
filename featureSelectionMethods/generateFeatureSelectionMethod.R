
generateFeatureSelectionMethod <- function(searchMethod,
                                           multivariateCriterion,
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
          ...)
      
      return(searchResult)
    }
  
  return(featureSelectionMethod)
}