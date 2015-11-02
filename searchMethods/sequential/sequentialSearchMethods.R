
library(sequentialSearch)

SFS_featureSelection <- 
  function(featuresNames,
           multivariateCriterion,
           data = data,
           labels = labels,
           ...) {
    
    result <-
      SFS(attributes = featuresNames,
          evaluationFunction = evaluateFeatureSubset,
          verbose = FALSE,
          multivariateCriterion = multivariateCriterion,
          data = data,
          labels = labels,
          ...)
    
    return(result$solution)
  }

SBE_featureSelection <- 
  function(featuresNames,
           multivariateCriterion,
           data = data,
           labels = labels,
           ...) {
    
    result <-
      SBE(attributes = featuresNames,
          evaluationFunction = evaluateFeatureSubset,
          verbose = FALSE,
          multivariateCriterion = multivariateCriterion,
          data = data,
          labels = labels,
          ...)
    
    return(result$solution)
  }

SFFS_featureSelection <- 
  function(featuresNames,
           multivariateCriterion,
           data = data,
           labels = labels,
           ...) {
    
    result <-
      SFFS(attributes = featuresNames,
           evaluationFunction = evaluateFeatureSubset,
           verbose = FALSE,
           multivariateCriterion = multivariateCriterion,
           data = data,
           labels = labels,
           ...)
    
    return(result$solution)
  }

SFBE_featureSelection <- 
  function(featuresNames,
           multivariateCriterion,
           data = data,
           labels = labels,
           ...) {
    
    result <-
      SFBE(attributes = featuresNames,
           evaluationFunction = evaluateFeatureSubset,
           verbose = FALSE,
           multivariateCriterion = multivariateCriterion,
           data = data,
           labels = labels,
           ...)
    
    return(result$solution)
  }

# Function to convert a feature subset to a number, by using
# a multivariate criterion
evaluateFeatureSubset <- 
  function(attributesSubset,
           multivariateCriterion,
           data, labels, ...) {
    
    if (length(attributesSubset) == 0)
      return(-Inf)
    
    dataSubset <- data[, attributesSubset, drop = FALSE]
    criterionValue <-
      multivariateCriterion(dataSubset, labels, ...)
    
    return(criterionValue)
  }


