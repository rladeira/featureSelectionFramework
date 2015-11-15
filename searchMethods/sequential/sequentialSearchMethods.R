
library(sequentialSearch)

sfs_fs <- function(featuresNames,
                   multivariateCriterion,
                   data, labels, ...) {
  
  result <- sfs(attributes = featuresNames,
                evaluationFunction = evaluateFeatureSubset,
                verbose = FALSE,
                multivariateCriterion = multivariateCriterion,
                data = data, labels = labels, ...)
  
  return(result$solution)
}

sbs_fs <- function(featuresNames,
                   multivariateCriterion,
                   data, labels, ...) {
  
  result <- sbs(attributes = featuresNames,
                evaluationFunction = evaluateFeatureSubset,
                verbose = FALSE,
                multivariateCriterion = multivariateCriterion,
                data = data, labels = labels, ...)
  
  return(result$solution)
}

sffs_fs <- function(featuresNames,
                    multivariateCriterion,
                    data, labels, ...) {
  
  result <- sffs(attributes = featuresNames,
                 evaluationFunction = evaluateFeatureSubset,
                 verbose = FALSE,
                 multivariateCriterion = multivariateCriterion,
                 data = data,
                 labels = labels,
                 ...)
  
  return(result$solution)
}

sfbs_fs <- function(featuresNames,
                    multivariateCriterion,
                    data, labels, ...) {
  
  result <- sfbs(attributes = featuresNames,
                 evaluationFunction = evaluateFeatureSubset,
                 verbose = FALSE,
                 multivariateCriterion = multivariateCriterion,
                 data = data,
                 labels = labels,
                 ...)
  
  return(result$solution)
}

## Function to convert a feature subset to a number by using
## a multivariate criterion
evaluateFeatureSubset <- function(attributesSubset,
                                  multivariateCriterion,
                                  data, labels, ...) {
  
  if (length(attributesSubset) == 0)
    return(-Inf)
  
  dataSubset <- data[, attributesSubset, drop = FALSE]
  criterionValue <- multivariateCriterion(dataSubset, labels, ...)
  
  return(criterionValue)
}


