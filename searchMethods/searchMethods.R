
source(file.path("searchMethods", "greedy", "greedy.R"))

sequentialForwardSelection <- 
  function(featuresNames,
           multivariateCriterion,
           data = data,
           labels = labels,
           runSearchInParallel = TRUE,
           ...) {
    
    forward(featuresNames,
            eval.fun = evaluateFeatureSubset,
            multivariateCriterion = multivariateCriterion,
            data = data,
            labels = labels,
            runSearchInParallel = runSearchInParallel,
            ...)
  }

sequentialBackwardElimination <- 
  function(featuresNames,
           multivariateCriterion,
           data = data,
           labels = labels,
           runSearchInParallel = TRUE,
           ...) {
    
    backward(featuresNames,
             eval.fun = evaluateFeatureSubset,
             multivariateCriterion = multivariateCriterion,
             data = data,
             labels = labels,
             runSearchInParallel = runSearchInParallel,
             ...)
  }

bestFirst <- 
  function(featuresNames,
           multivariateCriterion,
           data = data,
           labels = labels,
           runSearchInParallel = TRUE,
           ...) {
    
    best.first.search(
      featuresNames,
      eval.fun = evaluateFeatureSubset,
      multivariateCriterion = multivariateCriterion,
      data = data,
      labels = labels,
      runSearchInParallel = runSearchInParallel,
      ...)
  }

# Function to convert a feature subset to a number, by using
# a multivariate criterion
evaluateFeatureSubset <- 
  function(attributesSubset,
           multivariateCriterion,
           data,
           labels,
           ...) {
    
    dataSubset <- data[, attributesSubset, drop = FALSE]
    
    multivariateCriterion(dataSubset, labels, ...)
  }


