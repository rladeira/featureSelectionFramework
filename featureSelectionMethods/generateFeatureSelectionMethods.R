
source(file.path("featureSelectionMethods", "generateFeatureSelectionMethod.R"))

generateFeatureSelectionMethods <- 
  function(searchMethods, multivariateCriterions) {
    
    searchMethodsNames <- names(searchMethods)
    multivariateCriterionsNames <- names(multivariateCriterions)
    
    combinations <- 
      expand.grid(searchMethodsNames,
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
                  multivariateCriterion)
              
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