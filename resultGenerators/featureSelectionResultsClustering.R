
computeHierarchicalClusteringFor <- function(selectedFeatureSubsets) {
  
  getSelectedFeaturesForMethod <- function(methodName) {
    lapply(selectedFeatureSubsets,
           function(subsets) subsets[[methodName]])
  }
  
  featureSelectionMethodNames <- names(result$selectedFeaturesSubsets[[1]])
  
  nMethods <- length(featureSelectionMethodNames)
  
  distanceMatrix <- matrix(data = rep(0.0, nMethods*nMethods),
                           nrow = nMethods,
                           ncol = nMethods)
  
  colnames(distanceMatrix) <- featureSelectionMethodNames
  rownames(distanceMatrix) <- featureSelectionMethodNames
  
  methodsCombinations <- combn(featureSelectionMethodNames, 2)
  
  apply(methodsCombinations, 2,
        function (combination) {
          
          method1 <- combination[1]
          method2 <- combination[2]
          
          distanceMatrix[method2, method1] <<- jaccardDistance(
            getSelectedFeaturesForMethod(method1),
            getSelectedFeaturesForMethod(method2))
          
          return(invisible())
        })
  
  hc <- hclust(as.dist(distanceMatrix))
  
  return(list(
    hclust = hc,
    distanceMatrix = distanceMatrix
  ))
}

jaccardDistance <- function(selectedSubsets1, selectedSubsets2) {
  
  jaccardDistancesForDatasets <- mapply(
    meanJaccardDistanceForDatasetResult,
    selectedSubsets1,
    selectedSubsets2)
  
  return(mean(jaccardDistancesForDatasets))
}

meanJaccardDistanceForDatasetResult <- function(subsets1, subsets2) {
  
  jaccardDistances <- mapply(featureSubsetJaccardDistance,
                             subsets1, subsets2)
  
  return(mean(jaccardDistances))
}

featureSubsetJaccardDistance <- function(subset1, subset2) {
  
  jaccardDistance <- sets::set_dissimilarity(
    subset1, subset2,
    method = "Jaccard")
  
  return(jaccardDistance)
}

