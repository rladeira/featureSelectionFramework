
library(rUtils)

# Utility function to help data partitioning
partition <- function(data, selectedFeatures,
                      labels, trainIndexes,
                      testIndexes) {
  
  list(trainData = as.matrix(data[trainIndexes, selectedFeatures]),
       testData  = as.matrix(data[ testIndexes, selectedFeatures]),
       trainLabels = as.factor(labels[trainIndexes]),
       testLabels  = as.factor(labels[ testIndexes]))
}

concatCharacter <- function(ch1, ch2) {
  paste(ch1, ch2, sep = "")
}

library(stringr)

formatMethodName <- function(str) {
  str_replace_all(str, "[_]", " ")
}

encodeFeatureSubset <- function(featureSubset) {
  
  featureSubset <- paste(featureSubset, collapse=",")
  sprintf("{%s}", paste(featureSubset, collapse = "},{"))
}

encodeFeatureSubsetListToCharacter <- function(featureSubsetList) {
  
  encodedFeatureSubsetList <- sapply(
    featureSubsetList,
    function(featureSubset){
      encodeFeatureSubset(featureSubset)
    })
  
  paste(encodedFeatureSubsetList, collapse=",")
}

encodeFeatureSubsetList <- function(featureSubsetList) {   
  
  # order which individual selected subset
  featureSubsetList <- lapply(
    featureSubsetList,
    function(f) f[order(as.numeric(f))])
  
  # encode feature subset list to be displayed after
  encodeFeatureSubsetListToCharacter(featureSubsetList)
}
