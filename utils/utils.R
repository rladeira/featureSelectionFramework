
library(rUtils)

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
