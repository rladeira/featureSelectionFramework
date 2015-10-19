

# auxiliary function to assemble a data.frame from
# feature selection result information
# In a nutshell, this function converts a list of
# objects containing the results into an R data.frame.
featureSelectionResultDataframeGenerator <- 
  function(featureSelectionInfo, assessmentClassifiers) {
    
    allSelectedFeatures <-
      lapply(featureSelectionInfo,
             function (fsMethodInfo) {
               lapply(fsMethodInfo$selectedFeatures,
                      as.character)
             })
    
    meanSelectedFeaturesNumber <-
      sapply(allSelectedFeatures,
             function (selectedFeatures) {
               mean(sapply(selectedFeatures, length))
             })
    
    meanSelectedFeaturesNumber <-
      round(meanSelectedFeaturesNumber, 2)
    
    elapsedMinutes <-
      sapply(featureSelectionInfo,
             function (fsMethodInfo) {
               fsMethodInfo$elapsedMinutes
             })
    
    # construct the data.frame wrapping the solutions
    resultDataFrame <-
      data.frame(meanSelectedFeaturesNumber = meanSelectedFeaturesNumber,
                 elapsedMinutes = elapsedMinutes)
    
    nFeatureSelectionMethods <- nrow(resultDataFrame)
    nClassifiers <- length(assessmentClassifiers)
    
    accSum    <- numeric(nFeatureSelectionMethods)
    accSdSum  <- numeric(nFeatureSelectionMethods)
    giniSum   <- numeric(nFeatureSelectionMethods)
    giniSdSum <- numeric(nFeatureSelectionMethods)
    
    # Add information, related to each one of the classifiers
    # passed as parameter, to the result data.frame 
    for (classifierName in names(assessmentClassifiers)) {
      
      classifierAccMean <-
        sapply(featureSelectionInfo,
               function (fsMethodInfo) {
                 fsMethodInfo[[classifierName]][["accMean"]]
               })
      
      classifierAccSd <-
        sapply(featureSelectionInfo,
               function (fsMethodInfo) {
                 fsMethodInfo[[classifierName]][["accSd"]]
               })
      
      classifierGiniMean <-
        sapply(featureSelectionInfo,
               function (fsMethodInfo) {
                 fsMethodInfo[[classifierName]][["giniMean"]]
               })
      
      classifierGiniSd <-
        sapply(featureSelectionInfo,
               function (fsMethodInfo) {
                 fsMethodInfo[[classifierName]][["giniSd"]]
               })
      
      accSum <- accSum + classifierAccMean
      accSdSum <- accSdSum + classifierAccSd
      giniSum <- giniSum + classifierGiniMean
      giniSdSum <- giniSdSum + classifierGiniSd
      
      resultDataFrame[concatCharacter(classifierName, "_giniMean")] <- classifierGiniMean
      resultDataFrame[concatCharacter(classifierName, "_giniSd")] <- classifierGiniSd
      resultDataFrame[concatCharacter(classifierName, "_accMean")] <- classifierAccMean
      resultDataFrame[concatCharacter(classifierName, "_accSd")] <- classifierAccSd
    }
    
    # compute quantities related to all classifiers and add to
    # the result data.frame
    resultDataFrame["giniMean"] <- giniSum / nClassifiers
    resultDataFrame["giniSd"]   <- giniSdSum / nClassifiers
    resultDataFrame["accMean"]  <- accSum / nClassifiers
    resultDataFrame["accSd"]    <- accSdSum  / nClassifiers
    
    encodedSelectedFeatures <-
      sapply(allSelectedFeatures, encodeFeatureSubsetList)
    
    resultDataFrame["selectedFeatures"] <- encodedSelectedFeatures
    
    return(resultDataFrame)
  }