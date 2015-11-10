
# auxiliary function to assemble a data.frame from
# feature selection result information
# In a nutshell, this function converts a list of
# objects containing the results into an R data.frame.
featureSelectionResultDataframeGenerator <- 
  function(featureSelectionMethodsResultInfo, assessmentClassifiers) {

    allSelectedFeatures <-
      lapply(featureSelectionMethodsResultInfo,
             function (fsMethodInfo) {
               lapply(fsMethodInfo$selectedFeatures,
                      as.character)
             })
    
    sizeMetricsMatrix <-
      t(sapply(allSelectedFeatures,
               function (selectedFeatures) {
                 selectedFeaturesSizes <- sapply(selectedFeatures, length)
                 selectedFeaturesFractions <- sapply(selectedFeatures, length) / 7
                 sizeMetrics <- c(mean(selectedFeaturesFractions), mean(selectedFeaturesSizes),
                                  sd(selectedFeaturesFractions), sd(selectedFeaturesSizes))
                 names(sizeMetrics) <- c("meanSelectedFeaturesFraction", "meanSelectedFeaturesSize",
                                         "sdSelectedFeaturesFraction", "sdSelectedFeaturesSize")
                 sizeMetrics
               }))
    
    elapsedMinutes <-
      sapply(featureSelectionMethodsResultInfo,
             function (fsMethodInfo) {
               fsMethodInfo$elapsedMinutes
             })
    
    individualClassifiersPerformances <-
      t(sapply(featureSelectionMethodsResultInfo,
               function (resultInfo) {
                 checkInstall("gdata")
                 perforManceVector <- gdata::unmatrix(resultInfo$performance)
                 odr <- order(names(perforManceVector))
                 perforManceVector[odr]
               }))
    
    overallClassifiersPerformances <-
      t(sapply(featureSelectionMethodsResultInfo, 
               function (resultInfo) {
                 nColumns <- ncol(resultInfo$performance)
                 # remove stantard deviation columns
                 metrics <- resultInfo$performance[, 1:(nColumns/2)]
                 
                 metricMeans <- colMeans(metrics)
                 
                 metricSds <- apply(metrics, 2, sd)
                 names(metricSds) <- gsub("mean", "sd", names(metricSds))
                 
                 c(metricMeans, metricSds)
               }))
    
    if (all.equal(names(individualClassifiersPerformances),
                  names(overallClassifiersPerformances)) == FALSE)
      stop("mismatch between method names.")
  
    encodedSelectedFeatures <-
      sapply(allSelectedFeatures, encodeFeatureSubsetList)
    
    # construct the data.frame wrapping the computed metrics
    resultDataFrame <-
      data.frame(featureSelectionMethods = rownames(overallClassifiersPerformances),
                 sizeMetricsMatrix,
                 overallClassifiersPerformances,
                 individualClassifiersPerformances,
                 elapsedMinutes = elapsedMinutes,
                 encodedSelectedFeatures)
    
    return(resultDataFrame)
  }