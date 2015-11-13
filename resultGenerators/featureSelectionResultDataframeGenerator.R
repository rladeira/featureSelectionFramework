
# auxiliary function to assemble a data.frame from
# feature selection result information
# In a nutshell, this function converts a list of
# objects containing the results into an R data.frame.
featureSelectionResultDataframeGenerator <- 
  function(featureSelectionMethodsResultInfo, assessmentClassifiers) {

    # Project all selected features subsets.
    allSelectedFeatures <-
      lapply(featureSelectionMethodsResultInfo,
             function (fsMethodInfo) {
               lapply(fsMethodInfo$selectedFeatures,
                      as.character)
             })
    
    # Extract a matrix containing performance metrics about the number of
    # features in each solutions.
    sizeMetricsMatrix <-
      t(sapply(allSelectedFeatures,
               function (selectedFeatures) {
                 selectedFeaturesSizes <- sapply(selectedFeatures, length)
                 selectedFeaturesFractions <- sapply(selectedFeatures, length) / 7
                 sizeMetrics <- c(mean(selectedFeaturesFractions), mean(selectedFeaturesSizes),
                                  sd(selectedFeaturesFractions), sd(selectedFeaturesSizes))
                 names(sizeMetrics) <- c("mean.selectedFeaturesFraction", "mean.selectedFeaturesSize",
                                         "sd.selectedFeaturesFraction", "sd.selectedFeaturesSize")
                 sizeMetrics
               }))
    
    # Project time consumed by each feature selection method.
    elapsedMinutes <-
      sapply(featureSelectionMethodsResultInfo,
             function (fsMethodInfo) {
               fsMethodInfo$elapsedMinutes
             })
    
    # Extract a matrix containing performance metrics about each
    # of the classifiers used to assess the solutions.
    individualClassifiersPerformances <-
      t(sapply(featureSelectionMethodsResultInfo,
               function (resultInfo) {
                 checkInstall("gdata")
                 perforManceVector <- gdata::unmatrix(resultInfo$performance)
                 odr <- order(names(perforManceVector))
                 perforManceVector[odr]
               }))
    
    # Extract a matrix containing overall performance metric for
    # the assessment classifiers.
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
  
    # Project a vector of encoded feature subsets.
    encodedSelectedFeatures <-
      sapply(allSelectedFeatures, encodeFeatureSubsetList)
    
    # construct the data.frame wrapping the computed metrics
    resultDataFrame <-
      data.frame(featureSelectionMethods = rownames(overallClassifiersPerformances),
                 sizeMetricsMatrix,
                 overallClassifiersPerformances,
                 individualClassifiersPerformances,
                 elapsedMinutes = elapsedMinutes,
                 selectedFeatures = encodedSelectedFeatures)
    
    return(resultDataFrame)
  }