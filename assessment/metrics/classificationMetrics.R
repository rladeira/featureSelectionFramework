
library(HandTill2001)
library(desirability)

featureSelectionResultSummary <- 
  function(nSelectedFeatures,
           nTotalFeatures,
           observedClasses,
           classificationResult) {
    
    # desirability Function construction
    d_Metric <- dMax(low = 0, high = 1, scale = 2)
    d_subsetSize <- dMin(low = 1, high = nTotalFeatures)
    overall <- dOverall(d_Metric, d_subsetSize)
    
    acc <- mean(observedClasses == classificationResult$predictedClasses)
    gini <- computeGini(observedClasses, classificationResult$probabilities)
    
    accDesirability <- predict(overall, data.frame(acc, nSelectedFeatures))
    giniDesirability <- predict(overall, data.frame(gini, nSelectedFeatures))
    
    return(list(giniDesirability = giniDesirability,
                accDesirability = accDesirability,
                gini = gini,
                acc = acc))
  }

computeGini <- function(observed, probabilities) {
  
  AUC <- computeAUC(observed, probabilities)
  gini <- 2*AUC - 1
  
  return(gini)
}

computeAUC <- function(observed, probabilities) { 
  
  m <- multcap(observed, probabilities)
  
  return(auc(m))
}