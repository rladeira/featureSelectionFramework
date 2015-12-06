
library(FSelector)

options(java.parameters = "-Xmx10g")

chiSquared <- function(data, labels) {
  selectBestFeatures(chi.squared, data, labels)
}

informationGain <- function(data, labels) {
  selectBestFeatures(information.gain, data, labels)
}

gainRatio <- function(data, labels) {
  selectBestFeatures(information.gain, data, labels)
}

symmetricalUncertainty <- function(data, labels) {
  selectBestFeatures(symmetrical.uncertainty, data, labels)
}

cfs_ <- function(data, labels, data.fs) {
  extractBestFeatures(cfs, data, labels)
}

consistency_ <- function(data, labels) {
  extractBestFeatures(consistency, data, labels)
}

pearsonCorrelation <- function(data, labels) {
  selectBestFeatures(linear.correlation, data, labels)
}

spearmanCorrelation <- function(data, labels) {
  selectBestFeatures(rank.correlation, data, labels)
}

fSelectorMethods <- list(
  Information_Gain = informationGain,
  Chi_Squared = chiSquared,
  Gain_Ratio = gainRatio,
  Symmetrical_Uncertainty = symmetricalUncertainty,
  CFS = cfs_,
  Consistency = consistency_,
  Pearson_Correlation = pearsonCorrelation,
  Spearman_Correlation = spearmanCorrelation)


toFormula <- function(attributes, class = "label") {
  
  as.formula(paste(
    class, 
    paste(attributes, sep = "", collapse = " + "),
    sep = " ~ "))
}

# function for selecting best features when the FSelector method
# returns scores for each one of the features
selectBestFeatures <- function(featureSelectionMethod,
                               data, labels,
                               categoricalLabels = FALSE) {
  
  if(categoricalLabels == FALSE) 
    labels = as.numeric(labels)
  
  formula <- toFormula(colnames(data)) 
  
  data <- transform(as.data.frame(data),
                    label = labels)
  
  weights <- featureSelectionMethod(formula, data)
  
  cutoff.biggest.diff(weights)
}

# function for selecting best features when the FSelector method
# returns a subset of features
extractBestFeatures <- function(featureSelectionMethod,
                                data, labels){
  
  formula <- toFormula(colnames(data))
  data <- transform(as.data.frame(data), label = labels)
  
  featureSelectionMethod(formula, data)
}



