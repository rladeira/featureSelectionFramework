rm(list = ls())

library(rl.utils)

#------------------------ Sources -------------------------------------

source(file.path("ResultsGenerators", "Multivariate",
                 "featureSelectionCombinedResultGenerator.R"))
source(file.path("SearchMethods", "searchMethods.R"))
source(file.path("ClusterQualityIndexes", "clusterQualityIndexes.R"))
source(file.path("FeatureSelectionMethods", "featureSelectionMethods.R"))
source(file.path("Classifiers", "classifiersWrappers.R"))
source(file.path("DataSets", "datasets.R"))
source(file.path("Utils", "utils.R"))

#-----------------------------------------------------------------------

searchMethods = list(SFS = sequentialForwardSelection)#,
                     #SBE = sequentialBackwardElimination)

classifiers = list(LDA = ldaWrapper,
                   linearSVM = linearSVMWrapper,
                   randomForest = randomForestWrapper)

resultGenerator <- function() {
  result <<- 
    featureSelectionCombinedResultGenerator(
      dataset = datasets$breast.cancer,
      searchMethods = searchMethods,
      multivariateCriterions = clusterQualityIndexes,
      featureSelectionMethods = featureSelectionMethods,
      assessmentClassifiers = classifiers,
      runFeatureSelectionWithCritetionsCVInParallel = FALSE,
      runStandardFeatureSelectionCVInParallel = FALSE,
      runSearchInParallel = FALSE)
}

timeOperation(function() {
  execute.with.cpu.parallel.backend(resultGenerator)
})
