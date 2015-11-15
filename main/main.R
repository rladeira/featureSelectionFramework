rm(list = ls())

library(rUtils)
library(rDatasets)

#------------------------ Sources -------------------------------------

source(file.path("resultGenerators", "featureSelectionDatasetsResultGenerator.R"))
source(file.path("resultGenerators", "featureSelectionResultsClustering.R"))
source(file.path("resultGenerators", "featureSelectionResultPlotting.R"))
source(file.path("featureSelectionMethods", "FSelectorMethods.R"))
source(file.path("featureSelectionMethods", "generateFeatureSelectionMethods.R"))
source(file.path("clusterQualityIndexes", "clusterQualityIndexes.R"))
source(file.path("assessment", "classifiers", "classifiersWrappers.R"))
source(file.path("assessment", "metrics", "classificationMetrics.R"))
source(file.path("searchMethods", "sequential", "sequentialSearchMethods.R"))
source(file.path("utils", "utils.R"))

#-----------------------------------------------------------------------

# remove scientific notation from double printing
options(scipen = 999)

searchMethods = list(SFS = sfs_fs,
                     SFFS = sffs_fs)

classifiers = list(LDA = ldaWrapper,
                   svm = svmWrapper,
                   randomForest = randomForestWrapper)

datasets = list(iris_, twoMoonsB_)

clusterIndexesFeatureSelectionMethods <- generateFeatureSelectionMethods(
  searchMethods, multivariateCriterions = clusterQualityIndexes)

featureSelectionMethods <- c(
  featureSelectionMethods,
  clusterIndexesFeatureSelectionMethods)

resultGenerator <- function() {
  result <<- featureSelectionDatasetsResultGenerator(
    datasets = datasets,
    featureSelectionMethods = featureSelectionMethods,
    assessmentClassifiers = classifiers,
    summaryFunction = featureSelectionResultSummary,
    allowParallel = TRUE)
  
  clusteringResult <<- computeHierarchicalClusteringFor(
    result$selectedFeaturesSubsets)
}

elapsedSeconds <- timeOperation(
  function() {
    runWithCpuParallelBackend(resultGenerator)
  })

barplotForElapsedMinutes(result)
boxplotsForAllMetrics(result)

print(paste("Elapsed:", elapsedSeconds / 60, "minutes"))




