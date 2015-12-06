rm(list = ls())

library(rUtils)
library(rDatasets)

#------------------------ Sources -------------------------------------

source(file.path("resultGenerators", "featureSelectionDatasetsResultGenerator.R"))
source(file.path("resultGenerators", "featureSelectionResultsClustering.R"))
source(file.path("plots", "featureSelectionResultPlotting.R"))
source(file.path("plots", "dendrogramPlot.R"))
source(file.path("featureSelectionMethods", "FSelectorMethods.R"))
source(file.path("featureSelectionMethods", "generateFeatureSelectionMethods.R"))
source(file.path("clusterQualityIndexes", "clusterQualityIndexes.R"))
source(file.path("assessment", "classifiers", "classifiersWrappers.R"))
source(file.path("assessment", "metrics", "classificationMetrics.R"))
source(file.path("searchMethods", "sequential", "sequentialSearchMethods.R"))
source(file.path("utils", "syntheticBases.R"))
source(file.path("utils", "utils.R"))

#-----------------------------------------------------------------------

# remove scientific notation from double printing
options(scipen = 999)

searchMethods = list(SFS = sfs_fs,
                     SFFS = sffs_fs)

classifiers = list(LDA = ldaWrapper,
                   svm = svmWrapper,
                   randomForest = randomForestWrapper)

clusterIndexesFeatureSelectionMethods <- generateFeatureSelectionMethods(
  searchMethods, multivariateCriterions = selectedClusterQualityIndexes)

featureSelectionMethods <- c(
  fSelectorMethods,
  clusterIndexesFeatureSelectionMethods)

datasets <- list(breastCancer_#,
#                  cassini_,
#                  circle_,
#                  cuboids_,
#                  dna_,
#                  gaussian_,
#                  glass_,
#                  houseVotes84_,
#                  hypercube_,
#                  ionosphere_,
#                  ringnorm_,
#                  satellite_,
#                  shapes_,
#                  simplex_,
#                  smiley_,
#                  sonar_,
#                  spamBase_,
#                  spirals_,
#                  steelPlatesFaults_,
#                  threeNorm_,
#                  titanic_,
#                  twoMoons_,
#                  twoNorm_,
#                  vehicle_,
#                  waveform_,
#                  xorDataSet_
                 )

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

timePlot <- barplotForElapsedMinutes(result)
metricsPlots <- boxplotsForAllMetrics(result)

addCustomDescriptionToPlots(timePlot, metricsPlots)

plotDendrogram(clusteringResult$hclust)

print(paste("Elapsed:", elapsedSeconds / 60, "minutes"))

