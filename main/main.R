rm(list = ls())

library(rUtils)
library(rDatasets)

#------------------------ Sources -------------------------------------

source(file.path("resultGenerators", "multipleDatasetsFeatureSelectionResultGenerator.R"))
source(file.path("resultGenerators", "plotSingleDatasetResult.R"))
source(file.path("resultGenerators", "featureSelectionResultsClustering.R"))
source(file.path("searchMethods", "searchMethods.R"))
source(file.path("clusterQualityIndexes", "clusterQualityIndexes.R"))
source(file.path("featureSelectionMethods", "featureSelectionMethods.R"))
source(file.path("featureSelectionMethods", "generateFeatureSelectionMethods.R"))
source(file.path("classifiers", "classifiersWrappers.R"))
source(file.path("utils", "utils.R"))

#-----------------------------------------------------------------------

# remove scientific notation from double printing
options(scipen=999)

## TODO: 
##    - Adicionar desvio padrão a medida da distancia;
##    - Comentar tudo que estiver faltando;
##    - adicionar return ao final de todas funções;
##    - Analisar novamente nomenclatura das funções;
##    - Validar tudo que for possível em todas funções.

searchMethods = list(SFS = sequentialForwardSelection)

classifiers = list(LDA = ldaWrapper,
                   linearSVM = linearSVMWrapper,
                   randomForest = randomForestWrapper)

datasets_ = list(bc = mlBenchBreastCancer_,
                 gauss = gauss3_)

stdFs <- generateFeatureSelectionMethods(
  searchMethods,
  clusterQualityIndexes)

resultGenerator <- function() {
  
  result <<- 
    multipleDatasetsFeatureSelectionResultGenerator(
      datasets = datasets_,
      searchMethods = searchMethods,
      multivariateCriterions = clusterQualityIndexes,
      featureSelectionMethods = stdFs,
      assessmentClassifiers = classifiers,
      runFeatureSelectionWithCritetionsCVInParallel = TRUE,
      runStandardFeatureSelectionCVInParallel = FALSE,
      runSearchInParallel = TRUE)
  
  clusteringResult <<- 
    computeHierarchicalClusteringFor(
      result$selectedFeaturesSubsets)
}

elapsedSeconds <- timeOperation(
  function() {
    runWithCpuParallelBackend(resultGenerator)
  })

print(paste("Elapsed:", elapsedSeconds / 60, "minutes"))







