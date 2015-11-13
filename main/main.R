rm(list = ls())

library(rUtils)
library(rDatasets)

#------------------------ Sources -------------------------------------

source(file.path("resultGenerators", "featureSelectionDatasetsResultGenerator.R"))
source(file.path("resultGenerators", "featureSelectionResultsClustering.R"))
source(file.path("resultGenerators", "resultsPlotting.R"))
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

## TODO: 
##    - Comentar tudo que estiver faltando;
##    - Adicionar return ao final de todas funções;
##    - Validar tudo que for possível em todas funções.
##    - Adicionar opção genérica de método de preprocessamento dos dados.
##    - Gerenciar melhor as dependências das packages escritas para o projeto.
##    - Usar '_' como substituto a ' '

searchMethods = list(SFS = SFS_FS,
                     SFFS = SFFS_FS)

classifiers = list(LDA = ldaWrapper,
                   linearSVM = linearSVMWrapper,
                   randomForest = randomForestWrapper)

datasets = list(iris_, twoMoonsB_)

clusterIndexesFeatureSelectionMethods <-
  generateFeatureSelectionMethods(
    searchMethods,
    multivariateCriterions = clusterQualityIndexes)

featureSelectionMethods <-
  c(featureSelectionMethods,
    clusterIndexesFeatureSelectionMethods)

resultGenerator <- function() {
  result <<- 
    featureSelectionDatasetsResultGenerator(
      datasets = datasets,
      featureSelectionMethods = featureSelectionMethods[1:5],
      assessmentClassifiers = classifiers,
      summaryFunction = featureSelectionResultSummary,
      allowParallel = TRUE)
  
  clusteringResult <<- 
    computeHierarchicalClusteringFor(
      result$selectedFeaturesSubsets)
}

elapsedSeconds <- timeOperation(
  function() {
    runWithCpuParallelBackend(resultGenerator)
  })

barplotForElapsedMinutes(result)
boxplotsForAllMetrics(result)

print(paste("Elapsed:", elapsedSeconds / 60, "minutes"))




