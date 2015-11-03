rm(list = ls())

library(rUtils)
library(rDatasets)

#------------------------ Sources -------------------------------------

source(file.path("resultGenerators", "featureSelectionDatasetsResultGenerator.R"))
source(file.path("resultGenerators", "plotSingleDatasetResult.R"))
source(file.path("resultGenerators", "featureSelectionResultsClustering.R"))
source(file.path("featureSelectionMethods", "FSelectorMethods.R"))
source(file.path("featureSelectionMethods", "generateFeatureSelectionMethods.R"))
source(file.path("clusterQualityIndexes", "clusterQualityIndexes.R"))
source(file.path("classifiers", "classifiersWrappers.R"))
source(file.path("searchMethods", "sequential", "sequentialSearchMethods.R"))
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
##    - Refatorar módulo de métodos de busca.
##    - Adicionar opção genérica de método de preprocessamento dos dados.
##    - Gerenciar melhor as dependências das packages escritas para o projeto.

searchMethods = list(SFS = SFS_featureSelection,
                     SBE = SBE_featureSelection,
                     SFFS = SFFS_featureSelection,
                     SFBE = SFBE_featureSelection)

classifiers = list(LDA = ldaWrapper,
                   linearSVM = linearSVMWrapper,
                   randomForest = randomForestWrapper)

datasets = list(bc = mlBenchBreastCancer_)

clusterIndexesFeatureSelectionMethods <-
  generateFeatureSelectionMethods(
    searchMethods,
    multivariateCriterions = clusterQualityIndexes)

featureSelectionMethods <-
  c(clusterIndexesFeatureSelectionMethods,
    featureSelectionMethods)

resultGenerator <- function() {
  
  result <<- 
    featureSelectionDatasetsResultGenerator(
      datasets = datasets,
      featureSelectionMethods = featureSelectionMethods,
      assessmentClassifiers = classifiers,
      runFeatureSelectionCVInParallel = FALSE)
  
  clusteringResult <<- 
    computeHierarchicalClusteringFor(
      result$selectedFeaturesSubsets)
}

elapsedSeconds <- timeOperation(
  function() {
    runWithCpuParallelBackend(resultGenerator)
  })

print(paste("Elapsed:", elapsedSeconds / 60, "minutes"))







