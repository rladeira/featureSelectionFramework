rm(list = ls())

library(rUtils)
library(rDatasets)
library(HandTill2001)
library(desirability)

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
##    - Usar '_' como substituto a ' '


computeGini <- function(observed, probabilities) {
  
  AUC <- computeAUC(observed, probabilities)
  gini <- 2*AUC - 1
  
  return(gini)
}

computeAUC <- function(observed, probabilities) { 
  
  m <- multcap(observed, probabilities)
  
  return(auc(m))
}

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

searchMethods = list(SFS = SFS_FS,
                     SFFS = SFFS_FS)

classifiers = list(LDA = ldaWrapper,
                   linearSVM = linearSVMWrapper,
                   randomForest = randomForestWrapper)

datasets = list(iris = iris_,
                bc = mlBenchBreastCancer_)

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
      featureSelectionMethods = featureSelectionMethods[1:5],
      assessmentClassifiers = classifiers,
      summaryFunction = featureSelectionResultSummary,
      allowParallel = FALSE)
  
  clusteringResult <<- 
    computeHierarchicalClusteringFor(
      result$selectedFeaturesSubsets)
}

elapsedSeconds <- timeOperation(
  function() {
    runWithCpuParallelBackend(resultGenerator)
  })

print(paste("Elapsed:", elapsedSeconds / 60, "minutes"))







