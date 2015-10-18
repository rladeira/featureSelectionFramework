rm(list = ls())

library(rUtils)
library(rDatasets)

#------------------------ Sources -------------------------------------

source(file.path("resultGenerators", "multipleDatasetsFeatureSelectionResultGenerator.R"))
source(file.path("resultGenerators", "plotSingleDatasetResult.R"))
source(file.path("searchMethods", "searchMethods.R"))
source(file.path("clusterQualityIndexes", "clusterQualityIndexes.R"))
source(file.path("featureSelectionMethods", "featureSelectionMethods.R"))
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

searchMethods = list(SFS = sequentialForwardSelection,
                     SBE = sequentialBackwardElimination)

classifiers = list(LDA = ldaWrapper,
                   linearSVM = linearSVMWrapper,
                   randomForest = randomForestWrapper)

# datasets_ = list(pcirc = pcirc2bA_,
#                  bc = mlBenchBreastCancer_,
#                  gauss = gauss3_,
#                  twoMoons = twoMoonsB_)

datasets_ = list(bc = mlBenchBreastCancer_,
                 gauss = gauss3_,
                 pcirc = pcirc2bA_,
                 twoMoons = twoMoonsB_)

resultGenerator <- function() {
  result <<- 
    multipleDatasetsFeatureSelectionResultGenerator(
      datasets = datasets_,
      searchMethods = searchMethods,
      multivariateCriterions = clusterQualityIndexes,
      featureSelectionMethods = featureSelectionMethods,
      assessmentClassifiers = classifiers,
      runFeatureSelectionWithCritetionsCVInParallel = TRUE,
      runStandardFeatureSelectionCVInParallel = FALSE,
      runSearchInParallel = TRUE)
}

elapsedSeconds <- timeOperation(
  function() {
    runWithCpuParallelBackend(resultGenerator)
  })

print(paste("Elapsed:", elapsedSeconds / 60, "minutes"))

featuresSubsetJaccardDistance <- function(subset1, subset2) {
  
  jaccardDistance <- sets::set_dissimilarity(
    subset1, subset2,
    method = "Jaccard")
  
  return(jaccardDistance)
}

meanJaccardDistanceForDataset <- function(subsets1, subsets2) {
  
  jaccardDistances <- 
    mapply(featuresSubsetJaccardDistance,
           subsets1, subsets2)
  
  return(mean(jaccardDistances))
}

jaccardDistance <- function(selectedSubsets1, selectedSubsets2) {
  
  jaccardDistancesForDatasets <-
    mapply(meanJaccardDistanceForDataset,
           selectedSubsets1,
           selectedSubsets2)
  
  meanJaccardDistance <- mean(jaccardDistancesForDatasets)
  return(meanJaccardDistance)
}

getSelectedFeaturesForMethod<- function(methodName) {
  lapply(result$selectedFeaturesSubsets,
         function(subsets) subsets[[methodName]])
}

featureSelectionMethodNames <- 
  names(result$selectedFeaturesSubsets[[1]])

nMethods <- length(featureSelectionMethodNames)

distMatrix <- matrix(data = rep(0, nMethods*nMethods),
                     nrow = nMethods,
                     ncol = nMethods)

colnames(distMatrix) <- featureSelectionMethodNames
rownames(distMatrix) <- featureSelectionMethodNames

methodsCombinations <- combn(featureSelectionMethodNames, 2)

apply(methodsCombinations, 2,
      function (comb) {
        
        method1 <- comb[1]
        method2 <- comb[2]
        
        distMatrix[method2, method1] <<-
          jaccardDistance(
            getSelectedFeaturesForMethod(method1),
            getSelectedFeaturesForMethod(method2))
        
        return(invisible())
      })

hc <- hclust(as.dist(distMatrix))
plot(hc)







