
rm(list = ls())

library(foreach)

encoding <- c(0, 1, 1, 0)

allFeatures <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
noFeatures  <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)

dummyFunc <- function(featuresEncoding, features) {
  sum(as.logical(featuresEncoding))
}

dummyFuncNegative <- function(featuresEncoding, features) {
  -sum(as.logical(featuresEncoding))
}

SFS <- function(features,
                evaluationFunction, 
                verbose = TRUE,
                isHigherBetter = TRUE) {

  searchResult <-
    sequentialSearch(
      features,
      evaluationFunction,
      type = "SFS",
      verbose = verbose,
      isHigherBetter = isHigherBetter)
  
  return(searchResult)
}

SBE <- function(features,
                evaluationFunction, 
                verbose = TRUE,
                isHigherBetter = TRUE) {
  
  searchResult <-
    sequentialSearch(
      features,
      evaluationFunction,
      type = "SBE",
      verbose = verbose,
      isHigherBetter = isHigherBetter)
  
  return(searchResult)
}

sequentialSearch <- 
  function(features,
           evaluationFunction,
           type = c("SFS", "SBE"),
           verbose = TRUE,
           isHigherBetter = TRUE) {
    
    if(is.function(evaluationFunction) == FALSE)
      stop("evaluationFunction must be a function.")
    if(is.matrix(features) == FALSE &&
       is.data.frame((features) == FALSE))
      stop("features must be either a matrix or a data.frame")
    
    featureNames <- colnames(features)
    type <- match.arg(type)
    
    if(type == "SFS")
      currentEncoding <- rep(0, ncol(features))
    else # type == "SBE"
      currentEncoding <- rep(1, ncol(features))
    
    bestScoreSoFar <- evaluationFunction(currentEncoding)
    
    while(TRUE) {
      
      if (verbose)
        cat("Evaluating", currentEncoding, "\n")
      
      stepResult <- 
        sequentialSearchStep(
          currentEncoding, bestScoreSoFar, 
          evaluationFunction, type,
          verbose, isHigherBetter)
      
      if (stepResult$isFinalStep) {
        selectedFeatures <- 
          featureNames[as.logical(stepResult$finalEncoding)]
        
        return(selectedFeatures)
      }
      
      currentEncoding <- stepResult$nextEncoding
      bestScoreSoFar <- stepResult$bestScoreSoFar
    }
    
    stop("Should never reach this line...")
  }

sequentialSearchStep <-
  function(currentEncoding,
           bestScoreSoFar,
           evaluationFunction,
           type = c("SFS", "SBE"),
           verbose = TRUE,
           isHigherBetter = TRUE) {
    
    type <- match.arg(type)
    
    if (type == "SFS") 
      neighbors <- forwardNeighbors(currentEncoding)
    else # type == "SBE"
      neighbors <- backwardNeighbors(currentEncoding)
    
    if (length(neighbors) == 0)
      return(list(
        isFinalStep = TRUE,
        bestScoreSoFar = bestScoreSoFar,
        finalEncoding = currentEncoding
      ))
    
    neighborsEvaluations <-
      getNeighborsEvaluation(neighbors, features, evaluationFunction)
    
    # Test whether or not the current encoding is the best found.
    if (isHigherBetter) {
      
      bestNeighborEvaluation <- max(neighborsEvaluations)
      bestNeighborIndex <- which.max(neighborsEvaluations)
      
      if (bestScoreSoFar >= bestNeighborEvaluation)
        return(list(
          isFinalStep = TRUE,
          bestScoreSoFar = bestScoreSoFar,
          finalEncoding = currentEncoding
        ))
    } else {
      
      bestNeighborEvaluation <- min(neighborsEvaluations)
      bestNeighborIndex <- which.min(neighborsEvaluations)
      
      if (bestScoreSoFar <= bestNeighborEvaluation)
        return(list(
          isFinalStep = TRUE,
          bestScoreSoFar = bestScoreSoFar,
          finalEncoding = currentEncoding
        ))
    }
    
    # There's a better feature subset than the current.
    bestScoreSoFar <- bestNeighborEvaluation
    nextEncoding <- neighbors[[bestNeighborIndex]]
    
    return(list(
      isFinalStep = FALSE,
      bestScoreSoFar = bestScoreSoFar,
      nextEncoding = nextEncoding
    ))
  }

SFFS <- function(features,
                 evaluationFunction, 
                 verbose = TRUE,
                 isHigherBetter = TRUE) {
  
  if(is.function(evaluationFunction) == FALSE)
    stop("evaluationFunction must be a function.")
  if(is.matrix(features) == FALSE &&
     is.data.frame((features) == FALSE))
    stop("features must be either a matrix or a data.frame")
  
  featureNames <- colnames(features)
  currentEncoding <- rep(0, ncol(features))
  bestScoreSoFar <- evaluationFunction(currentEncoding)
  
  while(TRUE) {
    
    if (verbose)
      cat("Evaluating:", currentEncoding, "\n")
    
    # ------------ Step 1: Inclusion -------------------------
    forwardStepResult <-
      sequentialSearchStep(
        currentEncoding, bestScoreSoFar,
        evaluationFunction,
        type = "SFS",
        verbose = verbose,
        isHigherBetter = isHigherBetter)
    
    if (forwardStepResult$isFinalStep) {
      
      indexes <- as.logical(forwardStepResult$finalEncoding)
      selectedFeatures <- featureNames[indexes]
      
      return(selectedFeatures)
    }
    
    currentEncoding <- forwardStepResult$nextEncoding
    bestScoreSoFar <- forwardStepResult$bestScoreSoFar
    
    # ------------ Step 2: Conditional Exclusion ---------------
    backwardStepResult <-
      sequentialSearchStep(
        currentEncoding, bestScoreSoFar,
        evaluationFunction,
        type = "SBE",
        verbose = verbose,
        isHigherBetter = isHigherBetter)
    
    if (backwardStepResult$bestScoreSoFar <= bestScoreSoFar)
      next
    
    bestScoreSoFar <- backwardStepResult$bestScoreSoFar
    currentEncoding <- backwardStepResult$nextEncoding
  }
}

forwardNeighbors <- function(state) {
  
  neighbors <-
    lapply(1:length(state),
           function (i) {
             if (state[i] == 1)
               return(invisible())
             
             neighbor <- state
             neighbor[i] <- 1
             return(neighbor)
           })
  
  return(Filter(Negate(is.null), neighbors))
}

backwardNeighbors <- function(state) {
  
  neighbors <-
    lapply(1:length(state),
           function (i) {
             if (state[i] == 0)
               return(invisible())
             
             neighbor <- state
             neighbor[i] <- 0
             return(neighbor)
           })
  
  return(Filter(Negate(is.null), neighbors))
}

getNeighborsEvaluation <- 
  function(neighbors, features, evaluationFunction) {
    
    neighborsEvaluations <-
      foreach(n = neighbors,
              .combine = "c") %dopar% {
                evaluationFunction(n, features)
              }
    
    return(neighborsEvaluations)
}

SFS( iris[, -5], dummyFunc)
SBE( iris[, -5], dummyFuncNegative)
SFFS(iris[, -5], dummyFunc)
