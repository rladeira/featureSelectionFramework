ga.util <- (function(){
  
  extract.best.solutions <- function(GA, features.names)
  {
    best.solutions.as.binary <- unique(GA@bestSol)
    
    lapply(1:length(best.solutions.as.binary),
           function(i){ 
             solution <- best.solutions.as.binary[[i]]
             if(is.matrix(solution) == FALSE)
              return(features.names[which(solution == 1)])
             else{
               first.solution <- solution[1,]
               return(features.names[which(first.solution == 1)])
             }})
  }
  
  fs.fitness <- function(features.codification,
                         method,
                         features,
                         class.labels,
                         ...){
    
    features.to.be.evaluated <- which(features.codification == 1)
    
    # guarantee that a solution with zero features has pretty low score
    if(length(features.to.be.evaluated) == 0)
      return(-10000000)
    
    features.subset <- as.matrix(features[, features.to.be.evaluated])
    
    fitness.value <- method(features.subset, class.labels, ...)
    
    fitness.value
  }
  
  list(extract.best.solutions = extract.best.solutions,
       fs.fitness = fs.fitness)
})()