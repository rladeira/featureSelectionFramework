
ga.search <- function(attributes,
                      method,
                      data,
                      labels,
                      maxiter = 500,
                      popSize = 50,
                      run = 100,
                      ...){
  
  compute.ga.solution <- function(inParallel = TRUE){
    library(GA)
    source(file.path("Utils", "ga.util.R"))
    
    GA <- ga("binary",
             fitness = ga.util$fs.fitness,
             nBits = ncol(data),
             names = as.character(attributes),
             maxiter = maxiter,
             popSize = popSize,
             keepBest = TRUE,
             features = data,
             method = method,
             class.labels = labels,
             run = run,
             parallel = inParallel,
             monitor = function(object, ...) {},
             ...
    )
    
    ga.util$extract.best.solutions(GA, attributes)
  }
  
  compute.ga.solution.with.windows.parallel.config <- function(){
    library(parallel)
    library(doSNOW)
    cl <- makeCluster(detectCores(), type = "SOCK", outfile="")
    registerDoSNOW(cl)
    
    print("Running ga search in parallel with windows backend configuration...")
    solution <- compute.ga.solution()
    
    stopCluster(cl)
    
    solution
  }
  
  compute.ga.solution.with.linux.parallel.config <- function(){
    library(parallel)
    library(doMC)
    registerDoMC(detectCores())
    
    print("Running ga search in parallel with linux backend configuration...")
    compute.ga.solution()
  }
  
  compute.ga.solution.sequencially <- function(){
    print("Running ga search sequencially...")
    compute.ga.solution(inParallel = FALSE)
  }
  
  solutions <- tryCatch(
    if(executing.in.windows)
      compute.ga.solution.with.windows.parallel.config()
    else 
      compute.ga.solution.with.linux.parallel.config()
    ,
    error = function(e){
      compute.ga.solution.sequencially()
    })
  
  n.solutions <- length(solutions)
  solution <- solutions[[n.solutions]]
  solution
}
    
