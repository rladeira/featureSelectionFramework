
library(igraph)
library(rstackdeque)

extractNeighbors <- function(attrEncoding) {
  neighbors <-
    lapply(1:length(attrEncoding),
           function (i) {
             neighbor <- attrEncoding
             if (attrEncoding[i] == 1) {
               neighbor[i] <- 0
               return(neighbor)
             } else {
               neighbor[i] <- 1
               return(neighbor)
             }
           })
  
  return(neighbors)
}

encodingAsCharacter <- function(attrEncoding) {
  paste(attrEncoding, collapse = "")
}

neighborsAsCharacter <- function(neighbors) {
  sapply(neighbors,
         function (n) {
           encodingAsCharacter(n)
         })
}

expandNode <- function(attrEncoding, visited = list()) {
  
  neighbors <- extractNeighbors(attrEncoding)
  neighbors <- Filter(
    function (n) {
      !(encodingAsCharacter(n) %in% names(visited)) 
    },
    neighbors
  )
  neighborsCharacterVector <- neighborsAsCharacter(neighbors)
  current <- encodingAsCharacter(attrEncoding)
  combinations <- expand.grid(current, neighborsCharacterVector)
  edges <- as.character(apply(combinations, 1, c))
  
  return(list(
    edges = edges,
    neighbors = neighbors
  ))
}

expansion <- expandNode(rep(0, 10))
fsEdges <- expansion$edges

G <- make_graph(fsEdges, directed = FALSE)
plot(G)


createFeatureSelectionGraph <- function(size) {
  
  startNode <- rep(0, size)
  visited <- list()
  stack <- rstack()
  stack <- insert_top(stack, startNode)
  edges <- character()
  
  while (empty(stack) == FALSE) {
    
    current <- peek_top(stack)
    expansion <- expandNode(current, visited)
    edges <- c(edges, expansion$edges)
    visited[[encodingAsCharacter(current)]] <- TRUE
    stack <- without_top(stack)
    
    for (neighbor in expansion$neighbors) {
      
      if (is.null(visited[[encodingAsCharacter(neighbor)]]) == FALSE) 
        next
      stack <- insert_top(stack, neighbor)
    }
  } 
  
  G <- make_graph(edges, directed = FALSE)
  return(G)
}

g <- createFeatureSelectionGraph(size = 10)

#plot(g)

plot(g,
     vertex.label = NA,
     vertex.size = 1,
     layout = layout.circle(g))






