
library(igraph)
library(rstackdeque)

extractNeighbors <- function(attrEncoding) {
  neighbors <- lapply(
    1:length(attrEncoding),
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

createFeatureSelectionGraph <- function(nFeatures) {
  
  startNode <- rep(0, nFeatures)
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

########################## Single node neighborhood ############################################

emptyStateEncoding <- rep(0, 10)
expansion <- expandNode(emptyStateEncoding)
fsEdges <- expansion$edges

G <- make_graph(fsEdges, directed = FALSE)
plot(G, vertex.size = 25)

########################## Complete Graph plotting #############################################

plotFeatureSelectionGraph <- function(nFeatures) {
  
  G <- createFeatureSelectionGraph(nFeatures)
  
  plot(G,
       vertex.label = NA,
       vertex.size = 1.5,
       #layout = layout.sphere(g),
       layout = layout.fruchterman.reingold(G),
       vertex.color="blue",
       main = paste(nFeatures, "características"))
}

par(mfrow=c(2,2))
par(cex.main=3)

plotFeatureSelectionGraph(nFeatures = 2)
plotFeatureSelectionGraph(nFeatures = 4)
plotFeatureSelectionGraph(nFeatures = 8)
plotFeatureSelectionGraph(nFeatures = 12)







