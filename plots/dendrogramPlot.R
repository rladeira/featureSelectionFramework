
library(ape)

plotDendrogram <- function(hclust) {
  
  hc <- as.dendrogram(hclust)
  
  plot(as.phylo(clusteringResult$hclust),
       label.offset = 0.003)
}
