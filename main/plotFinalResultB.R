
rm(list = ls())

source(file.path("plots", "featureSelectionResultPlotting.R"))
source(file.path("plots", "dendrogramPlot.R"))

load(file.path("main", "resultData", "finalResultB.RData"))

barplotForElapsedMinutes(result)
boxplotsForAllMetrics(result)
plotDendrogram(clusteringResult$hclust)