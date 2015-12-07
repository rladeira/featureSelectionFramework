
rm(list = ls())

source(file.path("plots", "featureSelectionResultPlotting.R"))
source(file.path("plots", "dendrogramPlot.R"))

load(file.path("main", "resultData", "syntheticDatasetsResult.RData"))

barplotForElapsedMinutes(result)
boxplotsForAllMetrics(result)
plotDendrogram(clusteringResult$hclust)