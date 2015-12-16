
rm(list = ls())

source(file.path("plots", "featureSelectionResultPlotting.R"))
source(file.path("plots", "dendrogramPlot.R"))

load(file.path("main", "resultData", "finalResultA.RData"))

timePlot <- barplotForElapsedMinutes(result)
metricPlots <- boxplotsForAllMetrics(result)

addCustomDescriptionToPlots(timePlot, metricPlots)

plotDendrogram(clusteringResult$hclust)