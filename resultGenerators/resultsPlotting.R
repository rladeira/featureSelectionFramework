
library(reshape2)
library(ggplot2)

barplotForElapsedMinutes <- function (result) {
  
  data <- result$orderedByTotalElapsedMinutes$combined
  data <- data %>% arrange(featureSelectionMethod)
  
  fsOrdering <- order(data$totalElapsedMinutes)   
  data <- data[fsOrdering, ]
  
  data$featureSelectionMethods <- ordered(
    data$featureSelectionMethod,
    levels = levels(data$featureSelectionMethod)[fsOrdering])   
  
  ggplot(data, aes(x = featureSelectionMethods, y = totalElapsedMinutes)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12)) +
    xlab("Feature Selection Method") + ylab("Total Elapsed Minutes") +
    ggtitle("Elapsed Time for each Feature Selection Method") 
}

boxplotsForAllMetrics <- function (result) {
  
  metrics <- Filter(function (m) grepl("^mean.", m), names(result[[1]]$combined))
  
  featureSelectionMethodNames <- sort(
    as.character(result[[1]]$combined$featureSelectionMethod))
  
  for (metric in metrics) {
    
    metricDataFrame <- sapply(
      result[[1]]$datasetsResults,
      function (datasetResult) {
        datasetResult <- datasetResult %>% arrange(featureSelectionMethods)
        datasetResult[[metric]]
      })
    rownames(metricDataFrame) <- featureSelectionMethodNames
    
    meltedMetrics <- melt(metricDataFrame)
    meltedMetrics <- meltedMetrics %>% dplyr::select(Var1, value) %>% arrange(Var1)
    colnames(meltedMetrics) <- c("featureSelectionMethods", "values")
    
    fsOrdering <- order(as.numeric(
      by(meltedMetrics$values,
         meltedMetrics$featureSelectionMethods,
         median)))   
    
    meltedMetrics$featureSelectionMethods <- ordered(
      meltedMetrics$featureSelectionMethods,
      levels = levels(meltedMetrics$featureSelectionMethods)[fsOrdering])   
    
    p <- qplot(featureSelectionMethods, values,
               data = meltedMetrics, geom = "boxplot") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12)) +
      xlab("Feature Selection Method") + ylab(metric) +
      ggtitle(metric) 
    
    print(p)
    
    return(p)
  }
}





