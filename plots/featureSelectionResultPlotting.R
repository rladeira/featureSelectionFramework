
library(reshape2)
library(ggplot2)
library(dplyr)

barplotForElapsedMinutes <- function (result) {
  
  data <- result$orderedByTotalElapsedMinutes$combined
  data <- data %>% arrange(featureSelectionMethod)
  
  fsOrdering <- order(data$totalElapsedMinutes)   
  data <- data[fsOrdering, ]
  
  data$featureSelectionMethods <- ordered(
    data$featureSelectionMethod,
    levels = levels(data$featureSelectionMethod)[fsOrdering])   
  
  p <- ggplot(data, aes(x = featureSelectionMethods, y = totalElapsedMinutes)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12)) +
    xlab("Feature selection method") + ylab("Execution Time (minutes) ") +
    theme(axis.text.x = element_text(size = 16.5),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 25, face = "bold"))
  
  print(p)
  
  return(p)
}

boxplotsForAllMetrics <- function (result) {
  
  metrics <- Filter(function (m) grepl("^mean.", m), names(result[[1]]$combined))
  
  plots <- list()
  
  for (metric in metrics) {
    
    metricDataFrame <- sapply(
      result[[1]]$datasetsResults,
      function (datasetResult) {
        datasetResult <- datasetResult %>% arrange(featureSelectionMethods)
        metricValues <- datasetResult[, metric]
        names(metricValues) <- datasetResult$featureSelectionMethods
        metricValues
      })
    
    meltedMetrics <- melt(metricDataFrame)
    meltedMetrics <- meltedMetrics %>% dplyr::select(Var1, value) %>% arrange(Var1)
    colnames(meltedMetrics) <- c("featureSelectionMethods", "values")
    
    fsOrdering <- order(as.numeric(
      by(meltedMetrics$values,
         meltedMetrics$featureSelectionMethods,
         mean)))   
    
    meltedMetrics$featureSelectionMethods <- ordered(
      meltedMetrics$featureSelectionMethods,
      levels = levels(meltedMetrics$featureSelectionMethods)[fsOrdering])   
    
    p <- qplot(featureSelectionMethods, values,
               data = meltedMetrics, geom = "boxplot") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12)) +
      xlab("feature selection method") +
      ylab(metric) +
      ggtitle(metric) +
      theme(axis.text.x = element_text(size = 16.5),
            axis.text = element_text(size = 20),
            axis.title = element_text(size = 25, face = "bold"))
    
    print(p)
    plots[[metric]] <- p
  }
  
  return(plots)
}

addCustomDescriptionToPlots <- function(timePlot, metricsPlots) {
  
  timePlot <- timePlot +
    xlab("Métodos de seleção de características") +
    ylab("Tempo de execução (minutos)") +
    ggtitle("")
  
  print(timePlot)
  
  giniDesirabilityPlot <- metricsPlots[[3]]
  
  giniDesirabilityPlot <- 
    giniDesirabilityPlot +
    xlab("Métodos de seleção de características") +
    ylab("Gini Index desirability score") +
    ggtitle("") #+
    #geom_hline(yintercept = 0.99, linetype = 2, color = "red") +
    #geom_vline(xintercept = 15.5, linetype = 2,color = "blue")
  
  print(giniDesirabilityPlot)
  
  accDesirabilityPlot <- metricsPlots[[4]]
  accDesirabilityPlot <- accDesirabilityPlot +
    xlab("Métodos de seleção de características") +
    ylab("Acurácia desirability score") +
    ggtitle("")
  print(accDesirabilityPlot)
  
  giniPlot <- metricsPlots[[5]]
  giniPlot <- giniPlot +
    xlab("Métodos de seleção de características") +
    ylab("Gini Index")  +
    ggtitle("")
  print(giniPlot)
  
  accPlot <- metricsPlots[[6]]
  accPlot <- accPlot +
    xlab("Métodos de seleção de características") +
    ylab("Acurácia")  +
    ggtitle("") 
  
  print(accPlot)
}


