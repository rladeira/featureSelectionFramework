
library(ggplot2)
library(ggExtra)
library(caret)
library(RColorBrewer)
library(AppliedPredictiveModeling)

plotEasySyntheticDatasets <- function(easySyntheticDatasets) {
  
  customTheme <- theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 25, face = "bold"),
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_line(color = "gray90", size = 0.8),
    legend.text = element_text(size = rel(1.0)),
    legend.key.size = unit(1, "lines"),
    legend.title = element_text(size = rel(1.0), face = "bold", hjust = 0)) 
  
  customTheme$legend.position <- c(0.5, 0.80)
  
  p1 <- ggplot(easySyntheticDatasets[[1]]$data(), aes(x = feature1, fill = Y)) + 
    geom_histogram(position = "identity") +
    customTheme +
    xlab("Característica 1") +
    ylab("nº amostras")
  
  print(p1)
  
  customTheme$legend.position <- c(0.15, 0.80)
  
  p2 <- ggplot(easySyntheticDatasets[[2]]$data(), aes(feature1, feature2, colour = Y)) + 
    geom_point() +
    customTheme +
    xlab("Característica 1") +
    ylab("Característica 2")
  
  p2 <- ggMarginal(p2, type="histogram", 
                   fill = "blue4",
                   color = "black")
  
  print(p2)
  
  transparentTheme(trans = .9)
  
  colnames(easySyntheticDatasets[[3]]$X) <- c(
    c("Característica 1", "Característica 2", "Característica 3"),
    rep("", 97))
  
  p3 <- featurePlot(x = easySyntheticDatasets[[3]]$X[, 1:3],
                    y = easySyntheticDatasets[[3]]$Y,
                    plot = "density",
                    ## Pass in options to xyplot() to 
                    ## make it prettier
                    scales = list(x = list(relation="free"),
                                  y = list(relation="free")),
                    labels = c("", ""),
                    adjust = 1.5,
                    pch = "|",
                    layout = c(3, 1),
                    auto.key = list(columns = 3))
  print(p3)
}