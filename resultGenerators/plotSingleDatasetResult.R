
library(dplyr)
library(ggplot2)

plotSingleDatasetResult <- function(datasetResult) {

  join <- function(character1, character2) {
    mapply(function(x,y) paste(x, y, sep = " - "),
           character1,
           character2)
  }
  
  generateNoise <- function(length, distortionOptions) {
    runif(length) - sample(distortionOptions,
                           length, replace = TRUE)
  }
  
  datasetResult <- 
    datasetResult %>%
      filter(searchMethods != "-")
  
  nRows <- nrow(datasetResult)
  indexNumber <- 1:nRows
  
  legendLabels <- 
    join(indexNumber,
         datasetResult$featureSelectionMethods)
  
  horizontalLabelNoise <- 
    generateNoise(length = nRows,
                  distortionOptions = c(0.5, -0.5))
  
  verticalLabelsNoise <- 
    generateNoise(length = nRows,
                  distortionOptions = c(1, -1))
  
  datasetResult <- 
    datasetResult %>%
    mutate(index = indexNumber)
  
  p <- ggplot(datasetResult,
              aes(meanAcc,
                  featuresFraction,
                  label = index)) +
    geom_jitter(aes(colour = factor(featureSelectionMethods),
                    size = 1 - distance)) +
    geom_text(hjust = horizontalLabelNoise,
              vjust = verticalLabelsNoise) +
    scale_size_continuous(range = c(2, 10)) +
    scale_colour_discrete(name = "Cluster Quality Indexes",
                          labels = legendLabels) +
    guides(size = FALSE,
           color = guide_legend(ncol = 2)) +
    xlab("Mean Accuracy") +
    ylab("Features Selection Fraction") +
    theme_bw() 
  
  print(p)
  
}