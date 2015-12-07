
library(grid)
library(ggplot2)
library(Rmisc)
library(caret)
library(rDatasets)
library(AppliedPredictiveModeling)
library(rgl)

plot2dHardDatasets <- function() {
  
  datasets <- list(cassini_, circle_, gaussian_, ringnorm_, shapes_,
                   simplex_, smiley_, spirals_, threeNorm_, twoMoons_,
                   twoNorm_, xorDataSet_)
  
  customTheme <- theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18, face = "bold"),
    title = element_text(size = 18, face = "bold.italic"),
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_line(color = "gray90", size = 0.8),
    legend.position = "none") 
  
  plots <- list()
  
  for(dataset in datasets) {
    
    p <- ggplot(dataset$data(), aes(F1, F2, colour = Y)) + 
      geom_point(aes(size = 2)) +
      customTheme +
      xlab("Feature 1") +
      ylab("Feature 2") +
      ggtitle(paste(gsub("_", " ", dataset$name), "Benchmark"))
    
    plots[[dataset$name]] <- p
  }
  
  multiplot(plotlist = plots, cols = 3)
}

plot3dHardDatasets <- function() {
  
  #colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
  #customColors <<-  sample(colfunc(8))
  #plot(rep(1,8),col=customColors, pch=19,cex=2)
  customColors <<- c("#FF0000", "black", "#4169E1", "#FF6D00", "#09E98C", "#25A9B6", "#FFDA00", "purple")
  
  ncol <- 10
  colnames(cuboids_$X) <- paste("Feature", 1:ncol)
  colnames(hypercube_$X) <- paste("Feature", 1:ncol)
  
  transparentTheme(trans = .4)
  print(featurePlot(x = cuboids_$X[, 1:3],
                    y = cuboids_$Y,
                    plot = "ellipse",
                    auto.key = list(columns = 3),
                    main = paste(gsub("_", " ", cuboids_$name), "Benchmark"),
                    xlab = ""))
  
  plot3d(cuboids_$X[,1],
         cuboids_$X[,2],
         cuboids_$X[,3],
         xlab = "Feature 1",
         ylab = "Feature 2",
         zlab = "Feature 3",
         main = paste(gsub("_", " ", cuboids_$name), "Benchmark"),
         col = customColors[1:4][as.integer(cuboids_$Y)],
         size = 4)
  
  my_settings <- list(superpose.symbol=list(col = customColors, 
                                            fill = customColors,
                                            cex = rep(0.8, 8)))
  trellis.par.set(my_settings)
  
  print(featurePlot(x = hypercube_$X[, 1:3],
                    y = hypercube_$Y,
                    plot = "ellipse",
                    auto.key = list(columns = 3),
                    main = paste(gsub("_", " ", hypercube_$name), "Benchmark"),
                    xlab = "",
                    cex.main=1.5,
                    par.settings = my_settings))
  
  plot3d(hypercube_$X[,1],
         hypercube_$X[,2],
         hypercube_$X[,3],
         xlab = "Feature 1",
         ylab = "Feature 2",
         zlab = "Feature 3",
         main = paste(gsub("_", " ", hypercube_$name), "Benchmark"),
         col = customColors[as.integer(hypercube_$Y)],
         size = 4)
}

plot2dHardDatasets()
plot3dHardDatasets()

