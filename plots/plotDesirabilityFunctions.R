
rm(list = ls())

library(desirability)
library(ggplot2)
library(Rmisc)
library(dplyr)

nTotalFeatures <- 100

d_Metric <- dMax(low = 0, high = 1, scale = 2)
d_subsetSize <- dMin(low = 1, high = nTotalFeatures)
overall <- dOverall(d_Metric, d_subsetSize)

performanceValues <- seq(0, 1, by = 0.01)
subsetSizes <- seq(1, nTotalFeatures, by = 1)

dMetricsValues <- predict(d_Metric, performanceValues)
dSubsetValues <- predict(d_subsetSize, subsetSizes)

customTheme <- theme(
  axis.text = element_text(size = 18),
  axis.title = element_text(size = 35, face = "bold"),
  panel.background = element_rect(fill = "white", color = "black"),
  panel.grid.major = element_line(color = "gray90", size = 0.8),
  legend.position = c(0.10, 0.80),
  legend.text = element_text(size = rel(1.5)),
  legend.key.size = unit(2, "lines"),
  legend.title = element_text(size = rel(1.5), face = "bold", hjust = 0))

p1 <- ggplot() + 
  geom_line(data = data.frame(Performance = performanceValues,
                              d_1_max = dMetricsValues),
            aes(x = Performance,
                y = d_1_max,
                color = d_1_max,
                size = 2)) +
  scale_colour_gradient(low="red") +
  xlab("Performance Medida (Acurácia ou Gini Index)") +
  ylab("Max Desirability Function") +
  guides(size = FALSE) +
  customTheme

customTheme$legend.position <- c(0.90, 0.80)

p2 <- ggplot() + 
  geom_line(data = data.frame(x = subsetSizes,
                              d_2_min = dSubsetValues),
            aes(x = subsetSizes,
                y = d_2_min,
                color = d_2_min,
                size = 2)) +
  scale_colour_gradient(low="red") +
  xlab("Cardinalidade do Subconjunto Selecionado") +
  ylab("Min Desirability Function") +
  guides(size = FALSE) +
  customTheme

multiplot(plotlist = list(p1, p2), cols = 2)

library(rgl)

dOverralValues <- outer(performanceValues, subsetSizes,
                        function(p, s) predict(overall, data.frame(p, s)))

nbcol = 1000
color = (rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(dOverralValues, nbcol)
persp3d(x = performanceValues,
        y = subsetSizes,
        z = dOverralValues, 
        theta=-60, phi=25, expand=0.75,
        col=color[zcol], ticktype="detailed",
        xlab = "x",
        ylab = "y",
        zlab = "",
        axes = TRUE)

z <- dOverralValues

nrz <- nrow(z)
ncz <- ncol(z)
jet.colors <- colorRampPalette( c("red", "yellow", "green", "blue") )
nbcol <- 100
color <- jet.colors(nbcol)
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
facetcol <- cut(zfacet, nbcol)

persp(x = performanceValues,
        y = subsetSizes,
        z = dOverralValues, 
        theta=-60, phi=25, expand=0.75,
        col = color[facetcol], ticktype="detailed",
        xlab = "Performance Medida",
        ylab = "Cardinalidade",
        zlab = "D",
        axes = TRUE)


