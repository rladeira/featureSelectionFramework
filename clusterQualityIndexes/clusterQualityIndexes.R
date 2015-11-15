
library(clusterCrit)
library(rUtils)

#-------------------- Cluster Quality Indexes Methods Wrappers ----------------

Ball_Hall <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Ball_Hall",
                         isHigherBetter = FALSE)
}

Banfeld_Raftery <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Banfeld_Raftery",
                         isHigherBetter = FALSE)
}

C_Index <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "C_index",
                         isHigherBetter = FALSE)
}

Calinski_Harabasz <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Calinski_Harabasz")
}

Davies_Bouldin <- function(data, labels) {
  
  computeClusterCriteria(data, labels, 
                         "Davies_Bouldin", 
                         isHigherBetter = FALSE)
}

Det_Ratio <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Det_Ratio")
}

Dunn <- function(data, labels) {
  
  computeClusterCriteria(data, labels, 
                         "Dunn")
}

Gamma <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Gamma")
}

G_Plus <- function(data, labels) {
  
  computeClusterCriteria(data, labels, 
                         "G_plus",
                         isHigherBetter = FALSE)
}

GDI11 <- function(data, labels) {
  
  computeClusterCriteria(data, labels, 
                         "GDI11")
}

GDI12 <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "GDI12")
}

GDI13 <- function(data, labels) {
  
  computeClusterCriteria(data, labels, 
                         "GDI13")
}

GDI21 <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "GDI21")
}

GDI22 <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "GDI22")
}

GDI23 <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "GDI23")
}

GDI31 <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "GDI31")
}

GDI32 <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "GDI32")
}

GDI33 <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "GDI33")
}

GDI41 <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "GDI41")
}

GDI42 <- function(data, labels) {
  
  computeClusterCriteria(data, labels, 
                         "GDI42")
}

GDI43 <- function(data, labels){
  
  computeClusterCriteria(data, labels, 
                         "GDI43")
}

GDI51 <- function(data, labels) {
  
  computeClusterCriteria(data, labels, 
                         "GDI51")
}

GDI52 <- function(data, labels) {
  
  computeClusterCriteria(data, labels, 
                         "GDI52")
}

GDI53 <- function(data, labels) {
  
  computeClusterCriteria(data, labels, 
                         "GDI53")
}

Ksq_Detw <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Ksq_DetW",
                         isHigherBetter = FALSE)
}

Log_Det_Ratio <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Log_Det_Ratio")
}

Log_SS_Ratio <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Log_SS_Ratio")
}

McClain_Rao <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "McClain_Rao",
                         isHigherBetter = FALSE)
}

PBM <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "PBM")
}

Point_Biserial <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Point_Biserial")
}

Ray_Turi <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Ray_Turi",
                         isHigherBetter = FALSE)
}

Ratkowsky_Lance <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Ratkowsky_Lance")
}

Scott_Symons <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Scott_Symons",
                         isHigherBetter = FALSE)
}

Sd_Scat <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "SD_Scat",
                         isHigherBetter = FALSE)
}

Sd_Dis <- function(data, labels) {
  
  computeClusterCriteria(data, labels, 
                         "SD_Dis",
                         isHigherBetter = FALSE)
}

S_Dbw <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "S_Dbw",
                         isHigherBetter = FALSE)
}

Silhouette <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Silhouette")
}

Tau <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Tau")
}

Trace_W <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Trace_W",
                         isHigherBetter = FALSE)
}

Trace_Wib <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Trace_WiB")
}

Wemmert_Gancarski <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Wemmert_Gancarski")
}

Xie_Beni <- function(data, labels) {
  
  computeClusterCriteria(data, labels,
                         "Xie_Beni",
                         isHigherBetter = FALSE)
}

#----------------------- Cluster Quality Indexes List ---------------

# List with all the cluster quality indexes which are used
# in the feature selection experiment.
# The indexes deactived by comments were exposing some type
# of misbehavior, when applied to feature selection.

allClusterQualityIndexes = list(
  Banfeld_Raftery = Banfeld_Raftery,
  C_Index = C_Index,
  Calinski_Harabasz = Calinski_Harabasz,
  Davies_Bouldin = Davies_Bouldin,
  Dunn = Dunn,
  Gamma = Gamma,
  G_Plus = G_Plus,
  GDI11 = GDI11,
  GDI12 = GDI12,
  GDI13 = GDI13,
  GDI21 = GDI21,
  GDI22 = GDI22,
  GDI23 = GDI23,
  GDI31 = GDI31,
  GDI32 = GDI32,
  GDI33 = GDI33,
  GDI41 = GDI41,
  GDI42 = GDI42,
  GDI43 = GDI43,
  GDI51 = GDI51,
  GDI52 = GDI52,
  GDI53 = GDI53,
  McClain_Rao = McClain_Rao,
  PBM = PBM,
  Point_Biserial = Point_Biserial,
  Ray_Turi = Ray_Turi,
  Ratkowsky_Lance = Ratkowsky_Lance,
  Sd_Scat = Sd_Scat,
  Silhouette = Silhouette,
  Tau = Tau,
  Wemmert_Gancarski = Wemmert_Gancarski
)

selectedClusterQualityIndexes = list(
)

#--------- Utility functions for dealing with clusterCrit package --------------

# Function implementing a generic way of calling the cluster quality indexes 
# methods by evaluating strings as expressions to dynamically invoke the 
# functions in clusterCrit package. 
clusterCritInvoker <- function(data, labels, method.name) {
  
  criteria.result <- intCriteria(data, labels, method.name)
  
  expr <- paste("criteria.result$", tolower(method.name))
  
  evalExpression(expr)
}

# function to wrap the clusterCrit indexes computation
# in the context of feature selection
computeClusterCriteria <- function(data, labels, clusterQualityIndexName,
                                   isHigherBetter = TRUE) {
  
  if(is.character(clusterQualityIndexName) == FALSE)
    stop(paste("Invalid Argument! cluster.quality.method.name must be a string",
               "representing the cluster quality index method to used as",
               "defined in the clusterCrit package"))
  
  labels <- as.integer(labels)
  data <- as.matrix(data)
  
  criteriaValue <- clusterCritInvoker(data, labels,
                                      clusterQualityIndexName)
  
  ifelse(isHigherBetter, criteriaValue, -1*criteriaValue)
}

