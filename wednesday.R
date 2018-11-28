install.packages("class")
library(class)

install.packages("ggplot2")
library(ggplot2)


setwd("C:/Users/Admin/documents")


RawData <- read.csv("C:/Users/Admin/Documents/BreastCancerData.csv",header = FALSE)


names(RawData) <- c("ID","diagnosis","radius_mean","texture_mean","perimeter_mean","area_mean","smoothness_mean","compactness_mean","concavity_mean","concave points_mean","symmetry_mean","fractal_dimensions_mean","radius_se","texture_se","perimeter_se","area_se","smoothness_se","compactness_se","concavity_se","concave points_se","symmetry_se","fractal_dimensions_se","radius_worst","texture_worst","perimeter_worst","area_worst","smoothness_worst","compactness_worst","concavity_worst","concave points_worst","symmetry_worse","fractal_dimensions_worse")

Data_NoID <- RawData[,-1]
Data_NoResults <- Data_NoID[,-1]

FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }
Data_Normalised <- as.data.frame(lapply(Data_NoResults, FeatureScaling))
Data_Training <- Data_Normalised[1:369,]
Data_Test <- Data_Normalised[370:569,]
K_Value <- floor(sqrt(length(Data_Training[,1])))
Data_Predictions <- knn(Data_Training,Data_Test,RawData[1:369,2], k=K_Value)

Data_Reference <- Data_NoID[370:569,1]
table(Data_Predictions,Data_Reference)
