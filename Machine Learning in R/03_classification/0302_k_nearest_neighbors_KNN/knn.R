library(ggplot2)
library(ElemStatLearn)
library(caTools)
library(class)

dataSet <- read.csv("Social_Network_Ads.csv")[, 3:5]

# Encoding categorical as factor
dataSet$Purchased <- factor(dataSet$Purchased, levels = c(0,1))

# Split data
set.seed(123)
split <- sample.split(dataSet$Purchased, SplitRatio = 0.75)
trainSet <- subset(dataSet, split == TRUE)
testSet <- subset(dataSet, split == FALSE)

# Feature Scaling
trainSet[, 1:2] <- scale(trainSet[, 1:2])
testSet[, 1:2] <- scale(testSet[, 1:2])

# Fitting KNN Model
yPred <- knn(
  train = trainSet[, -3], 
  test = testSet[, -3],
  cl = trainSet[, 3], 
  k = 5
)
summary(yPred)

# Making confusion matrix
cmKNN <- table(testSet[, 3], yPred)

# Visualizing contours
set <- trainSet
  X1 <- seq(min(set[, 1]) - 1, max(set[, 1] + 1), by = 0.01)
  X2 <- seq(min(set[, 2]) - 1, max(set[, 2] + 1), by = 0.01)

gridSet <- expand.grid(X1, X2)
colnames(gridSet) <- c("Age", "EstimatedSalary")

yGrid <- knn(
  train = trainSet[, -3], 
  test = gridSet,
  cl = trainSet[, 3], 
  k = 5
)

plot(
  set[, -3],
  main = "K-Nearst Neighbor (Training Set)",
  xlab = "Age", ylab = "Estimated Salary",
  xlim = range(X1), ylim = range(X2)
)
contour(
  X1, X2, 
  matrix(
    as.numeric(yGrid), 
    length(X1), 
    length(X2)), 
  add = TRUE 
)

points(
  gridSet, 
  pch = ".", 
  col = ifelse(yGrid == 1, "royalblue", "orange")
)
points(
  set,
  pch = 21,
  bg = ifelse(set[, 3] == 1, "royalblue", "orange")
)
