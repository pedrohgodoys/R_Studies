library(ggplot2)
library(ElemStatLearn)
library(caTools)
library(caret) # pca
library(e1071) #svm
library(randomForest)

dataSet <- read.csv("Wine.csv")


# Split data
set.seed(123)
split <- sample.split(dataSet$Customer_Segment, SplitRatio = 0.8)
trainSet <- subset(dataSet, split == TRUE)
testSet <- subset(dataSet, split == FALSE)


# Feature Scaling
trainSet[, -14] <- scale(trainSet[, -14])
testSet[, -14] <- scale(testSet[, -14])


# Applying PCA
pca <- preProcess(
  x = trainSet[ ,-14], 
  method = "pca",
  pcaComp = 2
)

trainSet <- predict(pca, trainSet)
trainSet <- trainSet[, c(2,3,1)]

testSet <- predict(pca, testSet)
testSet <- testSet[, c(2,3,1)]


# Fitting SVM Model
classifier <- svm(
  formula = Customer_Segment ~ ., 
  data = trainSet, 
  type = "C-classification",
  kernel = "radial"
)
summary(classifier)

# Predict a new result
yPred <- predict(classifier, testSet[, -3])


# Making confusion matrix
cm <- table(testSet[, 3], yPred)
cm


# Visualizing contours
  ## TrainSet
  set <- trainSet
    X1 <- seq(min(set[, 1]) - 1, max(set[, 1] + 1), by = 0.01)
    X2 <- seq(min(set[, 2]) - 1, max(set[, 2] + 1), by = 0.01)

  gridSet <- expand.grid(X1, X2)
  colnames(gridSet) <- c("PC1", "PC2")

  yGrid <- predict(classifier, type = "response", newdata = gridSet)

  plot(
    set[, -3],
    main = "SVM Radial (Training Set)",
    xlab = "PC1", ylab = "PC2",
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
    col = ifelse(yGrid == 2, "orange", ifelse(yGrid == 1, "royalblue", "tomato"))
  )
  points(
    set,
    pch = 21,
    bg = ifelse(set[, 3] == 2, "orange", ifelse(set[, 3] == 1, "royalblue", "tomato"))
  )
  
  ## TestSet
  set <- testSet
    X1 <- seq(min(set[, 1]) - 1, max(set[, 1] + 1), by = 0.01)
    X2 <- seq(min(set[, 2]) - 1, max(set[, 2] + 1), by = 0.01)

    gridSet <- expand.grid(X1, X2)
  colnames(gridSet) <- c("PC1", "PC2")

  yGrid <- predict(classifier, type = "response", newdata = gridSet)

  plot(
    set[, -3],
    main = "SVM Radial (Training Set)",
    xlab = "PC1", ylab = "PC2",
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
    col = ifelse(yGrid == 2, "orange", ifelse(yGrid == 1, "royalblue", "tomato"))
  )
  points(
    set,
    pch = 21,
    bg = ifelse(set[, 3] == 2, "orange", ifelse(set[, 3] == 1, "royalblue", "tomato"))
  )