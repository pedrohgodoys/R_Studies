library(ggplot2)
library(ElemStatLearn)
library(caTools)
library(MASS) # lda
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
lda <- lda(
  formula = Customer_Segment ~ .,
  data = trainSet
)
summary(lda)

trainSet <- as.data.frame(predict(lda, trainSet))
trainSet <- trainSet[, c(5, 6, 1)]

testSet <- as.data.frame(predict(lda, testSet))
testSet <- testSet[, c(5, 6, 1)]


# Fitting SVM Model
classifier <- svm(
  formula = class ~ ., 
  data = trainSet, 
  type = "C-classification",
  kernel = "linear"
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
  colnames(gridSet) <- c("x.LD1", "x.LD2")

  yGrid <- predict(classifier, type = "response", newdata = gridSet)

  plot(
    set[, -3],
    main = "SVM Radial (Training Set)",
    xlab = "LD1", ylab = "LD2",
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
  colnames(gridSet) <- c("x.LD1", "x.LD2")

  yGrid <- predict(classifier, type = "response", newdata = gridSet)

  plot(
    set[, -3],
    main = "SVM Radial (Testing Set)",
    xlab = "LD1", ylab = "LD2",
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