library(ElemStatLearn)
library(ggplot2)
library(caTools)
library(rpart)

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

# Fitting Decision Tree Model
classifier <- rpart(
  formula = Purchased ~ .,
  data = trainSet
)

summary(classifier)

# Predict testSet results
yPred <- predict(object = classifier, newdata = testSet[, -3])
yPredClass <- predict(object = classifier, newdata = testSet[, -3], type = "class")


# Making confusion matrix
cm <- table(testSet[, 3], yPredClass)

#
plot(classifier)
text(classifier)

# Visualizing contours
  ## TrainSet
  set <- trainSet
    X1 <- seq(min(set[, 1]) - 1, max(set[, 1] + 1), by = 0.01)
    X2 <- seq(min(set[, 2]) - 1, max(set[, 2] + 1), by = 0.01)

  gridSet <- expand.grid(X1, X2)
  colnames(gridSet) <- c("Age", "EstimatedSalary")

  yGrid <- predict(classifier, type = "class", newdata = gridSet)

  plot(
    set[, -3],
    main = "Decision Tree (Training Set)",
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
    col = ifelse(yGrid == 1, "royalblue", "tomato")
  )
  points(
    set,
    pch = 21,
    bg = ifelse(set[, 3] == 1, "royalblue", "tomato")
  )

  ## TestSet
  set <- testSet
    X1 <- seq(min(set[, 1]) - 1, max(set[, 1] + 1), by = 0.01)
    X2 <- seq(min(set[, 2]) - 1, max(set[, 2] + 1), by = 0.01)

  gridSet <- expand.grid(X1, X2)
  colnames(gridSet) <- c("Age", "EstimatedSalary")

  yGrid <- predict(classifier, type = "class", newdata = gridSet)

  plot(
    set[, -3],
    main = "Decision Tree (Testing Set)",
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
    col = ifelse(yGrid == 1, "royalblue", "tomato")
  )
  points(
    set,
    pch = 21,
    bg = ifelse(set[, 3] == 1, "royalblue", "tomato")
  )
