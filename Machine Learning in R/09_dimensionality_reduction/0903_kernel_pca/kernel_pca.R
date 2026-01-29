#install.packages("kernlab")
library(kernlab) # kernel PCA
library(ggplot2)
library(ElemStatLearn)
library(caTools)

dataSet <- read.csv("Social_Network_Ads.csv")[, 3:5]

# Split data
set.seed(123)
split <- sample.split(dataSet$Purchased, SplitRatio = 0.75)
trainSet <- subset(dataSet, split == TRUE)
testSet <- subset(dataSet, split == FALSE)

# Feature Scaling
trainSet[, 1:2] <- scale(trainSet[, 1:2])
testSet[, 1:2] <- scale(testSet[, 1:2])

# Applying Kernel PCA
kpca <- kpca(
    x = ~.,
    data = trainSet[, -3],
    kernel = "rbfdot",
    features = 2
)

trainSetPCA <- as.data.frame(predict(kpca, trainSet))
trainSetPCA$Purchased <- trainSet$Purchased

testSetPCA <- as.data.frame(predict(kpca, testSet))
testSetPCA$Purchased <- testSet$Purchased

# Fitting Logistic Regression Model
classifier <- glm(
  formula = Purchased ~ ., 
  family = "binomial", # logistic regression
  data = trainSetPCA
)
summary(classifier)

# Predict testSet results
probPred <- predict(object = classifier, type = "response", newdata = testSetPCA[, -3])
yPred <- ifelse(probPred > 0.5, 1, 0)

# Making confusion matrix
cm <- table(testSet[, 3], yPred)

# Visualizing contours
set <- trainSetPCA
  X1 <- seq(min(set[, 1]) - 1, max(set[, 1] + 1), by = 0.01)
  X2 <- seq(min(set[, 2]) - 1, max(set[, 2] + 1), by = 0.01)

gridSet <- expand.grid(X1, X2)
colnames(gridSet) <- c("V1", "V2")

probSet <- predict(classifier, type = "response", newdata = gridSet)

yGrid <- ifelse(probSet > 0.5, 1, 0)

plot(
  set[, -3],
  main = "Logistic Regression (Training Set)",
  xlab = "V1", ylab = "V2",
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
