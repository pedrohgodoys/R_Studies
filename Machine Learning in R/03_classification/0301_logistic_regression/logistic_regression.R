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

# Fitting Logistic Regression Model
classifier <- glm(
  formula = Purchased ~ ., 
  family = "binomial", # logistic regression
  data = trainSet
)
summary(classifier)

# Predict testSet results
probPred <- predict(object = classifier, type = "response", newdata = testSet[, -3])
yPred <- ifelse(probPred > 0.5, 1, 0)

# Making confusion matrix
cm <- table(testSet[, 3], yPred)

# Visualizing contours
set <- trainSet
  X1 <- seq(min(set[, 1]) - 1, max(set[, 1] + 1), by = 0.01)
  X2 <- seq(min(set[, 2]) - 1, max(set[, 2] + 1), by = 0.01)

gridSet <- expand.grid(X1, X2)
colnames(gridSet) <- c("Age", "EstimatedSalary")

probSet <- predict(classifier, type = "response", newdata = gridSet)

yGrid <- ifelse(probSet > 0.5, 1, 0)

plot(
  set[, -3],
  main = "Logistic Regression (Training Set)",
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

# Visualizing Logistic Regression results GGPLT2
  ## trainSet
  ggplot(trainSet, aes(x = Age, y = Purchased)) + 
    geom_point(alpha =.5) +
    stat_smooth(method = "glm", se=FALSE, method.args = list(family=binomial)) +
    labs(title = "Logistic Regression (trainSet)") +
    xlab(label = "Age") + ylab(label = "Purchased Car") +
    theme_minimal()

  ggplot(trainSet, aes(x = EstimatedSalary, y = Purchased)) + 
    geom_point(alpha =.5) +
    stat_smooth(method = "glm", se=FALSE, method.args = list(family=binomial)) +
    labs(title = "Logistic Regression (trainSet)") +
    xlab(label = "Estimated Salary") + ylab(label = "Purchased Car") +
    theme_minimal()

  ## testSet
  ggplot(testSet, aes(x = Age, y = Purchased)) + 
    geom_point(alpha =.5) +
    stat_smooth(method = "glm", se=FALSE, method.args = list(family=binomial)) +
    labs(title = "Logistic Regression (trainSet)") +
    xlab(label = "Age") + ylab(label = "Purchased Car") +
    theme_minimal()

  ggplot(testSet, aes(x = EstimatedSalary, y = Purchased)) + 
    geom_point(alpha =.5) +
    stat_smooth(method = "glm", se=FALSE, method.args = list(family=binomial)) +
    labs(title = "Logistic Regression (trainSet)") +
    xlab(label = "Estimated Salary") + ylab(label = "Purchased Car") +
    theme_minimal()
