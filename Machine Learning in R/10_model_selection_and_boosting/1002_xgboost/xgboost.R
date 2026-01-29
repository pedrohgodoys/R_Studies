#install.packages("xgboost")
library(xgboost)
library(caTools)
library(caret) # cv

dataSet <- read.csv("Churn_Modelling.csv")[ , -c(1, 2, 3)]


# Encoding categorical as factor
dataSet$Geography <- as.numeric(
  factor(
    dataSet$Geography, 
    levels = c("France", "Spain", "Germany"),
    labels = c(1, 2, 3)
  )
)

dataSet$Gender <- as.numeric(
  factor(
    dataSet$Gender, 
    levels = c("Female", "Male"),
    labels = c(1, 2)
  )
)


# Split data
set.seed(123)
split <- sample.split(dataSet$Exited, SplitRatio = 0.8)
trainSet <- subset(dataSet, split == TRUE)
testSet <- subset(dataSet, split == FALSE)


# Fitting XGBoost to trainSet
classifier <- xgboost(
  data = as.matrix(trainSet[,-11]),
  label = trainSet$Exited,
  nrounds = 10
)
classifier

# Applying K-fold Cross Validation
folds <- createFolds(
  y = trainSet$Exited, 
  k = 10
)

cv <- lapply(
  X = folds, 
  FUN = function(x) {
    
    trainFold <- trainSet[-x, ]
    testFold <- trainSet[x, ]
    
    classifier <- xgboost(
      data = as.matrix(trainSet[,-11]),
      label = trainSet$Exited,
      nrounds = 10
    )

    yPred <- predict(object = classifier, newdata = as.matrix(testFold[, -11]))
    yPred <- yPred > 0.5



    cm <- table(testFold[, 11], yPred)

    accuracy <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
    
    return(accuracy)

  }
)

accuracy <- mean(as.numeric(cv)) # mean of accuracies
