library(caTools)
#install.packages("h2o")
library(h2o)

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

# Feature Scaling
trainSet[, 1:10] <- scale(trainSet[, 1:10])
testSet[, 1:10] <- scale(testSet[, 1:10])

# Fitting ANN Model
h2o.init(nthreads = - 1) # connect to a server
classifier <- h2o.deeplearning(
  y = "Exited", # response/dependent variable
  training_frame = as.h2o(trainSet), # trainSet/independent variables
  activation = "Rectifier", # activation method
  hidden = c(6, 6), # number of hidden layers
  epochs = 100, # how many times the dataset should be iterated
  train_samples_per_iteration = -2
)

# Predict testSet results
probPred <- h2o.predict(
  object = classifier, 
  newdata = as.h2o(
    testSet[, -11]
  )
)

yPred <- probPred > 0.5
yPred <- as.vector(yPred)

# Making confusion matrix
cm <- table(testSet[, 11], yPred)

accuracy <- (1520 + 213) / 2000

h2o.shutdown() # disconnect me from the server
