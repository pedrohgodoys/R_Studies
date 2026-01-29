#ML Course with R

dataSet <- read.csv("Data.csv")
#dataSet <- dataSet[ , c("Age", "Salary")]

# Splitting the Dataset
set.seed(123)

  ## Their way set.seed works
  # install.packages("caTools")
  library(caTools)
  split_index <- sample.split(
    Y = dataSet$factorialPurchased,
    SplitRatio = 0.8 # TRUE to train, FALSE to test
  )

  trainSet <- subset(dataSet, split_index == TRUE)
  testSet <- subset(dataSet, split_index == FALSE)

  ## My way set.seed don't work?
  split_index2 <- sample(
    x = 1:nrow(dataSet),
    size = 0.8 * nrow(dataSet), 
    replace = FALSE
  )

  trainSet <- dataSet[split_index2, ]
  testSet <- dataSet[-split_index2, ]


# Feature Scaling
# trainSet[ , c("Age", "Salary")] <- scale(x = trainSet[ , c("Age", "Salary")],)
# testSet[ , c("Age", "Salary")] <- scale(x = testSet[ , c("Age", "Salary")])
