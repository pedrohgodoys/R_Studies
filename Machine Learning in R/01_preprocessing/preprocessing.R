#ML Course with R

dataSet <- read.csv("Data.csv")

# Dealing with Missing Values
  ## Remove NA from Age column
  dataSet$Age <- ifelse(
    is.na(dataSet$Age), 
    ave(dataSet$Age,
      FUN = function(x) mean(x, na.rm = TRUE)
    ),
    dataSet$Age
  )

  ## Remove NA from Salary column
  dataSet$Salary <- ifelse(
    is.na(dataSet$Salary), 
    ave(dataSet$Salary,
      FUN = function(x) mean(x, na.rm = TRUE)
    ),
    dataSet$Salary
  )


# Encoding Categorical to Factors
  ## Enconding the Country column
  dataSet$factorialCountry <- factor(
    x = dataSet$Country,
    levels = c("France", "Spain", "Germany"),
    labels = c(1,2,3)
  )

  ## Enconding the Purchased column
  dataSet$factorialPurchased <- factor(
    x = dataSet$Purchased,
    levels = c("Yes", "No"),
    labels = c(1, 0)
  )


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
trainSet[ , c("Age", "Salary")] <- scale(x = trainSet[ , c("Age", "Salary")],)
testSet[ , c("Age", "Salary")] <- scale(x = testSet[ , c("Age", "Salary")])
