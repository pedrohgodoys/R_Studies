dataSet <- read.csv("50_Startups.csv")

# Dummy Variable - Encoding character into factors
dataSet$State <- factor(
  dataSet$State,
  levels = c("New York", "California", "Florida"),
  labels = c(1, 2, 3)
)

library(caTools)
set.seed(123)
split <- sample.split(dataSet$Profit, SplitRatio = 0.8)
trainSet <- subset(dataSet, split == TRUE)
testSet <- subset(dataSet, split == FALSE)

# Fit Multiple Linear Regresion Model to the trainSet
linear_regressor <- lm(formula = Profit ~ ., data = trainSet)
summary(linear_regressor) # best coefficient is R.D.Spend

# Predict
y_pred <- predict(
  object = linear_regressor, 
  newdata = testSet
)

# Build the optimal model using Backward Elimination
  ## 1. Define significance level. p < 0.05
  ## 2. Fit model with all possible predictors
        regressor <- lm(
          formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, 
          data = dataSet # the entire dataSet is necessary
        )
  ## 3. Look for the variables with significance level < 0.05
        summary(regressor)
  ## 4. Remove predictors with p > 0.05
        regressorA <- lm(
          formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, 
          data = dataSet # the entire dataSet is necessary
        )
        summary(regressorA)

        regressorB <- lm(
          formula = Profit ~ R.D.Spend + Administration + Marketing.Spend, 
          data = dataSet # the entire dataSet is necessary
        )
        summary(regressorB)        

        regressorC <- lm(
          formula = Profit ~ R.D.Spend + Marketing.Spend, 
          data = dataSet # the entire dataSet is necessary
        )
        summary(regressorC)
  ## 5. Fit model again with 