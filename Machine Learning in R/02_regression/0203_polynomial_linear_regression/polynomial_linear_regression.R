
library(ggplot2)

dataSet <- read.csv("Position_Salaries.csv")
dataSet <- dataSet[, c("Level", "Salary")]

# Linear Regression for Comparison
linearReg <- lm(formula = Salary ~ Level, data = dataSet)
summary(linearReg)

  ## Visualizing Linear Regression
  ggplot() +
    geom_point(aes(x = dataSet$Level, y = dataSet$Salary), color = "lightblue") +
    geom_point(aes(x = dataSet$Level, y = predict(object = linearReg, newdata = dataSet)), color = "navyblue") +
    geom_line(aes(x = dataSet$Level, y = predict(object = linearReg, newdata = dataSet)), color = "navyblue") +
    theme_minimal()

  ## Predict a new result with Linear Regression
  yPredLinear <- predict(
    object = linearReg,
    newdata = data.frame(Level = 6.5) # a single value
  )

# Polynomial Regression
dataSet$Level2 <- dataSet$Level^2
dataSet$Level3 <- dataSet$Level^3
dataSet$Level4 <- dataSet$Level^4

polyReg <- lm(formula = Salary ~ ., data = dataSet)
summary(polyReg)

  ## Predict a new result with Linear Regression
  yPredPoly <- predict(
    object = polyReg,
    newdata = data.frame(
      Level = 6.5, 
      Level2 = 6.5 ^2,
      Level3 = 6.5 ^3,
      Level4 = 6.5 ^4) 
  )

  ## Visualizing Polynomial Regression
  ggplot() +
    geom_point(aes(x = dataSet$Level, y = dataSet$Salary), color = "lightblue") +
    geom_point(aes(x = dataSet$Level, y = predict(object = polyReg, newdata = dataSet)), color = "navyblue") +
    geom_line(aes(x = dataSet$Level, y = predict(object = polyReg, newdata = dataSet)), color = "navyblue") +
    theme_minimal()