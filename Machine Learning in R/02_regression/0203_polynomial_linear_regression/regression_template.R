
library(ggplot2)

dataSet <- read.csv("Position_Salaries.csv")
dataSet <- dataSet[, c("Level", "Salary")]

# Fit Regression
dataSet$Level2 <- dataSet$Level^2
dataSet$Level3 <- dataSet$Level^3
dataSet$Level4 <- dataSet$Level^4

regressor <- lm(formula = Salary ~ ., data = dataSet)
summary(regressor)

  ## Predict a new result with Regression Model
  yPredPoly <- predict(
    object = regressor,
    newdata = data.frame(Level = 6.5))

    yPredPoly <- predict(
      object = regressor,
      newdata = data.frame(Level = 6.5, Level2 = 6.5^2, Level3 = 6.5^3, Level4 = 6.5^4))

  ## Visualizing Regression
  ggplot() +
    geom_point(aes(x = dataSet$Level, y = dataSet$Salary), color = "orange") +
    geom_point(aes(x = dataSet$Level, y = predict(object = regressor, newdata = dataSet)), color = "navyblue") +
    geom_line(aes(x = dataSet$Level, y = predict(object = regressor, newdata = dataSet)), color = "navyblue") +
    theme_minimal()
