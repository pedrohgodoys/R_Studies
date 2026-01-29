
library(ggplot2)
library(e1071) # to use the svm()

dataSet <- read.csv("Position_Salaries.csv")
dataSet <- dataSet[, 2:3]

# Fitting the SVR model
regressor <- svm(
  formula = Salary ~ ., 
  data = dataSet, 
  type = "eps-regression"
)
summary(regressor)

# Predict a new result
yPred <- predict(regressor, data.frame(Level = 6.5))

# Visualizing the SVR model
ggplot() +
  geom_point(aes(x = dataSet$Level, y = dataSet$Salary), color = "orange") +
  geom_point(aes(x = dataSet$Level, y = predict(regressor, newdata = dataSet)) , color = "navyblue") +
  geom_line(aes(x = dataSet$Level, y = predict(regressor, newdata = dataSet)) , color = "navyblue") +
  theme_minimal()
