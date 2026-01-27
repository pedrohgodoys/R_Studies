library(ggplot2)
library(randomForest)

dataSet <- read.csv("Position_Salaries.csv")[, 2:3]

# Fitting the Random Forest Regression Model
set.seed(1234)
regressor <- randomForest(
  x = dataSet[1], # independent variable
  y = dataSet$Salary, # dependent variable as vector
  ntree = 500
)
summary(regressor)

# Predict
yPred <- predict(regressor, data.frame(Level = 6.5))

# Visualizing the Random Forest Regression Model results
xGrid <- seq(
  min(dataSet$Level),
  max(dataSet$Level),
  0.01
)
ggplot() +
  geom_point(aes(x = dataSet$Level, y = dataSet$Salary), color = "orange") +
  #geom_point(aes(x = xGrid, y = predict(regressor, newdata = data.frame(Level = xGrid))) , color = "navyblue") +
  geom_line(aes(x = xGrid, y = predict(regressor, newdata = data.frame(Level = xGrid))) , color = "navyblue") +
  theme_minimal() + 
  labs(title = "Random Forest Regression") +
  xlab(label = "Level") + ylab(label = "Salary")
