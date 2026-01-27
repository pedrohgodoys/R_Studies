library(ggplot2)
#install.packages("rpart")
library(rpart) # to use the rpart() function

dataSet <- read.csv("Position_Salaries.csv")[, 2:3]

# Fitting the Decision Tree Regression Model
regressor <- rpart(
  formula = Salary ~ ., 
  data = dataSet,
  control = rpart.control(minsplit = 1) # Set min of splits
)
summary(regressor)

# Predict
yPred <- predict(regressor, data.frame(Level = 6.5))

# Visualizing the Decision Tree Regression Model results
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
  labs(title = "Decision Tree Regression") +
  xlab(label = "Level") + ylab(label = "Salary")
