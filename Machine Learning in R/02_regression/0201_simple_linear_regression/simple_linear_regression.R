
# y_hat = b0 + b1X1
#
#  y_hat: dependent variable
#  b0: y-intercept (constant)
#  b1: slope coefficient
#  X1: independent variable
# Salary: dependent
# Years of Experience: independent
library(caTools)
library(ggplot2)

dataSet <- read.csv("Salary_Data.csv")
is.na(dataSet)

# Pre Processing
split <- sample.split(
  Y = dataSet$Salary, 
  SplitRatio = 2/3 # could be 0.8
)

trainSet <- subset(dataSet, split == TRUE)
testSet <- subset(dataSet, split == FALSE)

# Fitting Simple Linear Regression to the Training Set
regressor <- lm(
  formula = Salary ~ YearsExperience, 
  data = trainSet
)
summary(regressor)

y_predictions <- predict(object = regressor, newdata = testSet)

# Visualising the Training Set Results
ggplot() +
  geom_point(
    aes(x = trainSet$YearsExperience, y = trainSet$Salary),
    color = "lightblue") +
  geom_point(
    aes(x = trainSet$YearsExperience, y = predict(object = regressor, newdata = trainSet)),
    color = "blue") + 
      geom_line(
        aes(x = trainSet$YearsExperience, y = predict(object = regressor, newdata = trainSet)),
        color = "blue") + 
  xlab("Years of Experience") + ylab("Salary") +
  ggtitle("Salary vs Experience (TrainSet)") +
  theme_minimal()

# Visualising the Test Set Results
ggplot() +
  geom_point(
    aes(x = testSet$YearsExperience, y = testSet$Salary),
    color = "lightblue") +
  geom_point(
    aes(x = trainSet$YearsExperience, y = predict(object = regressor, newdata = trainSet)),
    color = "blue") + 
  geom_line(
    aes(x = trainSet$YearsExperience, y = predict(object = regressor, newdata = trainSet)),
    color = "blue") + 
  xlab("Years of Experience") + ylab("Salary") +
  ggtitle("Salary vs Experience (TrainSet)") +
  theme_minimal()

