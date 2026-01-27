library(arules)

# Creating Sparce Matrix
#   install.packages("arules")
#   dataSet <- read.csv("Market_Basket_Optimisation.csv", header = FALSE)
dataSet <- read.transactions(
  file = "Market_Basket_Optimisation.csv", 
  sep = ",", 
  rm.duplicates = TRUE
)

summary(dataSet)
itemFrequencyPlot(
  dataSet, 
  topN = 10
)

# Training Eclat on the dataSet
rules <- eclat(
  data = dataSet,
  parameter = list(
    support = round((3*7)/7500, digits = 3), # 4 buyings a day, per week / total observations
    minlen = 2
  )
)

# Visualizing Rules results
inspect(
  sort(
    rules, 
    by = "support"
  )[1:10]
)
