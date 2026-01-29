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

# Training Apriori on the dataSet
rules <- apriori(
  data = dataSet,
  parameter = list(
    support = round((4*7)/7500, digits = 3), # 4 buyings a day, per week / total observations
    confidence = 0.2
  )
)

# Visualizing Rules results
inspect(
  sort(
    rules, 
    by = "lift"
  )[1:10]
)
