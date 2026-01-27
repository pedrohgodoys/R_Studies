library(cluster) 

dataSet <- read.csv("mall.csv")
subSet <- dataSet[, 4:5]

# Using the Elbow Method to find the optimal number of clusters
set.seed(6)
wcss <- vector()

for (i in 1:10) wcss[i] <- sum(kmeans(subSet, i)$withinss)

plot(
  1:10, wcss, type = "b", 
  main = "Clusters of Clients",
  xlab = "Number of Clusters", ylab = "WCSS"
)

# Applying K-Means Clustering
set.seed(29)
kmeans <- kmeans(
  x = subSet,
  centers = 5, 
  iter.max = 300,
  nstart = 10
)
yKmeans <- kmeans$cluster

# Vusualizing the clusters
clusplot(
  x = subSet,
  clus = yKmeans,
  lines = 0,
  shade = TRUE,
  color = TRUE,
  labels = 2,
  plotchar = FALSE,
  span = TRUE,
  main = paste("Clusters of Clients"),
  xlab = "Annual Income",
  ylab = "Spending Score"
)
