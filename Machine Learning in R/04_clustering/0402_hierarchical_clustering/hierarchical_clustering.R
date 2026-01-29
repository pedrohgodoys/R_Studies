library(cluster)

dataSet <- read.csv("mall.csv")
subSet <- dataSet[, 4:5]

# Using Dendrograms to find the optimal number of clusters
dendrogram <- hclust(
  d = dist(subSet, method = "euclidean"),
  method = "ward.D"
)

plot(
  dendrogram,
  main = "Dendrogram",
  xlab = "Costumers",
  ylab = "Euclidean distances (D)"
)

# Fitting hierarchical clustering
HC <- hclust(
  d = dist(subSet, method = "euclidean"),
  method = "ward.D2"
)

## vector of clusters which the observention pertences to
yHC <- cutree(
  tree = HC, 
  k = 5
)

# Visualizing the clusters
clusplot(
  x = subSet,
  clus = yHC,
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
