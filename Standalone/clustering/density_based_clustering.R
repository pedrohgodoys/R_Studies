library(ggplot2)
library(ggrepel)
library(dbscan)


dataSet <- as.data.frame(read.csv("measurements_real_data.csv"))
colnames(dataSet) <- gsub("X", "", colnames(dataSet))
rownames(dataSet) <- dataSet$Sample
# Extract data for an easier time
classes <- dataSet$Group
variables <- colnames(dataSet)[-c(1,2)]

matrix <- apply(dataSet[,-c(1, 2)], 2, as.numeric)
rownames(matrix) <- dataSet$Sample


# PCA
Dpca <- prcomp(matrix, scale = TRUE, center = TRUE)$x[, c("PC1", "PC2")]

# Density-based Clustering
Dfit <- dbscan(matrix, eps = 10, minPts = 5)

# Plot
## Plot coordinates
Dcoord <- cbind.data.frame(Samples = dataSet$Sample, Groups = dataSet$Group, Dpca, Clusters = Dfit$cluster)

## Actual plot (clusters)
ggplot(Dcoord, aes(x= PC1, y= PC2, col= factor(Clusters), group= factor(Clusters)))+
  stat_ellipse(geom = "polygon", alpha= 0.1) +
  geom_point(size= 3, alpha= 0.6) + ggtitle("Density-based Clustering") +
  geom_text_repel(label = Dcoord$Samples, size = 4, segment.color = "gray50") + 
  theme_minimal()
