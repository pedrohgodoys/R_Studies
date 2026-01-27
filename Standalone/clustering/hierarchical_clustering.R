library(ggplot2)
library(ggrepel)

dataSet <- as.data.frame(read.csv("measurements_real_data.csv"))
colnames(dataSet) <- gsub("X", "", colnames(dataSet))
rownames(dataSet) <- dataSet$Sample
# Extract data for an easier time
classes <- dataSet$Group
variables <- colnames(dataSet)[-c(1,2)]

matrix <- apply(dataSet[,-c(1, 2)], 2, as.numeric)
rownames(matrix) <- dataSet$Sample


# PCA
HCApca <- prcomp(matrix, scale = TRUE, center = TRUE)$x[, c("PC1", "PC2")]

# Hierarchical Clustering (euclidean distances)
HCAfit <- dist(matrix, method = "euclidean")

# Perform the clustering
HCA <- hclust(HCAfit, method = "complete")
typeof(HCA)


# Plot
## Dendrogram
plot(HCA)

## Plot coordinates
HCAclusters <- cutree(HCA, k = 3)
HCAcoord <- cbind.data.frame(Samples = dataSet$Sample, Groups = dataSet$Group, HCApca, Clusters = HCAclusters)

## Actual plot (clusters)
ggplot(HCAcoord, aes(x= PC1, y= PC2, col= factor(Clusters), group= factor(Clusters)))+
  stat_ellipse(geom = "polygon", alpha= 0.1) +
  geom_point(size= 3, alpha= 0.6) + ggtitle("Hierarchical Clustering") +
  geom_text_repel(label = HCAcoord$Samples, size = 4, segment.color = "gray50") + 
  theme_minimal()

