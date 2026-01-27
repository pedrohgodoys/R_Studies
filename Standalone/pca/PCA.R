library(ggplot2)
library(ggrepel)

# Load data
dataSet <- read.csv("measurements_real_data.csv", header = 1)
colnames(dataSet) <- gsub("X", "", colnames(dataSet))
rownames(dataSet) <- dataSet$Sample

# Extract data for an easier time
classes <- dataSet$Group
variables <- colnames(dataSet)[-c(1,2)]

matrix <- apply(dataSet[,-c(1, 2)], 2, as.numeric)
rownames(matrix) <- dataSet$Sample

# Principal Component Analysis 
  # for dirty or not normalized datasets, use:
  #   center = TRUE (x_1 - mean(x_i)), 
  #   scale. = TRUE (x_1/sd(x_i)).
pca <- prcomp(
  matrix,
  scale = TRUE,
  center = TRUE
) 
typeof(pca)
summary(pca)


# Standard Deviations
pca_stdev <- pca[[1]] 


# Loadings/Eigenvectors
loadings <- cbind.data.frame(variables, pca$rotation)


# Simple Plot
## Scree-plot
plot(pca, type= "l")

## Eigenvectors (in red; not quite visible...)
biplot(pca)

# GGPLOT
## PCA Coordenates
coordinates <- cbind.data.frame(
  Samples = dataSet$Sample, 
  Group = classes,
  pca$x[,1:2]
) # PCs in columns

## Actual plot
ggplot(coordinates, aes(x= PC1, y= PC2, col= Group, fill= Group)) +
  stat_ellipse(geom= "polygon", alpha= 0.1) +
  geom_point(size= 3, alpha= 0.6) +
  geom_text_repel(label = coordinates$Samples, size = 4, segment.color = "gray50") +
  #geom_text(label = coordinates$Samples, size = 4, vjust= -1, hjust = -0.2)
  theme_minimal()
