# ----------------------------------------------------------------------------
# Air Pollution Level Clustering using K-Means
# ----------------------------------------------------------------------------

# 1. Install & Load Required Packages
# Un-comment the next lines to install packages if you don't have them
# install.packages("ggplot2")
# install.packages("cluster")
# install.packages("factoextra")

library(ggplot2)
library(cluster)
library(factoextra)

# 2. Load the Dataset
# Ensure the working directory is set to where the CSV is located
cat("Loading dataset...\n")
data <- read.csv("air_pollution_dataset.csv")
cat("First few rows of the dataset:\n")
print(head(data))

# 3. Data Preprocessing
# We exclude the 'Date' column because clustering needs numerical data
clustering_data <- data[, c("PM2.5", "PM10", "NO2", "SO2", "CO", "O3")]

# Handle any missing values if present (removes rows with NA)
clustering_data <- na.omit(clustering_data)

# Scale the data (standardization)
# This step is critical since pollutants have different units and ranges
clustering_data_scaled <- scale(clustering_data)

# 4. Determine optimal number of clusters (K) using the Elbow Method
cat("Generating Elbow Method plot to find optimal K...\n")
set.seed(42)  # For reproducibility
elbow_plot <- fviz_nbclust(clustering_data_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal k")
# You can view the plot by uncommenting the following line:
# print(elbow_plot)
# Based on our sample dataset structure, evaluating k = 3.

# 5. Fit K-Means Algorithm
k <- 3
cat("Performing K-Means clustering with k =", k, "...\n")
kmeans_result <- kmeans(clustering_data_scaled, centers = k, nstart = 25)

# 6. Analyze the Results
cat("\nCluster Centers (Scaled):\n")
print(kmeans_result$centers)

cat("\nCluster Assignments distribution:\n")
print(table(kmeans_result$cluster))

# Append the cluster assignment back to the original dataset
data$Cluster <- as.factor(kmeans_result$cluster)

# 7. Visualization
cat("\nVisualizing clusters...\n")

# Plot 1: 2D representation of the clusters using PCA (Principal Component Analysis)
pca_plot <- fviz_cluster(kmeans_result, data = clustering_data_scaled,
                         palette = c("#2E9FDF", "#00AFBB", "#FC4E07"),
                         geom = "point",
                         ellipse.type = "convex", 
                         ggtheme = theme_minimal(),
                         main = "Air Pollution Clusters (PCA Projection)")

# Plot 2: Scatter plot comparing PM2.5 and PM10
scatter_plot <- ggplot(data, aes(x = PM2.5, y = PM10, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  scale_color_manual(values = c("#2E9FDF", "#00AFBB", "#FC4E07")) +
  labs(title = "Air Pollution Clusters: PM2.5 vs PM10",
       x = "PM2.5 Level (µg/m³)",
       y = "PM10 Level (µg/m³)")

# Display PCA plot
print(pca_plot)
# print(scatter_plot)

# 8. Save the resulting clustered dataset to a new CSV file
output_file <- "clustered_air_pollution.csv"
write.csv(data, output_file, row.names = FALSE)
cat(sprintf("Clustering complete! Results have been saved to '%s'.\n", output_file))
