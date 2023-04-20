# Load the dataset
imdb_data <- read.csv("C:\\Users\\Lenovo\\OneDrive\\Desktop\\Data mining Project\\movie_metadata.csv")

# Select relevant columns
selected_cols <- c("num_critic_for_reviews", "duration", "director_facebook_likes", "actor_1_facebook_likes", "gross", "imdb_score")
imdb_data_subset <- imdb_data[, selected_cols]

# Clean the data
imdb_data_subset <- na.omit(imdb_data_subset)

# Scale the data
scaled_data <- scale(imdb_data_subset)

# Perform k-means clustering
kmeans_model <- kmeans(scaled_data, centers = 5)

# Visualize the results
library(ggplot2)
library(gridExtra)

# Plot original data with colors indicating cluster assignment
plot_data <- as.data.frame(cbind(scaled_data, kmeans_model$cluster))
names(plot_data)[7] <- "cluster"
p1 <- ggplot(plot_data, aes(x = num_critic_for_reviews, y = imdb_score, color = factor(cluster))) +
  geom_point(alpha = 0.6) +
  scale_color_discrete(name = "Cluster") +
  ggtitle("K-Means Clustering") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot cluster centroids
centroids <- as.data.frame(kmeans_model$centers)
names(centroids) <- selected_cols
p2 <- ggplot(centroids, aes(x = num_critic_for_reviews, y = imdb_score)) +
  geom_point(aes(color = "centroid"), size = 5) +
  scale_color_manual(values = "red") +
  ggtitle("Cluster Centroids") +
  theme(plot.title = element_text(hjust = 0.5))

# Combine the plots
grid.arrange(p1, p2, ncol = 2)
