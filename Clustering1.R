# Load the required libraries
library(ggplot2)
library(dplyr)
library(factoextra)

# Load the dataset
movies <- read.csv("C:\\Users\\Lenovo\\OneDrive\\Desktop\\Data mining Project\\movie_metadata.csv",stringsAsFactors = FALSE)

# Data cleaning
movies_clean <- movies %>% 
  select(num_critic_for_reviews, duration, director_facebook_likes, 
         actor_3_facebook_likes, actor_1_facebook_likes, gross, 
         num_voted_users, cast_total_facebook_likes, num_user_for_reviews, 
         budget, title_year, imdb_score, movie_facebook_likes) %>% 
  na.omit()

# Feature scaling
movies_scaled <- scale(movies_clean)

# Elbow method to determine the optimal number of clusters
set.seed(123)
fviz_nbclust(movies_scaled, kmeans, method = "wss") +
  theme_classic() + ggtitle("Elbow Method")

# K-means clustering
set.seed(123)
kmeans_model <- kmeans(movies_scaled, centers = 4, nstart = 25)
movies_clustered <- movies_clean %>%
  mutate(cluster = kmeans_model$cluster)

# Cluster analysis
movies_cluster_analysis <- movies_clustered %>%
  group_by(cluster) %>%
  summarize(
    mean_num_critic_for_reviews = mean(num_critic_for_reviews),
    mean_duration = mean(duration),
    mean_director_facebook_likes = mean(director_facebook_likes),
    mean_actor_3_facebook_likes = mean(actor_3_facebook_likes),
    mean_actor_1_facebook_likes = mean(actor_1_facebook_likes),
    mean_gross = mean(gross),
    mean_num_voted_users = mean(num_voted_users),
    mean_cast_total_facebook_likes = mean(cast_total_facebook_likes),
    mean_num_user_for_reviews = mean(num_user_for_reviews),
    mean_budget = mean(budget),
    mean_title_year = mean(title_year),
    mean_imdb_score = mean(imdb_score),
    mean_movie_facebook_likes = mean(movie_facebook_likes)
  )

# Visualize the clusters
ggplot(movies_clustered, aes(x = imdb_score, y = gross, color = factor(cluster))) +
  geom_point(alpha = 0.8, size = 3) +
  scale_color_discrete(name = "Cluster") +
  theme_classic() +
  labs(title = "K-Means Clustering", 
       x = "IMDB Score", 
       y = "Gross") +
  theme(legend.position = "bottom")
