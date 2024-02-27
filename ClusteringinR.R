# Load necessary libraries
library(cluster) # for cosine similarity
library(factoextra) # for visualization
library(lsa)
# Load the necessary library
library(lsa)

# Read your normalized dataset
df_normalized <- read.csv('D:\\CSCI-5622-872 Machine Learning\\hclustdata.csv')  # Replace with your dataset path

df_transposed <- t(df_normalized)

# Compute the cosine similarity matrix between observations
similarity_matrix <- cosine(as.matrix(df_transposed))

# Convert the similarity matrix to a dissimilarity matrix for clustering
dissimilarity_matrix <- 1 - similarity_matrix

# Perform hierarchical clustering using complete linkage
hc <- hclust(as.dist(dissimilarity_matrix), method = "complete")

# Open a PNG device to save the dendrogram plot to a file with higher resolution


# Plot the dendrogram with a smaller line width
plot(hc, main = "Hierarchical Clustering with Cosine Similarity", lwd = 0.05, labels=FALSE)

# Add rectangles around the clusters, let's say we want to identify 4 clusters
rect.hclust(hc, k = 4, border = "red")

# Assuming 'hc' is your hierarchical clustering object from the previous code
# and 'df_normalized' is the original data frame with observations as rows

# Summary stats:

# Load the necessary libraries
library(dplyr)

# Ensure df_normalized is a dataframe
df_normalized <- as.data.frame(df_normalized)

# Assuming 'clusters' is a vector of cluster assignments
# Add the cluster assignments to the dataframe
df_normalized$Cluster <- as.factor(clusters)

# Calculate the summary statistics for each cluster
cluster_summaries <- df_normalized %>%
  group_by(Cluster) %>%
  summarise(across(everything(), list(
    mean = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    q1 = ~quantile(.x, probs = 0.25, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    q3 = ~quantile(.x, probs = 0.75, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE)
  )))

# View the summary statistics
print(cluster_summaries)










