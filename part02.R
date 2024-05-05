#Conducting PCA(Principal Component Analysis)

# Loading required package
library(readxl)
library(fpc)
library(cluster)
library(ggcorrplot)
library(NbClust)
library(factoextra)

# Loading the data set
Whitewine_v6 <- read_excel("Whitewine_v6.xlsx")

# Removing the last column of the data set (Quality)
modified_Whitewine_v6 <- Whitewine_v6[, -ncol(Whitewine_v6), drop = FALSE]

# Summary of the modified data
summary(modified_Whitewine_v6)

# Function to detect outliers using IQR
detect_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  outliers <- x < lower_bound | x > upper_bound  # Include upper bound check
  return(outliers)
}




# Get the outlier rows for each column
outlier_rows <- lapply(modified_Whitewine_v6, detect_outliers)
print(outlier_rows)  # See what rows are detected as outliers for each column

# Remove outliers function
remove_outliers_rows <- function(data, outlier_rows) {
  # Obtain a logical vector indicating any outlier presence per row
  outlier_indices <- which(rowSums(sapply(outlier_rows, as.numeric)) > 0)
  # Remove rows containing any outliers
  cleaned_data <- data[-outlier_indices, , drop = FALSE]
  return(cleaned_data)
}


# Removing outliers from modified_Whitewine_v6
cleaned_Whitewine_v6_1 <- remove_outliers_rows(modified_Whitewine_v6, outlier_rows)

#removing from 2,3,4,6,9
outlier_column_2 <-detect_outliers(cleaned_Whitewine_v6_1[[2]])
outlier_column_3 <-detect_outliers(cleaned_Whitewine_v6_1[[3]])
outlier_column_4 <-detect_outliers(cleaned_Whitewine_v6_1[[4]])
outlier_column_6 <-detect_outliers(cleaned_Whitewine_v6_1[[6]])
outlier_column_9 <-detect_outliers(cleaned_Whitewine_v6_1[[9]])

cleaned_Whitewine_v6_2 <- cleaned_Whitewine_v6_1[
  !outlier_column_2 &
    !outlier_column_3 &
    !outlier_column_4 &
    !outlier_column_6 & 
    !outlier_column_9,
]




# Removing outliers from modified_Whitewine_v6
cleaned_Whitewine_v6_3 <- detect_outliers(cleaned_Whitewine_v6_2 [[3]])

cleaned_Whitewine_v6 <-cleaned_Whitewine_v6_2 [!cleaned_Whitewine_v6_3,]

# Summary of cleaned data
summary(cleaned_Whitewine_v6)

#Preparing Data
wine_scaled <-scale(cleaned_Whitewine_v6)
head(wine_scaled)

#Calculating covariance matrix
wine.cov <-cov(wine_scaled)
wine.cov

library(ggcorrplot)
ggcorrplot(as.matrix((wine.cov)))

#Calculating the eigenvalues and eigenvectors
wine.eigen <-eigen(wine.cov)
wine.eigen
str(wine.eigen)

wine.eigen$values

#Acsseing eigen values and vectors seperately
wine.eigen$values
wine.eigen$vectors

wine.eigen$vectors <- -wine.eigen$vectors
wine.eigen$vectors

# -- Assign row and column names to the dataframe --

row.names(wine.eigen$vectors) <- colnames(cleaned_Whitewine_v6)
colnames(wine.eigen$vectors) <- c("PC1", "PC2", "PC3", "PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11")
wine.eigen$vectors
wine_scaled

#Selecting Number of PC
#Calculate PVE
wine.eigen$values
PVE <-wine.eigen$values/sum(wine.eigen$values)
round(PVE,2)

#PVE plot
PVEplot <- qplot(c(1:11), PVE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1)
print(PVEplot)

#Cumulative PVE plot
cumPVE <- qplot(c(1:11), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab(NULL) + 
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1)
print(cumPVE)
library(gridExtra)
grid.arrange(PVEplot, cumPVE, ncol = 2)

phi <-wine.eigen$vectors[,1:7]
phi

#Transform data points into new dimentional space
PC1 <- as.matrix(wine_scaled)%*% phi[,1]
PC1
PC2 <- as.matrix(wine_scaled)%*% phi[,2]
PC2
PC3 <- as.matrix(wine_scaled)%*% phi[,3]
PC3
PC4 <- as.matrix(wine_scaled)%*% phi[,4]
PC4
PC5 <- as.matrix(wine_scaled)%*% phi[,5]
PC5
PC6 <- as.matrix(wine_scaled)%*% phi[,6]
PC6
PC7 <- as.matrix(wine_scaled)%*% phi[,7]
PC7

# -- Create a dataframe with Principal Components scores --
pca_data <-data.frame(PC1, PC2, PC3, PC4, PC5, PC6, PC7)
pca_data
str(pca_data)

#Determining Clusters using NBClust
library(NbClust)

#Clusters for euclidean distance
euclidean_cluster_pca <- NbClust(data = pca_data, distance = "euclidean" , min.nc = 2,max.nc = 10,method = "kmeans",index="all")
euclidean_cluster_pca$All.index

#Clusters for manhattan distance
manhattan_cluster_pca <- NbClust(data = pca_data, distance = "manhattan" , min.nc = 2,max.nc = 10,method = "kmeans",index="all")
manhattan_cluster_pca$All.index

#Clusters for maximum distance
maximum_cluster_pca <- NbClust(data = pca_data, distance = "maximum" , min.nc = 2,max.nc = 10,method = "kmeans",index="all")
maximum_cluster$All.index

#Determining Clusters using Elbow method

wcss <- numeric(10)

#Running K means for different Values for K (1 to 10)

for (i in 1:10) {
  kmeans_model_pca <- kmeans(pca_data, centers = i)
  wcss[i] <- kmeans_model_pca$tot.withinss
}

#Plot the Elbow curve

plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters (K)", ylab = "Within-Cluster Sum of Squares (WCSS)",
     main = "Elbow Method for Optimal K")
# According to the Elbow method the number of clusters should be 2.


#Using Gap Statics Method

# Load the required package
library(cluster)

# Calculate gap statistics
gap_stat <- clusGap(pca_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# Plot the gap statistic results
plot(gap_stat, main = "Gap Statistic Plot")

#Extract the optimal number of clusters 
optimal_k <- maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"], method="Tibs2001SEmax")
cat("Optimal number of clusters:", optimal_k, "\n")

#Optimal number of clusters: 3

# Load required packages
library(cluster)

# Calculate silhouette widths for different numbers of clusters
sil_width <- numeric(10) 

for (k in 2:10) {
  # Perform k-means clustering
  km <- kmeans(pca_data, centers = k)
  
  # Compute silhouette widths
  sil <- silhouette(km$cluster, dist(pca_data))
  
  # Calculate the mean silhouette width
  sil_width[k] <- mean(sil[, "sil_width"])
}

# Plot silhouette widths
plot(1:10, sil_width[1:10], type = "b", xlab = "Number of clusters", ylab = "Silhouette Width")

# Find the optimal number of clusters
best_k <- which.max(sil_width)
cat("Optimal number of clusters (Silhouette method):", best_k, "\n")

#best is 2

#Performing Kmeans (2 clusters)
clusters_pca <- kmeans(pca_data, centers = 2)
print(clusters_pca)

# Visualize clusters
library(factoextra)
fviz_cluster(clusters_pca, data = pca_data)

# Calculating the BSS, WSS, TSS

TSS_pca <- clusters_pca$totss  # Total variance in the dataset
WSS_pca <- sum(clusters_pca$withinss)  # Total within-cluster sum of squares
BSS_pca <- clusters_pca$betweenss  # Between-cluster sum of squares

# Output the results
cat("Total Sum of Squares (TSS):", TSS_pca, "\n")
cat("Total Within-Cluster Sum of Squares (WSS):", WSS_pca, "\n")
cat("Between-Cluster Sum of Squares (BSS):", BSS_pca, "\n")
cat("Ratio of BSS/TSS:", BSS_pca / TSS_pca * 100, "%\n")

# Silhouette Plot and Average Silhouette Width
library(cluster)
silhouette_scores_pca <- silhouette(clusters_pca$cluster, dist(pca_data))
fviz_silhouette(silhouette_scores_pca)  # Visualizing the silhouette plot

avg_sil_width_pca <- mean(silhouette_scores_pca[, "sil_width"])
print(paste("Average silhouette width:", avg_sil_width_pca))


#Calinski-Harabasz Index for PCA dataset
library(fpc)
library(cluster)

# Calculate the dissimilarity matrix
diss <- dist(pca_data)

# Calculate cluster validation statistics using cluster.stats()
clust_stats <- cluster.stats(diss, clusters_pca$cluster)

# Access the Calinski-Harabasz index
calinski_harabasz_index <- clust_stats$ch

# Print the Calinski-Harabasz index
print(calinski_harabasz_index)
