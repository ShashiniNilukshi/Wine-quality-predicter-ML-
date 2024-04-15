# Loading required package
library(readxl)

# Loading the data set
Whitewine_v6 <- read_excel("~/yr2 sem02/Machine Learning and Data Mining/CourseWork/Whitewine_v6.xlsx")

# Removing the last column of the data set (Quality)
modified_Whitewine_v6 <- Whitewine_v6[, -ncol(Whitewine_v6), drop = FALSE]

# Summary of the modified data
summary(modified_Whitewine_v6)

# Function to detect outliers using Z-score
detect_outliers_zscore <- function(x, threshold = 3) {
  z_scores <- abs(scale(x))
  outliers <- which(z_scores > threshold)
  return(outliers)
}



# Get the outlier rows for each column
outlier_rows <- lapply(modified_Whitewine_v6, detect_outliers_zscore)

# Remove outliers function
remove_outliers_rows <- function(data, outlier_rows) {
  cleaned_data <- data[-unlist(outlier_rows), , drop = FALSE]
  return(cleaned_data)
}

# Removing outliers from modified_Whitewine_v6
cleaned_Whitewine_v6 <- remove_outliers_rows(modified_Whitewine_v6, outlier_rows)

# Summary of cleaned data
summary(cleaned_Whitewine_v6)


# Visualizing boxplots after outlier removal without displaying outliers
par(mfrow = c(3, 4), mar = c(3, 3, 2, 1), oma = c(1, 1, 1, 1))
for (i in 1:11) {
  boxplot(cleaned_Whitewine_v6[, i], main = names(cleaned_Whitewine_v6)[i], ylab = names(cleaned_Whitewine_v6)[i], outline = FALSE)
}

#Data normalization function
z_score<-function(data){
  normalzed_data <- scale(data)
  return (normalzed_data)
}

#normalizing data in cleaned_Whitewine_v6
normalized_Whitewine_v6 <- z_score(cleaned_Whitewine_v6)

#Getting the summery of the normalized_Whitewine_v6
summary(normalized_Whitewine_v6)

#Determining Clusters using NBClust
library(NbClust)

#Clusters for euclidean distance
euclidean_cluster <- NbClust(data = normalized_Whitewine_v6, distance = "euclidean" , min.nc = 2,max.nc = 10,method = "kmeans",index="all")
euclidean_cluster$All.index

#Clusters for manhattan distance
manhattan_cluster <- NbClust(data = normalized_Whitewine_v6, distance = "manhattan" , min.nc = 2,max.nc = 10,method = "kmeans",index="all")
manhattan_cluster$All.index

#Determining Clusters using Elbow method
wcss <- numeric(10)

#Running K means for different Values for K (1 to 10)
for (i in 1:10) {
  kmeans_model <- kmeans(normalized_Whitewine_v6, centers = i)
  wcss[i] <- kmeans_model$tot.withinss
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
gap_stat <- clusGap(normalized_Whitewine_v6, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# Plot the gap statistic results
plot(gap_stat, main = "Gap Statistic Plot")

#Extract the optimal number of clusters 
optimal_k <- maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"], method="Tibs2001SEmax")
cat("Optimal number of clusters:", optimal_k, "\n")

#Optimal number of clusters: 2 

#Using silhouette method

# Load required packages
library(cluster)

# Calculate silhouette widths for different numbers of clusters
sil_width <- numeric(10) 

for (k in 2:10) {
  # Perform k-means clustering
  km <- kmeans(normalized_Whitewine_v6, centers = k, nstart = 25)
  
  # Compute silhouette widths
  sil <- silhouette(km$cluster, dist(normalized_Whitewine_v6))
  
  # Calculate the mean silhouette width
  sil_width[k] <- mean(sil[, "sil_width"])
}

# Plot silhouette widths
plot(2:10, sil_width[2:10], type = "b", xlab = "Number of clusters", ylab = "Silhouette Width")

# Find the optimal number of clusters
best_k <- which.max(sil_width)
cat("Optimal number of clusters (Silhouette method):", best_k, "\n")



