# Loading required package
library(readxl)
library(NbClust)
library(cluster)


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

# Visualizing boxplots after outlier removal without displaying outliers
par(mfrow=c(3, 4), mar=c(3, 3, 2, 2))

# Loop to create a boxplot for each variable in the dataframe
for (i in 1:11) {
  boxplot(cleaned_Whitewine_v6[, i], 
          main = names(cleaned_Whitewine_v6)[i], 
          ylab = names(cleaned_Whitewine_v6)[i], 
          outline = TRUE, 
          col = "lightblue")  # Adding color for better visual distinction
}

# Reset the default plotting parameters after the loop
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)

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

#Clusters for maximum distance
maximum_cluster <- NbClust(data = normalized_Whitewine_v6, distance = "maximum" , min.nc = 2,max.nc = 10,method = "kmeans",index="all")
maximum_cluster$All.index

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
plot(1:10, sil_width[1:10], type = "b", xlab = "Number of clusters", ylab = "Silhouette Width")

# Find the optimal number of clusters
best_k <- which.max(sil_width)
cat("Optimal number of clusters (Silhouette method):", best_k, "\n")

#Performing Kmeans (2 clusters)
set.seed(123)
clusters <- kmeans(normalized_Whitewine_v6, centers = 2, nstart = 35)
print(clusters)



#Calculating the BSS,WSS,TSS
t_mean <- colMeans(normalized_Whitewine_v6)
TSS <- sum(colSums((normalized_Whitewine_v6 - t_mean)^2))

# Within-Cluster Sum of Squares (WSS) and Between-Cluster Sum of Squares (BSS)
WSS1 <- clusters$withinss
WSS <- clusters$tot.withinss
BSS <- TSS - WSS

cat("Total Sum of Squares (TSS):", TSS, "\n")
cat("Within-Cluster Sum of Squares (WSS):", WSS1, "\n")
cat("Between-Cluster Sum of Squares (BSS):", BSS, "\n")
cat("Ratio of BSS/TSS:", BSS / TSS, "\n")

#Silhoutte Plot
library(cluster)
silhouette_scores <- silhouette(clusters$cluster, dist(normalized_Whitewine_v6))
library(factoextra)
fviz_silhouette(silhouette_scores)


# Average silhouette width
avg_sil_width <- mean(silhouette_scores[, "sil_width"])
print(avg_sil_width)

#The average silhoutte width is 0.21 isn't necessarily bad but indicates that the clustering result is not highly distinct

