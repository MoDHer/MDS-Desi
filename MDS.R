# Load library
library(stats)
library(readxl)

# Baca data dari file xlsx, sesuaikan lokasi file tersebut
data <- read_excel("C:/Users/mdavi/Downloads/Documents/Data MDS.xlsx")
View(data)

# Pastikan semua kolom berupa numerik
data <- as.data.frame(sapply(data, as.numeric))



#Klorofil a
distance_matrix <- dist(data[,2], method = "euclidean")

# Lakukan analisis MDS
mds_result <- cmdscale(distance_matrix, k = 2, eig = TRUE)

# Tampilkan hasil koordinat
mds_coordinates <- as.data.frame(mds_result$points)
colnames(mds_coordinates) <- c("Dim1", "Dim2")

# Plot the results
plot(mds_coordinates[, 1], mds_coordinates[, 2], 
     main = "Chlorophyll a",
     type = "n", xlab = "MDS Dimension 1", 
     ylab = "MDS Dimension 2")

# Plot the points and label them with 
# the first two letters of the species name
points(mds_coordinates[, 1], mds_coordinates[, 2], 
       pch = 21, bg = "lightblue")
text(mds_coordinates[, 1], mds_coordinates[, 2], 
     labels = substr(data$Sampling, 1, 2), 
     pos = 3, cex = 0.8)

# Form clusters using K-means clustering (specify the number of clusters, e.g., 2)
kmeans_clusters <- kmeans(mds_coordinates, centers = 2)$cluster

# Add the cluster information to the plot
points(mds_coordinates[, 1], mds_coordinates[, 2], 
       pch = 21, bg = kmeans_clusters, cex = 1.2,
       )

legend(
  "topright",
  bty = "n",
  legend = c("Low", "High"),
  col = c(1,2),
  title = "Productivity",
  title.col = "black",
  pch = 19,
  cex = 1
)



#klorofil b
distance_matrix <- dist(data[,3], method = "euclidean")

# Lakukan analisis MDS
mds_result <- cmdscale(distance_matrix, k = 2, eig = TRUE)

# Tampilkan hasil koordinat
mds_coordinates <- as.data.frame(mds_result$points)
colnames(mds_coordinates) <- c("Dim1", "Dim2")

# Plot the results
plot(mds_coordinates[, 1], mds_coordinates[, 2], 
     main = "Chlorophyll b",
     type = "n", xlab = "MDS Dimension 1", 
     ylab = "MDS Dimension 2")

# Plot the points and label them with 
# the first two letters of the species name
points(mds_coordinates[, 1], mds_coordinates[, 2], 
       pch = 21, bg = "lightblue")
text(mds_coordinates[, 1], mds_coordinates[, 2], 
     labels = substr(data$Sampling, 1, 2), 
     pos = 3, cex = 0.8)

# Form clusters using K-means clustering (specify the number of clusters, e.g., 2)
kmeans_clusters <- kmeans(mds_coordinates, centers = 2)$cluster

# Add the cluster information to the plot
points(mds_coordinates[, 1], mds_coordinates[, 2], 
       pch = 21, bg = kmeans_clusters, cex = 1.2,
)

legend(
  "topright",
  bty = "n",
  legend = c("Low", "High"),
  col = c(1,2),
  title = "Productivity",
  title.col = "black",
  pch = 19,
  cex = 1
)



#Karotenoid
distance_matrix <- dist(data[,4], method = "euclidean")

# Lakukan analisis MDS
mds_result <- cmdscale(distance_matrix, k = 2, eig = TRUE)

# Tampilkan hasil koordinat
mds_coordinates <- as.data.frame(mds_result$points)
colnames(mds_coordinates) <- c("Dim1", "Dim2")

# Plot the results
plot(mds_coordinates[, 1], mds_coordinates[, 2], 
     main = "Carotenoid",
     type = "n", xlab = "MDS Dimension 1", 
     ylab = "MDS Dimension 2")

# Plot the points and label them with 
# the first two letters of the species name
points(mds_coordinates[, 1], mds_coordinates[, 2], 
       pch = 21, bg = "lightblue")
text(mds_coordinates[, 1], mds_coordinates[, 2], 
     labels = substr(data$Sampling, 1, 2), 
     pos = 3, cex = 0.8)

# Form clusters using K-means clustering (specify the number of clusters, e.g., 2)
kmeans_clusters <- kmeans(mds_coordinates, centers = 2)$cluster

# Add the cluster information to the plot
points(mds_coordinates[, 1], mds_coordinates[, 2], 
       pch = 21, bg = kmeans_clusters, cex = 1.2,
)

legend(
  "topright",
  bty = "n",
  legend = c("Low", "High"),
  col = c(1,2),
  title = "Productivity",
  title.col = "black",
  pch = 19,
  cex = 1
)



#digabung
distance_matrix <- dist(data[,-1], method = "euclidean")

# Lakukan analisis MDS
mds_result <- cmdscale(distance_matrix, k = 2, eig = TRUE)

# Tampilkan hasil koordinat
mds_coordinates <- as.data.frame(mds_result$points)
colnames(mds_coordinates) <- c("Dim1", "Dim2")

# Plot the results
plot(mds_coordinates[, 1], mds_coordinates[, 2], 
     main = "Clustering (Chlorophyll a, Chlorophyll b, Caretonoid)",
     type = "n", xlab = "MDS Dimension 1", 
     ylab = "MDS Dimension 2")

# Plot the points and label them with 
# the first two letters of the species name
points(mds_coordinates[, 1], mds_coordinates[, 2], 
       pch = 21, bg = "lightblue")
text(mds_coordinates[, 1], mds_coordinates[, 2], 
     labels = substr(data$Sampling, 1, 2), 
     pos = 3, cex = 0.8)

# Form clusters using K-means clustering (specify the number of clusters, e.g., 2)
kmeans_clusters <- kmeans(mds_coordinates, centers = 2)$cluster

# Add the cluster information to the plot
points(mds_coordinates[, 1], mds_coordinates[, 2], 
       pch = 21, bg = kmeans_clusters, cex = 1.2,
)

legend(
  "topright",
  bty = "n",
  legend = c("Low", "High"),
  col = c(1,2),
  title = "Productivity",
  title.col = "black",
  pch = 19,
  cex = 1
)
