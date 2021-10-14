# K-means Clustering algorithm that finds groupings from users hobbies and activities.

## Mamoune El Boukfaoui

# Data import and user input.
install.packages("readxl")
library("readxl")
datasets2 <- read_excel("C:\\Users\\MAMSL\\Downloads\\LAST DATA SET 2.xlsx")
datasets2



# Initiation of the data set
library(factoextra)

community_data <-  datasets2[2:18]  
community_data


# scale data
community_data_scale <- scale(community_data)

# Computation of the Euclidean Distance
community_data <- dist(community_data_scale)

# calculate how many clusters are optimal
fviz_nbclust(community_data_scale, kmeans, method="wss")+
  labs(subtitle = "elbow method")

# KMEANS
km.out <- kmeans(community_data_scale, centers = 3, nstart = 100)
print(km.out)

# visualize the clustering results 
km.clusters <- km.out$cluster
rownames(community_data_scale) <- paste(datasets2$Community, 1:dim(datasets2)[1],sep = "_")
fviz_cluster(list(data= community_data_scale, cluster=km.clusters))
table(km.clusters, datasets2$Community)


# Example; adding n= 20 additional user to the orginal dataset
# Asumming age is not taken when clustering
library(factoextra)

community_data <- cbind(datasets2[2:18],sample(seq(0,1,by= 0.001),20))  
community_data

# scale data
community_data_scale <- scale(community_data)

# Computation of the Euclidean Distance
community_data <- dist(community_data_scale)

# calculate how many clusters needed
fviz_nbclust(community_data_scale, kmeans, method="wss")+
  labs(subtitle = "elbow method")

# KMEANS
km.out <- kmeans(community_data_scale, centers = 5, nstart = 100)
print(km.out)

# visualize the clustering results 
km.clusters <- km.out$cluster
rownames(community_data_scale) <- paste(datasets2$Community, 1:dim(datasets2)[1],sep = "_")
fviz_cluster(list(data= community_data_scale, cluster=km.clusters))
table(km.clusters, datasets2$Community)


# Example; adding n= 100 additional user to the orginal dataset

library(factoextra)

community_data <- cbind(datasets2[2:18],sample(seq(0,1,by= 0.001),100))  
community_data

# scale data
community_data_scale <- scale(community_data)

# Computation of the Euclidean Distance
community_data <- dist(community_data_scale)

# calculate how many clusters needed
fviz_nbclust(community_data_scale, kmeans, method="wss")+
  labs(subtitle = "elbow method")

# KMEANS
km.out <- kmeans(community_data_scale, centers = 5, nstart = 100)
print(km.out)

# visualize the clustering results 
km.clusters <- km.out$cluster
rownames(community_data_scale) <- paste(datasets2$Community, 1:dim(datasets2)[1],sep = "_")
fviz_cluster(list(data= community_data_scale, cluster=km.clusters))
table(km.clusters, datasets2$Community)


#Example; adding n= 10000 additional user to the orginal dataset
community_data <- cbind(datasets2[2:18],sample(seq(0,1,by= 0.001),10000))  
community_data

# scale data
community_data_scale <- scale(community_data)

# Computation of the Euclidean Distance
community_data <- dist(community_data_scale)

# calculate how many clusters needed
fviz_nbclust(community_data_scale, kmeans, method="wss")+
  labs(subtitle = "elbow method for 10000 additional users")

# KMEANS
km.out <- kmeans(community_data_scale, centers = 5, nstart = 100)
print(km.out)

# visualize the clustering results 
km.clusters <- km.out$cluster
rownames(community_data_scale) <- paste(datasets2$Community, 1:dim(datasets2)[1],sep = "_")
fviz_cluster(list(data= community_data_scale, cluster=km.clusters))
table(km.clusters, datasets2$Community)
