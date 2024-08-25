#install and load packages
install.packages("plotly")
library(plotly)
install.packages("ggplot2")
library(ggplot2)
install.packages("fpc")
library(fpc)
install.packages("dplyr")
library(dplyr)
install.packages("cluster")
library(cluster)
install.packages('scatterplot3d')
library(scatterplot3d)

#Import csv and assign it to a data frame
mdf <- read_csv('Mall_Customers.csv')
View(mdf)

#Check null or duplicate values
sum(is.na(mdf))
mdf[duplicated(mdf), ]

#Create a dataframe to include only numerical columns
quantdf <- mdf[c(-1,-2)]
View(quantdf)

#Create histogram to inspect the data
ggplot(data = mdf) +
  geom_histogram(mapping = aes(x= quantdf$Age), binwidth = 5) +
  labs(x = "Age", y = "Count")

ggplot(data = mdf) +
  geom_histogram(mapping = aes(x= quantdf$`Annual Income (k$)`), binwidth = 1) +
  labs(x = "Annual Income", y = "Count")

ggplot(data = mdf) +
  geom_histogram(mapping = aes(x= quantdf$`Spending Score (1-100)`), binwidth = 1) +
  labs(x = "Spending Score", y = "Count")

#Scale the date to standardize to same unit
quantdfn <- scale(quantdf)


#Create a function to return WSS values for each value of k
wss <- function(k){
  kmeans(quantdfn, k, nstart= 10)
} $tot.withinss

#We will have 10 values of k from 1 to 10
k_values <- 1:10

#Perform wss function on each value of k
wss_values <- map_dbl(k_values, wss)

#Create a dataframe to include the values of k and WSS
elbowdf <- data.frame(k_values, wss_values)

#Visualize the elbow plot
ggplot(elbowdf, mapping = aes(x = k_values, y = wss_values)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(min(elbowdf$k_values), max(elbowdf$k_values), by = 1))

#We will run k means for 4 clusters
k4<- kmeans(quantdfn, 4, nstart = 1000)

#Observe the characterstics of the clusters
cluster.stats(dist(quantdfn, method = "euclidean"),k4$cluster)

#Bind the cluster column to the data frame with numeric columns
quantdfk4 <- cbind(quantdf, clusterID = k4$cluster)
View(quantdfk4)

scatterplot3d(quantdf$Age, 
              quantdf$`Annual Income (k$)`, 
              quantdf$`Spending Score (1-100)`, 
              color = quantdfk4$clusterID, 
              pch = 16, 
              main = "3D Scatter Plot of Clusters",
              xlab = "Age",
              ylab = "Annual Income (k$)",
              zlab = "Spending Score (1-100)")

#Summarise the clusters based on the average of values in the clusters
quantdfk4 %>%
  group_by(clusterID) %>%
  summarise_all(mean)

#Summarise the average of actual data
summarise_all(quantdf, mean)