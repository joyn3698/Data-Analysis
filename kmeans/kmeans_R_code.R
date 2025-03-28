# Install and load packages
install.packages("plotly")
library(plotly)
install.packages("ggplot2")
library(ggplot2)
install.packages("fpc")
library(fpc)
install.packages("dplyr")
library(dplyr)
install.packages("purrr")
library(purrr)
install.packages("cluster")
library(cluster)
install.packages('scatterplot3d')
library(scatterplot3d)
install.packages('readr')
library(readr)

# Import CSV and assign it to a data frame
mdf <- read_csv('/Users/joy/Desktop/Practise/Mall_Customers.csv')

# Rename columns to lowercase with underscores and update specific names
names(mdf) <- tolower(gsub(" ", "_", names(mdf)))
names(mdf)[names(mdf) == "annual_income_(k$)"] <- "annual_income_k"
names(mdf)[names(mdf) == "spending_score_(1-100)"] <- "spending_score"

# Check for null or duplicate values
sum(is.na(mdf))
mdf[duplicated(mdf), ]

# Visual check: Does gender affect age, income, or spending score?
ggplot(mdf, aes(x = gender, y = annual_income_k, fill = gender)) +
  geom_boxplot() +
  labs(title = "Annual Income by Gender")

ggplot(mdf, aes(x = gender, y = spending_score, fill = gender)) +
  geom_boxplot() +
  labs(title = "Spending Score by Gender")

ggplot(mdf, aes(x = gender, y = age, fill = gender)) +
  geom_boxplot() +
  labs(title = "Age by Gender")

# Create a dataframe with only numerical columns (excluding ID and Gender)
# Gender is excluded based on prior EDA which showed it doesn't cause significant variation in other variables
quantdf <- mdf[c(-1, -2)]  # Assuming 1st is ID, 2nd is gender

# Scatter plots before capping (relationship checks before clustering)
# These plots help visualize how age, income, and spending score relate to each other.
plot(quantdf$age, quantdf$annual_income_k,
     main = "Age vs Annual Income (Before Capping)",
     xlab = "age", ylab = "annual_income_k",
     col = 'blue', pch = 16)

plot(quantdf$annual_income_k, quantdf$spending_score,
     main = "Annual Income vs Spending Score (Before Capping)",
     xlab = "annual_income_k", ylab = "spending_score",
     col = 'darkgreen', pch = 16)

plot(quantdf$age, quantdf$spending_score,
     main = "Age vs Spending Score (Before Capping)",
     xlab = "age", ylab = "spending_score",
     col = 'purple', pch = 16)

# Cap Annual Income using IQR method
Q1 <- quantile(quantdf$annual_income_k, 0.25)
Q3 <- quantile(quantdf$annual_income_k, 0.75)
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

quantdf$annual_income_k <- ifelse(quantdf$annual_income_k > upper_bound, 
                                  upper_bound,
                                  ifelse(quantdf$annual_income_k < lower_bound, 
                                         lower_bound,
                                         quantdf$annual_income_k))

# Histograms for numerical features (to check distribution before scaling)
# These help explain why standardization is needed.
ggplot(mdf, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(x = "Age", y = "Count")

ggplot(mdf, aes(x = annual_income_k)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(x = "Annual Income (k$)", y = "Count")

ggplot(mdf, aes(x = spending_score)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(x = "spending_score", y = "Count")

# Scale the data to standardize it
quantdfn <- scale(quantdf)

# Function to compute WSS for a given k
wss <- function(k) {
  kmeans(quantdfn, k, nstart = 10)$tot.withinss
}

# Compute WSS for k = 1 to 10
k_values <- 1:10
wss_values <- map_dbl(k_values, wss)
elbowdf <- data.frame(k_values, wss_values)

# Elbow plot to determine optimal k
ggplot(elbowdf, aes(x = k_values, y = wss_values)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Elbow Method for Optimal k", x = "Number of Clusters (k)", y = "WSS")

# Apply K-means clustering for k = 4
k4 <- kmeans(quantdfn, 4, nstart = 1000)

# Evaluate cluster statistics
cluster_stats <- cluster.stats(dist(quantdfn), k4$cluster)
print(cluster_stats)

# Add cluster ID to original data
quantdfk4 <- cbind(quantdf, clusterID = k4$cluster)

# 3D Scatter plot of clusters
scatterplot3d(quantdf$age,
              quantdf$annual_income_k,
              quantdf$spending_score,
              color = quantdfk4$clusterID,
              pch = 16,
              main = "3D Scatter Plot of Clusters",
              xlab = "age",
              ylab = "annual_income_k",
              zlab = "spending_score")

# Summarize each cluster by mean of numerical features
cluster_summary <- quantdfk4 %>%
  group_by(clusterID) %>%
  summarise(across(everything(), mean))
print(cluster_summary)

# Overall summary of the data
overall_summary <- quantdf %>% summarise(across(everything(), mean))
print(overall_summary)

