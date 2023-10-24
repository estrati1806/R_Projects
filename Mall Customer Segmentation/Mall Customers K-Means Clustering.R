# libraries
library(caret)
library(DescTools)
library(ggplot2)
library(cluster)
library(factoextra) 
library(fpc)
library(dplyr)

# dataset
library(readr)
mall <- read_csv("Mall_Customers.csv")
View(mall)

# listing contents
ls(mall)

# preprocessing
dim(mall)
str(mall)
summary(mall)
# missing values
missing_values <- sum(is.na(mall))
missing_values
# no missing values

# exploring the data
# gender
gender_counts <- mall %>%
  group_by(Gender) %>%
  summarise(Count = n())
gender_counts

# age boxplot
ggplot(as.data.frame(mall$Age), aes(y = mall$Age)) + geom_boxplot(fill='#F8766D')

# age distribution between genders
ggplot(mall, aes( x = Age, fill = Gender)) + geom_density(alpha = 0.4)
# there are more female customers between 22 and 55 years of age
# as for the age group 55-70, men are more predominant than women
# both men and women groups have binomial distribution:
# women have 2 peak groups: one that peaks at the age of ~32, the other one at ~47
# men groups peak at ~33 and a smaller peak at ~62
# based on age and gender alone, there are 4 visible groups

# Annual income and spending score boxplots
library(gridExtra)
p1 <- ggplot(as.data.frame(mall$'Annual Income (k$)'), aes(y = mall$'Annual Income (k$)')) + geom_boxplot(fill='#2FA4FF')
p2 <- ggplot(as.data.frame(mall$'Spending Score (1-100)'), aes(y = mall$'Spending Score (1-100)')) + geom_boxplot(fill='#00AB08')
grid.arrange(p1, p2, ncol = 2)

# scatterplot of age vs spending score
plot(mall$Age,mall$'Spending Score (1-100)',col="red",xlab="Age",ylab = "Spending Score", main="Age VS Spending Score")
# no correlation
# scatterplot of income vs spending score
plot(mall$'Annual Income (k$)',mall$'Spending Score (1-100)',col="red",xlab="Annual Income",ylab = "Spending Score", main="Annual Income VS Spending Score")
# 5 visible groups

# k-means
# k-means only works with continuous variables
# we need to remove the CustomerID variable since it's irrelevant,
# as well as the Gender variable (binary)
# we will only use age, annual income, and spending score
Kdata <- mall[, -c(1, 2)]

# scaling the data
Kdata <- scale(Kdata)
Kdata <- as.data.frame(Kdata)
head(Kdata)

# implementing the model
# we will use 5 centers, since we saw that there seem to be 5 groups in the last scatterplot
k1 <- kmeans(Kdata, centers = 5, nstart = 30)
k1

# this model has a (between_SS / total_SS =  72.0 %) which sounds good
# WSS for cluster 5 is the lowest

# visualizing the cluster solution
fviz_cluster(k1, data = Kdata)

# Let's try kMC with 6 clusters
k2 <- kmeans(Kdata, centers = 6, nstart = 30)
k2

# the results are better
# Within cluster sum of squares by cluster:
# [1] 11.71664 20.52332 23.87015 34.51630 22.36267 20.20990
# (between_SS / total_SS =  77.7 %)

# visualizing the cluster solution
fviz_cluster(k2, data = Kdata)

# The larger the number of clusters, the better the WSS and BSS/TSS will be
# However, we don't want to create too many clusters, because that would be 
# inefficient from a marketing perspective - the larger the number of clusters 
# the more groups you need to cater to

# we will stick with 6

mall$kmeans <- k2$cluster
mall <- mall %>%
  mutate(Gender = ifelse(Gender == "Female", 1, 0))
# female: 1, male: 0
mall_clusters <- mall %>%
  group_by(kmeans) %>%
  summarise(Age_mean= mean(Age),
            Income_mean= mean(`Annual Income (k$)`),
            SpenScore_mean= mean(`Spending Score (1-100)`),
            Gender=(mean(Gender)),
            Count=(length(kmeans)))
mall_clusters

# The 6 clusters are as follows:
# Cluster 1: Early adults, higher income, with high spending scores
# Cluster 2: Early adults, medium income, medium spending scores
# Cluster 3: Middle aged, low income, low spending scores
# Cluster 4: Early adults, low income, high spending scores
# Cluster 5: Middle aged, higher income, low spending scores - only group with where men are predominant
# Cluster 6: Late middle age, medium income, medium spending scores - largest group

# Some suggestions for the mall owner:
# 1. Can target Cluster 4 (high avg income, low spending) to incentivize them to spend more money
# 2. Invest more to personalize Cluster 1's buying experience, since they have the highest avg spending scores and high income

