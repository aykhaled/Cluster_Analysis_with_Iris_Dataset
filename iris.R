# Cluster Analysis with Iris Data set
#####################################
# https://www.linkedin.com/in/aykhaled/
# https://medium.com/@aykhaled

#! Install Packages 
# install.packages("tidyverse") # for data work
# install.packages("cluster")   # for clustering work
# install.packages("reshape2")  # for melting data
# # note : not required if already installed

#! Load the packages
library(tidyverse)
library(cluster)
library(reshape2)

#! import data set
# setwd("E:/my_folder/work_folder")
# mydata <- read.csv("iris.csv")
# or,

mydata <- iris


#! Explore the data set
# !!!!!!!!!!!!!!!!!!!!!

glimpse(mydata)
head(mydata)
View(mydata)


# Sepal-Length vs. Sepal-Width
ggplot(mydata)+
  geom_point(aes(x = Sepal.Length, y = Sepal.Width), stroke = 2)+
  facet_wrap(~ Species) + 
  labs(x = 'Sepal Length', y = 'Sepal Width')+
  theme_bw()


# Petal-Length vs. Petal-Width
ggplot(mydata)+
  geom_point(aes(x = Petal.Length, y = Petal.Width), stroke = 2)+
  facet_wrap(~ Species) + 
  labs(x = 'Petal Length', y = 'Petal Width')+
  theme_bw()



# Sepal-Length vs. Petal-Length
ggplot(mydata)+
  geom_point(aes(x = Sepal.Length, y = Petal.Length), stroke = 2)+
  facet_wrap(~ Species) + 
  labs(x = 'Sepal Length', y = 'Petal Length')+
  theme_bw()


# Sepal-Width vs. Pedal-Width
ggplot(mydata)+
  geom_point(aes(x = Sepal.Width, y = Petal.Width), stroke = 2)+
  facet_wrap(~ Species) + 
  labs(x = 'Sepal Width', y = 'Pedal Width')+
  theme_bw()

# Box plots

ggplot(mydata)+
  geom_boxplot(aes(x = Species, y = Sepal.Length, fill = Species))+
  theme_bw()

ggplot(mydata)+
  geom_boxplot(aes(x = Species, y = Sepal.Width, fill = Species))+
  theme_bw()

ggplot(mydata)+
  geom_boxplot(aes(x = Species, y = Petal.Length, fill = Species))+
  theme_bw()

ggplot(mydata)+
  geom_boxplot(aes(x = Species, y = Petal.Width, fill = Species))+
  theme_bw()


#! k-means Clustering
# !!!!!!!!!!!!!!!!!!!

# Elbow method to find the optimal number of clusters
set.seed(123) # for reproduction
wcss <- vector()

for (i in 1:10) wcss[i] <- sum(kmeans(mydata[, -5], i)$withinss)

plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of Clusters',
     ylab = 'WCSS'
)

# apply kmeans
set.seed(123)  # for reproduction
km <- kmeans( x = mydata[, -5] , centers = 3)

yclus <- km$cluster
table(yclus)

# cluster plot
clusplot(mydata[, -5],
         yclus,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 0,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of Iris Flowers')
)

#! Compare clusters
# !!!!!!!!!!!!!!!!!

mydata$cluster.kmean <- yclus

cm <- table(mydata$Species, mydata$cluster.kmean)
cm

# Tiles plot
mtable <- melt(cm)

ggplot(mtable)+
  geom_tile(aes(x = Var1, y = Var2, fill = value))+
  labs(x = 'Species', y = 'kmeans Cluster')+
  theme_bw()

# scatter plots
mydata$cluster.kmean <- as.factor(mydata$cluster.kmean)

# Sepal-Length vs. Sepal-Width (Species)
ggplot(mydata)+
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, 
                 color = Species) , size = 10)+ 
  labs(x = 'Sepal Length', y = 'Sepal Width')+
  ggtitle("Species")+
  theme_bw()

# Sepal-Length vs. Sepal-Width (kmeans cluster)
ggplot(mydata)+
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, 
                 color = cluster.kmean) , size = 10)+ 
  labs(x = 'Sepal Length', y = 'Sepal Width')+
  ggtitle("kmeans Cluster")+
  theme_bw()


#/
# Petal-Length vs. Petal-Width (Species)
ggplot(mydata)+
  geom_point(aes(x = Petal.Length, y = Petal.Width, 
                 color = Species) , size = 10)+ 
  labs(x = 'Petal Length', y = 'Petal Width')+
  ggtitle("Species")+
  theme_bw()

# Petal-Length vs. Petal-Width (kmeans cluster)
ggplot(mydata)+
  geom_point(aes(x = Petal.Length, y = Petal.Width, 
                 color = cluster.kmean) , size = 10)+ 
  labs(x = 'Petal Length', y = 'Petal Width')+
  ggtitle("kmeans Cluster")+
  theme_bw()
 
