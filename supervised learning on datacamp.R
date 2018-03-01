library(caret)
library(broom)
library(e1071)

x <- readRDS("datacamp modules/x")

# Create the k-means model: km.out
km.out <- kmeans(x, centers = 3, nstart = 20)

# Inspect the result
summary(km.out)

# Print the cluster membership component of the model
km.out[["cluster"]]

# Print the km.out object
print(km.out)

# Scatter plot of x
plot(x, col = km.out$cluster, main = "k-means with 3 clusters", xlab = "", ylab = "")

# Set up 2 x 3 plotting grid
par(mfrow = c(2, 3))

# Set seed
set.seed(1)

for(i in 1:6) {
  # Run kmeans() on x with three clusters and one start
  km.out <- kmeans(x, centers = 3, nstart = 1)
  
  # Plot clusters
  plot(x, col = km.out$cluster, 
       main = km.out$tot.withinss, 
       xlab = "", ylab = "")
}

