library(class)
library(caret)
library(cluster)
library(ggplot2)

# read dataset
dataset <- read.csv("C:/Users/dlin3/OneDrive/Desktop/RPI/Data Analytics/Labs/abalone_dataset.csv")

# sample create list of 80% of the dataset indexes randomly sampled
n <- nrow(dataset)
s_abalone <- sample(n, n * 0.8)

## create train & test sets based on sampled indexes 
dataset.train <- dataset[s_abalone,]
dataset.test <- dataset[-s_abalone,]

# simple estimate of k
k <- round(sqrt(n))
k <- k - 1

## train model & predict in one step ('knn' function from 'class' library)
knn.predicted <- knn(train = dataset.train[,c("length", "diameter", "whole_weight", "shell_weight")], 
                     test = dataset.test[,c("length", "diameter", "whole_weight", "shell_weight")], 
                     cl = dataset.train$rings, k = 11)

# create contingency table/ confusion matrix 
contingency.table <- table(knn.predicted, dataset.test$rings, dnn=list('predicted','actual'))

contingency.table

# calculate classification accuracy
sum(diag(contingency.table))/length(dataset.test$rings)

## train knn models for multiple values of k and plot accuracies

# list of k
k.list <- seq(3, 21, by=2)  # Choosing odd k values for better results

# empty list for accuracy
accuracy.list <- c()

# loop: train&predict model for each k, compute accuracy and append it to list
for (k in k.list) {
  knn.predicted <- knn(train = dataset[,c("length", "diameter", "whole_weight", "shell_weight")], 
                       test = dataset[,c("length", "diameter", "whole_weight", "shell_weight")], 
                       cl = dataset$rings, k = k)
  
  contingency.table <- table(knn.predicted, dataset$rings, dnn=list('predicted','actual'))
  
  accuracy <- sum(diag(contingency.table))/length(dataset$rings)
  
  accuracy.list <- c(accuracy.list, accuracy)
}

# Find the best k
best.k <- k.list[which.max(accuracy.list)]
cat("Optimal k value:", best.k, "with accuracy:", max(accuracy.list), "\n")

# plot accuracy with k
plot(k.list, accuracy.list, type = "b", ylim = c(min(accuracy.list), max(accuracy.list)), 
     xlab = "k values", ylab = "Accuracy", main = "Optimal k Selection")

## Alternatively:
## train and evaluate multiple knn models to find optimal k
knn.model <- train(dataset[,c("length", "diameter", "whole_weight", "shell_weight")], dataset$rings, 
                   method = "knn", tuneLength = 10, trControl = trainControl(method = "cv"))

# print model outputs
print(knn.model)

# Determine the optimal K for K-Means 
wss <- sapply(1:10, function(k) {
  kmeans(dataset[,c("length", "diameter", "whole_weight", "shell_weight")], centers = k, nstart = 10)$tot.withinss
})

plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

# Train K-Means with optimal K
optimal.k <- which.min(diff(diff(wss))) + 1  # Approximate best K using elbow method
kmeans.model <- kmeans(dataset[,c("length", "diameter", "whole_weight", "shell_weight")], centers = optimal.k, nstart = 10)

# Plot clusters using two selected features
ggplot(dataset, aes(x = length, y = diameter, color = as.factor(kmeans.model$cluster))) +
  geom_point() +
  labs(title = "K-Means Clustering", x = "Length", y = "Diameter", color = "Cluster") +
  theme_minimal()

#### END ####

