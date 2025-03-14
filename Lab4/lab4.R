wine

colnames(wine) <- c("class", "Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavinoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")

library(caret)
library(ggfortify)

wine.df <- wine
head(wine.df)

X <- wine.df[,2:14]
principal_components <- princomp(X, cor = TRUE, score = TRUE)
#summary(principal_components)
principal_components$loadings


X <- wine.df[, setdiff(2:14, 6)]
principal_components <- princomp(X, cor = TRUE, score = TRUE)
#summary(principal_components)
principal_components$loadings

# Convert class to a factor for classification
wine.df$class <- as.factor(wine.df$class)

set.seed(123) 
trainIndex <- createDataPartition(wine.df$class, p = 0.7, list = FALSE)
trainData <- wine.df[trainIndex,]
testData <- wine.df[-trainIndex,]

knn_model_original <- train(class ~ ., data = trainData, method = "knn", tuneLength = 5)

predictions_original <- predict(knn_model_original, testData)

conf_matrix_original <- confusionMatrix(predictions_original, testData$class)
print(conf_matrix_original)

X <- wine.df[,2:14]
pca_result <- prcomp(X, center = TRUE, scale. = TRUE)

# Extract first 3 principal components
pca_scores <- as.data.frame(pca_result$x[,1:3])
pca_scores$class <- wine.df$class 

# Split data into training and testing sets
trainData_pca <- pca_scores[trainIndex,]
testData_pca <- pca_scores[-trainIndex,]

knn_model_pca <- train(class ~ ., data = trainData_pca, method = "knn", tuneLength = 5)

# Make predictions
predictions_pca <- predict(knn_model_pca, testData_pca)

# Evaluate the model
conf_matrix_pca <- confusionMatrix(predictions_pca, testData_pca$class)
print(conf_matrix_pca)

#contingency table
conf_matrix_original <- confusionMatrix(predictions_original, testData$class)
print(conf_matrix_original$table)

conf_matrix_pca <- confusionMatrix(predictions_pca, testData_pca$class)
print(conf_matrix_pca$table)
