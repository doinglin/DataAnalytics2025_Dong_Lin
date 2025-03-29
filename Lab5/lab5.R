# Load required libraries
library(readr)
library(ggplot2)
library(e1071)
library(caret)

# Read wine data
wine <- read_csv("C:/Users/dlin3/OneDrive/Desktop/RPI/Data Analytics/Labs/Lab5/wine.data", col_names = FALSE)


# Assign column names
colnames(wine) <- c("Type", "Alcohol","MalicAcid","Ash","AlcalinityAsh","Magnesium",
                    "TotalPhenols","Flavanoids","NonflavanoidPhenols","Proanthocyanins",
                    "ColorIntensity","Hue","OD280_OD315","Proline")

# Convert class variable to factor
wine$Type <- as.factor(wine$Type)

# Set seed and split into train/test sets
set.seed(123)
train.index <- createDataPartition(wine$Type, p = 0.75, list = FALSE)
train <- wine[train.index, ]
test <- wine[-train.index, ]

# Select subset of features (you may change these)
selected_features <- c("Alcohol", "MalicAcid", "ColorIntensity", "Proline")

# --------------------- SVM LINEAR ---------------------
tune.linear <- tune.svm(Type ~ ., data = train[, c("Type", selected_features)],
                        kernel = "linear",
                        cost = 2^(-1:5))

svm.linear <- tune.linear$best.model
pred.linear <- predict(svm.linear, test)
conf.linear <- confusionMatrix(pred.linear, test$Type)

# --------------------- SVM RADIAL ---------------------
tune.radial <- tune.svm(Type ~ ., data = train[, c("Type", selected_features)],
                        kernel = "radial",
                        cost = 2^(-1:5),
                        gamma = 2^(-5:1))

svm.radial <- tune.radial$best.model
pred.radial <- predict(svm.radial, test)
conf.radial <- confusionMatrix(pred.radial, test$Type)

# --------------------- NAIVE BAYES ---------------------
nb.model <- naiveBayes(Type ~ ., data = train[, c("Type", selected_features)])
pred.nb <- predict(nb.model, test)
conf.nb <- confusionMatrix(pred.nb, test$Type)

# --------------------- RESULTS ---------------------
cat("\nSVM (Linear) Performance:\n")
print(conf.linear$byClass[, c("Precision", "Recall", "F1")])

cat("\nSVM (Radial) Performance:\n")
print(conf.radial$byClass[, c("Precision", "Recall", "F1")])

cat("\nNaive Bayes Performance:\n")
print(conf.nb$byClass[, c("Precision", "Recall", "F1")])
