library("ggplot2")
library("readr")
setwd("C:/Users/dlin3/OneDrive/Desktop/RPI/Data Analytics/Labs/")

NY_House_Dataset <- read.csv("NY-House-Dataset.csv")
dataset <- NY_House_Dataset

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()


dataset <- dataset[dataset$PRICE < 195000000,]
dataset <- dataset[dataset$BATH > 0, ]

names(dataset)

model1 <- lm(PRICE ~ PROPERTYSQFT, data = dataset)
model1 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
model2 <- lm(PRICE ~ BEDS + BATH, data = dataset)
model2 <- lm(log10(PRICE)~log10(BEDS) + log10(BATH), data = dataset)
model3 <- lm(PRICE ~ PROPERTYSQFT + BEDS + BATH, data = dataset)
model3 <- lm(log10(PRICE)~log10(PROPERTYSQFT) + log10(BEDS) + log10(BATH), data = dataset)

summary(model1)
summary(model2)
summary(model3)

# Model 1 plots
plot(PRICE ~ PROPERTYSQFT, data = dataset)
abline(model1)

plot(log10(PRICE) ~ log10(PROPERTYSQFT), data = dataset)
abline(model1)

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# Model 2 plots
plot(PRICE ~ BEDS + BATH, data = dataset)
abline(model2)

plot(log10(PRICE) ~ BEDS + BATH, data = dataset)
abline(model2)

ggplot(dataset, aes(x = BEDS, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = BEDS, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "blue")

ggplot(dataset, aes(x = BEDS, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col = "blue")

# Model 3 plots
plot(PRICE ~ PROPERTYSQFT + BEDS + BATH, data = dataset)
abline(model3)

plot(log10(PRICE) ~ log10(PROPERTYSQFT) + BEDS + BATH , data = dataset)
abline(model3)

ggplot(dataset, aes(x = BATH, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = BATH, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "green")

ggplot(dataset, aes(x = BATH, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col = "green")

# Model 1 Residual Plot
ggplot(data = data.frame(fitted = fitted(model1), resid = residuals(model1)), 
       aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Model 2 Residual Plot
ggplot(data = data.frame(fitted = fitted(model2), resid = residuals(model2)), 
       aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")

# Model 3 Residual Plot
ggplot(data = data.frame(fitted = fitted(model3), resid = residuals(model3)), 
       aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "green")

