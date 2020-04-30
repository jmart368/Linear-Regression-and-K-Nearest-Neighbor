---
title: "Assignment 2"
output: "Linear Regression and K-Nearest Neighbor"
---
## Task 1 Simple Linear Regression

# Question 2
insurance.df <- read.csv("insurance.csv", header = T) # load data
dim(insurance.df) # view dimensions

# Question 3
library(dplyr)
non_smokers <- filter(insurance.df, smoker == "no")
dim(non_smokers)

# Question 4
library(ggplot2)
ggplot(non_smokers, aes(x = age, y = charges, color = charges)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  scale_color_gradientn(colours = rainbow(3)) +
  theme_classic()

# Question 5
names(non_smokers)
age_charges <- non_smokers[, c(1,7)]
cor(age_charges) # Correlation table
cor(non_smokers$age, non_smokers$charges, use="complete.obs") # alternative

# Question 6
sample_size <- floor(0.60 * nrow(non_smokers))
set.seed(180) # set seed for reporducing the partion
train_index <- sample(seq_len(nrow(non_smokers)), size = sample_size)

training <- non_smokers[train_index,]
validation <- non_smokers[-train_index,]

# Question 7
insurance_model <- lm(charges~age, data = training)
options(scipen = 999, digits = 0)
summary(insurance_model)

# Question 8
# regression equation -> y = -1877.5 + 263.5x
# if 35 = x, then,
-1877.5 + 263.5 * 35
# for being 35 years old, your charges would be $7,345

# Question 9
library(forecast)
prediction1 <- predict(insurance_model, training) # predict training
accuracy(prediction1, training$charges)
prediction2 <- predict(insurance_model, validation) # predict validation
accuracy(prediction2, validation$charges)
# With the exception of the ME, the RMSE, MAE, MPE, and MAPE are all within range
# This makes the training and validation set pretty accurate

## Task 2 K-Nearest Neighbor

# Question 2
fishmarket.df <- read.csv("fishmarket.csv", header = T) # load data
dim(fishmarket.df)

# Question 3
library(dplyr)
set.seed(180) # set seed for reporducing the partion
fishmarket <- sample_frac(fishmarket.df, 1)
training <- slice(fishmarket, 1:95)
validation <- slice(fishmarket, 96:159)

# Question 4 a)
# balloon_molly

# Question 4 b)
Weight <- runif(1, min(training$Weight), max(training$Weight))
Lenght1 <- runif(1, min(training$Length1), max(training$Length1))
Lenght2 <- runif(1, min(training$Length2), max(training$Length2))
Lenght3 <- runif(1, min(training$Length3), max(training$Length3))
Height <- runif(1, min(training$Height), max(training$Height))
Width <- runif(1, min(training$Width), max(training$Width))

balloon_molly <- data.frame(Weight = 172, 
                            Length1 = 12, 
                            Length2 = 43, 
                            Length3 = 48, 
                            Height = 10.841, 
                            Width = 7)

# Question 5
library(caret)
training.norm <- training
validation.norm <- validation
balloon_molly.norm <- balloon_molly

summary(training[, 2:7])

norm.values <- preProcess(training[,2:7], method = c("center", "scale"))
training.norm[, 2:7] <- predict(norm.values, training[, 2:7])
validation.norm[, 2:7] <- predict(norm.values, validation[, 2:7])
balloon_molly.norm[, 1:6] <- predict(norm.values, balloon_molly[, 1:6])


# Question 6
library(FNN)
nn <- knn(train = training.norm[, 2:7], 
          test = balloon_molly.norm[, 1:6], 
          cl = training.norm[, 1], 
          k = 7)
row.names(training)[attr(nn, "nn.index")]
nn

# Question 7 a)
accuracy.df <- data.frame(k = seq(1, 95, 1), accuracy = rep(0,95))

for(i in 1:95) {
  knn.pred <- knn(training.norm[, 2:7], 
                  validation.norm[, 2:7], 
                  cl = training.norm[, 1], 
                  k = i)
accuracy.df[i, 2] <- confusionMatrix(knn.pred, validation.norm[, 1])$overall[1]
}
accuracy.df

# Question 7 b)
library(ggplot2)
ggplot(accuracy.df, aes(x=k, y=accuracy)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE) + 
  theme_classic()

# Question 8
nn.new <- knn(train = training.norm[, 2:7], 
              test = balloon_molly.norm[, 1:6], 
              cl = training.norm[, 1], 
              k = 1)
nn.new
