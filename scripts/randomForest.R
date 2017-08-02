library(randomForest)
library(caret)
library(mlbench)
library(pROC)

# read in data
dataset <- readRDS("data/final.RDS")
# remove OOT data
dataset <- dataset %>%
  filter(date < "2010-10-01")
# remove record number and date fields
dataset <- dataset[,-c(1,2)]
# change fraud to factor
dataset$fraud <- as.factor(dataset$fraud)

### SET THESE VALUES BEFORE RUNNING ###
seed <- 1234
x <- dataset[,2:11] # features
y <- dataset[,1] # outcome variable
mtry <- c(1:10) # Number of variables randomly sampled as candidates at each split to test
ntree <- c(100, 200, 300) # number of trees to test
metric <- "Accuracy"

# custom caret functions
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# train model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=mtry, .ntree=ntree)
set.seed(seed)
custom <- train(fraud~., data=dataset, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)