library(randomForest)
library(caret)
library(mlbench)
library(pROC)

dataset <- readRDS("data/expert_card_payments_v3.RDS")

seed <- 1234

### SET THESE VALUES BEFORE RUNNING ###
x <- dataset[,1:20] # features
y <- dataset[,21] # outcome variable
mtry <- c(1:10) # Number of variables randomly sampled as candidates at each split to test
ntree <- c(10, 20, 30) # number of trees to test

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
custom <- train(Class~., data=dataset, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)