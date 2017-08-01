library(caret)
library(tidyverse)

# Import test/train set
train_dat <- readRDS("data/train.RDS")
#test_dat <- readRDS("data/test.RDS")

# Set seed
seed <- 1234

# Set variables to be used in neural net
e.var <- c()

# Train the model on the training set
my.grid <- expand.grid(.decay = c(0.1,0.5,0.9), .size = c(2, 3, 4))
myControl <-
nnet.fit <- train(income ~ prestige + education,
                  data = train_dat,
                  method = "nnet",
                  maxit = 1000,
                  tuneGrid = my.grid,
                  trace = F,
                  linout = 1)