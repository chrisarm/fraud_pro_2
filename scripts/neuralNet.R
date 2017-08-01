suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(tidyverse))

# Import train set
train_dat <- readRDS("data/train.RDS")

# Set seed
set.seed(1234)

# Train the model on the training set
my.grid <- expand.grid(.decay = c(0.1,0.5,0.9), .size = c(2, 3, 4))
my.control <- trainControl(method = 'cv', number = 10)
nnet.fit <- train(fraud ~ e.1meanamount_zip3 + e.7medianamount_zip3 + e.1medianamount_zip3 + e.7meanamount_cardnum +
                    e.7avgdailyfreq_cardnum + e.7meanamount_merchnum + e.7avgdailyfreq_merchnum + e.1countzip3_cardnum +
                    e.curdaybal_dow_cardnum + e.curdaybal_dow_merchnum,
                  data = train_dat,
                  method = "nnet",
                  trControl = my.control,
                  maxit = 1000,
                  tuneGrid = my.grid,
                  trace = F,
                  linout = 1)

# Save model
save(nnet.fit, file = "data/nnet_model.rda")

# Review results
summary(nnet.fit)