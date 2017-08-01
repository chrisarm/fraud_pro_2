suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(tidyverse))

# Import train set
train_dat <- readRDS("data/train.RDS")

# Set seed
set.seed(1234)

# Adjust data type for fraud variable
train_dat$fraud <- as.numeric(train_dat$fraud)

# Train the model on the training set
my.grid <- expand.grid(.interaction.depth = c(1,2), n.trees = c(2000,3000,4000))
my.control <- trainControl(method = 'cv', number = 10)

bt.fit <- train(fraud ~ e.1meanamount_zip3 + e.7medianamount_zip3 + e.1medianamount_zip3 + e.7meanamount_cardnum +
                    e.7avgdailyfreq_cardnum + e.7meanamount_merchnum + e.7avgdailyfreq_merchnum + e.1countzip3_cardnum +
                    e.curdaybal_dow_cardnum + e.curdaybal_dow_merchnum,
                  data = train_dat,
                  method = "bstTree",
                  trControl = my.control,
                  maxit = 1000,
                  tuneGrid = my.grid,
                  trace = F,
                  linout = 1)

# Save model
save(bt.fit, file = "models/bt_model.rda")

# Review results
summary(bt.fit)