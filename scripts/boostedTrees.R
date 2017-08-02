suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(tidyverse))

# Import train set
train_dat <- readRDS("data/train.RDS")

# Set seed
set.seed(1234)

# Adjust data type for fraud variable
train_dat$fraud <- as.numeric(train_dat$fraud)

# Train the model on the training set
# my.grid <- expand.grid(interaction.depth = 1:2,
#                        shrinkage = .1,
#                        n.trees = c(2000,3000,4000),
#                        n.minobsinnode = 10)
# my.control <- trainControl(method = 'cv', number = 10, classProbs = T)
# set.seed(1234)
# bt.fit <- train(fraud ~ e.1meanamount_zip3 + e.7medianamount_zip3 + e.1medianamount_zip3 + e.7meanamount_cardnum +
#                     e.7avgdailyfreq_cardnum + e.7meanamount_merchnum + e.7avgdailyfreq_merchnum + e.1countzip3_cardnum +
#                     e.curdaybal_dow_cardnum + e.curdaybal_dow_merchnum,
#                 data = train_dat,
#                 distribution = "gaussian",
#                 method = "gbm",
#                 trControl = my.control,
#                 # tuneGrid = my.grid,
#                 preProc = c("center", "scale"),
#                 metric = "ROC",
#                 verbose = FALSE,
#                 distribution = "adaboost")

bootControl <- trainControl( method = "boot", number = 10)
gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2, .n.trees = (1:5)*100, .shrinkage = .1, .n.minobsinnode=10)
set.seed(1234)
bt.fit <- train(train_dat[,4:13], train_dat[,3], method = "gbm", trControl = bootControl,
                verbose = FALSE, bag.fraction = 0.5, tuneGrid = gbmGrid)



# Save model
save(bt.fit, file = "models/bt_model.rda")

# Review results
summary(bt.fit)
plot(bt.fit)
