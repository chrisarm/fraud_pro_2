suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(tidyverse))

# Import train set
train_dat <- readRDS("data/train.RDS")

# Set seed
set.seed(1234)

# Adjust data type for fraud variable
train_dat$fraud <- as.numeric(train_dat$fraud)

# Train the model on the training set
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
