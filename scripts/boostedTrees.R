suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gbm))

# Import train set
train_dat <- readRDS("data/train.RDS")

# Set seed
set.seed(1234)

# Adjust data type for fraud variable
weights <- ifelse(train_dat$fraud, 10, 1)
train_dat$fraud <- as.factor(train_dat$fraud)

# Train the model on the training set
bootControl <- trainControl(method = "boot", number = 10)
gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2, .n.trees = (1:5)*100, .shrinkage = .1, .n.minobsinnode=10)
set.seed(1234)
bt.fit <- train(train_dat[,4:13], train_dat[,3], method = "gbm", distribution = "bernoulli",  trControl = bootControl,
                verbose = FALSE, bag.fraction = 0.5, tuneGrid = gbmGrid, w = weights)

train_dat$fraud <- as.character(train_dat$fraud)
train_dat$fraud <- ifelse(train_dat$fraud=="TRUE",1,0)
mod.bt <- gbm(formula = fraud ~ .,
              distribution = "bernoulli",
              data = train_dat[,3:13],
              n.trees = bt.fit$bestTune$n.trees,
              shrinkage = bt.fit$bestTune$shrinkage,
              n.minobsinnode = bt.fit$bestTune$n.minobsinnode,
              interaction.depth = bt.fit$bestTune$interaction.depth,
              cv.folds = 10,
              n.cores = 2,
              weights = weights)
# Check predictions
train_dat$pred_bt <- predict(mod.bt, train_dat[,4:13], type="response")

# Save model
save(bt.fit, file = "models/bt_model.rda")
save(mod.bt, file = "models/bt_model_rev.rda")

# Review results
summary(bt.fit)
plot(bt.fit)
