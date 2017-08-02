suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gbm))
suppressPackageStartupMessages(library(neuralnet))

# Import train set
train_dat <- readRDS("data/train.RDS")
test_dat <- readRDS("data/test.RDS")

# Import models
load("models/logit_model.rda")
# load("models/nnet_model.rda")
# load("models/bt_model.rda")
load("models/nnet_model_rev.rda")
load("models/bt_model_rev.rda")
mod.logit <- logit.model
# mod.bt <- bt.fit
# mod.nnet <- nnet.fit
# rm("bt.fit", "logit.model", "nnet.fit")
rm("logit.model")

# Adjust data type for fraud variable
train_dat$fraud <- as.numeric(train_dat$fraud)
test_dat$fraud <- as.numeric(test_dat$fraud)

# Merge train/test data sets
train_dat$train <- 1
test_dat$train <- 0
dat <- rbind(train_dat, test_dat)

# Create model from mod.bt
# mod.bt <- gbm(formula = fraud ~ .,
#               distribution = "gaussian",
#               data = train_dat[,3:13],
#               n.trees = mod.bt$bestTune$n.trees,
#               shrinkage = mod.bt$bestTune$shrinkage,
#               n.minobsinnode = mod.bt$bestTune$n.minobsinnode,
#               interaction.depth = mod.bt$bestTune$interaction.depth,
#               cv.folds = 10,
#               n.cores = 2)
# save(mod.bt, file = "models/bt_model_rev.rda") # Save revised boosted tree model

# Create model from mod.bt
# mod.nnet <- nnet(formula = fraud ~ .,
#                  data = train_dat[,3:13],
#                  size = mod.nnet$bestTune$size,
#                  decay = mod.nnet$bestTune$decay)
# save(mod.nnet, file = "models/nnet_model_rev.rda") # Save revised neural net model

# Create predictions
dat$pred_logit <- predict(mod.logit, dat[,4:13], type="response")
dat$pred_bt <- predict(mod.bt, dat[,4:13], type="response")
dat$pred_nn <- predict(mod.nnet, dat[,4:13]) %>% as.numeric()
dat_temp <- dat %>% select(fraud, pred_logit, pred_bt, pred_nn)

# Bin each prediction
dat$bin_bt <- ntile(dat$pred_bt,100)
dat$bin_nn <- ntile(dat$pred_nn,100)

# Combined predictions w/ equal weighting
# dat$bin_sum <- (dat$bin_bt+dat$bin_nn) %>% ntile(100)
dat$bin_max <- ifelse(dat$bin_bt > dat$bin_nn, dat$bin_bt, dat$bin_nn) %>% ntile(100)
# sum(dat$fraud[dat$bin_max>=97])/sum(dat$fraud)
# sum(dat$fraud[dat$bin_sum>=97])/sum(dat$fraud)

# Save data with predictions
saveRDS(dat,"data/pred_data.rds")
