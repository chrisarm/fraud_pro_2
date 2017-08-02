suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(pROC))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(gbm))
suppressPackageStartupMessages(library(neuralnet))

# Import train set
dat <- readRDS("data/OOT.RDS")

# Import models
load("models/nnet_model_rev.rda")
load("models/bt_model_rev.rda")

# Predict
dat$pred.nn <- predict(mod.nnet, dat[,4:13])
dat$pred.bt <- predict(mod.bt, dat[,4:13], type="response")

# Combine scores
dat$bin_nn <- ntile(dat$pred.nn,100)
dat$bin_bt <- ntile(dat$pred.bt,100)
dat$bin_max <- ifelse(dat$bin_bt > dat$bin_nn, dat$bin_bt, dat$bin_nn) %>% ntile(100)

# Apply threshold
dat$fraud <- ifelse(dat$fraud, "Fraud", "!Fraud")
dat$fraud_pred <- ifelse(dat$bin_max >= 66, "Fraud", "!Fraud")

# Confusion matrix
ct <- confusionMatrix(dat$fraud_pred, dat$fraud, positive = "Fraud")

# Save data
saveRDS(dat, "data/OOT_pred.RDS")
write.csv(dat, "data/OOT_pred.csv")