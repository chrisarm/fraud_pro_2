library(randomForest)
library(tidyverse)
library(pROC)
library(h2o)

# read in data
dat <- readRDS("data/final.RDS")
# remove OOT data
dat <- dat %>%
  filter(date < "2010-10-01")
# remove record number and date fields
dat <- dat[,-c(1,2)]
# change fraud to factor
dat$fraud <- as.factor(dat$fraud)

train_df <- readRDS("data/train.RDS")
train_df <- train_df[,-c(1:2)]
validate_df <- readRDS("data/test.RDS")
validate_df <- validate_df[,-c(1:2)]
seed <- 1234

outcome_name <- 'fraud'
feature_names <- setdiff(names(train_df), outcome_name)

set.seed(seed)
rf_model <- randomForest(x=train_df[,feature_names],
                         y=as.factor(train_df[,outcome_name]),
                         importance=TRUE, ntree=1500, mtry = sqrt(length(feature_names)))

validate_predictions <- predict(rf_model, newdata=validate_df[,feature_names], type="prob")

auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions[,2])

plot(auc_rf, print.thres = "best", main=paste('(base) AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')

# build autoencoder model
localH2O = h2o.init()
dat.hex<-as.h2o(train_df, destination_frame="train.hex")
dat.dl = h2o.deeplearning(x = feature_names, training_frame = dat.hex,
                               autoencoder = TRUE,
                               reproducible = T,
                               seed = 1234,
                               hidden = c(5), epochs = 50)

# interesting per feature error scores
# dat.anon = h2o.anomaly(dat.dl, dat.hex, per_feature=TRUE)
# head(dat.anon)

dat.anon = h2o.anomaly(dat.dl, dat.hex, per_feature=FALSE)
head(dat.anon)
err <- as.data.frame(dat.anon)

# interesting reduced features (defaults to last hidden layer)
# http://www.rdocumentation.org/packages/h2o/functions/h2o.deepfeatures
# reduced_new  <- h2o.deepfeatures(dat.dl, dat.hex)

plot(sort(err$Reconstruction.MSE))
limit <- 0.0125

# use the easy portion and model with random forest using same settings
train_df_auto <- train_df[err$Reconstruction.MSE < limit,]

set.seed(seed)
rf_model <- randomForest(x=train_df_auto[,feature_names],
                         y=as.factor(train_df_auto[,outcome_name]),
                         importance=TRUE, ntree=1500, mtry = sqrt(length(feature_names)))

validate_predictions_known <- predict(rf_model, newdata=validate_df[,feature_names], type="prob")

auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions_known[,2])

plot(auc_rf, print.thres = "best", main=paste('(easy) AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')

# use the hard portion and model with random forest using same settings
train_df_auto <- train_df[err$Reconstruction.MSE >= limit,]

set.seed(seed)
rf_model <- randomForest(x=train_df_auto[,feature_names],
                         y=as.factor(train_df_auto[,outcome_name]),
                       importance=TRUE, ntree=1500, mtry = sqrt(length(feature_names)))

validate_predictions_unknown <- predict(rf_model, newdata=validate_df[,feature_names], type="prob")
auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions_unknown[,2])

plot(auc_rf, print.thres = "best", main=paste('(hard) AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')

# bag both results set and measure final AUC score
valid_all <- (validate_predictions_known[,2] + validate_predictions_unknown[,2]) / 2
auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=valid_all)

plot(auc_rf, print.thres = "best", main=paste('(both) AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')