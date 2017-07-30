install.packages("h2o")

# point to the data set in the h2o folder - no need to load h2o in memory yet
datPath = system.file("data", "dat.csv", package = "h2o")
dat_df <- read.csv(datPath)

# We don't need the ID field
dat_df <- dat_df[,-1]
summary(dat_df)

## needremove last three months for OOT

## 70-30 split for training and testing, excluding OOT
set.seed(1234)
random_splits <- runif(nrow(dat_df))
train_df <- dat_df[random_splits > 0.3,]
dim(train_df)

validate_df <- dat_df[random_splits <= 0.3,]
dim(validate_df)

# Get benchmark score

install.packages('randomForest')
library(randomForest)

outcome_name <- 'FRAUD' # SET OUTCOME TO FRAUD VARIABLE
feature_names <- setdiff(names(dat_df), outcome_name)
set.seed(1234)
rf_model <- randomForest(x=train_df[,feature_names],
                         y=as.factor(train_df[,outcome_name]),
                         importance=TRUE, ntree=20, mtry = 3)

validate_predictions <- predict(rf_model, newdata=validate_df[,feature_names], type="prob")


install.packages('pROC')
library(pROC)
auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions[,2])

plot(auc_rf, print.thres = "best", main=paste('AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')

# build autoencoder model
library(h2o)
localH2O = h2o.init()
dat.hex<-as.h2o(train_df, destination_frame="train.hex")
dat.dl = h2o.deeplearning(x = feature_names, training_frame = dat.hex,
                               autoencoder = TRUE,
                               reproducible = T,
                               seed = 1234,
                               hidden = c(6,5,6), epochs = 50)

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

# use the easy portion and model with random forest using same settings
train_df_auto <- train_df[err$Reconstruction.MSE < 0.1,]

set.seed(1234)
rf_model <- randomForest(x=train_df_auto[,feature_names],
                         y=as.factor(train_df_auto[,outcome_name]),
                         importance=TRUE, ntree=20, mtry = 3)

validate_predictions_known <- predict(rf_model, newdata=validate_df[,feature_names], type="prob")

auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions_known[,2])

plot(auc_rf, print.thres = "best", main=paste('AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')

# use the hard portion and model with random forest using same settings
train_df_auto <- train_df[err$Reconstruction.MSE >= 0.1,]

set.seed(1234)
rf_model <- randomForest(x=train_df_auto[,feature_names],
                         y=as.factor(train_df_auto[,outcome_name]),
                         importance=TRUE, ntree=20, mtry = 3)

validate_predictions_unknown <- predict(rf_model, newdata=validate_df[,feature_names], type="prob")
auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions_unknown[,2])

plot(auc_rf, print.thres = "best", main=paste('AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')

# bag both results set and measure final AUC score
valid_all <- (validate_predictions_known[,2] + validate_predictions_unknown[,2]) / 2
auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=valid_all)

plot(auc_rf, print.thres = "best", main=paste('AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')
