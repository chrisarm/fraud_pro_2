# We don't need the ID field
dat_df <- dat_df[,-1]
summary(dat_df)

## 70-30 split for training and testing, excluding OOT
set.seed(1234)
random_splits <- runif(nrow(dat_df)) # CHANGE THIS TO RIGHT DATA SET
train_df <- dat_df[random_splits > 0.3,]
dim(train_df)

validate_df <- dat_df[random_splits <= 0.3,]
dim(validate_df)

saveRDS(train_df, "data/train_df.RDS")
saveRDS(validate_df, "data/validate_df.RDS")