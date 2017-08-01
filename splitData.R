# read in data
#dat_df <- readRDS("data/expert_card_payments.RDS")

# sort by date field
dat_df <- dat_df %>% arrange(record_number, date)

# create OOT data set
dat_oot <- dat_df %>%
  filter(date >= "2010-10-01")
saveRDS(dat_oot, "data/oot_df.RDS")

# split data intro training and testing
set.seed(1234)
dat_df <- dat_df %>%
  filter(date < "2010-10-01") %>% # MODIFY FILTER STATEMENT
  mutate(random_splits = runif(nrow(.)))

# create training data
train_df <- dat_df[random_splits > 0.33,]
dim(train_df)
saveRDS(train_df, "data/train_df.RDS")

# create test data
validate_df <- dat_df[random_splits <= 0.33,]
dim(validate_df)
saveRDS(validate_df, "data/validate_df.RDS")