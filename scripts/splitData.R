library(caret)
library(tidyverse)

seed <- 1234

# read in data
dat <- readRDS("data/.RDS")

# sort by date field
dat <- dat %>% arrange(record_number, date)

# create OOT data set
dat_oot <- dat %>%
  filter(date >= "2010-10-01")
saveRDS(dat_oot, "data/oot.RDS")

# remove OOT data from dataset
dat <- dat %>%
  filter(date < "2010-10-01")

set.seed(seed)

# create partition index
trainIndex <- createDataPartition(dat$fraud, p = .66,
                                  list = FALSE,
                                  times = 1)
head(trainIndex)

# create training data
train_df <- dat[trainIndex,]
test_df <- dat[-trainIndex,]
saveRDS(train_df, "data/train.RDS")
saveRDS(test_df, "data/test.RDS")