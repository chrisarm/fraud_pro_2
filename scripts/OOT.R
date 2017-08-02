suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))

# Import train set
dat <- readRDS("data/pred_data.rds")
oot_pred <- readRDS("data/OOT_pred.RDS")

test_pred <- dat %>% filter(train==0)
train_pred <- dat %>% filter(train == 1)

# Examine results over bins for train data
train_pred <- data.table(train_pred)
train_pred$fraud <- ifelse(train_pred$fraud=="1", "Fraud","Valid") %>% factor(levels=c("Valid","Fraud"))
train_pred[, .(fraud_n = sum(fraud == "Fraud"),
  good_n = sum(fraud == "Valid"),
  total = .N,
  percent = 100 - bin_max), by = "bin_max"][order(-bin_max), .(percent, good_n, fraud_n, total, FDR = fraud_n/sum(fraud_n))][1:10,,with = TRUE]

# Examine results over bins for test data
test_pred <- data.table(test_pred)
test_pred$fraud <- ifelse(test_pred$fraud=="1", "Fraud","Valid") %>% factor(levels=c("Valid","Fraud"))
test_pred[, .(fraud_n = sum(fraud == "Fraud"),
  good_n = sum(fraud == "Valid"),
  total = .N,
  percent = 101 - bin_max), by = "bin_max"][order(-bin_max), .(percent, good_n, fraud_n, total, FDR = fraud_n/sum(fraud_n))][1:10,,with = TRUE]

# Examine results over bins for OOT data
oot_pred <- data.table(oot_pred)
oot_pred$fraud <- ifelse(oot_pred$fraud=="Fraud", "Fraud","Valid") %>% factor(levels=c("Valid","Fraud"))
oot_pred[, .(fraud_n = sum(fraud == "Fraud"),
  good_n = sum(fraud == "Valid"),
  total = .N,
  percent = 101 - bin_max), by = "bin_max"][order(-bin_max), .(percent, good_n, fraud_n, total, FDR = fraud_n/sum(fraud_n))][1:10,,with = TRUE]
