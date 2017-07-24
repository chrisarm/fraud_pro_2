suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(data.table))

###################
# Read in the data

card_payments <- read_xlsx("data/original_card_payments.xlsx", sheet = "Sheet1", col_names = T)

###########################
# Standardize column names
colnames(card_payments) <- sapply(colnames(card_payments), gsub, pattern = " ", replacement = "_")
card_payments$record_number <- as.factor(card_payments$record_number)
card_payments$cardnum <- as.factor(card_payments$cardnum)
card_payments$date <- as.Date(card_payments$date)
card_payments$merchnum <- as.factor(card_payments$merchnum)
card_payments$merch_description <- as.character(card_payments$merch_description)
card_payments$merch_state <- as.factor(card_payments$merch_state)
card_payments$merch_zip <- as.factor(card_payments$merch_zip)
card_payments$transtype <- as.factor(card_payments$transtype)
card_payments$amount <- as.numeric(card_payments$amount)
card_payments$fraud <- as.logical(card_payments$fraud)

############################
# Find columns with NAs
cdt <- data.table(card_payments)
setkey(cdt, record_number)
total_variables <- ncol(card_payments)
match_up_dt <- data.table(record_number = card_payments$record_number)

for (variable in seq_len(total_variables)){
    if(sum(is.na(card_payments[,(variable)][[1]])) > 0){
    match_up_dt <- cbind(missing = cdt[,variable, with = F], match_up_dt)
  }
}
match_up_dt <- cbind(card_payments, match_up_dt)

############################
# Manual work to fix NAs
initial_NA <- sum(is.na(cdt[,merchnum]))
merchnum_na <- match_up_dt[,.(.N),c("merch_description","merchnum")][is.na(merchnum)]
merchnum_fill <- match_up_dt[,.(.N),c("merch_description","merchnum")][!is.na(merchnum)][N == 1,.SD,c("merch_description")]
single_merchnum <- merchnum_fill[merchnum_na, nomatch=0L, on = "merch_description"][,.(merch_description,merchnum)][,.N,"merch_description"][N == 1][merchnum_fill, nomatch = 0L, on = "merch_description"]
### NEED CODE to fill in missing rows of merchnum with the single valid ones in single_merchnum
#cdt[,merchnum_filled := ifelse(is.na(merchnum), ifelse(single_merchnum[row_number = ]))]
saveRDS(data.frame(cdt), "data/cleaned_card_payments.RDS")
