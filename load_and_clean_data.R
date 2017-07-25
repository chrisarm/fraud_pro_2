##########################
# Script Description:
# 1) Loads the original card payment data
# 2) Fills in missing values as much as possible
# 3) Saves data.frame with cleaned dataset

suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(uuid))


###################
# Read in the data
card_payments <- read_xlsx("data/original_card_payments.xlsx", sheet = "Sheet1", col_names = T)
cdt <- data.table(card_payments)
rm("card_payments")


###########################
# Standardize column names
colnames(cdt) <- sapply(colnames(cdt), gsub, pattern = " ", replacement = "_")
cdt$record_number <- as.factor(cdt$record_number)
cdt$cardnum <- as.factor(cdt$cardnum)
cdt$date <- as.Date(cdt$date)
cdt$merchnum <- as.character(cdt$merchnum)
cdt$merch_description <- as.character(cdt$merch_description)
cdt$merch_state <- as.factor(cdt$merch_state)
cdt$merch_zip <- as.character(cdt$merch_zip)
cdt$transtype <- as.factor(cdt$transtype)
cdt$amount <- as.numeric(cdt$amount)
cdt$fraud <- as.logical(cdt$fraud)
setkey(cdt, record_number)


#################################
# Missing Value Transformations

## Merch Description Transformations
cdt[,merch_description := toupper(merch_description)] # Uppercase everything
cdt[,merch_description := gsub(pattern = "[^[:alnum:] ]", replacement = "", x = merch_description, perl = T)] # Remove punctuation

### Fix FEDEX AB# names
cdt[grepl('FEDEX.+SHP.+AB#',x = merch_description, perl = T), merch_description := "FEDEX SHP AB#"]
cdt[grepl('FEDEX.+AB#',x = merch_description,perl = T)]

## "ALASKA NATIVE" - WTF? ###

################################
## Merchnum tranformations
cdt[merchnum == 0, merchnum:= NA]
initial_NA <- sum(is.na(cdt[,merchnum]))

### Use matching table to fill in missing values
match_up_dt <- cdt[,.(record_number, merchnum, merch_description, merch_zip)]

### Use most common merchnum based on merch_description match to fill in missing
merchnum_na <- match_up_dt[,.(.N),c("merch_description","merchnum")][is.na(merchnum)]
merchnum_fill <- match_up_dt[,.(freq = .N),c("merch_description","merchnum")][!is.na(merchnum)][,.(freq_max = max(freq)),c("merch_description")]

### Match most common count to original counts
merchnum_fill <- match_up_dt[,.(freq = .N),c("merch_description","merchnum")][!is.na(merchnum)][merchnum_fill, nomatch = 0L, on = "merch_description"][freq == freq_max,]
max_merchnum <- merchnum_fill[merchnum_na, nomatch=0L, on = "merch_description"][,.(merch_description,merchnum)][,.N,"merch_description"][N == 1][merchnum_fill, nomatch = 0L, on = "merch_description"][,.(merchnum,merch_description)]

### Fill in missing rows of merchnum with the most common valid ones by merch_description
cdt <- max_merchnum[cdt, on = "merch_description"] #left_join
cdt[, merchnum := ifelse(is.na(merchnum), i.merchnum, merchnum)] #fill in missing
cdt[, i.merchnum := NULL] #remove unnecessary row

### Cleanup merchnum transformation stuff
print(paste("Filled in", initial_NA - sum(is.na(cdt$merchnum)), " NAs for merchnum"))
rm("match_up_dt", "max_merchnum", "merchnum_fill","merchnum_na", "initial_NA")

### Fill in the rest with UUIDs
cdt[,uuid := UUIDgenerate(), merch_description]
cdt[is.na(merchnum), merchnum := uuid]
cdt[,uuid:=NULL]

##############################
## Merchzip Transformations
initial_NA <- sum(is.na(cdt[,merch_zip]))

### Use matching table to fill in missing values
match_up_dt <- cdt[,.(record_number, merch_description, merch_zip)]

### Use most common merch_zip based on merch_description match to fill in missing
merch_zip_na <- match_up_dt[,.(.N),c("merch_description","merch_zip")][is.na(merch_zip)]
merch_zip_fill <- match_up_dt[,.(freq = .N),c("merch_description","merch_zip")][!is.na(merch_zip)][,.(freq_max = max(freq)),c("merch_description")]

### Match most common count to original counts
merch_zip_fill <- match_up_dt[,.(freq = .N),c("merch_description","merch_zip")][!is.na(merch_zip)][merch_zip_fill, nomatch = 0L, on = "merch_description"][freq == freq_max,]
max_merch_zip <- merch_zip_fill[merch_zip_na, nomatch=0L, on = "merch_description"][,.(merch_description,merch_zip)][,.N,"merch_description"][N == 1][merch_zip_fill, nomatch = 0L, on = "merch_description"][,.(merch_zip,merch_description)]

### Fill in missing rows of merch_zip with the most common valid ones by merch_description
cdt <- max_merch_zip[cdt, on = "merch_description"] #left_join
cdt[, merch_zip := ifelse(!is.na(i.merch_zip), i.merch_zip, merch_zip)] #fill in missing
cdt[, i.merch_zip := NULL] #remove unnecessary row

### Cleanup merch_zip transformation stuff
print(paste("Filled in", initial_NA - sum(is.na(cdt$merch_zip)), " NAs for merch_zip"))
rm("match_up_dt", "max_merch_zip", "merch_zip_fill","merch_zip_na", "initial_NA")

### Fill in the rest with "Other"
cdt[,merch_zip := as.factor(merch_zip)]
cdt[is.na(merch_zip), merch_zip := "Other"]


#########################
# Merchstate Transforamtions

# TBD #

saveRDS(data.frame(cdt), "data/cleaned_card_payments.RDS", ascii = T)
