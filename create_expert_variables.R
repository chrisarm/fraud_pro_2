suppressPackageStartupMessages(library(memoise))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))

if(!exists("cdt")){
  card_payments <- readRDS("data/cleaned_card_payments.RDS")
  cdt <- data.table(card_payments)
}

# # Test process on sample of the data
# cdt <- cdt[1:1000, .SD, with = TRUE]
# card_payments <- data.frame(cdt)

getHistory <- function(dtable, rowdate, record, days) {
  # Returns all rows in the time window, compares rowdate - "days" &  min(date) to fix date problems for the beginning rows in the table
  dtable[record_number <= record & date >= max(rowdate - days, min(date)) & date <= (rowdate), .SD]
}
#Try to speed things up using memoise
mgetHistory <- memoise(getHistory)

##################
#-----------------

start.time <- Sys.time() #Timing how long this takes...

## Set up the main table to make this go faster ##
# Create each column ahead of time

# zip3
card_payments$e.7meanamount_zip3 <- as.numeric(NA)
card_payments$e.3meanamount_zip3 <- as.numeric(NA)
card_payments$e.1meanamount_zip3 <- as.numeric(NA)
card_payments$e.7medianamount_zip3 <- as.numeric(NA)
card_payments$e.3medianamount_zip3 <- as.numeric(NA)
card_payments$e.1medianamount_zip3 <- as.numeric(NA)
card_payments$e.7avgdailyfreq_zip3 <- as.numeric(NA)
card_payments$e.3avgdailyfreq_zip3 <- as.numeric(NA)
card_payments$e.1avgdailyfreq_zip3 <- as.numeric(NA)

# cardnum
card_payments$e.7meanamount_cardnum <- as.numeric(NA)
card_payments$e.3meanamount_cardnum <- as.numeric(NA)
card_payments$e.1meanamount_cardnum <- as.numeric(NA)
card_payments$e.7medianamount_cardnum <- as.numeric(NA)
card_payments$e.3medianamount_cardnum <- as.numeric(NA)
card_payments$e.1medianamount_cardnum <- as.numeric(NA)
card_payments$e.7avgdailyfreq_cardnum <- as.numeric(NA)
card_payments$e.3avgdailyfreq_cardnum <- as.numeric(NA)
card_payments$e.1avgdailyfreq_cardnum <- as.numeric(NA)

# merchnum
card_payments$e.7meanamount_merchnum <- as.numeric(NA)
card_payments$e.3meanamount_merchnum <- as.numeric(NA)
card_payments$e.1meanamount_merchnum <- as.numeric(NA)
card_payments$e.7medianamount_merchnum <- as.numeric(NA)
card_payments$e.3medianamount_merchnum <- as.numeric(NA)
card_payments$e.1medianamount_merchnum <- as.numeric(NA)
card_payments$e.7avgdailyfreq_merchnum <- as.numeric(NA)
card_payments$e.3avgdailyfreq_merchnum <- as.numeric(NA)
card_payments$e.1avgdailyfreq_merchnum <- as.numeric(NA)

########################################
# Fill in expert variables for each row:
for(row in 1:nrow(card_payments)){

  # Create reference variables to time based limits
  refdate <-  card_payments[row,]$date
  refrecord <- card_payments[row,]$record_number #Used to limit the records used in calculations for the current transaction's date

  ## Set up the history tables for this row
  # Get the 7-day History including current day's transactions up to this record#
  day7History <- mgetHistory(dtable = cdt, rowdate =  refdate, record = refrecord, days = 7)#Used function to make this go a bit faster by implementing memoise
  day3History <- day7History[date <= refdate & date >= refdate - 3,] #Subset of day7History is day3History
  day1History <- day3History[date <= refdate & date >= refdate - 1,] #Subset of day3History is day1History

  #################################################
  ## Calculate the expert variables for this row ##

  # Expert variables by cardnum
  refcardnum <- card_payments[row,]$cardnum
  card_payments[row,]$e.7meanamount_cardnum <- day7History[cardnum == refcardnum, mean(amount)]
  card_payments[row,]$e.3meanamount_cardnum <- day3History[cardnum == refcardnum, mean(amount)]
  card_payments[row,]$e.1meanamount_cardnum <- day1History[cardnum == refcardnum, mean(amount)]
  card_payments[row,]$e.7medianamount_cardnum <- day7History[cardnum == refcardnum, median(amount)]
  card_payments[row,]$e.3medianamount_cardnum <- day3History[cardnum == refcardnum, median(amount)]
  card_payments[row,]$e.1medianamount_cardnum <- day1History[cardnum == refcardnum, median(amount)]
  card_payments[row,]$e.7avgdailyfreq_cardnum <- day7History[cardnum == refcardnum, .N/7]
  card_payments[row,]$e.3avgdailyfreq_cardnum <- day3History[cardnum == refcardnum, .N/3]
  card_payments[row,]$e.1avgdailyfreq_cardnum <- day1History[cardnum == refcardnum, .N]

  # Expert variables by merch_zip3
  refzip3 <- card_payments[row,]$merch_zip3
  card_payments[row,]$e.7meanamount_zip3 <- day7History[merch_zip3 == refzip3, mean(amount)]
  card_payments[row,]$e.3meanamount_zip3 <- day7History[merch_zip3 == refzip3, mean(amount)]
  card_payments[row,]$e.1meanamount_zip3 <- day7History[merch_zip3 == refzip3, mean(amount)]
  card_payments[row,]$e.7medianamount_zip3 <- day7History[merch_zip3 == refzip3, median(amount)]
  card_payments[row,]$e.3medianamount_zip3 <- day7History[merch_zip3 == refzip3, median(amount)]
  card_payments[row,]$e.1medianamount_zip3 <- day7History[merch_zip3 == refzip3, median(amount)]
  card_payments[row,]$e.7avgdailyfreq_zip3 <- day7History[merch_zip3 == refzip3, .N/7]
  card_payments[row,]$e.3avgdailyfreq_zip3 <- day3History[merch_zip3 == refzip3, .N/3]
  card_payments[row,]$e.1avgdailyfreq_zip3 <- day1History[merch_zip3 == refzip3, .N]

  # Expert variables by merchnum
  ref_merch <- card_payments[row,]$merchnum
  card_payments[row,]$e.7meanamount_merchnum <- day7History[merchnum == ref_merch, mean(amount)]
  card_payments[row,]$e.3meanamount_merchnum <- day7History[merchnum == ref_merch, mean(amount)]
  card_payments[row,]$e.1meanamount_merchnum <- day7History[merchnum == ref_merch, mean(amount)]
  card_payments[row,]$e.7medianamount_merchnum <- day7History[merchnum == ref_merch, median(amount)]
  card_payments[row,]$e.3medianamount_merchnum <- day7History[merchnum == ref_merch, median(amount)]
  card_payments[row,]$e.1medianamount_merchnum <- day7History[merchnum == ref_merch, median(amount)]
  card_payments[row,]$e.7avgdailyfreq_merchnum <- day7History[merchnum == ref_merch, .N/7]
  card_payments[row,]$e.3avgdailyfreq_merchnum <- day3History[merchnum == ref_merch, .N/3]
  card_payments[row,]$e.1avgdailyfreq_merchnum <- day1History[merchnum == ref_merch, .N]

  if(row %% 3000 == 0){
    cat(paste("Reached row", row, ". Sleeping for ~1 second to let the system ... breathe?"))
    Sys.sleep(0.9)
  }
}

print(Sys.time()-start.time)
saveRDS(card_payments, "data/expert_card_payments.RDS")