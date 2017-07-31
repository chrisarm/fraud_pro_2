suppressPackageStartupMessages(library(memoise))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))

if(!exists("cdt")){
  card_payments <- readRDS("data/cleaned_card_payments.RDS")
  cdt <- data.table(card_payments)
}

# Test process on sample of the data
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
cdt$e.7meanamount_zip3 <- as.numeric(NA)
cdt$e.3meanamount_zip3 <- as.numeric(NA)
cdt$e.1meanamount_zip3 <- as.numeric(NA)
cdt$e.7medianamount_zip3 <- as.numeric(NA)
cdt$e.3medianamount_zip3 <- as.numeric(NA)
cdt$e.1medianamount_zip3 <- as.numeric(NA)
cdt$e.7avgdailyfreq_zip3 <- as.numeric(NA)
cdt$e.3avgdailyfreq_zip3 <- as.numeric(NA)
cdt$e.1avgdailyfreq_zip3 <- as.numeric(NA)

# cardnum
cdt$e.7meanamount_cardnum <- as.numeric(NA)
cdt$e.3meanamount_cardnum <- as.numeric(NA)
cdt$e.1meanamount_cardnum <- as.numeric(NA)
cdt$e.7medianamount_cardnum <- as.numeric(NA)
cdt$e.3medianamount_cardnum <- as.numeric(NA)
cdt$e.1medianamount_cardnum <- as.numeric(NA)
cdt$e.7avgdailyfreq_cardnum <- as.numeric(NA)
cdt$e.3avgdailyfreq_cardnum <- as.numeric(NA)
cdt$e.1avgdailyfreq_cardnum <- as.numeric(NA)

# merchnum
cdt$e.7meanamount_merchnum <- as.numeric(NA)
cdt$e.3meanamount_merchnum <- as.numeric(NA)
cdt$e.1meanamount_merchnum <- as.numeric(NA)
cdt$e.7medianamount_merchnum <- as.numeric(NA)
cdt$e.3medianamount_merchnum <- as.numeric(NA)
cdt$e.1medianamount_merchnum <- as.numeric(NA)
cdt$e.7avgdailyfreq_merchnum <- as.numeric(NA)
cdt$e.3avgdailyfreq_merchnum <- as.numeric(NA)
cdt$e.1avgdailyfreq_merchnum <- as.numeric(NA)

setkey(cdt, date)

########################################
# Fill in expert variables for each row:
for(row in 1:nrow(cdt)){

  # Create reference variables to time based limits
  refdate <-  cdt[row, date, with = TRUE]
  refrecord <- cdt[row, record_number, with = TRUE] #Used to limit the records used in calculations for the current transaction's date

  # Get the 7-day History including current day's transactions up to this record#
  day7History <- mgetHistory(dtable = cdt, rowdate =  refdate, record = refrecord, days = 7)#Used function to make this go a bit faster by implementing memoise
  day3History <- day7History[date <= refdate & date >= refdate - 3,] #Subset of day7History is day3History
  day1History <- day3History[date <= refdate & date >= refdate - 1,] #Subset of day3History is day1History

  ## Set up the history tables for this row
  setkey(day7History, merchnum)
  setkey(day3History, merchnum)
  setkey(day1History, merchnum)
  ref_merch <- cdt[row, merchnum, with = TRUE]
  day7History_merchnum <- day7History[merchnum == ref_merch,]
  day3History_merchnum <- day3History[merchnum == ref_merch,]
  day1History_merchnum <- day1History[merchnum == ref_merch,]

  refcardnum <- cdt[row, cardnum, with = TRUE]
  setkey(day7History, cardnum)
  setkey(day3History, cardnum)
  setkey(day1History, cardnum)
  day7History_cardnum <- day7History[cardnum == refcardnum,]
  day3History_cardnum <- day3History[cardnum == refcardnum,]
  day1History_cardnum <- day1History[cardnum == refcardnum,]

  refzip3 <- cdt[row, merch_zip3, with = TRUE]
  setkey(day7History, merch_zip3)
  setkey(day3History, merch_zip3)
  setkey(day1History, merch_zip3)
  day7History_zip3 <- day7History[merch_zip3 == refzip3,]
  day3History_zip3 <- day3History[merch_zip3 == refzip3,]
  day1History_zip3 <- day1History[merch_zip3 == refzip3,]


  #################################################
  ## Calculate the expert variables for this row ##

  # Expert variables by cardnum
  cdt[row, `:=` (e.7meanamount_cardnum = day7History_cardnum[, mean(amount)],
    e.3meanamount_cardnum = day3History_cardnum[, mean(amount)],
    e.1meanamount_cardnum = day1History_cardnum[, mean(amount)],
    e.7medianamount_cardnum = day7History_cardnum[, median(amount)],
    e.3medianamount_cardnum = day3History_cardnum[, median(amount)],
    e.1medianamount_cardnum = day1History_cardnum[, median(amount)],
    e.7avgdailyfreq_cardnum = day7History_cardnum[, .N/7],
    e.3avgdailyfreq_cardnum = day3History_cardnum[, .N/3],
    e.1avgdailyfreq_cardnum = day1History_cardnum[, .N],

    # Expert variables by merch_zip3
    e.7meanamount_zip3 = day7History_zip3[, mean(amount)],
    e.3meanamount_zip3 = day3History_zip3[, mean(amount)],
    e.1meanamount_zip3 = day1History_zip3[, mean(amount)],
    e.7medianamount_zip3 = day7History_zip3[, median(amount)],
    e.3medianamount_zip3 = day3History_zip3[, median(amount)],
    e.1medianamount_zip3 = day1History_zip3[, median(amount)],
    e.7avgdailyfreq_zip3 = day7History_zip3[, .N/7],
    e.3avgdailyfreq_zip3 = day3History_zip3[, .N/3],
    e.1avgdailyfreq_zip3 = day1History_zip3[, .N],

    # Expert variables by merchnum
    e.7meanamount_merchnum = day7History_merchnum[, mean(amount)],
    e.3meanamount_merchnum = day3History_merchnum[, mean(amount)],
    e.1meanamount_merchnum = day1History_merchnum[, mean(amount)],
    e.7medianamount_merchnum = day7History_merchnum[, median(amount)],
    e.3medianamount_merchnum = day3History_merchnum[, median(amount)],
    e.1medianamount_merchnum = day1History_merchnum[, median(amount)],
    e.7avgdailyfreq_merchnum = day7History_merchnum[, .N/7],
    e.3avgdailyfreq_merchnum = day3History_merchnum[, .N/3],
    e.1avgdailyfreq_merchnum = day1History_merchnum[, .N]), with = TRUE]

  if(row %% 415 == 0){
    print(paste("Reached row", row))
    print(Sys.time()-start.time)
    forget(mgetHistory)
  }
}

print(Sys.time()-start.time)
saveRDS(cdt, "data/expert_card_payments.RDS")