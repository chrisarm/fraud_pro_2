suppressPackageStartupMessages(library(memoise))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))

if(!exists("cdt")){
  card_payments <- readRDS("data/expert_card_payments.RDS")
  card_payments$dow <- weekdays(card_payments$date) # Include "day of week" field
  cdt <- data.table(card_payments)
}

# Split
cdt <- cdt[1:50000, .SD, with = TRUE]
card_payments <- data.frame(cdt)

getHistory <- function(dtable, rowdate, record, days) {
  # Returns all rows in the time window, compares rowdate - "days" &  min(date) to fix date problems for the beginning rows in the table
  dtable[record_number <= record & date >= max(rowdate - days, min(date)) & date <= (rowdate), .SD]
}

getHistory_dowMerchnum <- function(dtable, weekday, merch, date_inp) {
  dtable[dow == weekday & merchnum == merch & date < date_inp, .SD]
}

getHistory_dowCardnum <- function(dtable, weekday, card, date_inp) {
  dtable[dow == weekday & cardnum == card & date < date_inp, .SD]
}

getHistory_dailyMerchnum <- function(dtable, record, date_inp, merch) {
  dtable[record_number <= record & date == date_inp & merchnum == merch, .SD]
}

getHistory_dailyCardnum <- function(dtable, record, date_inp, card) {
  dtable[record_number <= record & date == date_inp & cardnum == card, .SD]
}

#Try to speed things up using memoise
mgetHistory <- memoise(getHistory)

##################
#-----------------

start.time <- Sys.time() #Timing how long this takes...

## Set up the main table to make this go faster ##
# Create each column ahead of time

# cardnum
cdt$e.7countzip3_cardnum <- as.numeric(NA)
cdt$e.3countzip3_cardnum <- as.numeric(NA)
cdt$e.1countzip3_cardnum <- as.numeric(NA)

# dow ("day of week")
cdt$e.curdaybal_dow_cardnum <- as.numeric(NA)
cdt$e.curdaybal_dow_merchnum <- as.numeric(NA)
cdt$e.curdaytran_dow_cardnum <- as.numeric(NA)
cdt$e.curdaytran_dow_merchnum <- as.numeric(NA)

setkey(cdt, date)

########################################
# Fill in expert variables for each row:
for(row in nrow(cdt):1){

  # Create reference variables to time based limits
  refdate <-  cdt[row, date, with = TRUE]
  refrecord <- cdt[row, record_number, with = TRUE] #Used to limit the records used in calculations for the current transaction's date
  refzip3 <- cdt[row, merch_zip3, with = TRUE]
  ref_merch <- cdt[row, merchnum, with = TRUE]
  refcardnum <- cdt[row, cardnum, with = TRUE]

  # Get the 7-day History including current day's transactions up to this record#
  day7History <- mgetHistory(dtable = cdt, rowdate =  refdate, record = refrecord, days = 7)#Used function to make this go a bit faster by implementing memoise
  day3History <- day7History[date <= refdate & date >= refdate - 3,] #Subset of day7History is day3History
  day1History <- day3History[date <= refdate & date >= refdate - 1,] #Subset of day3History is day1History

  ## Set up the history tables for this row
  setkey(day7History, cardnum)
  setkey(day3History, cardnum)
  setkey(day1History, cardnum)
  day7History_cardnum <- day7History[cardnum == refcardnum,]
  day3History_cardnum <- day3History[cardnum == refcardnum,]
  day1History_cardnum <- day1History[cardnum == refcardnum,]

  # Day of week
  refdow <- cdt[row, dow, with = TRUE]
  dowHistory_merch <- getHistory_dowMerchnum(dtable = cdt, weekday = refdow, merch = ref_merch, date = refdate)
  dowHistory_card <- getHistory_dowCardnum(dtable = cdt, weekday = refdow, card = refcardnum, date = refdate)

  # Current balances (amount and # of transactions)
  dailyHistory_merch <- getHistory_dailyMerchnum(dtable = cdt, record = refrecord, date_inp = refdate, merch = ref_merch)
  dailyHistory_card <- getHistory_dailyCardnum(dtable = cdt, record = refrecord, date_inp = refdate, card = refcardnum)

  # Get dow averages

  # ref_dow_card_amt <- ifelse(nrow(dowHistory_card)==0, 0, (dowHistory_card %>% select(date, amount) %>% group_by(date) %>%
  #                              summarise("tot" = sum(amount)) %>% .[["tot"]] %>% mean()))
  ref_dow_card_amt <- ifelse(nrow(dowHistory_card)==0, 0, dowHistory_card[, .("tot_amount" = sum(amount)), by = "date"][, mean(tot_amount)])

  # ref_dow_merch_amt <- ifelse(nrow(dowHistory_merch)==0, 0, (dowHistory_merch %>% select(date) %>% group_by(date) %>%
  #                               summarise("tot" = sum(amount)) %>% .[["tot"]] %>% mean()))
  ref_dow_merch_amt <- ifelse(nrow(dowHistory_merch)==0, 0, dowHistory_merch[, .("tot_amount" = sum(amount)), by = "date"][, mean(tot_amount)])

  # ref_dow_card_tran <- ifelse(nrow(dowHistory_card)==0, 0, (dowHistory_card %>% select(date) %>% group_by(date) %>%
  #                               summarise("freq" = n()) %>% .[["freq"]] %>% mean()))
  ref_dow_card_tran <- ifelse(nrow(dowHistory_card)==0, 0, dowHistory_card[, .("freq" = .N), by = "date"][, mean(freq)])

  # ref_dow_merch_tran <- ifelse(nrow(dowHistory_merch)==0, 0, (dowHistory_merch %>% select(date) %>% group_by(date) %>%
  #                                summarise("freq" = n()) %>% .[["freq"]] %>% mean()))
  ref_dow_merch_tran <- ifelse(nrow(dowHistory_merch)==0, 0, dowHistory_merch[, .("freq" = .N), by = "date"][, mean(freq)])

  #################################################
  ## Calculate the expert variables for this row ##

  # Number of unique zips by cardnumber
  cdt[row, `:=` (e.7countzip3_cardnum = day7History_cardnum[, c(num_zip = uniqueN(merch_zip3))],
    e.3countzip3_cardnum = day3History_cardnum[, c(num_zip = uniqueN(merch_zip3))],
    e.1countzip3_cardnum = day1History_cardnum[, c(num_zip = uniqueN(merch_zip3))],

    # "Day of week" metrics
    e.curdaybal_dow_cardnum = dailyHistory_card[, sum(amount)]/ref_dow_card_amt,
    e.curdaybal_dow_merchnum = dailyHistory_merch[, sum(amount)]/ref_dow_merch_amt,
    e.curdaytran_dow_cardnum = dailyHistory_card[, .N]/ref_dow_card_tran,
    e.curdaytran_dow_merchnum = dailyHistory_merch[, .N]/ref_dow_merch_tran), with = TRUE]

  if(row %% 415 == 0){
    print(paste("Reached row", row))
    print(Sys.time()-start.time)
    forget(mgetHistory)
  }
}

print(Sys.time()-start.time)
saveRDS(cdt, "data/expert_card_payments_v2-BRYCE.RDS")