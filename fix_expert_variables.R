### Fix expert variables after creating them
library(dplyr)
library(data.table)

expert_card_payments <- readRDS("data/expert_card_payments_v2.RDS")

# First seven days suck, need to fix frequencies for these days since I divided by a hard coded number creating these
expert_card_payments[date <= as.Date("2010-01-06"),
  `:=`(e.7avgdailyfreq_cardnum = e.7avgdailyfreq_cardnum * 7 / (as.integer(date - as.Date("2010-01-01")) + 1),
    e.7avgdailyfreq_merchnum = e.7avgdailyfreq_merchnum * 7 / (as.integer(date - as.Date("2010-01-01")) + 1),
    e.7avgdailyfreq_zip3 = e.7avgdailyfreq_zip3 * 7 / (as.integer(date - as.Date("2010-01-01")) + 1))]

expert_card_payments[date <= as.Date("2010-01-02"),
  `:=`(e.3avgdailyfreq_cardnum = e.3avgdailyfreq_cardnum * 3 / (as.integer(date - as.Date("2010-01-01")) + 1),
    e.3avgdailyfreq_merchnum = e.3avgdailyfreq_merchnum * 3 / (as.integer(date - as.Date("2010-01-01")) + 1),
    e.3avgdailyfreq_zip3 = e.3avgdailyfreq_zip3 * 3 / (as.integer(date - as.Date("2010-01-01")) + 1))]

expert_card_payments[is.infinite(e.curdaybal_dow_cardnum), e.curdaybal_dow_cardnum := 1]
expert_card_payments[is.infinite(e.curdaybal_dow_merchnum), e.curdaybal_dow_merchnum := 1]
expert_card_payments[is.infinite(e.curdaytran_dow_cardnum), e.curdaytran_dow_cardnum := 1]
expert_card_payments[is.infinite(e.curdaytran_dow_merchnum), e.curdaytran_dow_merchnum := 1]

expert_card_payments <- saveRDS(data.frame(expert_card_payments), "data/expert_card_payments_v3.RDS")
