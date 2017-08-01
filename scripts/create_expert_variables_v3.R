suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(data.table))

# Upload individual expert variable data sets
card_payments1 <- readRDS("data/expert_card_payments_v2-BRYCE.RDS") # 1:50000
card_payments2 <- readRDS("data/expert_card_payments_v2-CHRIS.RDS") # 50001:75000
card_payments3 <- readRDS("data/expert_card_payments_v2-KAM.RDS") # 75001:85000
card_payments4 <- readRDS("data/expert_card_payments_v2-JOSH.RDS") # 85001:95483

# Filter by index and columns
card_payments1 <- card_payments1 %>% select(record_number, dow,
                                            e.7countzip3_cardnum, e.3countzip3_cardnum, e.1countzip3_cardnum,
                                            e.curdaybal_dow_cardnum, e.curdaybal_dow_merchnum,
                                            e.curdaytran_dow_cardnum, e.curdaytran_dow_merchnum) %>% .[1:50000,]
card_payments2 <- card_payments2 %>% select(record_number, dow,
                                            e.7countzip3_cardnum, e.3countzip3_cardnum, e.1countzip3_cardnum,
                                            e.curdaybal_dow_cardnum, e.curdaybal_dow_merchnum,
                                            e.curdaytran_dow_cardnum, e.curdaytran_dow_merchnum) %>% .[50001:75000,]
card_payments3 <- card_payments3 %>% select(record_number, dow,
                                            e.7countzip3_cardnum, e.3countzip3_cardnum, e.1countzip3_cardnum,
                                            e.curdaybal_dow_cardnum, e.curdaybal_dow_merchnum,
                                            e.curdaytran_dow_cardnum, e.curdaytran_dow_merchnum) %>% .[75001:85000,]
card_payments4 <- card_payments4 %>% select(record_number, dow,
                                           e.7countzip3_cardnum, e.3countzip3_cardnum, e.1countzip3_cardnum,
                                           e.curdaybal_dow_cardnum, e.curdaybal_dow_merchnum,
                                           e.curdaytran_dow_cardnum, e.curdaytran_dow_merchnum) %>% .[85001:95483,]

# Bind rows for card_payment files
card_payment <- rbind(card_payments1, card_payments2, card_payments3, card_payments4)
rm("card_payments1", "card_payments2", "card_payments3", "card_payments4")

# Check binding
nrow(card_payment) == 95483
length(unique(card_payment$record_number)) == 95483

# Load original data set with first round of expert variables
cdt <- readRDS("data/expert_card_payments.RDS")

# Merge
cdt <- cdt[card_payment, on="record_number"]
rm("card_payment")

# Check merging
nrow(cdt) == 95483
length(unique(cdt$record_number)) == 95483
sum(is.na(cdt$e.7countzip3_cardnum)) == 0
sum(is.na(cdt$e.3countzip3_cardnum)) == 0
sum(is.na(cdt$e.1countzip3_cardnum)) == 0
sum(is.na(cdt$e.curdaybal_dow_cardnum)) == 0
sum(is.na(cdt$e.curdaybal_dow_merchnum)) == 0
sum(is.na(cdt$e.curdaytran_dow_cardnum)) == 0
sum(is.na(cdt$e.curdaytran_dow_merchnum)) == 0

# Save revised data set
saveRDS(cdt, "data/expert_card_payments_v2.RDS")