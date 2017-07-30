suppressPackageStartupMessages(library(memoise))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))

if(!exists("cdt")){
  card_payments <- readRDS("data/cleaned_card_payments.RDS")
  cdt <- data.table(card_payments)
}

getHistory <- function(dtable, rowdate, variable, entity, days) {
  ## Creates new column that is the mean of "variable" grouped by "entity" using a windows time "days"
  dtable[date >= max(rowdate - days, min(date)) & date <= (rowdate), .SD, .SDcols = c("date", variable, entity)]
}

mgetHistory <- memoise(getHistory)


#-----------------

start.time <- Sys.time()
card_payments$e.mean7amountdf_zip <- as.numeric(NA)
for(row in 1:nrow(card_payments)){
  card_payments[row,]$e.mean7amountdf_zip <- mgetHistory(dtable = cdt, rowdate =  card_payments[row,]$date, variable =  "amount", entity =  "merch_zip", days = 3)[merch_zip == card_payments[row,]$merch_zip, mean(amount), by = "merch_zip"][,V1]
}
print(Sys.time()-start.time)
