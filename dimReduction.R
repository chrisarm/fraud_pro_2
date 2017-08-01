# read in data
dat_df <- readRDS("data/expert_card_payments_v3.RDS")

# reduce data set to only expert variables
dat_df <- dat_df[ ,!(names(dat_df) %in% c("merch_zip", "merch_description", "merchnum",
                                          "record_number", "cardnum", "date", "merch_state",
                                          "transtype", "amount", "merch_zip3", "dow"))]

#### UNIVARIATE KS ####
#install.packages("ks")
library(ks)

# create dataframe for ks test results
ks.results <- data.frame(variables = colnames(dat_df)[2:length(dat_df)]) %>%
  mutate(D.stat = vector(mode = "numeric", length = (length(.))))

# univariate test each expert variable --- NOT WORKING
for(i in 1:nrow(ks.results)){
  good <- filter(.data = dat_df, fraud==FALSE) %>%
    select(colnames(.)[i+1])
  bad <- filter(.data = dat_df, fraud==TRUE) %>%
    select(colnames(.)[i+1])
  tmp <- ks.test(good[,1], bad[,1])
  ks.results[i,2] <- tmp$statistic
}

# arrange variables according to D-statistic
ks.results <- ks.results %>% arrange(D.stat)

# save KS results to RDS file
saveRDS(ks.results, "data/KSresults.RDS")

# keep the top n variables based on D-stat
top <- 25
ks.results <- head(ks.results, n = top)

# reduce dat_df to variables in ks.results from previous step
dat_df <- dat_df[ ,(names(dat_df) %in% c("fraud", ks.results$variables))]

# remove extra variables in memory
rm(good, bad, i, tmp, top)

#### LOGISTIC REGRESSION -- STEPWISE ####
library(radiant)

# create logistic regression
logit <- logistic(dataset = dat_df, rvar = "fraud", lev = "TRUE",
                  evar = colnames(dat_df)[2:length(dat_df)], check = "stepwise-backward",
                  int = c("e.1countzip3_cardnum:e.1meanamount_cardnum", "e.1countzip3_cardnum:e.1medianamount_cardnum"))

# summarize results of logit
summary(logit)

# coefficient plot
plot(logit, plots = "coef", custom = TRUE) +
  labs(title = "Coefficient plot")
ggsave("plots/logit_coeff.png")
