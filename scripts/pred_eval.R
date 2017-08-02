suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(pROC))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(reshape2))

# Import train set
dat <- readRDS("data/pred_data.RDS")
dat <- dat %>% filter(train==0)

# Building ROC curves
# roc_logit <- roc(dat$fraud, dat$pred_logit, levels=c(1,0))
# roc_bt <- roc(dat$fraud, dat$pred_bt, levels=c(1,0))
# roc_nn <- roc(dat$fraud, dat$pred_nn, levels=c(1,0))

# Plot ROC curves
# roc_logit.plot <- ggroc(roc_logit)
# roc_nn.plot <- ggroc(roc_nn)
# roc_bt.plot <- ggroc(roc_bt)

# AUC calculations
# auc_logit <- auc(roc_logit)
# auc_nn <- auc(roc_nn)
# auc_bt <- auc(roc_bt)

# Modify ROC curves
# roc_logit.plot <- roc_logit.plot + ggtitle("ROC Curve: Logit", subtitle = paste0("AUC: ", percent(auc_logit))) +
#   geom_abline(slope = 1, intercept = 1)
# roc_bt.plot <- roc_bt.plot + ggtitle("ROC Curve: Boosted Tree", subtitle = paste0("AUC: ", percent(auc_bt))) +
#   geom_abline(slope = 1, intercept = 1)
# roc_nn.plot <- roc_nn.plot + ggtitle("ROC Curve: Neural Network", subtitle = paste0("AUC: ", percent(auc_nn))) +
#   geom_abline(slope = 1, intercept = 1)

# Examine accuracy over several thresholds (run on combined max bin score)
threshold <- seq(0,100,by=1)
accuracy <- NA
FP <- NA
TP <- NA
accu.fn <- data.frame(threshold, accuracy, FP, TP)
dat$fraud <- ifelse(dat$fraud==1, "Fraud","Not Fraud") %>% factor(levels=c("Not Fraud","Fraud"))

for (i in 1:length(threshold)){
  dat$pred.fraud <- ifelse(dat$bin_max <= accu.fn$threshold[i], "Not Fraud", "Fraud") %>% factor(levels=c("Not Fraud","Fraud"))
  cf <- confusionMatrix(dat$pred.fraud, dat$fraud, positive = "Fraud")
  accu.fn$accuracy[i] <- (cf$table[1,1]+cf$table[2,2])/sum(cf$table)
  accu.fn$TP[i] <- cf$table[2,2]/sum(cf$table[,2])
  accu.fn$FP[i] <- cf$table[2,1]/sum(cf$table[,1])
}

p <- melt(accu.fn[,1:4], id.vars ="threshold") %>% ggplot(aes(x = threshold, y=value, color=variable)) + geom_point()

accu.fn$inters.accuTP <- abs(accu.fn$accuracy - accu.fn$TP)
target.threshold <- accu.fn[which(accu.fn$inters.accuTP == min(accu.fn$inters.accuTP)),]$threshold

p + geom_vline(xintercept=target.threshold, linetype="dotted") +
  ggtitle(paste0("Optimal Threshold for Scoring (",percent(target.threshold/100),")")) + theme(legend.title = element_blank())
