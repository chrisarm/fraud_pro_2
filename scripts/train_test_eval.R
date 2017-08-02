# Library
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(tidyverse))

# Import train set
dat <- readRDS("data/pred_data.RDS")
train <- dat %>% filter(train==1)
test <- dat %>% filter(train==0)

# Create eval data.frame
eval.train <- data.frame("threshold" = 1:100, "nn"=NA, "bt"=NA, "max"=NA)
eval.test <- data.frame("threshold" = 1:100, "nn"=NA, "bt"=NA, "max"=NA)
for(i in 1:100){
  # Training set
  eval.train$nn[i] <- sum(train$fraud[train$bin_nn >= i])/sum(train$fraud)
  eval.train$bt[i] <- sum(train$fraud[train$bin_bt >= i])/sum(train$fraud)
  eval.train$max[i] <- sum(train$fraud[train$bin_max >= i])/sum(train$fraud)
  # Test set
  eval.test$nn[i] <- sum(test$fraud[test$bin_nn >= i])/sum(test$fraud)
  eval.test$bt[i] <- sum(test$fraud[test$bin_bt >= i])/sum(test$fraud)
  eval.test$max[i] <- sum(test$fraud[test$bin_max >= i])/sum(test$fraud)
}
eval.train <- eval.train %>% arrange(desc(threshold))
eval.test <- eval.test %>% arrange(desc(threshold))

# Save tables
write.csv(eval.train, "data/evaltab_train.csv")
write.csv(eval.test, "data/evaltab_test.csv")

# Plot tables
eval.train %>%
  gather("metric","score",2:4) %>%
  ggplot(aes(x=threshold,y=score,color=metric))+geom_line(size=1)+
  xlab("Bin Threshold")+ylab("% of Total Fraud Caught")+ggtitle("FDR Curve: Training Set")+
  scale_color_discrete(label = c("Boosted Tree","Max", "Neural Network"))+
  theme(legend.title = element_blank())+
  scale_y_continuous(breaks = seq(0,1,.1))+
  geom_abline(slope=-0.01, intercept=1, linetype="dashed")

eval.test %>%
  gather("metric","score",2:4) %>%
  ggplot(aes(x=threshold,y=score,color=metric))+geom_line(size=1)+
  xlab("Bin Threshold")+ylab("% of Total Fraud Caught")+ggtitle("FDR Curve: Test Set")+
  scale_color_discrete(label = c("Boosted Tree","Max", "Neural Network"))+
  theme(legend.title = element_blank())+
  scale_y_continuous(breaks = seq(0,1,.1))+
  geom_abline(slope=-0.01, intercept=1, linetype="dashed")
