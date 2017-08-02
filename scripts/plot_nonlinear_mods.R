# Import tuning and model objects
load("models/bt_model.rda") # BT tuning
load("models/bt_model_rev.rda") # BT model (final)
load("models/nnet_model.rda") # NN tuning
load("models/nnet_model_rev.rda") # NN model (final)

# Import Libraries
library(clusterGeneration)
library(nnet)
library(devtools)
library(gbm)

# Import from Github a plotting function for neural networks
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')

# Nerual networks
nnet.fit # tuning results
plot(nnet.fit) # plot of tuning results
plot.nnet(mod.nnet) # plot of final nerual network model

# Boosted Tree
bt.fit # tuning results
plot(bt.fit) # plot of tuning results
summary.gbm(mod.bt) # plot of final booste tree model