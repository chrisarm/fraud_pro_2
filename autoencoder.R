#install.packages("h2o")
library(h2o)

# build autoencoder model
localH2O = h2o.init()
dat.hex<-as.h2o(train_df, destination_frame="train.hex")
dat.dl = h2o.deeplearning(x = feature_names, training_frame = dat.hex,
                          autoencoder = TRUE,
                          reproducible = T,
                          seed = 1234,
                          hidden = c(6,5,6), epochs = 50)

# interesting per feature error scores
# dat.anon = h2o.anomaly(dat.dl, dat.hex, per_feature=TRUE)
# head(dat.anon)

dat.anon = h2o.anomaly(dat.dl, dat.hex, per_feature=FALSE)
head(dat.anon)
err <- as.data.frame(dat.anon)

# interesting reduced features (defaults to last hidden layer)
# http://www.rdocumentation.org/packages/h2o/functions/h2o.deepfeatures
# reduced_new  <- h2o.deepfeatures(dat.dl, dat.hex)

plot(sort(err$Reconstruction.MSE))

