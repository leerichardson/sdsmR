# Prepare the Niwot Data-frame!
niwot <- read.csv("/home/lee/Dropbox/work/ncar/data/snotel/niwot.csv")
niwot$date <- as.Date(niwot$date)
train <- split_dataframe(niwot)$train
test <- split_dataframe(niwot)$test
observed_temp <- test[,"temp_avg"]
obsrved_precip <- test[,"precip_inc"]


# Downscale Temperature ------------------
library(glmnet)
missing_train <- c(which(is.na(train[, 6])), which(is.na(train[, 8])))
missing_test <- c(which(is.na(test[, 6])), which(is.na(test[, 8])))
lasso_mod <- cv.glmnet(as.matrix(train[-missing_train, 8:ncol(train)]), train[-missing_train, 6])
lambda <- upper_bound_lambda(lasso_mod, max_params = 200)
lasso_preds <- predict(lasso_mod, newx= as.matrix(test[-missing_test, 8:ncol(test)]), s = lambda)
lasso_obs <- test[-missing_test, "temp_avg"]
plot(lasso_obs)
lines(lasso_preds, col = "red")
compute_error(predictions = lasso_preds, observed = lasso_obs)

# Downscale Precipitation ----------------




# Downscale Snow! ------------------------
