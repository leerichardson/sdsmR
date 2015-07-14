# Calculate the SWE Anamolies.
niwot <- read.csv("/home/lee/Dropbox/work/ncar/data/snotel/niwot.csv")
niwot$date <- as.Date(niwot$date)
temp_precip_snow <- niwot[, 1:7]
temp_precip_snow <- add_autoregression(temp_precip_snow, "swe")
anamolies <- temp_precip_snow$swe - temp_precip_snow$autoregression
temp_precip_snow$anamolies <- anamolies

# Prepare covariates --------------------------------
smooth_temps <- rollmean(x = temp_precip_snow$temp_avg, n = 6)
smooth_precip <- rollmean(x = temp_precip_snow$precip_inc, n = 3)
smooth_precip_accum <- rollmean(x = temp_precip_snow$precip_accum, n = 6)
temp_precip_snow$smooth_temp <- smooth_temps
temp_precip_snow$smooth_precip <- smooth_precip
temp_precip_snow$smooth_precip_accum <- smooth_precip_accum

# Split the data-set into a training and test set
train <- split_dataframe(dataframe = temp_precip_snow, percentage = .7)$train
test <- split_dataframe(dataframe = temp_precip_snow, percentage = .7)$test
observed <- test[, "swe"]
rm(temp_precip_snow, anamolies, smooth_precip, smooth_precip_accum, smooth_temps)

# Thin Plate Spline ---------------------------------
# Fit the thing plate spline
library(fields)
X <- train[, c("smooth_temp", "smooth_precip_accum")]
missing_train <- which(is.na(X[, 1]))
X <- X[-missing_train, ]
swe <- train[-missing_train, "swe"]
swe_tps <- Tps(X, swe)

# Use fitted spline to predict snow on a held out data-set
Xtest <- test[, c("smooth_temp", "smooth_precip_accum")]
missing_test <- unique(c(which(is.na(Xtest[, 1])), which(is.na(Xtest[, 2]))))
observed_swe <- test[-missing_test, "swe"]
Xtest <- Xtest[-missing_test, ]
predict_swe <- predict(object = swe_tps, x = Xtest)
smooth_preds <- rollmean(x = predict_swe, n = 20)
dates <- test[-missing_test, "date"]
plot(dates, observed_swe)
lines(dates, predict_swe, col = "red")
lines(dates, smooth_preds, col = "blue")
compute_error(predictions = predict_swe, observed = observed_swe)

# Linear Model --------------------------
# Autoregression, Precip, and Temp
lm1 <- lm(swe ~ temp_avg + precip_accum + autoregression, data = train)
lm1_preds <- predict(object = lm1, newdata = test)
plot(test$date, test$swe)
lines(test$date, lm1_preds, col = "blue")

# No Autoregression
lm2 <- lm(swe ~ temp_avg + precip_accum + precip_inc, data = train)
lm2_preds <- predict(object = lm2, newdata = test)
plot(test$date, test$swe)
lines(test$date, lm2_preds, col = "blue")
compute_error(predictions = lm2_preds[!is.na(lm2_preds)], observed = test$swe[!is.na(lm2_preds)])

# Functional Model ----------------------
# Estimate a using the monthly means surrounding it.




