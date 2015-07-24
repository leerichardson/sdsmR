# Prepare the Niwot Data-frame! Add a period, year,
# month, columns
niwot <- read.csv("/home/lee/Dropbox/work/ncar/data/snotel/niwot_site/niwot.csv")
niwot$date <- as.Date(niwot$date)
niwot <- add_period(niwot)
years <- as.numeric(format.Date(niwot$date, "%Y"))
niwot$year <- years
months <- as.numeric(format.Date(niwot$date, "%m"))
niwot$month <- months

# Split the training data into training and test
# sets to be used for model calibration. Then store
# the rows which will cause errors due to missing values :)
train <- split_dataframe(niwot)$train
missing_train <- complete.cases(train)
test <- split_dataframe(niwot)$test
missing_test <- complete.cases(test)

# Downscale Temperature ------------------
library(glmnet)
lasso_mod <- cv.glmnet(as.matrix(train[missing_train, 8:ncol(train)]), train[missing_train, 6])
lambda <- upper_bound_lambda(lasso_mod, max_params = 15)
coef_names <- coef(lasso_mod, s = lambda)@Dimnames[[1]]
coefs <- coef_names[coefs <- coef(lasso_mod, s = lambda)@i + 1]
lasso_preds <- predict(lasso_mod, newx = as.matrix(test[missing_test, 8:ncol(test)]), s = lambda)
plot(test$date, test[, "temp_avg"])
lines(test$date[missing_test], lasso_preds, col = "red")

# Downscale Precipitation ----------------

# Generate the Diagnostic Plots
generate_table(plotname = "/home/lee/precip", dataframe = niwot, y = "precip_inc")

# Fit a conditional model using vairables selected from the diagnostic table
mod_vars <- c("date", "precip_inc", "rhum_700", "vort_600", "div_300", "hgt_600")
train <- train[complete.cases(train), ]
test <- test[complete.cases(test), ]
cond_mod <- calibrate_model(train[, colnames(train) %in% mod_vars], y = "precip_inc", model_type = "seasonal", process = "conditional")
prec_preds <- generate_weather(models = cond_mod, new_dataframe = test, y = "precip_inc")
plot(test$date, test[, "precip_inc"])
lines(test$date, prec_preds[, 2], col = "red")


# Downscale Snow! ------------------------

# Use these monthly and yearly variables to obtain the monthly
# averages for our covariates of interest.
modeling_df <- generate_modeling_df(niwot)

# Remove the missing values from our lm data-frame, and then fit
# a model which can predict peak
lm_df <- lm_df[complete.cases(lm_df), ]
lm_df_train <- split_dataframe(dataframe = lm_df, percentage = .8)$train
lm_df_test <- split_dataframe(dataframe = lm_df, percentage = .8)$test

first_day_mod <- lm(first ~ temp_9 + temp_10, data = lm_df_train)
first_day_preds <- predict.lm(first_day_mod, newdata = lm_df_test)
plot(lm_df_test[, "first"], pch = 16, ylim = c(70, 130))
lines(first_day_preds)

peak_mod <- lm(peaks ~ temp_4 + prec_1 + prec_4, data = lm_df_train)
peak_preds <- predict.lm(peak_mod, newdata = lm_df_test)
plot(lm_df_test[, "peaks"], pch = 16, ylim = c(8, 20))
lines(peak_preds)

# Try to predict with downscaled predictions
downscaled_preds <- cbind(prec_preds, lasso_preds)
downscaled_preds <- add_period(downscaled_preds)
ds_swe <- test[test$date %in% downscaled_preds$date, "swe"]
downscaled_preds$swe <- ds_swe
colnames(downscaled_preds) <- c("date", "precip_inc", "temp_avg", "period", "swe")
swe_ds_test <- rep(NA, 0)
test_periods <- unique(downscaled_preds$period)
for (tp in test_periods) {
    print(tp)
    # Subset the data-frame to only the given period.
    # Then pull out the temperature and prepitation vectors
    # from this subset.
    test_period <- subset(downscaled_preds, period == tp)
    temp <- test_period[, "temp_avg"]
    prec <- test_period[, "precip_inc"]
    dates <- test_period[, "date"]

    # Using the temp/precip vectors along with the peak and first
    # day models, predict SWE trajectories in the given period and
    # then plot it.
    swe_tp <- predict_swe_period(temp_vec = temp, precip_vec = prec,
                date_vec = dates, start_model = first_day_mod,
                peak_model = peak_mod)
    swe_ds_test <- c(swe_ds_test, swe_tp)
    print(length(swe_test))
    plot(test_period[, "swe"], col = "black", pch = 16,
         ylim = c(0, 25), cex = .8)
    lines(swe_tp, col = "red")
    browser()
}

# Plot the completely downscaled predictions against the observed SWE
# Values.
plot(downscaled_preds$date, downscaled_preds$swe, ylim = c(0, 25), pch = 16, cex = .5,
     main = "Fully Downscaled Predictions",
     xlab = "", ylab = "Snow Water Equivalent")
lines(downscaled_preds$date, swe_ds_test, col= "blue", lwd = 1.5)
legend("topleft", c("Downscaled Functional Model", "Actual SWE"), col = c("blue", "black"), lwd = 3)
compute_error(predictions = swe_ds_test, observed = downscaled_preds[, "swe"])

