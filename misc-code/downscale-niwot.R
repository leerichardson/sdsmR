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
train <- split_dataframe(niwot, percentage = .5)$train
missing_train <- complete.cases(train)
test <- split_dataframe(niwot, percentage = .5)$test
missing_test <- complete.cases(test)

# Downscale Temperature ------------------
library(glmnet)
mp <- 15
lasso_mod <- cv.glmnet(as.matrix(train[missing_train, 8:(ncol(train) - 3)]), train[missing_train, 6])
lambda <- upper_bound_lambda(lasso_mod, max_params = mp)
coef_names <- coef(lasso_mod, s = lambda)@Dimnames[[1]]
coefs <- coef_names[coefs <- coef(lasso_mod, s = lambda)@i + 1]
lasso_preds <- predict(lasso_mod, newx = as.matrix(test[missing_test, 8:(ncol(test) - 3)]), s = lambda)
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


# Build Peak and First day models -----------------------------

# Use these monthly and yearly variables to obtain the monthly
# averages for our covariates of interest.
lm_df <- generate_modeling_df(niwot)

# Remove the missing values from our lm data-frame, and then fit
# a model which can predict peak
lm_df <- lm_df[complete.cases(lm_df), ]
lm_df <- add_autoregression(lm_df, response_name = "peaks")
lm_df_train <- split_dataframe(dataframe = lm_df, percentage = .8)$train
lm_df_test <- split_dataframe(dataframe = lm_df, percentage = .8)$test

first_day_mod <- lm(first ~ temp_9 + temp_10, data = lm_df_train)
first_day_preds <- predict.lm(first_day_mod, newdata = lm_df_test, interval = "prediction")
plot(lm_df_test[, "first"], pch = 16, ylim = c(50, 150))
lines(first_day_preds[, 1])
lines(first_day_preds[, 2], col = "red")
lines(first_day_preds[, 3], col = "blue")

peak_mod <- lm(peaks ~ temp_4 + prec_1 + autoregression, data = lm_df_train)
peak_preds <- predict.lm(peak_mod, newdata = lm_df, interval = "prediction")
plot(lm_df[, "peaks"], pch = 16, ylim = c(0, 30))
lines(peak_preds[, 1])
lines(peak_preds[, 2], col = "blue")
lines(peak_preds[, 3], col = "red")



# Observed T + P Snow predictions -------------------------------



# NCEP Downsclaed Preds -----------------------------------------
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
    plot(test_period[, "swe"], col = "black", pch = 16,
         ylim = c(0, 25), cex = .8)
    lines(swe_tp, col = "red")
}

# Plot the completely downscaled predictions against the observed SWE
# Values.


