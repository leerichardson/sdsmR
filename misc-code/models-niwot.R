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
mp <- 10
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
train_cond <- train[complete.cases(train), ]
test_cond <- test[complete.cases(test), ]
cond_mod <- calibrate_model(train_cond[, colnames(train_cond) %in% mod_vars], y = "precip_inc", model_type = "seasonal", process = "conditional")
prec_preds <- generate_weather(models = cond_mod, new_dataframe = test_cond, y = "precip_inc")
plot(test_cond$date, test_cond[, "precip_inc"])
lines(test_cond$date, prec_preds[, 2], col = "red")


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

peak_mod <- lm(peaks ~ temp_4 + prec_1 + prec_4, data = lm_df_train)
peak_preds <- predict.lm(peak_mod, newdata = lm_df_test, interval = "prediction")
plot(lm_df_test[, "peaks"], pch = 16, ylim = c(0, 30))
lines(peak_preds[, 1])
lines(peak_preds[, 2], col = "blue")
lines(peak_preds[, 3], col = "red")

