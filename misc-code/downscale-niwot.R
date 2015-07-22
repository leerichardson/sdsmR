# Prepare the Niwot Data-frame!
niwot <- read.csv("/home/lee/Dropbox/work/ncar/data/snotel/niwot.csv")
niwot$date <- as.Date(niwot$date)
train <- split_dataframe(niwot)$train
train <- add_period(train)
test <- split_dataframe(niwot)$test
test <- add_period(test)
observed_temp <- test[, "temp_avg"]
observed_precip <- test[, "precip_inc"]
observed_snow <- test[, "swe"]
missing_train <- c(which(is.na(train[, 6])), which(is.na(train[, 8])))
missing_test <- c(which(is.na(test[, 6])), which(is.na(test[, 8])))


# Downscale Temperature ------------------
library(glmnet)
lasso_mod <- cv.glmnet(as.matrix(train[-missing_train, 8:ncol(train)]), train[-missing_train, 6])
lambda <- upper_bound_lambda(lasso_mod, max_params = 15)
coef_names <- coef(lasso_mod, s = lambda)@Dimnames[[1]]
coefs <- coef_names[coefs <- coef(lasso_mod, s = lambda)@i + 1]
lasso_preds <- predict(lasso_mod, newx = as.matrix(test[-missing_test, 8:ncol(test)]), s = lambda)
lasso_obs <- test[-missing_test, "temp_avg"]
plot(lasso_obs)
lines(lasso_preds, col = "red")
compute_error(predictions = lasso_preds, observed = lasso_obs)

# Downscale Precipitation ----------------

# Select Variables to use
lasso_precip_mod <- cv.glmnet(as.matrix(train[-missing_train, 8:ncol(train)]), train[-missing_train, 7])
lambda <- upper_bound_lambda(lasso_precip_mod, max_params = 10)
lasso_precip_preds <- predict(lasso_precip_mod, newx = as.matrix(test[-missing_test, 8:ncol(test)]), s = lambda)
precip_coefs <- coef(lasso_precip_mod, s = lambda)@Dimnames[[1]]
precip_coefs <- coef_names[coefs <- coef(lasso_precip_mod, s = lambda)@i + 1]

generate_table(plotname = "/home/lee/precip", dataframe = niwot, y = "prec_inc")

    # Find the appropriate box cox transformation
library(MASS)
precip_df <- cbind(train[, names(train) %in% precip_coefs], train[, "precip_inc"])
colnames(precip_df)[5] <- "prcp"
precip_df <- subset(precip_df, prcp != 0)
precip_mod <- lm(prcp ~ ., data = precip_df)
bc <- boxcox(precip_mod)
bc_index <- which(bc$y == max(bc$y))
bc_lambda <- .25

# Find a conditional model using the transformed precipiration variables
final_precip_df <- cbind(train[, names(train) %in% precip_coefs], train[, c("precip_inc", "date")])
colnames(final_precip_df)[5] <- "prcp"
non_zero <- which(final_precip_df[, "prcp"] != 0)
final_precip_df[non_zero, "prcp"] <- final_precip_df[non_zero, "prcp"]
cond_mod <- calibrate_model(dataframe = final_precip_df, y = "prcp", model_type = "seasonal", process = "conditional")
missing_precip_test <- c(which(is.na(test[, 7])), which(is.na(test[, 8])))
precip_preds <- generate_weather(models = cond_mod, new_dataframe = test[-missing_precip_test, ])
colnames(precip_preds)[1] <- "date"

# Compare the predicted precipitation accumulation with
# actual accumulation in the held out data-set
combined_precip <- merge(test, precip_preds, by = "date")
for (per in unique(combined_precip$period)) {

    tmp <- subset(combined_precip, period == per)
    if (nrow(tmp) < 365) {
        next
    }
    browser()
    plot(tmp[93:nrow(tmp), "precip_accum"], ylim = c(0, 30))
    pred_accum <- cumsum(tmp[93:nrow(tmp), "predictions"])
    lines(pred_accum, col = "red")
}


# Downscale Snow! ------------------------
