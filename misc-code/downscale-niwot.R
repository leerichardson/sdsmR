# Load in the models
source("misc-code/models-niwot.R")

# Observed T + P Snow predictions -------------------------------
observed_data <- test[, c("date", "year", "swe", "temp_avg", "precip_inc",
                          "period")]
swe_obs_preds <- rep(NA, 0)
test_periods <- unique(observed_data$period)
for (tp in test_periods) {
    print(tp)
    # Subset the data-frame to only the given period.
    # Then pull out the temperature and prepitation vectors
    # from this subset.
    test_period <- subset(observed_data, period == tp)
    temp <- test_period[, "temp_avg"]
    prec <- test_period[, "precip_inc"]
    dates <- test_period[, "date"]

    # Using the temp/precip vectors along with the peak and first
    # day models, predict SWE trajectories in the given period and
    # then plot it.
    swe_tp <- predict_swe_period(temp_vec = temp, precip_vec = prec,
                                 date_vec = dates, start_model = first_day_mod,
                                 peak_model = peak_mod)
    swe_obs_preds <- c(swe_obs_preds, swe_tp)
    plot(test_period[, "swe"], col = "black", pch = 16,
         ylim = c(0, 25), cex = .8)
    lines(swe_tp, col = "red")
}

# Plot the results of the Preds using observed
# Temperature and Precipitation predictions
plot_index <- which(observed_data$year <= 2010)
plot(observed_data$date[plot_index], observed_data$swe[plot_index], pch = 16, cex = .5,
     main = "Actual vs. Predicted SWE Using Observed T & P",
     xlab = "Year", ylab = "Snow Water Equivalent", cex.main = 2,
     cex.lab = 1.2)
lines(observed_data$date[plot_index], swe_obs_preds[plot_index], col = "blue", lwd = 2)
legend("topleft", c("Predicted SWE", "Actual SWE"), col = c("blue", "black"), lwd = 3)


# NCEP Downsclaed Preds -----------------------------------------
downscaled_preds <- cbind(prec_preds, lasso_preds)
years <- as.numeric(format.Date(downscaled_preds$date, "%Y"))
downscaled_preds$year <- years
downscaled_preds <- add_period(downscaled_preds)
ds_swe <- test[test$date %in% downscaled_preds$date, "swe"]
downscaled_preds$swe <- ds_swe
colnames(downscaled_preds) <- c("date", "precip_inc", "temp_avg", "year", "period", "swe")
swe_ds_preds <- rep(NA, 0)
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
    swe_ds_preds <- c(swe_ds_preds, swe_tp)
    plot(test_period[, "swe"], col = "black", pch = 16,
         ylim = c(0, 25), cex = .8)
    lines(swe_tp, col = "red")
}

# Plot the completely downscaled predictions against the observed SWE
# Values.
plot(downscaled_preds$date, downscaled_preds$swe, pch = 16, cex = .5,
     main = "Actual vs. Predicted SWE Using Downscaled T & P",
     xlab = "Year", ylab = "Snow Water Equivalent", cex.main = 2,
     cex.lab = 1.2)
lines(downscaled_preds$date, swe_ds_preds, col = "red", lwd = 2)
legend("topleft", c("Predicted SWE", "Actual SWE"), col = c("red", "black"), lwd = 3)

# Overlay both of the fits on top of eachother
obs_preds <- cbind(observed_data, swe_obs_preds)
obs_preds_overlap <- obs_preds[obs_preds$date %in% downscaled_preds$date, "swe_obs_preds"]
both_preds <- cbind(downscaled_preds, swe_ds_preds, obs_preds_overlap)

plot(both_preds$date, both_preds$swe, pch = 16, cex = .5,
     main = "Observed vs. Downscaled T & P",
     xlab = "Year", ylab = "Snow Water Equivalent", cex.main = 2,
     cex.lab = 1.2, ylim = c(0, 25))
lines(both_preds$date, both_preds$swe_ds_preds, col = "red", lwd = 1)
lines(both_preds$date, both_preds$obs_preds_overlap, col = "blue", lwd = 1)
legend("topleft", c("Observed T&P", "Downscaled T&P", "Actual SWE"), col = c("blue", "red", "black"), lwd = 3)
compute_error(predictions = both_preds$swe_ds_preds, observed = both_preds$swe)
compute_error(predictions = both_preds$obs_preds_overlap, observed = both_preds$swe)
