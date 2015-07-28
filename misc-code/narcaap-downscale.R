# Read in the NARCAAP predictor data-sets
narcaap_preds <- read.csv("/home/lee/Dropbox/work/ncar/data/snotel/niwot_site/niwot_narcaap.csv")

# Convert the narcaap names to ncep names
narcaap_names <- colnames(narcaap_preds)
colnames(narcaap_preds) <- convert_names(narcaap_names)

# Add period, year, and month to the narcaap
# predictors data-frame
date <- seq(as.Date("1979-01-01"), by = 1, len = nrow(narcaap_preds))
narcaap_preds$date <- date
narcaap_preds <- add_period(narcaap_preds)
years <- as.numeric(format.Date(narcaap_preds$date, "%Y"))
narcaap_preds$year <- years
months <- as.numeric(format.Date(narcaap_preds$date, "%m"))
narcaap_preds$month <- months
rm(months, years, date)

# Temperature ------------------------

# Predict temperature from the lasso model. Need to create a
# newx matrix which is identical to the training data-set used for
# fitting the lasso model.
niwot_names <- colnames(niwot[, 8:(ncol(niwot) - 3)])
narcaap_names <- colnames(narcaap_preds)
same_names <- intersect(niwot_names, narcaap_names)
overlapping_names <- which(niwot_names %in% narcaap_names)
indices <- seq(from = 1, to = length(niwot_names), by = 1)
non_overlapping_names <- setdiff(indices, overlapping_names)
niwot_shell <- matrix(NA, nrow = 0, ncol = length(same_names))
merge1 <- rbind(niwot_shell, narcaap_preds[, same_names])
niwot_fill <- matrix(NA, nrow = nrow(merge1), ncol = length(non_overlapping_names))
colnames(niwot_fill) <- niwot_names[non_overlapping_names]
narcaap_covs <- as.matrix(cbind(merge1, niwot_fill))
narcaap_covs <- narcaap_covs[, niwot_names]
narcaap_temp_preds <- predict(lasso_mod, newx = narcaap_covs, s = lambda)
plot(niwot$date, niwot[, "temp_avg"])
lines(narcaap_preds$date, narcaap_temp_preds, col = "red")


# Precipitation --------------------------
# Fit a conditional model using vairables selected from the diagnostic table
narcaap_prec_preds <- generate_weather(models = cond_mod, new_dataframe = narcaap_preds, y = "precip_inc")
plot(test$date, test[, "precip_inc"])
lines(test$date, prec_preds[, 2], col = "red")


# Snow -----------------------------------
# Try to predict with downscaled predictions
narcaap_ds <- cbind(narcaap_prec_preds, narcaap_temp_preds)
niwot_overlap <- which(narcaap_preds$date %in% niwot$date)
narcaap_ds <- narcaap_ds[niwot_overlap, ]
narcaap_ds <- add_period(narcaap_ds)
swe_dates <- which(niwot$date %in% narcaap_preds$date)
ds_swe <- niwot[dates, "swe"]
narcaap_ds$swe <- ds_swe
colnames(narcaap_ds) <- c("date", "precip_inc", "temp_avg", "period", "swe")
swe_ds_test <- rep(NA, 0)
test_periods <- unique(narcaap_ds$period)

for (tp in test_periods) {
    print(tp)
    # Subset the data-frame to only the given period.
    # Then pull out the temperature and prepitation vectors
    # from this subset.
    test_period <- subset(narcaap_ds, period == tp)
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
    browser()
}

# Plot the en
plot(narcaap_ds$date, narcaap_ds$swe, ylim = c(0, 25), pch = 16, cex = .5,
     main = "Fully Downscaled Predictions",
     xlab = "", ylab = "Snow Water Equivalent")
lines(narcaap_ds$date, swe_ds_test, col= "blue", lwd = 1.5)
legend("topleft", c("Downscaled Functional Model", "Actual SWE"), col = c("blue", "black"), lwd = 3)
compute_error(predictions = swe_ds_test, observed = downscaled_preds[, "swe"])

