# Load in the models
source("misc-code/models-niwot.R")

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
# fitting the lasso model. This takes a bit of finnegaling
# between the two data-sets
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

# Look at the temperature downscaled predictions
narcaap_temp_dates <- which(niwot$date %in% narcaap_preds$date & !is.na(niwot$temp_avg))
plot(niwot$date[narcaap_temp_dates], niwot[narcaap_temp_dates, "temp_avg"])
lines(narcaap_preds$date, narcaap_temp_preds, col = "red")


# Precipitation --------------------------

# Fit a conditional model using vairables selected from the diagnostic table
narcaap_prec_preds <- generate_weather(models = cond_mod, new_dataframe = narcaap_preds, y = "precip_inc")
narcaap_prec_dates <- which(niwot$date %in% narcaap_preds$date & !is.na(niwot$precip_inc))
plot(niwot$date[narcaap_prec_dates], niwot[narcaap_prec_dates, "precip_inc"])
lines(narcaap_prec_preds$date, narcaap_prec_preds[, 2], col = "red")


# Snow -----------------------------------

# Predict with the
narcaap_ds <- cbind(narcaap_prec_preds, narcaap_temp_preds)
niwot_overlap <- which(narcaap_preds$date %in% niwot$date)
narcaap_ds <- narcaap_ds[niwot_overlap, ]
swe_dates <- which(niwot$date %in% narcaap_preds$date)
ds_swe <- niwot[swe_dates, "swe"]
narcaap_ds$swe <- ds_swe
narcaap_ds <- add_period(narcaap_ds)
colnames(narcaap_ds) <- c("date", "precip_inc", "temp_avg", "swe", "period")
swe_crcm_ncep_preds <- rep(NA, 0)
test_periods <- unique(narcaap_ds$period)

for (tp in test_periods) {

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
    swe_crcm_ncep_preds <- c(swe_crcm_ncep_preds, swe_tp)
    plot(test_period[, "swe"], col = "black", pch = 16,
         ylim = c(0, 25), cex = .8)
    lines(swe_tp, col = "red")
    browser()
}

# Plot the NARCAAP downscaled predictions!
plot(narcaap_ds$date, narcaap_ds$swe, ylim = c(0, 25), pch = 16, cex = .5,
     main = "CRCM-NCEP Downscaled SWE Predictions",
     xlab = "", ylab = "Snow Water Equivalent")
lines(narcaap_ds$date, swe_crcm_ncep_preds, col= "blue", lwd = 1.5)
legend("topleft", c("CRCM-NCEP Downscaled Preds", "Actual SWE"), col = c("blue", "black"), lwd = 3)
compute_error(predictions = swe_crcm_ncep_preds, observed = narcaap_ds[, "swe"])

# Add on the NARCAAP-CRCM preds to the others
narcaap_dates <- which(both_preds$date %in% narcaap_ds$date)
three_preds <- both_preds[narcaap_dates, ]
three_preds <- cbind(three_preds, swe_crcm_ncep_preds[1:nrow(three_preds)])
colnames(three_preds)[9] <- "swe_crcm_ncep_preds"

plot(three_preds$date, three_preds$swe, pch = 16, cex = .5,
     main = "Three Different Downscaling Models",
     xlab = "Year", ylab = "Snow Water Equivalent", cex.main = 2,
     cex.lab = 1.2, ylim = c(0, 25))
lines(three_preds$date, three_preds$obs_preds_overlap, col = "blue")
lines(three_preds$date, three_preds$swe_ds_preds, col = "red")
lines(three_preds$date, three_preds$swe_crcm_ncep_preds, col = "green")
legend("topleft", c("Observed T&P", "Downscaled T&P", "CRCM-NCEP T&P", "Actual SWE"), col = c("blue", "red", "green", "black"), lwd = 3)
compute_error(predictions = three_preds$obs_preds_overlap, observed = three_preds$swe)
compute_error(predictions = three_preds$swe_ds_preds, observed = three_preds$swe)
compute_error(predictions = three_preds$swe_crcm_ncep_preds, observed = three_preds$swe)



