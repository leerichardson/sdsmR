# Load in the models
source("misc-code/models-niwot.R")

# Read in the NARCAAP predictor data-sets
current <- read.csv("/home/lee/Dropbox/work/ncar/data/snotel/niwot_site/final_datasets/niwot-crcm-current.csv")
colnames(current) <- convert_names(colnames(current))
future <- read.csv("/home/lee/Dropbox/work/ncar/data/snotel/niwot_site/final_datasets/niwot-crcm-future.csv")
colnames(future) <- convert_names(colnames(future))

# Compute anamolies based on the earlier climate model
# for both the current and future climate projections
means <- lapply(current, mean)
sds <- lapply(current, sd)
for (var in colnames(current)) {
    print(var)
    vec <- current[, var]
    index <- which(var == names(means))
    mean <- means[[index]]
    sd <- sds[[index]]
    current[, var] <- (vec - mean)/sd
}

for (var in colnames(future)) {
    print(var)
    vec <- future[, var]
    index <- which(var == names(means))
    mean <- means[[index]]
    sd <- sds[[index]]
    future[, var] <- (vec - mean)/sd
}

# Add period, year, and month to the narcaap
# predictors data-frame for the current simulations
date <- seq(as.Date("1971-01-01"), by = 1, len = nrow(current))
current$date <- date
years <- as.numeric(format.Date(current$date, "%Y"))
current$year <- years
months <- as.numeric(format.Date(current$date, "%m"))
current$month <- months
rm(months, years, date)
current <- add_period(current)

# Add period, year, and month to the narcaap
# predictors data-frame for the current simulations
date <- seq(as.Date("2040-01-01"), by = 1, len = nrow(future))
future$date <- date
years <- as.numeric(format.Date(future$date, "%Y"))
future$year <- years
months <- as.numeric(format.Date(future$date, "%m"))
future$month <- months
rm(months, years, date)
future <- add_period(future)


# Current Situation ---------------------------------------

# Downscale SWE with the current climate model projections
# from 1971 - 1998
niwot_names <- colnames(niwot[, 8:(ncol(niwot) - 3)])
current_names <- colnames(current)
same_names <- intersect(niwot_names, current_names)
overlapping_names <- which(niwot_names %in% current_names)
indices <- seq(from = 1, to = length(niwot_names), by = 1)
non_overlapping_names <- setdiff(indices, overlapping_names)
niwot_shell <- matrix(NA, nrow = 0, ncol = length(same_names))
merge1 <- rbind(niwot_shell, current[, same_names])
niwot_fill <- matrix(NA, nrow = nrow(merge1), ncol = length(non_overlapping_names))
colnames(niwot_fill) <- niwot_names[non_overlapping_names]
current_covs <- as.matrix(cbind(merge1, niwot_fill))
current_covs <- current_covs[, niwot_names]
current_temp_preds <- predict(lasso_mod, newx = current_covs, s = lambda)

# Look at the temperature downscaled predictions
current_temp_dates <- which(niwot$date %in% current$date & !is.na(niwot$temp_avg))
# plot(niwot$date[current_temp_dates], niwot[current_temp_dates, "temp_avg"])
# lines(current$date, current_temp_preds, col = "red")

# Precipitation --------------------------

# Fit a conditional model using vairables selected from the diagnostic table
current_prec_preds <- generate_weather(models = cond_mod, new_dataframe = current, y = "precip_inc")
current_prec_dates <- which(niwot$date %in% current$date & !is.na(niwot$precip_inc))
# plot(niwot$date[current_prec_dates], niwot[current_prec_dates, "precip_inc"])
# lines(current_prec_preds$date, current_prec_preds[, 2], col = "red")

current_ds <- cbind(current_prec_preds, current_temp_preds)
current_ds <- add_period(current_ds)
colnames(current_ds) <- c("date", "precip_inc", "temp_avg", "period")
swe_crcm_current_preds <- rep(NA, 0)
test_periods <- unique(current_ds$period)
for (tp in test_periods) {

    # Subset the data-frame to only the given period.
    # Then pull out the temperature and prepitation vectors
    # from this subset.
    test_period <- subset(current_ds, period == tp)
    temp <- test_period[, "temp_avg"]
    prec <- test_period[, "precip_inc"]
    dates <- test_period[, "date"]

    # Using the temp/precip vectors along with the peak and first
    # day models, predict SWE trajectories in the given period and
    # then plot it.
    swe_tp <- predict_swe_period(temp_vec = temp, precip_vec = prec,
                                 date_vec = dates, start_model = first_day_mod,
                                 peak_model = peak_mod)
    swe_crcm_current_preds <- c(swe_crcm_current_preds, swe_tp)
}
current_ds$swe_preds <- swe_crcm_current_preds
par(mfrow = c(1,2))
plot(current_ds$date, swe_crcm_current_preds, type = "l", ylim = c(0, 25))


# Future Situation ---------------------------------------

# Downscale SWE with the current climate model projections
# from 1971 - 1998
niwot_names <- colnames(niwot[, 8:(ncol(niwot) - 3)])
future_names <- colnames(future)
same_names <- intersect(niwot_names, future_names)
overlapping_names <- which(niwot_names %in% future_names)
indices <- seq(from = 1, to = length(niwot_names), by = 1)
non_overlapping_names <- setdiff(indices, overlapping_names)
niwot_shell <- matrix(NA, nrow = 0, ncol = length(same_names))
merge1 <- rbind(niwot_shell, future[, same_names])
niwot_fill <- matrix(NA, nrow = nrow(merge1), ncol = length(non_overlapping_names))
colnames(niwot_fill) <- niwot_names[non_overlapping_names]
future_covs <- as.matrix(cbind(merge1, niwot_fill))
future_covs <- future_covs[, niwot_names]
future_temp_preds <- predict(lasso_mod, newx = future_covs, s = lambda)

# Look at the temperature downscaled predictions
# plot(future$date, future_temp_preds, col = "red")


# Precipitation --------------------------

# Fit a conditional model using vairables selected from the diagnostic table
future_prec_preds <- generate_weather(models = cond_mod, new_dataframe = future, y = "precip_inc")
# plot(future_prec_preds$date, future_prec_preds[, 2], col = "red", type = "l")


future_ds <- cbind(future_prec_preds, future_temp_preds)
future_ds <- add_period(future_ds)
colnames(future_ds) <- c("date", "precip_inc", "temp_avg", "period")
swe_crcm_future_preds <- rep(NA, 0)
test_periods <- unique(future_ds$period)
for (tp in test_periods) {

    # Subset the data-frame to only the given period.
    # Then pull out the temperature and prepitation vectors
    # from this subset.
    test_period <- subset(future_ds, period == tp)
    temp <- test_period[, "temp_avg"]
    prec <- test_period[, "precip_inc"]
    dates <- test_period[, "date"]

    # Using the temp/precip vectors along with the peak and first
    # day models, predict SWE trajectories in the given period and
    # then plot it.
    swe_tp <- predict_swe_period(temp_vec = temp, precip_vec = prec,
                                 date_vec = dates, start_model = first_day_mod,
                                 peak_model = peak_mod)
    swe_crcm_future_preds <- c(swe_crcm_future_preds, swe_tp)
}
future_ds$swe_preds <- swe_crcm_future_preds
plot(future_ds$date, swe_crcm_future_preds, type = "l", ylim = c(0, 25))
