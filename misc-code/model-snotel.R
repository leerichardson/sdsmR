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
predict_swe[predict_swe < .05] <- 0
dates <- test[-missing_test, "date"]
plot(dates, observed_swe, pch = 16, cex = .5)
lines(dates, predict_swe, col = "red")
compute_error(predictions = predict_swe, observed = observed_swe)


# Smooth Predictions
smooth_preds <- rollmean(predict_swe, n = 3)
compute_error(predictions = smooth_preds[!is.na(smooth_preds)], observed = observed_swe[!is.na(smooth_preds)])
plot(dates[!is.na(smooth_preds)], observed_swe[!is.na(smooth_preds)],
     main = "Predicted Snowpack 2006-2015 Using a Thin Plate Spline",
     xlab = "Time", ylab = "Snow Water Equivalent", pch = 16, cex = .5)
lines(dates[!is.na(smooth_preds)], smooth_preds[!is.na(smooth_preds)], col = "red")

# Optimize over smoothing
n <- 50
rmse <- rep(NA, n)
for (bw in seq_along(1:n)) {
    smooth_preds <- rollmean(predict_swe, n = bw)
    tmp_rmse <- compute_error(predictions = smooth_preds[!is.na(smooth_preds)], observed = observed_swe[!is.na(smooth_preds)])$rmse
    rmse[bw] <- tmp_rmse
}

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
library(dplyr)
library(magrittr)

# Add the month and year as columns on this dataframe. We will
# use these to estimate the three parameters of interest:
# Start, Peak, and Duration!
years <- as.numeric(format.Date(niwot$date, "%Y"))
niwot$year <- years
months <- as.numeric(format.Date(niwot$date, "%m"))
niwot$month <- months

# Establish a Period variable to seperate the dataset into.
# Then add this variable onto the data-set
year_index <- unique(years)
period <- rep(NA, length = nrow(niwot))
count <- 0
for (year in year_index) {
    # Index the start and end data of the dataset based on a
    # data wen there is never swe, JULY 15th!
    start_index <- which(niwot$date == paste0(year, "-07-01"))
    end_index <- which(niwot$date == paste0(year + 1, "-06-30"))

    # If the year doesn't have a complete year, then don't make the plot.
    if (length(start_index) == 0 | length(end_index) == 0) {
        next
    }

    # Update the cout and assign it to the period vector
    count <- count + 1
    period[start_index:end_index] <- count
}
niwot$period <- period

# Use these monthly and yearly variables to obtain the monthly
# averages for our covariates of interest.
monthly_means <- select(niwot, date, year, month, period, precip_accum, precip_inc, temp_avg) %>%
                    group_by(year, month, period) %>%
                        summarise(temp = mean(temp_avg, na.rm = TRUE),
                                  precip_accum = mean(precip_accum, na.rm = TRUE),
                                  precip_inc = mean(precip_inc, na.rm = TRUE))


period_parameters <- select(niwot, date, year, month, swe, period, precip_accum, precip_inc, temp_avg) %>%
                        group_by(period) %>%
                            summarise(peaks = find_peak(swe),
                                      peak_index = find_peak_index(swe),
                                      duration = find_duration(swe),
                                      first = find_first(swe))

# Combine the parameter data-frame with the covariate dataframe!
modeling_df <- merge(period_parameters, monthly_means, by = "period")

# Create the Peaks parameters
peak_months <- relevant_months <- filter(modeling_df, month %in% 2:6, !is.na(period)) %>%
                    select(period, month, temp, precip_accum, precip_inc, peaks)
b_matrix <- data.frame(matrix(NA, nrow = 0, ncol = 16))
for (i in unique(relevant_months$period)) {
    print(i)
    if (is.na(i)) {
        next
    }
    per <- filter(peak_months, period == i)
    per <- per[order(per$month), ]
    tmps <- per[, "temp"]
    prec_inc <- per[, "precip_inc"]
    prec_acc <- per[, "precip_accum"]
    peaks <- unique(per$peaks)
    row <- c(peaks, tmps, prec_inc, prec_acc)
    b_matrix <- rbind(b_matrix, row)
}
colnames(b_matrix) <- c("peaks", paste0("temp", 2:6), paste0("prec_inc", 2:6), paste0("prec_acc", 2:6))

# add an autoregression term.
peaks <- b_matrix[, "peaks"]
peak_lag <- lag(peaks, 1)
b_matrix$autoregression <- peak_lag

# See how well we can recover the maximum
b_matrix <- b_matrix[complete.cases(b_matrix), ]
bmat_train <- split_dataframe(dataframe = b_matrix)$train
bmat_test <- split_dataframe(dataframe = b_matrix)$test
bmat_obs <- bmat_test[, "peaks"]

# Lasso Fit
library(glmnet)
sparse_mod <- cv.glmnet(x = as.matrix(bmat_train[, 2:ncol(bmat_train)]), y = bmat_train[, 1])
sparse_preds <- predict(sparse_mod, newx = as.matrix(bmat_test[, 2:ncol(bmat_test)]), s = "lambda.min")

# Linear Model Fit
lmb1 <- lm(peaks ~ ., data = bmat_train)
lmb1_preds <- predict(object = lmb1, newdata = bmat_test)
lmb2 <- lm(peaks ~ prec_acc4, data = bmat_train)
lmb2_preds <- predict(object = lmb2, newdata = bmat_test)

# Evaluate Models visually and with Test Error
plot(bmat_obs, ylim = c(5, 30))
lines(lmb1_preds, col = "blue")
lines(lmb2_preds, col = "red")
lines(sparse_preds, col = "green")
compute_error(predictions = lmb1_preds, observed = bmat_obs)
compute_error(predictions = lmb2_preds, observed = bmat_obs)
compute_error(predictions = sparse_preds, observed = bmat_obs)

# Plot Temperature During Peaks ------------------------------
# Plot the profile of Temperature during the peak Snowfall in
# the Niwot data-frame.
find_period_start <- function(dataframe, period) {
    index_of_period <- which(dataframe$period == period)
    return(min(index_of_period))
}

find_period_peak <- function(dataframe, per, peak_var) {
    period_start <- find_period_start(dataframe, per)
    per_df <- dataframe[dataframe$period == per & !is.na(dataframe$period), ]
    peak_var_vector <- per_df[, peak_var]
    peak_var_index <- find_peak_index(peak_var_vector)
    return(period_start + peak_var_index)
}

indices_window <- function(index, window = 5) {
    ten_day_index <- seq(from = index - window, to = index + window, by = 1)
    return(ten_day_index)
}

periods <- unique(niwot$period[!is.na(niwot$period)])
decline_indices <- matrix(NA, nrow = 11, ncol = length(periods))
for (i in seq_along(periods)) {
    period_peak <- find_period_peak(dataframe = niwot, per = i, peak_var = "swe")
    indices <- indices_window(period_peak)
    decline_indices[, i] <- indices
}

count <- 0
smooth_temps <- rollmean(x = niwot[, "temp_avg"], n = 3)
for (i in seq_along(periods)) {
    plot_index <- decline_indices[, i]

    # Skip if the temperature here has any NA's
    smooth_temp <- smooth_temps[plot_index]
    if (sum(is.na(smooth_temp)) > 8) {
        next
    }

    print(smooth_temp)
    count <- count + 1
    if (count == 1) {
        plot(1:length(plot_index), smooth_temp, ylim = c(15, 55),
             type = "l", col = sample(colours()[73:110], 1),
             main = "Smooth Temperature During Peak SWE", ylab = "Smooth Temperature",
             xlab = "Day's surrounding Peak SWE")
    } else {
        lines(1:length(plot_index), smooth_temp,
              col = sample(colours()[73:110], 1))
    }
}
abline(v = 6, lwd = 3)




predict_swe_peak <- function(temp_vec, precip_vec, model) {
    # Grab the months based on the length of the temp and
    # precipitation vectors (if it's a leap year)
    if (length(temp_vec) == 365) {
        dates <- seq(as.Date("1981-07-01"), to = as.Date("1982-06-30"), by=1)
        month_vec <- format.Date(dates, "%m")
    } else if (length(temp_vec == 366)) {
        dates <- seq(as.Date("1980-07-01"), to = as.Date("1981-06-30"), by=1)
        month_vec <- format.Date(dates, "%m")
    }

    month_vec <- as.numeric(month_vec)
    dataframe <- as.data.frame(cbind(month_vec, temp_vec, precip_vec))
    monthly_temps <- aggregate(formula = temp_vec ~ month_vec, data = dataframe, FUN = mean)
    monthly_precip <- aggregate(formula = precip_vec ~ month_vec, data = dataframe, FUN = mean)

    # Fit the input linear model using the monthly covariates
    browser()
    num <- subset(monthly_precip, month_vec == 4)[, 2]
    predictors <- data.frame(prec_acc4 = num)
    peak_pred <- predict(model, newdata = predictors)
}

peak_prediction <- predict_swe_peak(temp, precip_vec = prec_accum, model = lmb2)

# Function which takes in a precipitation vector, temperature
# vector, and compute what SWE would look like over the course
# of the year.
predict_swe_period <- function(temp_vec, precip_vec, min_date = 150,
                               percent_snow = .9, first_day = 97, model) {
    # Find the date in which the peak of the heat will occur
    smooth_temp <- rollmean(x = temp_vec, n = 3)
    peak_snow <- predict_swe_peak(temp_vec, precip_vec, model)
    hot_enough <- (smooth_temp > decline_temp)
    min_dates <- min_date:length(temp_vec)
    decline_index <- min(intersect(hot_enough, min_dates))

    # Loop through each one of the days and predict the SWE
    # at this given day
    num_days <- length(temp_vec)
    swe_results <- rep(NA, num_days)
    current_swe <- 0
    for (day in 1:num_days) {
        print(day)
        if (day < decline_index & day < first_day) {
            current_swe = 0
            swe_results[day] <- current_swe
        } else if (day < decline_index & day >= first_day) {
            current_swe <- (percent_snow * precip_vec[day])
            swe_results[day] <- current_swe
        } else if (day >= decline_index & current_swe > 0) {
            current_swe <- current_swe - .5
            if (current_swe < 0) {
                current_swe = 0
            }
            swe_results[day] <- current_swe
        } else {
            current_swe = 0
            swe_results[day] <- current_swe
        }
        print(current_swe)
    }
    # Return the vector of SWE for this time period
    return(swe_results)
}

# Test the Predict SWE Function
for ( i in 11:34) {
    test_period <- subset(niwot, period == i)
    temp <- test_period[, "temp_avg"]
    prec_accum <- test_period[, "precip_accum"]

    swe_test <- predict_swe_period(temp_vec = temp, precip_vec = prec_accum)
    plot(test_period[, "swe"], ylim = c(0, 30))
    lines(swe_test)
}

