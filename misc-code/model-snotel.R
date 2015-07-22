# Prepare the Niwot Dataset by adding an autoregression term along with
# month and period variables.
niwot <- read.csv("/home/lee/Dropbox/work/ncar/data/snotel/niwot.csv")
niwot$date <- as.Date(niwot$date)
niwot <- add_autoregression(niwot, "swe")

# Add the month and year as columns on this dataframe. We will
# use these to estimate the three parameters of interest:
# Start, Peak, and Duration!
years <- as.numeric(format.Date(niwot$date, "%Y"))
niwot$year <- years
months <- as.numeric(format.Date(niwot$date, "%m"))
niwot$month <- months

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

# Remove everything not in one of the periods
niwot <- niwot[!is.na(niwot$period), ]

rm(count, end_index, months, period, start_index, year, year_index, years)

# Prepare covariates --------------------------------
smooth_temps <- rollmean(x = niwot$temp_avg, n = 6)
smooth_precip <- rollmean(x = niwot$precip_inc, n = 3)
smooth_precip_accum <- rollmean(x = niwot$precip_accum, n = 6)
niwot$smooth_temp <- smooth_temps
niwot$smooth_precip <- smooth_precip
niwot$smooth_precip_accum <- smooth_precip_accum

# Split the data-set into a training and test set
start_per_25 <- min(which(niwot$period == 25))
train <- niwot[1:(start_per_25 - 1), ]
test <- niwot[start_per_25:nrow(niwot), ]
observed <- test[, "swe"]
rm(smooth_precip, smooth_precip_accum, smooth_temps, start_per_25)

# Thin Plate Spline ---------------------------------
# Fit the thing plate spline
# library(fields)
# X <- train[, c("smooth_temp", "smooth_precip_accum")]
# missing_train <- which(is.na(X[, 1]))
# X <- X[-missing_train, ]
# swe <- train[-missing_train, "swe"]
# swe_tps <- Tps(X, swe)
#
# # Use fitted spline to predict snow on a held out data-set
# Xtest <- test[, c("smooth_temp", "smooth_precip_accum")]
# null_rows <- which(!complete.cases(Xtest))
# Xtest[!complete.cases(Xtest), ] <- c(0, 0)
# predict_swe <- predict(object = swe_tps, x = Xtest)
# predict_swe[null_rows] <- NA
# predict_swe[predict_swe < .05] <- 0
# dates <- test[, "date"]
# plot(dates, observed, pch = 16, cex = .5)
# lines(dates, predict_swe, col = "red")
# compute_error(predictions = predict_swe[-null_rows], observed = observed[-null_rows])
# rm(X, missing_train, swe, Xtest)
#
# # Smooth Predictions
# smooth_preds <- rollmean(predict_swe, n = 10)
# compute_error(predictions = smooth_preds[!is.na(smooth_preds)], observed = observed[!is.na(smooth_preds)])
# plot(dates[!is.na(smooth_preds)], observed[!is.na(smooth_preds)],
#      main = "Predicted Snowpack 2006-2015 Using a Thin Plate Spline",
#      xlab = "Time", ylab = "Snow Water Equivalent", pch = 16, cex = .5)
# lines(dates[!is.na(smooth_preds)], smooth_preds[!is.na(smooth_preds)], col = "red")

# # Linear Model --------------------------
# # Autoregression, Precip, and Temp
# lm1 <- lm(swe ~ temp_avg + precip_accum + autoregression, data = train)
# lm1_preds <- predict(object = lm1, newdata = test)
# plot(test$date, test$swe)
# lines(test$date, lm1_preds, col = "blue")
#
# # No Autoregression
# lm2 <- lm(swe ~ temp_avg + precip_accum + precip_inc, data = train)
# lm2_preds <- predict(object = lm2, newdata = test)
# plot(test$date, test$swe)
# lines(test$date, lm2_preds, col = "blue")
# compute_error(predictions = lm2_preds[!is.na(lm2_preds)], observed = test$swe[!is.na(lm2_preds)])



# Functional Model ----------------------
library(dplyr)
library(magrittr)

# Use these monthly and yearly variables to obtain the monthly
# averages for our covariates of interest.
monthly_means <- select(niwot, date, year, month, period, precip_accum, precip_inc, temp_avg) %>%
                    group_by(year, month, period) %>%
                        summarise(temp = mean(temp_avg, na.rm = TRUE),
                                  precip_accum = mean(precip_accum, na.rm = TRUE),
                                  precip_inc = mean(precip_inc, na.rm = TRUE))


period_parameters <- select(niwot, date, year, month, swe, period, precip_accum, precip_inc, temp_avg) %>%
                        group_by(period) %>%
                            summarise(peaks = max(swe),
                                      peak_index = find_peak_index(swe),
                                      duration = find_duration(swe),
                                      first = find_first(swe))

# Combine the parameter data-frame with the covariate dataframe!
modeling_df <- merge(period_parameters, monthly_means, by = "period")

first_months <- filter(modeling_df, month %in% 8:11, !is.na(period)) %>%
    select(period, month, temp, precip_accum, precip_inc, first)
first_matrix<- data.frame(matrix(NA, nrow = 0, ncol = 13))
for (i in unique(first_months$period)) {
    per <- filter(first_months, period == i)
    per <- per[order(per$month), ]
    tmps <- per[, "temp"]
    prec_inc <- per[, "precip_inc"]
    prec_acc <- per[, "precip_accum"]
    first <- unique(per$first)
    row <- c(first, tmps, prec_inc, prec_acc)
    first_matrix <- rbind(first_matrix, row)
}
colnames(first_matrix) <- c("first", paste0("temp", 8:11), paste0("prec_inc", 8:11), paste0("prec_acc", 8:11))

fmat <- first_matrix[complete.cases(first_matrix), ]
fmat_train <- split_dataframe(dataframe = fmat)$train
fmat_test <- split_dataframe(dataframe = fmat)$test
fmat_obs <- fmat_test[, "first"]

lma <- lm(first ~ temp10 , data = fmat)
lma_preds <- predict(lma, fmat_test)
compute_error(predictions = lma_preds, observed = fmat_obs)
plot(fmat_obs)
lines(lma_preds)


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

# add an autoregression term
peaks <- b_matrix[, "peaks"]
peak_lag <- lag(peaks, 1)
b_matrix$autoregression <- peak_lag
rm(monthly_means, peak_months, per, period_parameters, relevant_months, i, peak_lag,
   peaks, prec_inc, prec_acc, row, tmps first, )

# Split data-set and add in an autoregressive term
# along with temp/precipitation maximums.
b_matrix <- b_matrix[complete.cases(b_matrix), ]
bmat_train <- split_dataframe(dataframe = b_matrix)$train
bmat_test <- split_dataframe(dataframe = b_matrix)$test
bmat_obs <- bmat_test[, "peaks"]

# Lasso Fit
library(glmnet)
sparse_mod <- cv.glmnet(x = as.matrix(bmat_train[, 2:ncol(bmat_train)]), y = bmat_train[, 1])
sparse_preds <- predict(sparse_mod, newx = as.matrix(bmat_test[, 2:ncol(bmat_test)]), s = "lambda.min")

# Linear Model Fit
lmb <- lm(peaks ~ prec_acc4 + prec_acc5, data = bmat_train)
lmb_preds <- predict(object = lmb, newdata = bmat_test)

# Evaluate Models visually and with Test Error
plot(bmat_obs, ylim = c(5, 30))
lines(lmb_preds, col = "red")
lines(sparse_preds, col = "green")
compute_error(predictions = lmb_preds, observed = bmat_obs)
compute_error(predictions = sparse_preds, observed = bmat_obs)

predict_swe_peak <- function(temp_vec, precip_vec, model) {
    # Grab the months based on the length of the temp and
    # precipitation vectors (if it's a leap year)
    if (length(temp_vec) == 365) {
        dates <- seq(as.Date("1981-07-01"), to = as.Date("1982-06-30"), by=1)
        month_vec <- format.Date(dates, "%m")
    } else if (length(temp_vec == 366)) {
        dates <- seq(as.Date("1979-07-01"), to = as.Date("1980-06-30"), by=1)
        month_vec <- format.Date(dates, "%m")
    }

    month_vec <- as.numeric(month_vec)
    dataframe <- as.data.frame(cbind(month_vec, temp_vec, precip_vec))
    monthly_temps <- aggregate(formula = temp_vec ~ month_vec, data = dataframe, FUN = mean)
    monthly_precip <- aggregate(formula = precip_vec ~ month_vec, data = dataframe, FUN = mean)

    model_names <- names(model$coefficients)
    newdf <- c(monthly_temps[, 2], monthly_precip[, 2])
    cov_names <- c(paste0("temp", 1:12), paste0("prec_acc", 1:12))
    covs <- which(cov_names %in% model_names)

    # Fit the input linear model using the monthly covariates
    predictors <- data.frame(matrix(newdf[covs], nrow = 1, ncol = length(covs)))
    names(predictors) <- cov_names[covs]
    peak_pred <- predict(model, newdata = predictors)
}


predict_swe_start <- function(temp_vec, precip_vec, model) {
    # Grab the months based on the length of the temp and
    # precipitation vectors (if it's a leap year)
    if (length(temp_vec) == 365) {
        dates <- seq(as.Date("1981-07-01"), to = as.Date("1982-06-30"), by=1)
        month_vec <- format.Date(dates, "%m")
    } else if (length(temp_vec == 366)) {
        dates <- seq(as.Date("1979-07-01"), to = as.Date("1980-06-30"), by=1)
        month_vec <- format.Date(dates, "%m")
    }

    month_vec <- as.numeric(month_vec)
    dataframe <- as.data.frame(cbind(month_vec, temp_vec, precip_vec))
    monthly_temps <- aggregate(formula = temp_vec ~ month_vec, data = dataframe, FUN = mean)
    monthly_precip <- aggregate(formula = precip_vec ~ month_vec, data = dataframe, FUN = mean)

    model_names <- names(model$coefficients)
    newdf <- c(monthly_temps[, 2], monthly_precip[, 2])
    cov_names <- c(paste0("temp", 1:12), paste0("prec_acc", 1:12))
    covs <- which(cov_names %in% model_names)

    # Fit the input linear model using the monthly covariates
    predictors <- data.frame(matrix(newdf[covs], nrow = 1, ncol = length(covs)))
    names(predictors) <- cov_names[covs]
    peak_pred <- predict(model, newdata = predictors)
}


temp <-niwot[which(niwot$period == 30), "temp_avg"]
prec <-niwot[which(niwot$period == 30), "precip_accum"]
start_prediction <- predict_swe_start(temp_vec = temp, precip_vec = prec, model = lma)
peak_prediction <- predict_swe_peak(temp_vec = temp, precip_vec = prec, model = lmb)

# Function which takes in a precipitation vector, temperature
# vector, and compute what SWE would look like over the course
# of the year.
predict_swe_period <- function(temp_vec, precip_vec, decline_rate = .35,
                               percent_snow = .9, first_day = 97, peak_model,
                               start_model) {
    # Find the date in which the peak of the heat will occur
    smooth_temp <- rollmean(x = temp_vec, n = 3)
    peak_snow <- predict_swe_peak(temp_vec, precip_vec, peak_model)
    first_day <- round(predict_swe_start(temp_vec, precip_vec, start_model), digits = 0)
    print(first_day)
    if (first_day < 92) {
        first_day <- 93
    }

    # Loop through each one of the days and predict the SWE
    # at this given day
    num_days <- length(temp_vec)
    swe_results <- rep(NA, num_days)
    current_swe <- 0
    peaked <- "no"
    for (day in 1:num_days) {
        if (current_swe < peak_snow & day < first_day) {
            current_swe = 0
            swe_results[day] <- current_swe
        } else if (current_swe <= peak_snow & day >= first_day & peaked == "no") {
            current_swe <- (percent_snow * precip_vec[day])
            swe_results[day] <- current_swe
        } else if (current_swe > peak_snow & peaked == "no") {
            peaked <- "yes"
            swe_results[day] <- current_swe
        } else if (peaked == "yes" & current_swe > 0) {
            current_swe <- current_swe - decline_rate
            if (current_swe < 0) {
                current_swe = 0
            }
            swe_results[day] <- current_swe
        } else {
            current_swe = 0
            swe_results[day] <- current_swe
        }
    }
    # Return the vector of SWE for this time period
    return(swe_results)
}

swe_test <- predict_swe_period(temp_vec = temp, precip_vec = prec, start_model = lma, peak_model = lmb)

# Test the Predict SWE Function
swe_test <- rep(NA, 0)
test_periods <- unique(test$period)
for (tp in test_periods) {
    test_period <- subset(niwot, period == tp)
    temp <- test_period[, "temp_avg"]
    prec <- test_period[, "precip_accum"]

    swe_tp <- predict_swe_period(temp_vec = temp, precip_vec = prec, start_model = lma, peak_model = lmb)
    swe_test <- c(swe_test, swe_tp)
    plot(test_period[, "swe"], ylim = c(0, 30), pch = 16, cex = .5)
    lines(swe_tp, col = "red")
}

# Compare TPS with Functional Model
plot(test$date, observed, ylim = c(0, 25), pch = 16, cex = .5,
     main = "Functional Model Predictions 2006-2015",
     xlab = "", ylab = "Snow Water Equivalent")
lines(test$date, swe_test, col= "blue", lwd = 1.5)
legend("topleft", c("Functional Model", "Thin Plate Spline"), col = c("blue", "red"), lwd = 3)
compute_error(predictions = swe_test, observed = observed)

