# Functions to find the Parameters used for the functional
# model of Snow. Used to estimate the peak, max, and duration.
find_first <- function(x) {
    snow_days <- which(x > 0.2)
    first_day <- min(snow_days)
    return(first_day)
}

find_peak_index <- function(x, first_day = 97, func = max) {
    after_first <- round(x[97:length(x)], digits = 0)
    per_max <- func(after_first, na.rm = TRUE)
    peak_index <- which(after_first == per_max)[1]
    if (is.na(peak_index)) {
        return(NA)
    } else {
        return(peak_index + first_day)
    }
}

find_val <- function(x, first_day = 97, func = mean) {
    after_first <- round(x[97:length(x)], digits = 0)
    per_max <- func(after_first, na.rm = TRUE)
}

find_duration <- function(x) {
    beginning <- find_first(x)
    peak_index <- which(x == max(x))[1]
    zeros <- which(x == 0)
    after_cycle <- zeros[which(zeros > peak_index)]
    end <- min(after_cycle)
    return(end - beginning)
}

find_period_start <- function(dataframe, period) {
    index_of_period <- which(dataframe$period == period)
    return(min(index_of_period))
}

find_period_peak <- function(dataframe, per, peak_var, func = max) {
    period_start <- find_period_start(dataframe, per)
    per_df <- dataframe[dataframe$period == per & !is.na(dataframe$period), ]
    peak_var_vector <- per_df[, peak_var]
    peak_var_index <- find_peak_index(peak_var_vector, first_day = 97,  func)
    df_index <-period_start + peak_var_index
    df_value <- dataframe[df_index, peak_var]
    return(list(index = df_index, value = df_value))
}

indices_window <- function(index, window = 5) {
    ten_day_index <- seq(from = index - window, to = index + window, by = 1)
    return(ten_day_index)
}

# Find the lambda corresponding to the glmnet fit, resticting
# the maximum number of variables allowed in the model.
upper_bound_lambda <- function(model, max_params = 15) {
    # Make sure the modle has been built with
    # glmnet and has cross validation
    stopifnot(class(model) == "cv.glmnet")

    # Restrict the acceptable models to the ones which
    # have less than the maximum number of term
    num_params <- as.numeric(model$nzero)
    sparse_enough <- which(num_params < max_params)
    mse <- model$cvm
    lambda <- model$lambda

    # Choose the model with the minimum cross validation
    # score out of the remaining models.
    min_cv <- min(mse[sparse_enough])
    min_index <- which(mse == min_cv)
    min_lambda <- lambda[min_index]
    return(min_lambda)
}

# Establish a Period variable to seperate the dataset into.
# Then add this variable onto the data-set
add_period <- function(dataframe) {
    years <- as.numeric(format.Date(dataframe$date, "%Y"))
    year_index <- unique(years)
    period <- rep(NA, length = nrow(dataframe))
    count <- 0
    for (year in year_index) {
        # Index the start and end data of the dataset based on a
        # data wen there is never swe, JULY 15th!
        start_index <- which(dataframe$date == paste0(year, "-07-01"))
        end_index <- which(dataframe$date == paste0(year + 1, "-06-30"))

        # If the year doesn't have a complete year, then don't make the plot.
        if (length(start_index) == 0 | length(end_index) == 0) {
            next
        }

        # Update the cout and assign it to the period vector
        count <- count + 1
        period[start_index:end_index] <- count
    }
    dataframe$period <- period

    # Remove everything not in one of the periods
    dataframe <- dataframe[!is.na(dataframe$period), ]
}

generate_modeling_df <- function(dataframe) {
    # Load in the packages for easy summary statistics.
    library(tidyr)
    library(dplyr)
    library(magrittr)

    # Use these monthly and yearly variables to obtain the monthly
    # averages for our covariates of interest.
    monthly_means <- select(dataframe, date, month, period, precip_inc, temp_avg, swe) %>%
        group_by(month, period) %>%
        summarise(cov_temp = mean(temp_avg, na.rm = TRUE),
                  cov_prec = mean(precip_inc, na.rm = TRUE))


    period_parameters <- select(dataframe, date, year, month, swe, period, precip_accum, precip_inc, temp_avg) %>%
        group_by(period) %>%
        summarise(peaks = max(swe),
                  first = find_first(swe))

    # Combine the parameter data-frame with the covariate dataframe. Then
    # Create a new modeling df which makes it easy to fir linear models
    modeling_df <- merge(period_parameters, monthly_means, by = "period")
    lm_df  <- modeling_df %>%
        gather(Var, val, starts_with("cov")) %>%
        unite(Var1, Var, month) %>%
        spread(Var1, val)
    colnames(lm_df) <- gsub("cov_", "", colnames(lm_df))
    return(lm_df)
}

compute_monthly_covs <- function(temp_vec, precip_vec, date_vec = NULL) {
    # Make sure that the temperature and precipitation vectors
    # are the same
    stopifnot(length(temp_vec) == length(precip_vec))

    # Create a month vector based on the length of the temp and
    # precipitation vectors (if it's a leap year)
    if (!is.null(date_vec)) {
        month_vec <- format.Date(date_vec, "%m")
    } else if (length(temp_vec) == 365) {
        dates <- seq(as.Date("1981-07-01"), to = as.Date("1982-06-30"), by = 1)
        month_vec <- format.Date(dates, "%m")
    } else if (length(temp_vec == 366)) {
        dates <- seq(as.Date("1979-07-01"), to = as.Date("1980-06-30"), by = 1)
        month_vec <- format.Date(dates, "%m")
    } else {
        stop("Vectors must be either 365 or 366 days!")
    }

    # Use this monthly vector to obtain the monthly averages for
    # both temperature and precipication
    month_vec <- as.numeric(month_vec)
    dataframe <- as.data.frame(cbind(month_vec, temp_vec, precip_vec))
    monthly_temps <- aggregate(formula = temp_vec ~ month_vec, data = dataframe, FUN = mean)
    monthly_precip <- aggregate(formula = precip_vec ~ month_vec, data = dataframe, FUN = mean)

    # Combine these monthly averages into a one row data-frame
    # that can be used for making predictions on a model with the
    # same covariate names
    newvals <- c(monthly_temps[, 2], monthly_precip[, 2])
    df <- data.frame(matrix(newvals, nrow = 1, ncol = length(newvals)))
    cov_names <- c(paste0("temp_", 1:12), paste0("prec_", 1:12))
    colnames(df) <- cov_names
    return(df)
}

predict_swe_peak <- function(temp_vec, precip_vec, date_vec = NULL, model) {
    # Compute the monthly covariates using this particular
    # temp and precip
    predictors <- compute_monthly_covs(temp_vec, precip_vec, date_vec)

    # Use these predictors to make a prediction for the
    # peak snowfall in this period
    peak_pred <- predict(model, newdata = predictors)
    return(peak_pred)
}

predict_swe_start <- function(temp_vec, precip_vec, date_vec = NULL, model) {

    # Compute the monthly covariates using this particular
    # temp and recip vector
    predictors <- compute_monthly_covs(temp_vec, precip_vec, date_vec)

    # Use these predictors to make a prediction for the
    # peak snowfall in this period
    first_day_pred <- predict(model, newdata = predictors)
    return(first_day_pred)
}

# Function which takes in a precipitation vector, temperature
# vector, and compute what SWE would look like over the course
# of the year.
predict_swe_period <- function(temp_vec, precip_vec, date_vec = NULL, decline_rate = .5,
                               percent_snow = .9, first_day = 95, peak_model,
                               start_model) {

    # Estimate the parameters (first day and peak) for this particular
    # period using the two linear models and temp + precip vectors.
    peak_snow <- predict_swe_peak(temp_vec, precip_vec, date_vec, peak_model)
    first_day <- floor(predict_swe_start(temp_vec, precip_vec, date_vec, start_model))

    # Calculate Precip Accumulation after the first day has
    # been estimated
    zero_prec <- rep(0, first_day - 1)
    prec_accum <- cumsum(prec[first_day:length(precip_vec)])
    precip_vec <- c(zero_prec, prec_accum)

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
