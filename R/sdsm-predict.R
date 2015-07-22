#' Make predictions using calibrated models and a new data frame.
#'
#' This function is meant to be used after running the calibrate_models
#' function. The idea is that it checks the model type, and based on this
#' information it makes the appropriate predictions. generate_Weather
#' returns a data frame with dates, and predictions, and either ensembles
#' or prediction intervals if that option has been specified. Note that you can't
#' produce prediction intervals for conditional models.
#'
#' @export
#'
#' @param models A list containing either 1, 4, or 12 models depending
#' on whether the model is annual, seasonal, or monthly.
#' @param new_dataframe A new dataframe in which predictions are to
#' be made from the calibrated models.
#' @param uncertainty Either "ensemble" or "interval".
#' @param num_ensembles If uncertainty = "ensembles", then this specifies
#' how many ensembles to return. Default is set to one.
#' @param If sing an autoregressive model, you must specify the
#' response variable.
#' @return A two column dataframe with dates and predicted weather. The dataframe
#' is ordered chronologically from the dates column.
generate_weather <- function(models, new_dataframe, uncertainty = "ensemble",
                             num_ensembles = 1, y = NULL) {

    # Make sure the uncertainty is either ensemble or
    # interval
    stopifnot(uncertainty %in% c("ensemble", "interval"))
    if (uncertainty == "interval" & length(models[[1]]) == 2) {
        stop("Can't generate prediction intervals for conditional models")
    }

    # Determine how many models in the list
    num_mods <- length(models)

    # Pull out which column is the date class
    classes <- unlist(lapply(new_dataframe, class))
    date_column_index <- as.numeric(which(classes == "Date"))
    if (length(date_column_index) == 0) {
        stop("Need a column of the Date Class")
    }

    # Check to see if the models include an autoregressive
    # term. If yes, add the autoregressive variable to the
    # dataframe
    if ("autoregression" %in% names(models[[1]]$coefficients)) {
        if (is.null(y)) {
            stop("Must specify the name of the response variable
                 if using an autoregressive model. Make sure y is
                 a character vector")
        } else {
            new_dataframe <- add_autoregression(new_dataframe, y)
        }
    }

    # Initialize prediction and date vectors which will be
    # used to store the results of the generate weather function.
    (dates <- structure(rep(NA_real_, 0), class = "Date"))
    if (uncertainty == "interval") {
        preds <- data.frame(matrix(data = NA, nrow = 0, ncol = 3))
        colnames(preds) <- c("fit", "lwr", "upr")
    } else if (length(models[[1]]) == 2 & num_ensembles > 1) {
        preds <- data.frame(matrix(data = NA, nrow = 0, ncol = num_ensembles))
    } else {
        preds <- vector(mode = "numeric", length = 0)
    }

    # Depending on whether it's a monthly, seasonal, or annual model, make
    # predictions on the variable of interest for each day in the new data-set
    if (num_mods == 12) {
        new_dataframe <- add_month(new_dataframe)
        for (i in names(models)) {
            sub_month <- subset(new_dataframe, month == which(names(models) == i))

            # If the type of model is conditional, instead of unconditional,
            # obtain the predictions from the predict_conditional
            # function
            if (length(models[[i]]) == 2) {
                if (num_ensembles == 1) {
                    sub_preds <- predict_conditional(sub_month, models[[i]]$step1,
                                    models[[i]]$step2, num_ensembles = num_ensembles)
                    preds <- c(preds, sub_preds)
                } else {
                    sub_preds <- predict_conditional(sub_month, models[[i]]$step1,
                                    models[[i]]$step2, num_ensembles = num_ensembles)
                    preds <- rbind(preds, sub_preds)
                }
            } else {
                # Check whether the type of uncertainty is "interval". If yes,
                # then derive a prediction interval and append to the preds dataframe.
                # If no, then append the point predictions.
                if (uncertainty == "interval") {
                    sub_preds <- predict.lm(models[[i]], newdata = sub_month,
                                            interval = "prediction")
                    preds <- rbind(preds, sub_preds)
                } else {
                    sub_preds <- predict.lm(models[[i]], newdata = sub_month)
                    preds <- c(preds, sub_preds)
                }
            }
            dates <- c(dates, sub_month[, date_column_index])
        }
    }
    else if (num_mods == 4) {
        new_dataframe <- add_month(new_dataframe)
        new_dataframe <- add_season(new_dataframe, "month")
        for (i in names(models)) {
            sub_season <- subset(new_dataframe, season == i)

            # If the type of model is conditional, instead of unconditional,
            # obtain the predictions from the predict_conditional
            # function
            if (length(models[[i]]) == 2) {
                if (num_ensembles == 1) {
                    sub_preds <- predict_conditional(sub_season, models[[i]]$step1,
                                    models[[i]]$step2, num_ensembles = num_ensembles)
                    preds <- c(preds, sub_preds)
                } else {
                    sub_preds <- predict_conditional(sub_season, models[[i]]$step1,
                                    models[[i]]$step2, num_ensembles = num_ensembles)
                    preds <- rbind(preds, sub_preds)
                }
            } else {
                if (uncertainty == "interval") {
                    sub_preds <- predict.lm(models[[i]], newdata = sub_season,
                                            interval = "prediction")
                    preds <- rbind(preds, sub_preds)
                } else {
                    sub_preds <- predict.lm(models[[i]], newdata = sub_season)
                    preds <- c(preds, sub_season)
                }
            }
            dates <- c(dates, sub_season[, date_column_index])
        }

    } else if (num_mods == 1) {

        # If the type of model is conditional, instead of unconditional,
        # obtain the predictions from the predict_conditional
        # function
        if (length(models[[1]]) == 2) {
            preds <- predict_conditional(new_dataframe, models[[1]]$step1,
                        models[[1]]$step2, num_ensembles = num_ensembles)
        } else {

            if (uncertainty == "interval") {
                preds <- predict.lm(models[[1]], newdata = new_dataframe,
                                        interval = "prediction")
            } else {
                preds <- predict.lm(models[[1]], newdata = new_dataframe)
            }
        }
        dates <- c(dates, new_dataframe[, date_column_index])

    } else {
        stop("List must contain 1, 4, or 12 models!")
    }

    # Combine the dates along with the
    final_df <- data.frame(dates = dates, predictions = preds)
    order_by_date <- order(final_df[, "dates"])

    # Add in the column names onto the dataframe if
    # we are adding ensmbles to the conditional models
    if (num_ensembles > 1 & length(models[[1]]) == 2) {
        colnames(final_df)[2:ncol(final_df)] <- paste0("ensemble_", 1:num_ensembles)
    }

    # Return the final dataframe, unless we are adding ensembles onto
    # a conditional model.
    if ((uncertainty == "ensemble" & num_ensembles == 1)
        | uncertainty == "interval" | length(models[[1]]) == 2) {
        return(final_df[order_by_date,])
    } else if (uncertainty == "ensemble" & num_ensembles > 1) {
        average_sigma <- mean(unlist(lapply(models,
                                        function(x) summary(x)$sigma)))
        ensembles <- generate_ensembles(final_df$predictions,
                        num_ensembles, sigma = mean(average_sigma))
        final_with_ensembles <- cbind(final_df, ensembles)
        colnames(final_with_ensembles)[3:ncol(final_with_ensembles)] <-
            paste0("ensemble_", 1:num_ensembles)
        return(final_with_ensembles)
    }
}

# Function to generate ensembles for various predictions made with
# the generate_weather function
generate_ensembles <- function(predictions, num_ensembles, sigma,
                               conditional = FALSE) {
    # Get the number of predicted values from the input, and use this
    # along with the number of ensembles to create a white noise
    # matrix. The white noise matrix uses the average standard
    # error over all linear models used in the prediction.
    num_preds <- length(predictions)
    white_noise <- matrix(rnorm(num_preds * num_ensembles, mean = 0,
                                sd = sigma), ncol = num_ensembles)
    ensemble_matrix <- white_noise + predictions
    return(ensemble_matrix)
}

# Function which generates conditional predictions based on
# two seperate models for a given row.
predict_row <- function(row, lm1, lm2) {
    # Use the first linear model to predict whether or not there
    # will be a wet day
    wet_day <- predict(lm1, row)
    rand <- runif(1)

    # Determine how much precipitation based on predicting wet_dat.
    # If it's a wet day, determine the amount. If it's a non wet day,
    # then return a 0
    if (rand < wet_day) {
        precip_amount <- predict(lm2, row)
    } else {
        return(0)
    }
}

# Function to generate conditional ensembles. when more than
# one prediction is expected.
conditional_ensembles <- function(row, lm1, lm2, num_ensembles) {
    row_ensemble <- data.frame(matrix(NA, nrow = 1, ncol = num_ensembles))
    wet_day <- predict(lm1, row)
    sigma <-summary(lm2)$sigma
    for (i in 1:num_ensembles) {
        rand <- runif(1)
        if (rand < wet_day) {
            precip_amount <- predict(lm2, row)
            row_ensemble[1, i] <- precip_amount + rnorm(1, mean = 0, sd = sigma)
        } else {
            row_ensemble[1, i] <- 0
        }
    }
    return(row_ensemble)
}

# Function which uses predict_row to make predictions for an
# entire dataframe
predict_conditional <- function(dataframe, lm1, lm2, num_ensembles) {
    if (num_ensembles == 1) {
        cond_preds <- vector(mode = "numeric", length = 0)
        for (i in 1:nrow(dataframe)) {
            prediction <- predict_row(dataframe[i, ], lm1, lm2)
            cond_preds <- c(cond_preds, prediction)
        }
        return(cond_preds)
    } else {
        ensemble_preds <- data.frame(matrix(data = NA, nrow = 0, ncol = num_ensembles))
        for (i in 1:nrow(dataframe)) {
            prediction <- conditional_ensembles(dataframe[i, ], lm1, lm2, num_ensembles)
            ensemble_preds <- rbind(ensemble_preds, prediction)
        }
        return(ensemble_preds)
    }
}

