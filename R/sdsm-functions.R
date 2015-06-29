# Function to add a month onto a dataframe
add_month <- function(dataframe) {
    classes <- unlist(lapply(dataframe, class))
    date_column_index <- as.numeric(which(classes == "Date"))
    dataframe$month <- as.numeric(format.Date(dataframe[, date_column_index], "%m"))
    return(dataframe)
}

# Function to add a season onto a dataframe, given a month
# column
add_season <- function(dataframe, month_column) {

    # Determine the column with the month variable
    test <- try(class(month_column))
    if (class(test) != "character" & test != "data.frame") {
        month_column_name <- deparse(substitute(month_column))
    } else {
        month_column_name <- month_column
    }

    # Based on the month column, add the season variable to the dataframe
    season <- rep(NA, nrow(dataframe))
    season[dataframe$month %in% c(12, 1, 2)] <- "winter"
    season[dataframe$month %in% c(3, 4, 5)] <- "spring"
    season[dataframe$month %in% c(6, 7, 8)] <- "summer"
    season[dataframe$month %in% c(9, 10, 11)] <- "fall"
    dataframe$season <- season

    # Make sure all of the columns have a season
    if (sum(is.na(dataframe$season)) > 0) {
        stop("Some months to not have a season!")
    }

    return(dataframe)
}

#' Build a Linear Regression for Statistical Downscaling
#'
#' This function is meant to replicate the calibrate model
#' section of the SDSM tool. It will date in a dataframe, the response
#' variable, the dates column, as well as other specific modeling
#' oprions. As an output, calibrate_model with generate a list
#' with appropriate number of linear models. The reason it outputs
#' a list of models, rather than just one, is for consistency when
#' the "monthly" or "seasonal" option is chosen, which will fit 4 or 12
#' seperate models to the dataframe.
#'
#' @export
#'
#' @param dataframe a dataframe object which contains the data
#' you are fitting the model with
#' @param y the repsonse variable. Input must be a character vector
#' @param model_type "annual", "monthly", or "seasonal". The default
#' used is "annual"
#' @param autoregression Whether or not to include an autoregressive term
#' in the model
#' @param process Either conditional or unconditional
#' @param date_column Column which contains the dates. Should be
#' of the class Date in R to work, input must be a character!
#' @return A list of either one, four, or twelve linear models, with
#' the length of the list determined by the model_type parameter.
calibrate_model <- function(dataframe, y, model_type = "annual",
                            autoregression = "false", process = "unconditional",
                            date_column = NULL) {

    # Change the response name input into a character vector to be
    # used later on when indexing the
    if (!exists("y")) {
        stop("Must specify the response (y) variable")
    } else if (class(y) != "character") {
        stop("Response variable (y) must be a character (string)")
    } else {
        response_name <- y
    }


    # Grab the date column and response variable column names from the inputs.
    # These will be used later in various convinient ways, such as indexing the
    # dataframe when needed.
    if (!is.null(date_column)) {
        date_column_name <- date_column
    } else {
        classes <- unlist(lapply(dataframe, class))
        date_column_index <- as.numeric(which(classes == "Date"))
        date_column_name <- colnames(dataframe)[date_column_index]
    }

    # Add in a lagged variable to the regression models if that option
    # has beens specified
    if (autoregression == "true") {
        response <- ts(dataframe[, response_name])
        lagged <- lag(response, -1)
        tmp = cbind(lagged, response)
        lagged_vec <- tmp[1:(nrow(tmp) - 1),1]
        dataframe$autoregression <- lagged_vec
    } else if (autoregression != "false") {
        stop("autoregression must be true or false")
    }

    # Based on the time frame of the input model, allocate a list
    # with components for either all months, all seasons, or an annual
    # model
    if (model_type == "monthly") {
        # Allocate twelve slots in a list in order to store each
        # of the twelve monthly models
        models <- vector(mode = "list", length = 12)
        month_names <- c("jan", "feb", "mar", "apr", "may", "jun",
                         "jul", "aug", "sept", "oct", "nov", "dec")
        names(models) <- month_names

        # Pull out the list of months in the dataframe which we
        # will use to fit each specific monthly linear model. Then, add the month
        # column to the corresponding dataframe to be used in the following loop
        months <- unique(format.Date(dataframe[, date_column_name], "%m"))
        dataframe$month <- factor(format.Date(dataframe[, date_column_name], "%m"))

        # Loop through each one of the months and fit a linear model to the
        # corresponding dataframe. Then, store each monthly linear model as an
        # element of a list in which we have created above.
        for (m in months) {
            month_dataframe <- subset(dataframe, month == m)
            month_lm <- lm(month_dataframe[, response_name] ~ .,
                data = month_dataframe[,-which(names(month_dataframe) %in% c(response_name, date_column_name, "month"))])
            models[[as.numeric(m)]] <- month_lm
        }



    } else if (model_type == "seasonal") {
        # Allocate four slots in a list in order to store each of
            # the four seasonal models
        models <- vector(mode = "list", length = 4)
        season_names <- c("winter", "spring", "summer", "fall")
        names(models) <- season_names


        # In order to create the seasonal variable, we must first
        # create one which represents the months
        dataframe$month <- factor(format.Date(dataframe[, date_column_name], "%m"))

        # Using the month variable jut created, make another
        # variable which represents the season. Then, get a list of the
        # different seasons to use in the loop below
        dataframe$season <- factor(ifelse(dataframe$month %in% c("12", "01", "02"), season_names[1],
                                    ifelse(dataframe$month %in% c("03", "04", "05"), season_names[2],
                                    ifelse(dataframe$month %in% c("06", "07", "08"), season_names[3],
                                    ifelse(dataframe$month %in% c("09", "10", "11"), season_names[4],  NA)))))
        seasons <- unique(dataframe$season)

        # Loop through each one of the months and fit a linear model to the
        # corresponding dataframe. Then, store each monthly linear model as an
        # element of a list in which we have created above.
        for (s in seasons) {
            season_dataframe <- subset(dataframe, season == s)
            unused_indices <- which(names(season_dataframe) %in% c(response_name, date_column_name, "month", "season"))
            season_lm <- lm(season_dataframe[, response_name] ~ ., data = season_dataframe[,-unused_indices])
            models[[as.character(s)]] <- season_lm
        }

    } else if (model_type == "annual") {
        models <- vector(mode = "list", length = 1)
        names(models) <- "annual"
        annual_lm <- lm(dataframe[, response_name] ~ .,
                        data = dataframe[,-which(names(dataframe) %in% c(response_name, date_column_name))])
        models[[1]] <- annual_lm

    } else {
        stop("Model type must be annual, seasonal, or monthly")
    }

    # Return the list of models
    return(models)
}

#' Obtain summary statistics for calibrated models
#'
#' This function is meant to be used after running the calibrate_models
#' function.
#'
#' @export
#'
#' @param model_list A list build with the calibrate_model function
#' @return A list of summary statistics for the corresponding models
#' fitted with the calibrate_models function
summarize_models <- function(model_list) {
    # Apply the lapply summary functions
    # to this model in order to return the
    # quantities of interest from the given list of models
    month_summaries <- lapply(model_list,
                              function(x) {
                                summary <- summary(x)
                                r_squared <- summary$r.squared
                                standard_error <- summary$sigma
                                coefs <- summary$coefficients
                                return(list(r.squared = r_squared,
                                            standard.error = standard_error,
                                            coefficients = coefs))
                            }
                        )

}

#' Make predictions using calibrated models and a new data frame
#'
#' This function is meant to be used after running the calibrate_models
#' function. The idea is that it checks the model type, and then works
#' through the different subsets of the new dataframe to make predictions,
#' and returnsa data frame with dates, and predictions, and ensembles
#' of predictions if that option has been specified.
#'
#' @export
#'
#' @param models A list containing either 1, 4, or 12 models depending
#' on whether the model is annual, seasonal, or monthly.
#' @param new_dataframe A new dataframe in which predictions are to
#' be made from the calibrated models.
#' @param uncertainty Either "ensembles" or "intervals".
#' @param num_ensembles If uncertainty = "ensembles", then this specifies
#' how many ensembles to return. Default is set to one.
#' @return A two column dataframe with dates and predicted weather. The dataframe
#' is ordered chronologically from the dates column.
generate_weather <- function(models, new_dataframe,
                             uncertainty = "ensemble", num_ensembles = 1) {

    # Determine how many models in the list
    num_mods <- length(models)

    # Pull out which column is the date class
    classes <- unlist(lapply(new_dataframe, class))
    date_column_index <- as.numeric(which(classes == "Date"))
    if (length(date_column_index) == 0) {
        stop("Need a column of the Date Class")
    }

    # Pull out the seasonal/monthly aspects of the data
    # frame if they're not already included
    (dates <- structure(rep(NA_real_, 0 ), class="Date"))
    preds <- vector(mode = "numeric", length = 0)
    if (num_mods == 12) {
        new_dataframe <- add_month(new_dataframe)
        for (i in names(models)) {
            sub_month <- subset(new_dataframe, month == which(names(models) == i))
            sub_preds <- predict.lm(models[[i]], newdata = sub_month)
            preds <- c(preds, sub_preds)
            dates <- c(dates, sub_month[, date_column_index])
        }
    }
    else if (num_mods == 4) {
        new_dataframe <- add_month(new_dataframe)
        new_dataframe <- add_season(new_dataframe, "month")
        for (i in names(models)) {
            sub_season <- subset(new_dataframe, season == i)
            sub_preds <- predict.lm(models[[i]], newdata = sub_season)
            preds <- c(preds, sub_preds)
            dates <- c(dates, sub_season[, date_column_index])
        }
    } else if (num_mods == 1) {
        preds <- predict.lm(models[[1]], newdata = new_dataframe)
        dates <- c(dates, new_dataframe[, date_column_index])
    } else {
        stop("List must contain 1, 4, or 12 models!")
    }

    # Return a dataframe ordered by the datesused
    final_df <- data.frame(dates = dates, predictions = preds)
    order_by_date <- order(final_df[, "dates"])

    # Based on the uncertainty, either return just the predictions,
    # or the number of desired ensembles.
    if (uncertainty == "ensemble" & num_ensembles == 1) {
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
generate_ensembles <- function(predictions, num_ensembles, sigma){
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


