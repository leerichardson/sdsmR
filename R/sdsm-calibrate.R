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

# Function to add in the autoregressive vairable into
# the linear model
add_autoregression <- function(dataframe, response_name) {
    response <- ts(dataframe[, response_name])
    lagged <- as.vector(lag(response, 1))
    dataframe$autoregression <- lagged
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
        dataframe <- add_autoregression(dataframe, response_name)
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
        dataframe <- add_month(dataframe)
        months <- unique(dataframe$month)

        # Loop through each one of the months and fit a linear model to the
        # corresponding dataframe. Then, store each monthly linear model as an
        # element of a list in which we have created above.
        for (m in months) {
            month_dataframe <- subset(dataframe, month == m)
            exclude_index <- -which(names(month_dataframe) %in% c(response_name, date_column_name, "month"))

            # Based on the process, fit either a conditional or unconditional
            # model to the data. Store the result of this fitting procedure in a list.
            # If conditional, there will be a list of two models stored
            if (process == "unconditional") {
                month_lm <- lm(month_dataframe[, response_name] ~ .,
                               data = month_dataframe[, exclude_index])
            } else if (process == "conditional") {
                month_lm <- fit_conditional(month_dataframe[, exclude_index],
                                            month_dataframe[, response_name])
            } else {
                stop("process must be conditional or unconditional")
            }
            models[[as.numeric(m)]] <- month_lm
        }

    } else if (model_type == "seasonal") {
        # Allocate four slots in a list in order to store each of
        # the four seasonal models. Next, use the add month and
        # add season functions to add on these columns to the
        # dataframe. Finally, pull the unique season names and
        # set the names for the list for storing models equal to these
        # seasons
        models <- vector(mode = "list", length = 4)
        dataframe <- add_month(dataframe)
        dataframe <- add_season(dataframe, "month")
        seasons <- unique(dataframe$season)
        names(models) <- seasons

        # Loop through each one of the months and fit a linear model to the
        # corresponding dataframe. Then, store each monthly linear model as an
        # element of a list in which we have created above.
        for (s in seasons) {
            # Subset the dataframe to only contain the season we are
            # interested in, and pull out the indexes we want to exclude from
            # the modeling
            season_dataframe <- subset(dataframe, season == s)
            exclude_index <- -which(names(season_dataframe) %in% c(response_name, date_column_name, "month", "season"))

            # Fit a linear model based on whether the process is conditional or
            # unconditional
            if (process == "unconditional") {
                season_lm <- lm(season_dataframe[, response_name] ~ .,
                               data = season_dataframe[, exclude_index])
            } else if (process == "conditional") {
                season_lm <- fit_conditional(season_dataframe[, exclude_index],
                                             season_dataframe[, response_name])
            } else {
                stop("process must be conditional or unconditional")
            }
            models[[as.character(s)]] <- season_lm
        }

    } else if (model_type == "annual") {
        models <- vector(mode = "list", length = 1)
        names(models) <- "annual"
        exclude_index <- -which(names(dataframe) %in% c(response_name, date_column_name))

        # Fit a linear model based on whether the process is conditional or
        # unconditional
        if (process == "unconditional") {
            annual_lm <- lm(dataframe[, response_name] ~ .,
                            data = dataframe[, exclude_index])
        } else if (process == "conditional") {
            annual_lm <- fit_conditional(dataframe[, exclude_index],
                                         dataframe[, response_name])
        } else {
            stop("process must be conditional or unconditional")
        }
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

# Function to fit a conditional model. Used in the calibrate_model
# funtion if the process = "conditional" option is specified.
fit_conditional <- function(dataframe, cond_var) {
    dataframe$wetday <- ifelse(cond_var > 0, 1, 0)
    step1 <- lm(wetday ~ ., data = dataframe)
    data_subset <- subset(dataframe, wetday == 1)
    cond_var_subset <- cond_var[cond_var > 0]
    step2 <- lm(cond_var_subset ~ ., data = data_subset[, -ncol(dataframe)])
    return(list(step1 = step1, step2 = step2))
}

