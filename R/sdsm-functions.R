calibrate_model <- function(dataframe, y, date_column, model_type = "annual",
                            autoregression = "false", process = "unconditional") {

    # Grab the date column and response variable column names from the inputs.
    # These will be used later on in various convinient ways.
    response_name <- deparse(substitute(y))
    date_column_name <- deparse(substitute(date_column))

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

