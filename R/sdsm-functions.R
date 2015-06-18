#' Combines individual predictor files
#'
#' This function will take a list of files which has vectors
#' of equal length, and combine them into one dataframe. It is
#' ideal when each one of the predictor variables comes in
#' it's own file.
#'
#' @param file_list A list of files path for which t
#' here are vectors of data we want to combine into one dataframe
#' @param var_names An optional character vector which specifies the
#' names of the columes in the resulting dataframe
#' @return A dataframe of the equal length vectors merged together
combine_predictors <- function(file_list, var_names = NULL){

    ## Calculate the number of rows in each file
    temp <- read.table(file_list[1])
    n <- dim(temp)[1]
    rm(temp)

    ## Append all of the files into one data frame
    predictor_matrix <- as.data.frame(matrix(NA, nrow=n, ncol=0))
    count <- 0
    for(file in file_list){
        count <- count +1
        single_pred <- read.table(file)
        if(length(var_names) > 0){
            colnames(single_pred) <- var_names[count]
        }
        predictor_matrix <- cbind(predictor_matrix, single_pred)
    }
    return(predictor_matrix)
}

# Function to look up the variable name from the file name
name_lookup <- function(filename){
    if (length(which(file_lookup[,"normal"] == filename)) == 0) {
        var.name <- file_lookup[which(file_lookup[,"one.day.lag"] == filename),"name"]
        var.name <- paste0(var.name, "_lag")
    } else {
        var.name <- file_lookup[which(file_lookup[,"normal"] == filename),"name"]
    }
    ## Remove the whitespace from the variable names
    var.name <- gsub(" ", "", var.name)
    return(var.name)
}

generate_table <- function(plotname, dataframe, y){
    ## Get the initital information such as number of predictors and data-points
    times <- c("january", "february", "march", "april", "may", "june", "july",
                "august", "september", "october", "november", "december", "annual")
    num_obs <- dim(dataframe)[1]
    num_vars <- dim(dataframe)[2]
    response_name <- deparse(substitute(y))

    ## Create a matrix to store the results in and
    ## name the columns and rows to represent the variables and times
    results <- matrix(NA, nrow=num_vars, ncol=13)
    colnames(results) <- times
    rownames(results) <- names(dataframe)

    pdf(paste0(plotname, "_cor_matrices.pdf"), height = 15, width=15)
    par(oma=c(1,1,5,1))
    for(i in 1:length(times)){
        ## Subset the initial dataframe based on the month
        if(i < 10){
            subset_df <- subset(dataframe, format.Date(dates, "%m")== paste0(0, i))
        } else if(i < 13){
            subset_df <- subset(dataframe, format.Date(dates, "%m")== paste0(i))
        } else{
            subset_df <- dataframe
        }
        ## Get the correlation matrix for the top 30ish variables
        tmp <- cor(subset_df[,!names(subset_df) %in% c("dates")])
        top_vars <- order(abs(as.vector(tmp[,response_name])), decreasing=TRUE)
        if (length(top_vars) < 15) {
            corrplot(tmp[top_vars, top_vars], method="square", addgrid.col="black",
                     addCoef.col = "black", tl.col="black",
                     title = paste0(times[i], " Top Variable Correlations"))
        } else {
            corrplot(tmp[top_vars[1:15], top_vars[1:15]], method="square", addgrid.col="black",
                     addCoef.col = "black", tl.col="black",
                     title = paste0(times[i], " Top Variable Correlations"))
        }


        for(j in 1:num_vars){
            ## Skip if we are either going through the response variable or
            ## the dates column
            if(class(subset_df[,j]) != "numeric"){
                next
            }
            if(names(subset_df)[j] == response_name){
                next
            }

            r <- cor(subset_df[,j], subset_df[,response_name])
            results[j, i] <- round(r^2, digits=2)
        }
    }
    dev.off()

    ## Remove the rows with nothing in them, either the response variable or the dates
    results <- results[!is.na(results[,"annual"]),]

    ## Order by the most correlated annually
    annual_rank <- order(as.vector(results[,"annual"]), decreasing=TRUE)
    results <- results[annual_rank,]

    ## Create a PDF of the dataframe
    maxrow = 15;
    npages = ceiling(nrow(results)/maxrow);
    pdf(paste0(plotname, "_cor_explained.pdf"), height = 15, width=15)

    ## Get the correct index of the matrix for a specific page of the PDF
    for(i in 1:npages){
        page <- seq(1+((i-1)*maxrow), i*maxrow)
        if(i == npages){
            page <- seq(1+((i-1)*maxrow), nrow(results))
        }
        ## Correlation Plot to add into the
        corrplot(results[page,], method="circle", is.corr=FALSE, cl.lim = c(0,1),
                 addgrid.col="black",addCoef.col = "black", tl.col="black",
                 title = "Monthly and Annual explained \n Variance by Predictor")
    }
    dev.off()

    ## Return the correlation matrix
    return(results)
}

compute_error <- function(predictions, observations, type = "regression"){
    if (type == "regression") {
        rmse <- sqrt(sum((predictions - observed)^2)/length(predictions))
        mae <- sum(abs(predictions - observed))/length(predictions)
        return(list(rmse=rmse, mae=mae))
    } else if (type == "classification") {
        abs_error <- sum(abs(predictions - observations))/length(predictions)
        return(abs_error)
    }
}

split_dataframe <- function(dataframe, percentage=.6){
    split_num <- floor(nrow(dataframe)*.6)
    train  <- dataframe[1:split_num,]
    test <- dataframe[(split_num+1):nrow(dataframe),]
    return(list(train = train, test = test))
}


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

