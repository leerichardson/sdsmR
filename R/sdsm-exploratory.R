#' Combines individual predictor files
#'
#' This function will take a list of files which has vectors
#' of equal length, and combine them into one dataframe. It is
#' ideal when each one of the predictor variables comes in
#' it's own file.
#'
#' @export
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

#' Generate diagnostic plots for predictors and predictands.
#'
#' This function is meant to replicate the screen variables
#' section of the SDSM tool. It creates two plots, the table
#' which shows the correlations between predictors in all individual
#' months and annually, as well as the
#'
#' @export
#'
#' @param plotname The filepath in which you want to save t
#' @param dataframe a dataframe object which contains a column
#' of dates, the predictand (response) variable, as well as the
#' predictor variablesto explore.
#' @param y The predictand (response) variable name from the
#' same dataframe
#' @return A correlation matrix between predictor variables and
#' two PDF file diagnostic plots.
generate_table <- function(plotname, dataframe, y){

    # Get the initital information such as number of predictors and data-points
    times <- c("january", "february", "march", "april", "may", "june", "july",
               "august", "september", "october", "november", "december", "annual")
    num_obs <- dim(dataframe)[1]
    num_vars <- dim(dataframe)[2]
    response_name <- deparse(substitute(y))

    # Create a matrix to store the results in and
    # name the columns and rows to represent the variables and times
    results <- matrix(NA, nrow=num_vars, ncol=13)
    colnames(results) <- times
    rownames(results) <- names(dataframe)

    png(paste0(plotname, "_cor_matrices.pdf"), height = 15, width=15)
    par(oma=c(1,1,5,1))
    for (i in 1:length(times)) {
        # Subset the initial dataframe based on the month
        if (i < 10) {
            subset_df <- subset(dataframe, format.Date(dates, "%m")== paste0(0, i))
        } else if (i < 13) {
            subset_df <- subset(dataframe, format.Date(dates, "%m")== paste0(i))
        } else {
            subset_df <- dataframe
        }

        # Make sure we are removing the NA's from the dataset. This
        # is only added because the cor function does not have
        # the na.rm option.
        subset_df <- subset_df[complete.cases(subset_df), ]

        # Get the correlation matrix for the top 30ish variables
        tmp <- cor(subset_df[,!names(subset_df) %in% c("dates")], use = "complete")
        top_vars <- order(abs(as.vector(tmp[,response_name])), decreasing=TRUE)
        if (length(top_vars) < 15) {
            corrplot::corrplot(tmp[top_vars, top_vars], method="square", addgrid.col="black",
                               addCoef.col = "black", tl.col="black",
                               title = paste0(times[i], " Top Variable Correlations"))
        } else {
            corrplot::corrplot(tmp[top_vars[1:15], top_vars[1:15]], method="square", addgrid.col="black",
                               addCoef.col = "black", tl.col="black",
                               title = paste0(times[i], " Top Variable Correlations"))
        }


        for (j in 1:num_vars) {
            ## Skip if we are either going through the response variable or
            ## the dates column
            if (class(subset_df[,j]) != "numeric") {
                next
            }
            if (names(subset_df)[j] == response_name) {
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
    png(paste0(plotname, "_cor_explained.pdf"), height = 15, width=15)

    ## Get the correct index of the matrix for a specific page of the PDF
    for(i in 1:npages){
        page <- seq(1+((i-1)*maxrow), i*maxrow)
        if(i == npages){
            page <- seq(1+((i-1)*maxrow), nrow(results))
        }
        ## Correlation Plot to add into the
        corrplot::corrplot(results[page,], method="circle", is.corr=FALSE, cl.lim = c(0,1),
                           addgrid.col="black",addCoef.col = "black", tl.col="black",
                           title = "Monthly and Annual explained \n Variance by Predictor")
    }
    dev.off()

    ## Return the correlation matrix
    return(results)
}
