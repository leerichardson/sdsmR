check_dataframe <- function(dataframe) {
    if (class(dataframe) != "data.frame") {
        stop("Data must be in a dataframe!")
    }
    print("Verified Dataframe")
}

check_date <- function(dataframe) {
    classes <- unlist(lapply(dataframe, class))
    date_column_index <- as.numeric(which(classes == "Date"))
    if (length(date_column_index) == 0) {
        stop("Need a date column in this data-frame")
    }
    print("Verified Date Column")
}

check_missing <- function(dataframe) {
    num_complete <- sum(complete.cases(dataframe))
    if (num_complete != nrow(dataframe)) {
        stop("Data has missing values.")
    }
}

#' Check to make sure the data-set is appropriate for sdsmR.
#'
#' This function is meant to replicate the quality control
#' section of the SDSM tool. It takes in the data-frame you
#' are hoping to use and verify that it has all the components
#' needed to work with the other sdsmR functions
#'
#' @export
#'
#' @param dataframe The name you want to save the plots as. Ie:
#' if your data comes from the blogsville data-set, you might
#' want to call this "blogsville". If you want to save the plot
#' in a different directory, just specify this in this name:
#' "/home/blogsville" will save the plot's in the home directory.
#' @return Either an error message specifying what went wrong
#' or print statements verifying all of the components of your
#' dataframe
#' @examples
#' # Check the Blogsville data-frame for errors
#' quality_control(blogsville)
#'
#' # Remove the Date column and to return an error
#' quality_control(blogsville[, 2:5])
quality_control <- function(dataframe) {
    check_dataframe(dataframe)
    check_date(dataframe)
    check_missing(dataframe)
}
