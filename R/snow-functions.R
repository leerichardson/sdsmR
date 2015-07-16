# Functions to find the Parameters used for the functional
# model of Snow. Used to estimate the peak, max, and duration.
find_first <- function(x) {
    snow_days <- which(x > 0)
    first_day <- min(snow_days)
    return(first_day)
}

find_peak <- function(x) {
    peak <- max(x)
    return(peak)
}

find_peak_index <- function(x) {
    peak_index <- which(x == find_peak(x))[1]
    return(peak_index)
}

find_duration <- function(x) {
    beginning <- find_first(x)
    peak_index <- which(x == find_peak(x))[1]
    zeros <- which(x == 0)
    after_cycle <- zeros[which(zeros > peak_index)]
    end <- min(after_cycle)
    return(end - beginning)
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
    sparse_enough <- which(num_terms < max_params)
    mse <- model$cvm
    lambda <- model$lambda

    # Choose the model with the minimum cross validation
    # score out of the remaining models.
    min_cv <- min(mse[sparse_enough])
    min_index <- which(mse == min_cv)
    min_lambda <- lambda[min_index]
    return(min_lambda)
}

