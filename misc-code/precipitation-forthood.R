# Change the working directory to the location on the computer you're using
setwd("/home/lee/Dropbox/work/ncar/")

# Load in the current SDSM functions
source("code/sdsm.R")

# Read in the GLMNet Package for performing Lasso regression
# on this subset of variables.
library("glmnet")

# Tidy the data into a dataframe -----------------

# Read in a lookup table for variable names
file_lookup <- read.csv("data/forthood/forthood_descriptions.csv", stringsAsFactors = FALSE)

# Obtain a list of all the predictor file and observation files
predictor_files <- list.files("data/forthood/forthood_ncep_predictors/")
obs_files <- list.files("data/forthood/forthood_obs/")

# Loop through each of the predictor variables in order to create a data frame
test <- read.table("data/forthood/forthood_ncep_predictors/ncep5hlgfh.dat")
n <- dim(test)[1]
rm(test)

# Set up a matrix to store all of the predictor variables
predictor_matrix <- as.data.frame(matrix(NA, nrow=n, ncol=0))
for(file in predictor_files){

    temp <- read.table(paste0("data/forthood/forthood_ncep_predictors/", file))
    colnames(temp) <- name_lookup(file)
    predictor_matrix <- cbind(predictor_matrix, temp)
    rm(temp)
}

# Create a dataframe with the response variable,
# the dates, and the previously build predictor matrix
dates <- seq(as.Date("1963-01-01"), by=1, len = n)
precFortHood <- read.table("data/forthood//forthood_obs/precFortHood.dat")
colnames(precFortHood) <- "precFortHood"
precMatrix <- cbind(dates, precFortHood, predictor_matrix)

# Generate Diagnostic plots for precipitation in Fort Hood
diagnostics <- generate_table("outputs/images/precip_forthood", precMatrix, precFortHood)

# Remove the excess objects from memory before continuing
rm(precFortHood, predictor_matrix, diagnostics)

# Conditional model for Precipitation --------------------------

# Split the data into training and test data-sets
train <- split_dataframe(precMatrix)$train
test <- split_dataframe(precMatrix)$test
observed <- test[,"precFortHood"]

# Create 0/1 response variable to model whether there was precipitation
# or not for a given day
train$wetday <- ifelse(train$precFortHood > 0, 1, 0)
test$wetday <- ifelse(test$precFortHood > 0, 1, 0)

# Add a fh in front of each of the names so we can use different
# models in R which don't allow     x
colnames(train) <- paste0("fh_", colnames(train))
colnames(test) <- paste0("fh_", colnames(test))


# MODELING STEP 1 -----------------------------------------------------------

# Fit linear and logistic regression models to explore the fit used in the first
# stage of precipitation prediction, classifying whether a day is rainy or not. First,
# use linear and logistic regression models using Rachel's hand picked variables.
linear_step1 <- lm(fh_wetday ~ fh_850mbVorticity_lag + fh_925mbDivergence_lag +
                  fh_700mbRelativeHumidity + fh_850mbRelativeHumidity_lag,
                  data = train)
linear_preds <- predict(linear_step1, test)
linear_classification <- ifelse(linear_preds < .5, 0, 1)
compute_error(linear_classification, test[,"fh_wetday"],
              type = "classification")

# Logistic Regression
logistic_step1 <- glm(fh_wetday ~ fh_850mbVorticity_lag + fh_925mbDivergence_lag +
                         fh_700mbRelativeHumidity + fh_850mbRelativeHumidity_lag,
                         data = train, family = "binomial")
logistic_preds <- predict(logistic_step1, test, type = "response")
logistic_classification <- ifelse(logistic_preds < .5, 0, 1)
compute_error(logistic_classification, test[,"fh_wetday"],
              type = "classification")


# Use lasso and stepwise regression to select models to be used for classification
lasso_step1 <- cv.glmnet(as.matrix(train[,-c(1, 2, ncol(train))]),
                         train[,"fh_wetday"], family = "binomial",
                         type.measure = "class")
lasso_preds_step1 <- predict(lasso_step1, as.matrix(test[,-c(1, 2, ncol(test))]),
                             type = "class", s = "lambda.min")
compute_error(as.numeric(lasso_preds_step1), test[,"fh_wetday"],
              type = "classification")

# Stepwise Regression
null_mod <-lm(fh_wetday ~ 1, data = train[,-c(1,2)])
full_mod <- lm(fh_wetday ~ ., data = train[,-c(1,2)])
aic_step1 <- step(full_mod, scope = list(lower = null_mod, upper = full_mod),
                  direction = "backward", k = 2)
aic_step1_preds <- predict(aic_step1, test)
compute_error(aic_step1_preds, test[,"fh_wetday"], type = "classification")

bic_step1 <- step(full_mod, scope = list(lower = null_mod, upper = full_mod),
                  direction = "backward", k = log(n))
bic_step1_preds <- predict(bic_step1, test)
compute_error(bic_step1_preds, test[,"fh_wetday"], type = "classification")


# MODELING STEP 2 ---------------------------------------------------------

# Fit a Linear Regression model to the subset of data in which it DOES rain
linear_step2 <- lm(fh_precFortHood ~ fh_850mbVorticity_lag + fh_925mbDivergence_lag +
                      fh_700mbRelativeHumidity + fh_850mbRelativeHumidity_lag,
                      data = train[train$fh_wetday == 1,])

# Lasso Predictions



# PREDICTIONS AND DIAGNOSTICS -------------------------------------------------

# Function to use the two calibrated models in to predict precipitation
# amount for a given day in our testing dataset.
predict_row <- function(lm1, lm2, row) {
    # Use the first linear model to predict whether or not there
    # will be a wet day
    wet_day <- predict(lm1, row)
    rand <- runif(1)

    # Determine how much precipitation based on the corresponding prediction.
    # If it's a wet day, determine the amount. However, if it's a non wet day,
    # then return a 0
    if (rand < wet_day) {
        precip_amount <- predict(lm2, row)
    } else {
        return(0)
    }
}

# Testing this function on a choice of two seperate models
preds <- vector("numeric")
for (i in 1:nrow(test)) {
    preds <- c(preds, predict_row(lasso_step1, linear_step2, test[i,]))
}

compute_error(preds, observed)

plot(test$fh_dates, observed, pch = 16)
lines(test$fh_dates, preds, col = "blue")



