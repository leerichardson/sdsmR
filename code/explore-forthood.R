# Change the working directory to the location on the computer you're using 
setwd("/home/lee/Dropbox/work/ncar")

# Load in the current SDSM functions
source("code/sdsm.R")

# Load in the glmnet package so we can run the Lasso
library("glmnet")
library("nlme") 

# Change the number of decimals that R will Print out 
options(digits=3)

# SETUP/DIAGNOSTICS ------------------------------

# Read in the file which has all of the variable and corresponding filenames. 
file_lookup <- read.csv("data/forthood/forthood_descriptions.csv", stringsAsFactors = FALSE)
# Obtain a list of all the predictor file and observation files 
predictor_files <- list.files("data/forthood/forthood_ncep_predictors/")
obs_files <- list.files("data/forthood/forthood_obs/")

# Loop through each of the predictor variables in order to create a data frame 
test <- read.table("data/forthood/forthood_ncep_predictors/ncep5hlgfh.dat")
n <- nrow(test)
rm(test)

# Set up a matrix to store all of the predictor variables 
predictor_matrix <- as.data.frame(matrix(NA, nrow=n, ncol=0))
for (file in predictor_files) {
  temp <- read.table(paste0("data/forthood/forthood_ncep_predictors/", file))
  colnames(temp) <- name_lookup(file)
  predictor_matrix <- cbind(predictor_matrix, temp)
  rm(temp)
}

# Create Fort Hood Dates 1963-1994
dates <- seq(as.Date("1963-01-01"), by=1, len = n)

# Create a matrix with both of the response variables
tmaxFortHood <- read.table("data/forthood/forthood_obs/tmaxFortHood.dat")
colnames(tmaxFortHood) <- "tmaxFortHood"
tmaxMatrix <- cbind(dates, tmaxFortHood, predictor_matrix)
tmaxMatrix_notemp <- tmaxMatrix[,-c(grep("Temperature", colnames(tmaxMatrix)))]
rm(tmaxFortHood, dates, file_lookup, predictor_matrix)

nth
# Generate a table of most predictive covariates per month
var_list <- c("dates", "500mbDivergence_lag", "500mbMeridionalWind_lag", "700mbDivergence_lag", 
              "700mbMeridionalWind_lag", "850mbDivergence_lag", "850mbMeridionalWind_lag", 
              "tmaxFortHood")
diags <- generate_table("outputs/images/tmax_forthood", tmaxMatrix[,var_list], tmaxFortHood)



# MONTHLY/SEASONAL MODELS  -------------------------

# Get various monthly, annual, and seasonal models similarto the way SDSM models 
# are calibrated

vars <- c("SurfaceTemperature_lag", "autoregression", "tmaxFortHood", "dates")

month_models <- calibrate_model(tmaxMatrix[,var], y = tmaxFortHood, date_column = dates, 
                      model_type = "monthly", autoregression = "true")
monthly_summaries <- summarize_models(month_models)


seasonal_models <- calibrate_model(tmaxMatrix, y = tmaxFortHood, date_column = dates, 
                                   model_type = "seasonal")
seasonal_summaries <- summarize_models(seasonal_models)

annual_model <- calibrate_model(tmaxMatrix, tmaxFortHood, date_column = dates)
annual_summaries <- summarize_models(annual_model)
# TESTING THE MODELS --------------------------------

# Split the dataset into a training and test set using 60% of the data (the default of this function
train <- split_dataframe(tmaxMatrix)$train
test <- split_dataframe(tmaxMatrix)$test
observed <- test[,"tmaxFortHood"]

# Create the training and test datasets without using the 
# temprature variables 
train_notemp <- split_dataframe(tmaxMatrix_notemp)$train
test_notemp <- split_dataframe(tmaxMatrix_notemp)$test
    
# Train the Lasso and obtain predictions for the held out 
# dataset
lasso_mod <- cv.glmnet(as.matrix(train[,-c(1,2)]), train[,c(2)])
lasso_preds <- predict(lasso_mod, newx= as.matrix(test[,-c(1,2)]), s="lambda.min")

# Build and predict the lasso model without actually using the 
# temperature explicitly. 
lasso_mod_notemp <- cv.glmnet(as.matrix(train_notemp[,-c(1,2)]), train_notemp[,c(2)])
lasso_preds_notemp <- predict(lasso_mod_notemp, newx= as.matrix(test_notemp[,-c(1,2)]), s="lambda.min")

# Train the stepwise regression model and obtain predictions for 
# the held out data set
aic_null<- lm(tmaxFortHood ~ 1, data = train[,-1])
aic_full <- lm(tmaxFortHood ~ ., data = train[,-1])
step_mod <- step(aic_train, scope = list(lower=aic_null, upper=aic_full), direction="both")
step_preds <- predict.lm(step_mod, test[,-2])

# Look into the backwards BIC model to see how well it performs 
bic_null <- lm(tmaxFortHood ~ 1, data = train[,-1])
bic_full <- lm(tmaxFortHood ~ ., data = train[,-1])
bic_mod <- step(bic_full, scope = list(lower = bic_null, upper = bic_full), 
                direction="backward", k = log(n))
bic_preds <- predict.lm(bic_mod, test[,-2])

# Fit Rachel's hand picked model. Note that the first two lines 
# are there because the gls can't take variables starting with 
# numbers. 
colnames(train)[26] <- "GeopotentialHeight500mb"
colnames(test)[26] <- "GeopotentialHeight500mb"

# Add monthly and seasonal variables to use as factor's for Rachel's 
# model
train$month <- factor(format.Date(train$dates, "%m"))
test$month <- factor(format.Date(test$dates, "%m"))

# Using the month variable jut created, make another 
# variable which represents the season 
train$season <- factor(ifelse(train$month %in% c("12", "01", "02"), "winter", 
                            ifelse(train$month %in% c("03", "04", "05"), "spring", 
                                ifelse(train$month %in% c("06", "07", "08"), "summer",         
                                    ifelse(train$month %in% c("09", "10", "11"), "fall", NA)))))

test$season <- factor(ifelse(test$month %in% c("12", "01", "02"), "winter", 
                        ifelse(test$month %in% c("03", "04", "05"), "spring", 
                            ifelse(test$month %in% c("06", "07", "08"), "summer",         
                                ifelse(test$month %in% c("09", "10", "11"), "fall", NA)))))

rachel_mod <- gls(tmaxFortHood ~ MeanSeaLevelPressure + SurfaceMeridionalWind + 
                      SurfaceRelativeHumidity + GeopotentialHeight500mb + factor(month), 
                      correlation = corAR1(form=~1), data=train)
rachel_preds <- predict(rachel_mod, test)

# Compare with Rachel's output from SDSM
sdsm_ensembles <- read.table("data/forthood/rachel/tmax_comparison_lee.out")
sdsm_test <- split_dataframe(sdsm_ensembles)$test
plot(test$dates[1:365], rachel_preds[1:365], ylim = c(0, 50))
for (i in 1:20) {
    lines(test$dates, sdsm_test[,i], col = rainbow(i))
    browser()
}
lines(rachel_preds, col = "red")


# Compute the error on the held out dataset using our three models. 
# This is just a function that computes both the MSE and MAE loss 
# functions. 
compute_error(lasso_preds, observed)
compute_error(lasso_preds_notemp, observed)
compute_error(step_preds, observed)
compute_error(bic_preds, observed)
compute_error(rachel_preds, observed)

data.frame(lasso = compute_error(lasso_preds, observed)$rmse, 
           lasso_notemp = compute_error(lasso_preds_notemp, observed)$rmse, 
           aic = compute_error(step_preds, observed)$rmse, 
           bic = compute_error(bic_preds, observed)$rmse, 
           rachel = compute_error(rachel_preds, observed)$rmse)

# Plot the predictions for our three models against the 
# observed, held out values 
plot(test$dates[1:365], observed[1:365], col="black", pch = 16, main = "Predictions vs. Observed with three different modeling techniques \n 
March 1982- March 1983", xlab = "Time", ylab = "Maximum Temperature")
lines(test$dates, bic_preds, col = "orange")
lines(test$dates, lasso_preds, col="purple")
lines(test$dates, lasso_preds_notemp, col="green")
lines(test$dates, step_preds, col="red")
lines(test$dates, rachel_preds, col="blue")
legend("bottomleft", col = c("purple", "green", "red", "blue"), 
       legend = c("lasso", "lasso_notemp", "stepwise", "rachel"), pch = 16)

# write the predictions to a csv for rachel to explore within her 
# own software. 
dataframe_complete <- data.frame(observed, lasso_preds, lasso_preds_notemp, step_preds, rachel_preds)
colnames(dataframe_complete) <- c("observed", "lasso", "lasso_notemp", "stepwise", "rachel")
write.csv(dataframe_complete, "outputs/temp_predictions.csv")

# Compare different penalization factors based on only. Start by initializing
# both the values of lambda to try along with the vectors to store the test 
# errors
lambda_vals <- lasso_mod$lambda
rmse_vector <- vector(mode = "numeric", length = length(lambda_vals))
mae_vector <- vector(mode = "numeric", length = length(lambda_vals))
num_coefs <- vector("integer", length = length(lambda_vals))

# Loop through each value of lambda and store both the RMSE 
# and MAE test errors in their corresponding vectors
count <- 0
for (lambda in lambda_vals) {
    count <- count + 1
    preds <- predict(lasso_mod, newx= as.matrix(test[,-c(1,2, 81, 82)]), s = lambda)
    rmse_vector[count] <- compute_error(preds, observed)$rmse
    mae_vector[count] <- compute_error(preds, observed)$mae
    num_coefs[count] <- length(coef(lasso_mod, s = lambda)@x)
}

plot(log(lambda_vals), sqrt(lasso_mod$cvm), main = "RMSE of Lasso using 10 Fold Cross Validation on \n Training set compared with Test error out of sample", xlab = "log(lambda)", ylab = "RMSE", col = "red", pch = 16)
text(log(lambda_vals)[seq(1, length(lambda_vals), 10)], 6, labels = num_coefs[seq(1, length(lambda_vals), 10)])
points(log(lambda_vals), rmse_vector, col = "blue", pch = 16)
legend("topleft", col = c("red", "blue"), legend = c("training", "testing"), pch = 16)

# COLINEARITY -------------------------------------------------------

obj <- svd(tmaxMatrix[,-c(1,2)])
d <- obj$d
plot((1-  sqrt( cumsum (d^2) / sum( d^2) ))[1:76], log = "y")

