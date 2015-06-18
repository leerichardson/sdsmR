# PREPARE DATA --------------------------------

# Obtain a list of all the predictor file and combine them into a data frame
predictor_names <- c("uxx", "vxx", "zxx", "xx500", "humxx")
variable_names <- list.files("/home/lee/Dropbox/work/ncar/data/blogsville/ncep1961-90/")
predictor_files <- paste0("/home/lee/Dropbox/work/ncar/data/blogsville/ncep1961-90/", variable_names)
blogsville_predictors <- combine_predictors(predictor_files, var_names = predictor_names)

# Load the predictand variable and combine it into the same data frame
tmax_predictand <- read.table("/home/lee/Dropbox/work/ncar/data/blogsville/observed1961-90/TMAX.DAT", stringsAsFactors = FALSE)
tmax_predictand[10105,] <- NA
tmax_predictand[,1] <- as.numeric(tmax_predictand[,1])
pcrp_predictand <- read.table("/home/lee/Dropbox/work/ncar/data/blogsville/observed1961-90/PRCP.DAT", stringsAsFactors = FALSE)
colnames(tmax_predictand) <- "tmax"
colnames(pcrp_predictand) <- "pcrp"
dates <- seq(as.Date("1960-01-01"), by=1, len = 10957)
blogsville <- cbind(dates, blogsville_predictors, tmax_predictand, pcrp_predictand)



# Split the dataset into training and testing
middle <- floor(nrow(blogsville)/2) + 1
train <- blogsville[1:middle,]
test <- blogsville[(middle+1):nrow(blogsville),]

# CALIBRATE MODEL -----------------------------

# Fit a linear model of the predictors to the predictand
blogsville_model <- lm(tmax ~ uxx + vxx + zxx + xx500 + humxx, data=train)
summary(blogsville_model) # Output matches from SDSM!

# WEATHER GENERATOR ------------------------------------

generate_weather <- predict.lm(blogsville_model, test[,c("uxx", "vxx", "zxx", "xx500", "humxx")])
prediction_results<- cbind(test[,"tmax"], generate_weather)

## Plot the weather generator results
plot(test[,"dates"], prediction_results[,1], col="black", main="Downscaled Max Temperatures vs. Obervations 1976-1990", pch=16, ylab="Temperature Max",
     xlab="Days")
lines(test[,"dates"], prediction_results[,2], col="red")


# SCENARIO GENERATOR ------------------------------

# Read in the GCM MODEL output from both time period's and build the covariate matrices
gcm_file_names <- list.files("data/blogsville/gcmx1961-90/")
s1_files <- paste0("data/blogsville/gcmx1961-90/", gcm_file_names)
s2_files <- paste0("data/blogsville/gcmx2070-99/", gcm_file_names)
s1_predictors <- combine_predictors(s1_files, var_names = predictor_names)
s2_predictors <- combine_predictors(s2_files, var_names = predictor_names)

# Generate the Scenario 1960-1990
preds_s1 <- predict.lm(blogsville_model, s1_predictors[,c("uxx", "vxx", "zxx", "xx500", "humxx")])
s1_dates <-  seq(as.Date("1960-01-01"), by=1, len=length(preds_s1))
plot(s1_dates, preds_s1, type="l", col="red", main="Downscaled Temperature from 1960-1990")

# Generate the Scenario from 2070-2099
preds_s2 <- predict.lm(blogsville_model, s2_predictors[,c("uxx", "vxx", "zxx", "xx500", "humxx")])
s2_dates <-  seq(as.Date("2070-01-01"), by=1, len=length(preds_s2))
plot(s2_dates, preds_s2, type="l", col="red", main="Downscaled Temperature from 2070-2099")


# MONTHLY/SEASONAL MODELS  -------------------------
# Get a list of the months in our dataframe and add a column
# which contains the month into our blogsville dataframe
months <- unique(format.Date(blogsville$dates, "%m"))
blogsville$month <- factor(format.Date(blogsville$dates, "%m"))

# Using the month variable jut created, make another
# variable which represents the season
blogsville$season <- factor(ifelse(blogsville$month %in% c("12", "01", "02"), "winter",
                                   ifelse(blogsville$month %in% c("03", "04", "05"), "spring",
                                          ifelse(blogsville$month %in% c("06", "07", "08"), "summer",
                                                 ifelse(blogsville$month %in% c("09", "10", "11"), "fall", NA)))))
seasons <- unique(blogsville$season)

month_r2 <- data.frame(month = months, r2 = NA, se = NA)
count <- 0
for (m in months) {
    count <- count + 1
    month_dataframe <- subset(blogsville, month == m)
    month_lm <- lm(tmax ~ uxx + vxx + zxx + xx500 + humxx, data = month_dataframe)
    month_r2[count,"se"] <- round(summary(month_lm)$sigma, digits =  3)
    month_r2[count,"r2"] <- round(summary(month_lm)$r.squared, digits =  3)
}

season_r2 <- data.frame(season = seasons, r2 = NA, se = NA)
count <- 0
for (s in seasons) {
    count <- count + 1
    season_dataframe <- subset(blogsville, season == s)
    season_lm <- lm(tmax ~ uxx + vxx + zxx + xx500 + humxx, data = season_dataframe)
    season_r2[count,"se"] <- round(summary(season_lm)$sigma, digits =  3)
    season_r2[count,"r2"] <- round(summary(season_lm)$r.squared, digits =  3)
}


# GENERATE ENSEMBLES -------------------------------

# Generate the deterministic predictions from our test data-set.
# We supply the predict.lm function a new data frame with values of
# the predictors that we want predictions for. In this case, that is
# the testing data set which we created above. Note that if we don't
# provide new data, this function will just return the fitted values.
generate_weather <- predict.lm(blogsville_model,
                        newdata = test[,c("uxx", "vxx", "zxx", "xx500", "humxx")])

# Add white noise to the fitted prediction intervals in order to
# create ensembles of scenario's. In this case, we will create 20 different
# ensembles, which is the default option in the SDSM tool.
num_predictions <- nrow(test)
num_ensembles <- 20
white_noise <- matrix(rnorm(num_predictions * num_ensembles), ncol = num_ensembles)
ensembles <- white_noise + generate_weather

# Plot the ensembles along with a scatterplot of observed vs. predicted
observed <- test[,"tmax"]
plot(observed, pch = 16)
cols <- rainbow(20)
for (i in 1:20) {
    lines(ensembles[,i], col = cols[i])
}



# CONDITIONAL PRECIPITATION ------------------------------------------
# Create 0/1 response variable to model whether there was precipitation
# or not for a given day
train$wetday <- ifelse(train$pcrp > 0, 1, 0)
test$wetday <- ifelse(test$pcrp > 0, 1, 0)

# Fit a Logistic Regression model to the 0/1 response variable. Note that I' mW
# doing the first colname switch because linear models can't use
# variables which start with
prec_step1 <- lm(wetday ~ uxx + vxx + zxx + xx500 + humxx, data = train)

# Fit a Linear Regression model to the subset of data in which it DOES rain
prec_step2 <- lm(pcrp ~ uxx + vxx + zxx + xx500 + humxx , data = train[train$wetday == 1,])

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

# Testing this function on a subset of models
cond_preds <- vector("numeric")
for (i in 1:nrow(test)) {
    cond_preds <- c(cond_preds, predict_row(prec_step1, prec_step2, test[i,]))
}

