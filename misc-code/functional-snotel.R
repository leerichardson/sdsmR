# Split the dataset into 25 functions ----------------------
niwot <- read.csv("/home/lee/Dropbox/work/ncar/data/snotel/niwot.csv")
niwot$date <- as.Date(niwot$date)
years <- as.numeric(format.Date(niwot$date, "%Y"))
niwot$year <- years

# Remove the leap yeats for the purposes of this analysis
leap_days <- grep("02-29", as.character(niwot$date))
niwot <- niwot[-leap_days, ]

# Loop through each one of the time periods and overlay
# a plot of what the SWE looks like in this time period.
# Also, store the SWE, Temp, and Precip outputs in their
# seperate dataframes, for use in later functions such as
# PCA and estimating parameters.
year_index <- unique(years)
count <- 0
year_df <- data.frame(matrix(NA, nrow = 365, ncol = 33))
precip_df <- data.frame(matrix(NA, nrow = 365, ncol = 33))
temp_df <- data.frame(matrix(NA, nrow = 365, ncol = 33))
row.names(year_df) <- paste0("day_", 1:365)
for (year in year_index) {
    # Index the start and end data of the dataset based on a
    # data wen there is never swe, JULY 15th!
    start_index <- which(niwot$date == paste0(year, "-07-15"))
    end_index <- which(niwot$date == paste0(year + 1, "-07-14"))
    # If the year doesn't have a complete year, then don't make the plot.
    if (length(start_index) == 0 | length(end_index) == 0) {
        next
    } else {
        count <- count + 1

        # Subset the data, and store the correpsonding SWE,
        # precip, and temperatur evalues in seperate dataframes.
        data <- niwot[start_index:end_index, ]

        # SWE
        swe <- data$swe
        year_df[1:length(swe), count] <- swe
        colnames(year_df)[count] <- paste0("year_", year)

        # PRECIP
        precip <- data$precip_accum
        precip_df[1:length(precip), count] <-precip
        colnames(precip_df)[count] <- paste0("year_", year)

        # TEMP
        temp <- data$temp_avg
        temp_df[1:length(temp), count] <- temp
        colnames(temp_df)[count] <- paste0("year_", year)

        print(year)
        if (count == 1) {
            date_range <- data$date
            plot(date_range, data$swe, type = "l", ylim = c(0, 30), col = sample(colours(), 1),
                 main = "SWE Trajectories in Niwot from 1980-2015",
                 xlab = "", ylab = "Snow Water Equivalent", cex.main = 1.8, cex.lab = 1.4)
        } else {
            lines(date_range, data$swe, type = "l", ylim = c(0, 30), col = sample(colours(), 1))
        }
    }
}


# Principal Component Analysis --------------------
library(fda)
year_basis <- create.bspline.basis(c(0, 365), nbasis = 30, norder = 5)
year_fd <- smooth.basis(1:365, as.matrix(year_df), year_basis)$fd
years_pca <- pca.fd(year_fd, nharm = 6)
par(mfrow=c(2,3))
plot.pca.fd(years_pca)
par(mfrow=c(1,1))

# Compute Parameters ----------------------

# These three functions will pull out the parameters
# we are interested in from each year in our data-set.
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

first_days <- sapply(year_df, find_first)
peaks <- sapply(year_df, find_peak)
peak_index <- sapply(year_df, find_peak_index)
duration <- sapply(year_df, find_duration)

par(mfrow = c(1, 1))
plot(duration, type = "l", col = "red", ylim = c(0, 300), main = "Parameters over Time",
     ylab = "parameters", xlab = "year")
lines(peaks, type = "l", col = "blue")
lines(first_days, type = "l", col = "green")
legend("topleft", c("duration", "peaks", "first_days"),
       col = c("red", "blue", "green"), lty = c(1,1))

# Check the correlations
param_matrix <- matrix(c(duration, peaks, first_days), ncol = 3, nrow = length(duration))
cor(param_matrix)


# Estimate Parameters ------------------
corresponding_value <- function(x, index) {
    return(x[index])
}

temp_firstday <- unlist(Map(corresponding_value, precip_df, first_days))
precip_firstday <- unlist(Map(corresponding_value, temp_df, first_days))
first_day <- cbind(first_days, temp_firstday, precip_firstday)

temp_peaks <- unlist(Map(corresponding_value, precip_df, peaks))
precip_peaks <- unlist(Map(corresponding_value, temp_df, peaks))
peak_df <- cbind(peaks, temp_peaks, precip_peaks)

# Spline fitting.
library(fields)
time <- 1:365
Y <- year_df[, 11]
X <- cbind(temp_df[, 11], precip_df[, 11])
X <- scale(X)
objT <- Tps(X, Y, df = 100)
