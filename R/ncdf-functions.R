# Function to check how many levels of elevation a given
# Net CDF file has
check_elevation <- function(ncdf_filename) {
    # Load in the NCDF file and check to see
    # if it has a levels dimensions
    ncdf <- nc_open(ncdf_filename)
    level_test <- try(ncdf$dim$level)

    # Return true if there is a level variable,
    # and return false if there is no level variables.
    if (is.null(level_test) == TRUE) {
        nc_close(ncdf)
        return(FALSE)
    } else{
        nc_close(ncdf)
        return(TRUE)
    }
}

# Function to extract a time series from the closest gridbox
# based on an input latitude and longitude and a ncdf object. Returns
# a numeric time series with al of the observations from this gridbox
# within a specific file
pull_timeseries <- function(ncdf_filename, lat, lon, level = NULL) {
    # Open the connection of the desired netCDF4 file
    # and pull out the vectors of all possible latitudes and
    # longitudes contained within the file
    ncdf <- nc_open(ncdf_filename)
    latitudes <- ncdf$dim$lat$vals
    longitudes <- ncdf$dim$lon$vals

    # Change around the longitude to the appropriate scale if
    # the input longitude is negative and and longitude scales
    # from the netCDF file is 0-360
    if (sum(longitudes < 0) == 0 & lon < 0) {
        lon <- 360 + lon
    }

    # Locate the indexes which contain the closest gridbox
    # of the latitudes and longitudes specified as inputs to
    # the function
    closest_lat <- min(abs(latitudes - lat))
    lat_index <- which(abs(latitudes - lat) == closest_lat)
    closest_lon <- min(abs(longitudes - lon))
    lon_index <- which(abs(longitudes - lon) == closest_lon)

    # Check to see if the ncdf file has a level index as well.
    # If it does, determine the index from which we will subset
    # the time series. If not,
    if (is.null(level) == FALSE) {
        levels = ncdf$dim$level$vals
        closest_level <- min(abs(level - levels))
        level_index <- which(abs(levels - level) == closest_level)
    }

    # Pull the variable out from the netCDF file and extract
    # the time series based on the closest indexes which we just
    # determined above.
    var <- ncvar_get(ncdf)
    if (is.null(level) == TRUE) {
        ts <- var[lon_index, lat_index, ]
    } else {
        ts <- var[lon_index, lat_index, level_index, ]
    }

    # Close the connection with the netCDF file and return the
    # desired time seriesvec
    nc_close(ncdf)
    return(ts)
}


# Function which inputs either a directory or a file list, and a
# latitude and longitude. Then the function loops through each one of
# the files, extracts the necessary time series, normalizes, then creates
# a lag vector
combine_files <- function(type = "directory", file_list = NULL,
                          directory = NULL, lat, lon, level = NULL) {

    # Get a list of all of the netCDF files in the
    # specified directory. Alternatively, you can also specificy a
    # list of files as opposed to a directory.
    if (type == "directory") {
        files <- list.files(directory)
    } else if (type == "file_list") {
        files <- file_list
    } else {
        stop("type must be either directory or file_list")
    }

    # Loop through all of the air files and pull out the time
    # series at the specified latitude and longitude for each one.
    # Combine all of these files into one large vector
    vec <- vector("double")
    for (file in files) {
        if (is.null(level) == TRUE) {
            timeseries <- pull_timeseries(files, lat, lon)
        } else {
            timeseries <- pull_timeseries(files, lat, lon, level)
        }
        vec <- c(vec, timeseries)
    }

    # Return the final combined and normalized vector
    # built from this directory. Normalize by subtracting off
    # the mean and dividing by the standard error of the entire
    # vector
    normalized_vec <- (vec - mean(vec))/sd(vec)
    return(normalized_vec)
}


# Function to grab the NCEP variables for a given latitude
# and longitude and save the time series in a corresponding
# output directory
get_ncep_vars <- function(input_dir = "/glade/p/image/rmccrary/NCEP2/",
                          output_dir = "/glade/p/work/lrich/ncep_vars/test/",
                          latitude, longitude, levels =c(925, 850, 700, 500)) {

    # Make sure that the ncdf4 package is installed before someone runs
    # this function.
    if (!requireNamespace("ncdf4", quietly = TRUE)) {
        stop("ncdf4 package needed for this function to work. Please install",
             call. = FALSE)
    }

    # Check to see if the directory already exists. If yes, recursively
    # remove it and start an empty directory for saving the file
    # If no, then create the directory and start saving the files here.
    if (dir.exists(output_dir)) {
        unlink(output_dir, recursive = TRUE)
        dir.create(output_dir)
    } else {
        dir.create(output_dir)
    }


    # Get a list of the files, only keep the files with the proper structure,
    # and grab the unique variable names present in each structure.
    filenames <- list.files(input_dir)
    datafiles <- filenames[grep("[[:digit:]].nc", filenames)]
    varname_list <- gsub(".[[:digit:]]{4}.nc", "", datafiles)
    vars <- unique(varname_list)

    # Loop through each one of the variables, grab all of its
    # corresponding netCDF files, and then pull our the needed time series
    # and append them all together.
    for (var in vars) {

        # Grab a list of all the files corresponding to the
        # specific variable we are currently looping through
        files <- datafiles[grep(paste0(var, ".[[:digit:]]{4}.nc"), datafiles)]
        files <- paste0(input_dir, files)

        # Check to see how many levels are in the given net CDF File.
        # If there is only one, then write the time series for the surface
        # and surface lag variables. If
        if (check_elevation(files[1]) == FALSE) {
                ts <- combine_files(type = "file_list", file_list = files,
                                    lat = latitude, lon = longitude)
                n <- length(ts)
                ts_lag <- c(NA, ts[1:(n-1)])
                write.csv(ts, paste0(output_dir, var, "_surface.csv"))
                write.csv(ts_lag, paste0(output_dir, var, "_surface_lag.csv"))

        } else {
            for (level in levels) {
                ts <- combine_files(type = "file_list", file_list = files,
                                    lat = latitude, lon = longitude,
                                    level = level)
                n <- length(ts)
                ts_lag <- c(NA, ts[1:(n-1)])
                write.csv(ts, paste0(output_dir, var, "_", level, ".csv"))
                write.csv(ts_lag, paste0(output_dir, var, "_", level, "_lag.csv"))}
        }
    }
}

