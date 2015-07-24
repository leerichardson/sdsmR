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
            timeseries <- pull_timeseries(file, lat, lon)
        } else {
            timeseries <- pull_timeseries(file, lat, lon, level)
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

# Function to combine the CSV's into one giant dataframe
combine_csvs <- function(input_dir, output_dir,
                         csv_name = "combined_df.csv") {
    # Get a list of all the files inside an input diretory
    files <- list.files(input_dir)

    # Pull the number of expected rows from a given file. We
    # will check in the code to pull the variables that they
    # match this number of rows
    sample_dataframe <- read.csv(paste0(input_dir, files[1]))
    expected_rows <- nrow(sample_dataframe)

    for (file in files) {
        print(file)

        tmp <- read.csv(paste0(input_dir, file))
        if (nrow(tmp) != expected_rows) {
            files = files[-which(files == file)]
            print("SKIPPED")
            print(paste0(nrow(tmp)))
            next
        }

        # Check to see if this is the first file we are
        # looping through. If yes, then create a new dataframe.
        # If no, then just append
        if (file == files[1]) {
            dataframe <- data.frame(row1 =  tmp)
        } else {
            dataframe <- cbind(dataframe, tmp)
        }
    }

    # Add in the column names to the final dataframe based
    # on the filenames, removing the .csv
    var_names <- gsub(".csv", "", files)
    colnames(dataframe) <- var_names

    # Write the final csv as a dataframe in the output
    # directory and return the dataframe
    write.csv(dataframe, paste0(output_dir, csv_name), row.names = FALSE)
    return(dataframe)
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
    if (file.exists(output_dir)) {
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
    count <- 0
    for (var in vars) {
        count <- count + 1
        print(paste0("Variable: ", var, ", Number: ", count,  " Of ", length(vars)))

        # Grab a list of all the files corresponding to the
        # specific variable we are currently looping through
        files <- datafiles[grep(paste0(var, ".[[:digit:]]{4}.nc"), datafiles)]
        files <- paste0(input_dir, files)

        # Remove the files from 2011 and 2012
        num_chars <- unique(nchar(files))
        if (length(num_chars) > 1) {
            stop("Should only be one length for all files")
        }
        years <- as.numeric(substr(files, num_chars - 6, num_chars - 3))
        if (length(which(years > 2010)) == 0) {
            message("Data until 2010!")
        } else {
            files <- files[-which(years > 2010)]
        }

        # Check to see how many levels are in the given net CDF File.
        # If there is only one, then write the time series for the surface
        # and surface lag variables. If
        if (check_elevation(files[1]) == FALSE) {
                ts <- combine_files(type = "file_list", file_list = files,
                                    lat = latitude, lon = longitude)
                n <- length(ts)
                ts_lag <- c(NA, ts[1:(n-1)])
                write.csv(ts, paste0(output_dir, var, "_surface.csv"),
                          row.names = FALSE)
                write.csv(ts_lag, paste0(output_dir, var, "_surface_lag.csv"),
                          row.names = FALSE)

        } else {
            for (level in levels) {
                ts <- combine_files(type = "file_list", file_list = files,
                                    lat = latitude, lon = longitude,
                                    level = level)
                n <- length(ts)
                ts_lag <- c(NA, ts[1:(n-1)])
                write.csv(ts, paste0(output_dir, var, "_", level, ".csv"),
                          row.names = FALSE)
                write.csv(ts_lag, paste0(output_dir, var, "_", level, "_lag.csv"),
                          row.names = FALSE)
            }
        }
    }

    combine_csvs(input_dir = output_dir, output_dir = "/glade/p/work/lrich/", csv_name = "combined_df.csv")
}

get_narcaap_vars <- function(input_dir = "/glade/p/image/rmccrary/narccap/T6/NCEP2/CRCM/ncep/",
                                      output_dir = "/glade/p/work/lrich/ncep_vars/narcaap/",
                                      latitude, longitude) {

    # Make sure that the ncdf4 package is installed before someone runs
    # this function.
    if (!requireNamespace("ncdf4", quietly = TRUE)) {
        stop("ncdf4 package needed for this function to work. Please install",
             call. = FALSE)
    }

    # Check to see if the directory already exists. If yes, recursively
    # remove it and start an empty directory for saving the file
    # If no, then create the directory and start saving the files here.
    if (file.exists(output_dir)) {
        unlink(output_dir, recursive = TRUE)
        dir.create(output_dir)
    } else {
        dir.create(output_dir)
    }

    # Get a list of all th filenames in which we want to
    # pull the time series data from
    filenames <- list.files(input_dir)

    # Loop through each file, extract the time series, save
    # the time series inside the output directory
    for (file in filenames) {
        print(file)
        ts <- pull_timeseries(ncdf_filename = paste0(input_dir, file),
                              lat = latitude, lon = longitude)
        norm_ts <- (ts - mean(ts))/sd(ts)
        split_name <- unlist(strsplit(file, "_"))
        var_name <- split_name[1]
        level <- gsub("\\..*$", "", split_name[4])
        write.csv(norm_ts, paste0(output_dir, var_name, "_", level, ".csv"),
                  row.names = FALSE)
    }
    combine_csvs(input_dir = output_dir, output_dir = "/glade/p/work/lrich/", csv_name = "combined_df.csv")
}
