# Load in the Net CDF Functions used 
source("/glade/u/home/lrich/code/ncdf-functions.R")

# Load in the ncdf4 package 
library("ncdf4")

# Load in the input and output diectories, which 
# contains the input data, and location for stoage, respectively. 
# Next, specify the levels that we will look for inside the NCEP 
# variables, along with the appropriate latitude and longitude.  
input_directory <- "/glade/p/image/rmccrary/NCEP2/"
output_directory <- "/glade/p/work/lrich/ncep_vars/"
levels <- c(925, 850, 700, 500)
latitude <- 40.04
longitude <- -105.54

# Get a list of the files, only keep the files with the proper structure, 
# and grab the unique variable names present in each structure. 
filenames <- list.files(input_directory)
datafiles <- filenames[grep("[[:digit:]].nc", filenames)]
varname_list <- gsub(".[[:digit:]]{4}.nc", "", datafiles)
vars <- unique(varname_list)

# Loop through each one of the variables, grab all of its 
# corresponding netCDF files, and then pull our the needed time series 
# and append them all together. 
for (var in vars) {
    
     if (var != "shum") {
         next
     } else {
         browser()
     }
    
    # Grab a list of all the files corresponding to the 
    # specific variable we are currently looping through 
    files <- datafiles[grep(paste0(var, ".[[:digit:]]{4}.nc"), datafiles)]
    files <- paste0(input_directory, files)
    print(files)
    
    # Check to see how many levels 
    if (check_elevation(files[1]) == FALSE) {
        ts <- combine_files(type = "file_list", file_list = files, 
                            lat = latitude, lon = longitude)
        write.csv(ts, paste0(output_directory, var, "_surface.csv"))
        
    } else {
        for (level in levels) {
            ts <- combine_files(type = "file_list", file_list = files, 
                                lat = latitude, lon = longitude, level = level)    
            write.csv(ts, paste0(output_directory, var, "_", level, ".csv"))
        }
    }
}
