library(CropScapeR)
library(dplyr)
library(sf)
library(terra)
library(raster)
library(ggplot2)

#--------------------- Update API function to return SpatRaster ---------------#
# Get CDL Data using a bounding box
GetCDLDataST <- function(fips, year, tol_time, save_path, readr){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&fips=', fips)
  data <- tryCatch(httr::GET(url, httr::timeout(tol_time)), error = function(x) x)

  if(class(data)[1] != 'response') stop(paste0('No response from the server. Error message from server is:\n', data$message))
  dataX <- httr::content(data, 'text')
  num <- gregexpr('returnURL', dataX)
  url2 <- substr(dataX, num[[1]][1]+10, num[[1]][2]-3)

  if(!is.null(save_path)){
    file_info <- httr::GET(url2, httr::write_disk(path = save_path, overwrite = T))
    message(paste0('Data is saved at:', save_path))
    if(isTRUE(readr)){
      outdata <- terra::rast(save_path)
    }else{
      outdata <- NULL
    }
  }else{
    if(!isTRUE(readr)) warning('readr focred to be TRUE, because no save_path is provided. \n')
    outdata <- terra::rast(url2)
  }
  return(outdata)
}

#______________________________________________________________________________
#                                  Test with One Year
#______________________________________________________________________________


# try getting data for target states
ut_cdl <- GetCDLDataST(fips = 49, year = 2010, tol_time = 120, save_path = NULL, readr = TRUE)
plot(ut_cdl)

id_cdl <- GetCDLDataST(fips = 16, year = 2010, tol_time = 120, save_path = NULL, readr = TRUE)
plot(id_cdl)

wy_cdl <- GetCDLDataST(fips = 56, year = 2010, tol_time = 120, save_path = NULL, readr = TRUE)
plot(wy_cdl)

#------------------ Try removing 0s and replace with NA ----------------------#
subset_ut <- ut_cdl
subset_ut[subset_ut <= 0] <- NA

subset_id <- id_cdl
subset_id[subset_id <= 0] <- NA

subset_wy <- wy_cdl
subset_wy[subset_wy <= 0] <- NA

merge_states <- merge(subset_ut, subset_id, subset_wy, na.rm = TRUE)

plot(merge_states)


#----------------------- Try masking with Watershed --------------------------#
bl_watershed <- st_read("shapefiles")
bl_watershed_trans <- st_transform(bl_watershed, crs = st_crs(merge_states))
mask_test <- mask(merge_states, bl_watershed_trans)
plot(mask_test)
unique(mask_test)

#______________________________________________________________________________
#                                  Try with All Years
#______________________________________________________________________________
# Load necessary libraries
library(httr)
library(terra)

# Define the function
GetCDLDataST <- function(fips, year, tol_time, save_path = NULL, readr = TRUE) {
  # Initialize a list to store the results
  results <- list()

  # Iterate over each combination of fips and year
  for (f in fips) {
    for (y in year) {
      # Construct the URL
      url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', y, '&fips=', f)

      # Make the GET request
      data <- tryCatch(httr::GET(url, httr::timeout(tol_time)), error = function(x) x)

      # Check if the request was successful
      if (class(data)[1] != 'response') {
        warning(paste0('No response from the server for FIPS: ', f, ' and Year: ', y, '. Error message: ', data$message))
        next
      }

      # Extract the URL for the data
      dataX <- httr::content(data, 'text')
      num <- gregexpr('returnURL', dataX)
      url2 <- substr(dataX, num[[1]][1]+10, num[[1]][2]-3)

      # Define the output object
      outdata <- NULL

      # Handle saving and reading of the data
      if (!is.null(save_path)) {
        # Define the file path for saving
        file_path <- file.path(save_path, paste0("CDL_FIPS_", f, "_Year_", y, ".tif"))

        # Save the file
        file_info <- httr::GET(url2, httr::write_disk(path = file_path, overwrite = TRUE))
        message(paste0('Data is saved at:', file_path))

        # Read the data if required
        if (isTRUE(readr)) {
          outdata <- terra::rast(file_path)
        }
      } else {
        warning('readr forced to be TRUE, because no save_path is provided. \n')
        outdata <- terra::rast(url2)
      }

      # Store the result in the list
      results[[paste0("FIPS_", f, "_Year_", y)]] <- outdata
    }
  }

  return(results)
}

# Example usage
fips <- c("16", "56", "49")
year <- as.character(2008:2023)
tol_time <- 180
save_path <- NULL
readr <- TRUE

cdl_by_state <- GetCDLDataST(fips, year, tol_time, save_path, readr)
id_all_years <- c(cdl_by_state[1:16])
id_all_years

#_____________________________________________________________________________
#               Try to get all years for one state at a time
#_____________________________________________________________________________
# Load necessary libraries
library(httr)
library(terra)

# Get CDL Data using a bounding box for multiple years
GetCDLDataST <- function(fips, years, tol_time, save_path, readr) {
  # Initialize an empty list to store the results
  results <- list()

  # Loop through each year
  for (year in years) {
    url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&fips=', fips)
    data <- tryCatch(httr::GET(url, httr::timeout(tol_time)), error = function(x) x)

    if (class(data)[1] != 'response') stop(paste0('No response from the server. Error message from server is:\n', data$message))
    dataX <- httr::content(data, 'text')
    num <- gregexpr('returnURL', dataX)
    url2 <- substr(dataX, num[[1]][1] + 10, num[[1]][2] - 3)

    if (!is.null(save_path)) {
      file_name <- paste0(save_path, "_", year, ".tif")  # Save each file with year in the name
      file_info <- httr::GET(url2, httr::write_disk(path = file_name, overwrite = TRUE))
      message(paste0('Data for year ', year, ' is saved at: ', file_name))
      if (isTRUE(readr)) {
        outdata <- terra::rast(file_name)
      } else {
        outdata <- NULL
      }
    } else {
      if (!isTRUE(readr)) warning('readr forced to be TRUE, because no save_path is provided.\n')
      outdata <- terra::rast(url2)
    }

    # Store the result in the list
    results[[as.character(year)]] <- outdata
  }

  return(results)
}


years <- as.character(2008:2023)  # Vector of years
tol_time <- 120 # Timeout in seconds
save_path <- NULL  # Directory to save files
readr <- TRUE  # Whether to read the files after downloading
# Access the raster data for a specific year
cdl_2010 <- cdl_data[["2010"]]

# Get CDL Data for the specified Idaho code and years
fips <- "16"  # State FIPS code
cdl_data_id <- GetCDLDataST(fips, years, tol_time, save_path, readr)
id_rasters <- terra::rast(cdl_data_id[1:16]) # create raster "stack"
plot(id_rasters[["2008"]])

# Define the path where you want to save the file
output_path <- "Intermediate_rasters/id_rasters.tif"
# Write the SpatRaster to a file
terra::writeRaster(id_rasters, filename = output_path)
test_id <- rast(output_path)
plot(test_id[["2008"]])
unique(test_id[["2008"]])

# Get CDL Data for the specified Utah code and years
fips <- "49"  # State FIPS code
cdl_data_ut <- GetCDLDataST(fips, years, tol_time, save_path, readr)
ut_rasters <- terra::rast(cdl_data_ut[1:16])

# Define the path where you want to save the file
output_path <- "Intermediate_rasters/ut_rasters.tif"
# Write the SpatRaster to a file
terra::writeRaster(ut_rasters, filename = output_path)

years[1]

# Get CDL Data for the specified Wyoming code and years
fips <- "56"  # State FIPS code
cdl_data_wy <- GetCDLDataST(fips, years, tol_time, save_path, readr)
wy_rasters <- terra::rast(cdl_data_wy[1:16])

# Define the path where you want to save the file
output_path <- "Intermediate_rasters/wy_rasters.tif"
# Write the SpatRaster to a file
terra::writeRaster(wy_rasters, filename = output_path)

#_____________________________________________________________________________
#                 Try different ways to merge
#_____________________________________________________________________________

# merge with mosaic max function (soooo slow)
test_mosaic <- terra::mosaic(ut_rasters[["2008"]], id_rasters[["2008"]], wy_rasters[["2008"]], fun = max)


# merge with NAs instead of 0s
# try masking first to reduce time, making automatically creates NAs so not need
# to convert 0s to NAs after
test_id <- rast("Intermediate_rasters/id_rasters.tif")
test_ut <- rast("Intermediate_rasters/ut_rasters.tif")
test_wy <- rast("Intermediate_rasters/wy_rasters.tif")

bl_watershed <- st_read("shapefiles")
plot(bl_watershed$geometry)
bl_watershed_trans <- st_transform(bl_watershed, crs = st_crs(test_id))
mask_id <- mask(test_id[["2008"]], bl_watershed_trans)
mask_ut <- mask(test_ut[["2008"]], bl_watershed_trans)
mask_wy <- mask(test_wy[["2008"]], bl_watershed_trans)

id_rasters_na <- terra::subst(mask_id, 0, NA)
ut_rasters_na <- terra::subst(mask_ut, 0, NA)
wy_rasters_na <- terra::subst(mask_wy, 0, NA)
plot(wy_rasters_na)
plot(ut_rasters_na)
plot(id_rasters_na)

# check can we convert NAs back to 0 values if needed (yes)
test_merge <- terra::merge(id_rasters_na, ut_rasters_na, na.rm = TRUE)
plot(test_merge)
test_merge_w0s <- terra::subst(test_merge, NA, 0)
plot(test_merge_w0s)
unique(test_merge_w0s)

test_merge_all <- terra::merge(id_rasters_na, ut_rasters_na, wy_rasters_na, na.rm = TRUE)
plot(test_merge_all)


#____________________________________________________________________________
#                       Try with All Data
#____________________________________________________________________________

# Read in raster for each state for all years
test_id <- rast("Intermediate_rasters/id_rasters.tif")
test_ut <- rast("Intermediate_rasters/ut_rasters.tif")
test_wy <- rast("Intermediate_rasters/wy_rasters.tif")

# Read in shapefile
bl_watershed <- st_read("shapefiles")

# Transforms CRS to match rasters and mask with watershed poly
bl_watershed_trans <- st_transform(bl_watershed, crs = st_crs(test_id))
id_mask <- mask(test_id, bl_watershed_trans)
ut_mask <- mask(test_ut, bl_watershed_trans)
wy_mask <- mask(test_wy, bl_watershed_trans)


plot(wy_mask[["2009"]])
plot(ut_mask[["2008"]])
plot(id_mask[["2008"]])

# Substitute 0s with NAs
id_rasters_nas <- terra::subst(id_mask, 0, NA)
ut_rasters_nas <- terra::subst(ut_mask, 0, NA)
wy_rasters_nas <- terra::subst(wy_mask, 0, NA)

# Merge all masks while ignoring NAs
bl_cdl_data <- terra::merge(id_rasters_nas, ut_rasters_nas, wy_rasters_nas,
                            na.rm = TRUE)
bl_cdl_data
plot(bl_cdl_data[["2008"]])
plot(bl_cdl_data[["2012"]])

# Save final SpatRaster to file
# Define the path where you want to save the file
file_path <- "Final_rasters/bl_cdl_data.tif"
# Write the SpatRaster to a file
terra::writeRaster(bl_cdl_data, filename = file_path)








