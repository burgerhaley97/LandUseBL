# Or the development version from GitHub
remotes::install_github("mikejohnson51/AOI")
library(AOI)
remotes::install_github("mikejohnson51/climateR")
library(climateR)


utah <- aoi_get(state = "UT", county = "all")
chirps <- getCHIRPS(AOI = utah,
                   varname = "precip",
                   startDate = "2008-10-29",
                   endDate  = "2008-11-06")

plot(chirps[[1]][1])
chirps

gridMet <- getGridMET(AOI = utah,
                    varname = "pr",
                    startDate = "2008-01-01",
                    endDate  = "2008-12-31")
class(gridMet[[1]])

# get summary stats for each 5km by 5km pixel from 366 layers of daily precip
max <- terra::app(gridMet[[1]], fun = "max")
plot(max)
mean <- terra::app(gridMet[[1]], fun = "mean")
plot(mean)
median <- terra::app(gridMet[[1]], fun = "median")
plot(median)


################################################################################
# CSV from CHCND data from Ben
################################################################################
# station USC00424856 (with data up to yesterday)

df <- read.csv("C:\\Users\\A02425259\\Downloads\\Daily_USC00424856.csv", stringsAsFactors = FALSE)
view(df)

# Filter the data frame based on the last four characters of the Date column
# years 2007-2023
# Temps are in tenths of degrees C (convert to C by diving by 10)
df_subset <- df %>%
  filter(str_sub(Date, -4) %in% as.character(2007:2023))
tail(df_subset)


###############################################################################
# GHCND Gridded climate data NClimGrid (monthly averages 1895 to present)
###############################################################################
# Install and load the ncdf4 package
install.packages("ncdf4")
library(ncdf4)

# Open the NetCDF file
ncfile <- nc_open("C:\\Users\\A02425259\\Downloads\\nclimgrid-tavg.nc")

# Print the contents of the NetCDF file
print(ncfile)

# Extract a variable
data <- ncvar_get(ncfile, "tavg")
dim(data)

# get layers for each month from 2007 to 2023
lyr_min <- (1553-5)- 12 * (24-7)
lyr_min

lyr_max <- (1553-5)
lyr_max

lon <- ncvar_get(ncfile, "lon")
lat <- ncvar_get(ncfile, "lat", verbose = F)
t <- ncvar_get(ncfile, "time")

nc_close(ncfile)

data.slice <- data[, ,1344:1548]
crs <- crs(bl_crop)
class(data.slice)
r <- rast(data.slice, crs= crs)
crs_r <- crs(r)
plot(r$lyr.1)

t <- t(r$lyr.1)
r_flip <- flip(t, "vertical")
plot(r_flip)


# crop to just bear lake area
crop_bl_area <- bl_crop[["2008"]]
crop_bl_area <- project(crop_bl_area, crs_r)
tavg_data <- crop(r, crop_bl_area)
plot(bl_crop)

ext(r)
crs(r) == crs(bl_crop)
ext(bl_crop)

r
