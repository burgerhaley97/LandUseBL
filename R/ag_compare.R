# load packages
library(lattice)
library(terra)
# install.packages("magick")
library(magick)
library(dplyr)
library(CropScapeR)


# load data
cdl_all_years <- terra::rast("Final_rasters//bl_cdl_data.tif")
id_test_raster <- terra::rast("Intermediate_rasters//id_rasters.tif")
test_id_mask <- mask(id_test_raster[["2008"]], bl_watershed_trans)
plot(test_id_mask)
freq_id <- freq(test_id_mask)
freq_id
id_mask_0s <- terra::subst(test_id_mask, 0, NA)
plot(id_mask_0s)
freq_after_sub <- freq(id_mask_0s)
freq_after_sub

check_0s <- terra::app(test_id_mask, fun = function(x) {
  ifelse(x ==  0, 0, 1)
})

plot(check_0s)

check_0s_whole_state <- terra::app(id_test_raster[["2008"]], fun = function(x) {
  ifelse(x == 0, 0, 1)
})

plot(check_0s_whole_state)

# Read in shapefile
bl_watershed <- st_read("shapefiles")

# Transforms CRS to match rasters and mask with watershed poly
bl_watershed_trans <- st_transform(bl_watershed, crs = st_crs(cdl_all_years))
mask_test <- mask(cdl_all_years[["2023"]], bl_watershed_trans)
plot(mask_test)
any(is.na(values(mask_test)))
values(mask_test)
mask_freq <- freq(mask_test)



plot(cdl_all_years[["2009"]])
rasters_0s <- terra::subst(cdl_all_years[["2009"]], NA, 0)
mask_test_0s <- mask(cdl_all_years[["2023"]], bl_watershed_trans)
plot(mask_test_0s)
freq_0s <- freq(rasters_0s)
freq_0s
freq_mask_0s <- freq(mask_test_0s)
freq_mask_0s


# look at frequency of crops across our area of interest and link crop data
crop_freq <- terra::freq(cdl_all_years[["2008"]])
crop_freq

# make data frame of pixel counts with GEOID
crop_data <-crop_freq %>%
  # matrix to data.frame
  data.frame(.)

GEOID <- c(1:nrow(crop_data))

crop_data <- crop_data %>%
  mutate(GEOID = GEOID)
crop_data

# join code with crop name
crop_data <- dplyr::left_join(crop_data, linkdata, by = c('value' = 'MasterCat'))
crop_data

# need to reclassify before applying this ag mask
cdl_reclass <- new_class(crop_data, reclass, GEOID, Crop, Count = count, New_Crop)

# Define the values you want to treat as non-agricultural
ag_mask <- c(61:65, 81:83, 87:88, 92, 111:112, 121:124,
             131, 141:143, 152, 176, 181, 190, 195)

# Create a logical raster where matching values are 0 and others are 1
# 0 = non ag, 1 = ag
ag_raster_2008 <- terra::ifel(cdl_all_years[["2008"]] %in% ag_mask, 0, 1)
# Create a logical raster where matching values are 0 and others are 1
ag_raster_2008 <- terra::app(cdl_all_years[["2008"]], fun = function(x) {
  ifelse(x %in% ag_mask, 0, 1)
})



# Apply the function to classify the raster values
ag_raster_2008_nan <- terra::app(cdl_all_years[["2008"]], fun = function(x) {
  ifelse(is.nan(x), 2, ifelse(x %in% ag_mask, 0, 1))
})

# Plot the resulting raster to verify the result
plot(ag_raster_2008_nan)


# Plot some comparisons between the years of ag vs other area
plot(ag_raster_2008)
plot(ag_raster_all)


#------------------------- Save these plots as a GIF -------------------------#
# add gif of non ag vs ag pixels over time
# Define the years
years <- 2008:2023

# Directory to save the images
dir.create("plots")

# Loop through each layer, plot, and save as PNG
for (i in 1:nlyr(ag_raster_all)) {
  layer <- ag_raster_all[[i]]  # Extract the i-th layer
  year <- years[i]  # Corresponding year
  plot_title <- paste("Year:", year)

  # Create the plot and save it as a PNG file
  png_filename <- paste0("plots/plot_", year, ".png")
  png(png_filename)
  plot(layer, main = plot_title, col = c("white", "black"), legend = FALSE)
  dev.off()
}

# Create a GIF from the PNG files
png_files <- list.files("plots", pattern = "plot_.*\\.png", full.names = TRUE)
gif <- image_read(png_files)
gif <- image_animate(gif, fps = 1) # Set frames per second

# Save the GIF
image_write(gif, "plots/animation.gif")

# Clean up
unlink("plots", recursive = TRUE)


# add gif of all pixels over time



