# load packages
library(lattice)
library(terra)
# install.packages("magick")
library(magick)
library(dplyr)
library(CropScapeR)
install.packages("landscapemetrics")
library(landscapemetrics)
library(stringr)

##################### Step 1 Reclassify Data #################################
# Load the data
cdl_all_years <- terra::rast("Final_rasters//bl_cdl_data.tif")

# Define reclassification masks
# non-ag values
ag_mask <- c(61:65, 81:83, 87:88, 92, 111:112, 121:124,
             131, 141:143, 152, 176, 181, 190, 195)

# alfalfa and other hay
alfalfa <- c(36:37)

# corn, soy, cotton, wheat (and mixed categories)
major_ag <- c(1, 2, 5, 12, 13, 22:24, 26, 225:226, 228, 234, 236, 238:241, 254)

# Reclassify the raster values into non-ag = 0, ag = 1, and NaN = NaN
ag_raster_2008_nan <- terra::app(cdl_all_years[["2008"]], fun = function(x) {
  ifelse(is.nan(x), NaN, ifelse(x %in% ag_mask, 0, 1))
})

# plot the reclassified raster
plot(ag_raster_2008_nan)

# Reclassify the raster values into non-ag = 0, ag = 1, alfalfa = 2, and NaN = NaN
ag_raster_2008_alf <- terra::app(cdl_all_years[["2008"]], fun = function(x) {
  ifelse(is.nan(x), NaN,
         ifelse(x %in% alfalfa, 2,
                ifelse(x %in% ag_mask, 0, 1)))
})

# plot the reclassified raster
plot(ag_raster_2008_alf)

# reclassify into non_ag = 0,  ag = 1, major_ag = 2, and NaN = NaN
ag_raster_2008_maj <- terra::app(cdl_all_years[["2008"]], fun = function(x) {
  ifelse(is.nan(x), NaN,
         ifelse(x %in% major_ag, 2,
                ifelse(x %in% ag_mask, 0, 1)))
})

# plot the reclassified raster
plot(ag_raster_2008_maj)

# reclassify into non_ag = 0,  other ag = 1, major_ag = 2, alfalfa = 3,
# and NaN = NaN
ag_raster_combo <- terra::app(cdl_all_years, fun = function(x) {
  ifelse(is.nan(x), NaN,
         ifelse(x %in% major_ag, 2,
                ifelse(x %in% alfalfa, 3,
                ifelse(x %in% ag_mask, 0, 1))))
})

# plot the reclassified raster
plot(ag_raster_2008_combo)




###############################################################################
# Landscape summary of patch level metrics (example using Rich County)
###############################################################################

#                       area metrics
#______________________________________________________________________________

# get sf shape file data for a county to start
Rich_county <- tigris::counties(state = "UT", cb = TRUE) %>%
  st_as_sf() %>%
  filter(NAME %in% "Rich")
Rich_county <- st_transform(Rich_county, crs = st_crs(cdl_all_years))
mask_rich <- crop(cdl_all_years[["2008"]], Rich_county)
freq_rich <- freq(mask_rich)
freq_rich
plot(mask_rich)

# check that the raster meets package criteria
landscapemetrics::check_landscape(mask_rich)

# get patches for all the different classes
test_patch <- landscapemetrics::get_patches(mask_rich, directions = 8)
show_patches(landscape = mask_rich, class = c(152, 37), labels = FALSE)


# calculate all the landscape patch level metrics
rich_lsm_metrics <- calculate_lsm(mask_rich, what = "patch")

# look at total area per class in hectares
rich_patch_area <- rich_lsm_metrics %>%
  filter(str_detect(metric, "area")) %>%
  group_by(class) %>%
  summarise(total_area = sum(value, na.rm = TRUE)) %>%
  arrange(desc(total_area))

rich_patch_area

# this function does the same as above
rich_lsm_area <- lsm_c_ca(mask_rich, directions = 8)
rich_lsm_desc <- rich_lsm_area %>%
arrange(desc((value)))
rich_lsm_desc

# looks at mean patch area
mean_area <- lsm_c_area_mn(mask_rich, directions = 8)
mean_area <- mean_area %>%
  arrange(desc((value)))
mean_area

# standard deviation of patch sizes within each unique class
sd_within_class <- lsm_c_area_sd(mask_rich, directions = 8)
sd_within_class <- sd_within_class %>%
  arrange(desc((value)))
sd_within_class

# area and edge metric, describes percentage of landscape covered by the largest
# patch of each class. Measure of dominance
rich_lpi <- lsm_c_lpi(mask_rich, directions = 8)
rich_lpi <- rich_lpi %>%
  arrange(desc((value)))
rich_lpi

# describes percentage of landscape belonging to each class, measure of
# composition
rich_pland <- lsm_c_pland(mask_rich, directions = 8)
rich_pland <- rich_pland %>%
  arrange(desc((value)))
rich_pland
#                       patch spatial relationships metrics
#______________________________________________________________________________

# cohesion of class, this is an aggregation metric that gives info about the
# configuration of the landscape, used to access if patches of the same class are
# located next to each other or appear in isolation
# 0 = if isolated, 100 if cohesion is present (units are percent)
rich_cohesion <- lsm_c_cohesion(mask_rich, directions = 8)
rich_cohesion <- rich_cohesion %>%
  arrange(desc(value))
print(rich_cohesion, n = 27)

# assess spatial connectedness, orthogonally connected pixels are weighted higher
# diagonal pixels. 0 means that contiguity index is the same for all patches,
# increases without limit as variation of CONTIG increases
rich_contig <- lsm_c_contig_cv(mask_rich, directions = 8)
rich_contig <- rich_contig %>%
  arrange(desc(value))
rich_contig

# Describes intermixing of classes, interspersion and juxtaposition index
# approaches 0 if a class is only adjacent to another class, 100 when equally
# adjacent to all other classes
rich_iji <- lsm_c_iji(mask_rich)
rich_iji

# look at enn (nearest neighbor in same class) as a measure of patch isolation
rich_patch_enn <- rich_lsm_metrics %>%
  filter(str_detect(metric, "enn")) %>%
  group_by(class) %>%
  summarise(total_neighbors = sum(value, na.rm = TRUE)) %>%
  arrange(total_neighbors)

rich_patch_enn

# describes how patchy the classes are, increases a landscape get more patchy
# max is 1e+06
rich_pd <- lsm_c_pd(mask_rich, directions = 8)
print(rich_pd, n =27)

# shannon's diversity index
rich_shdi <- lsm_l_shdi(mask_rich)
rich_shdi

###############################################################################
# Landscape summary of patch level metrics (using BL watershed)
###############################################################################
# are we interested in rooks (4) or queens(8) direction connected patches?

## Try with cropping combo raster with bl watershed
bl_crop <- crop(ag_raster_2008_combo, bl_watershed_trans)
plot(bl_crop)

# check raster
landscapemetrics::check_landscape(bl_crop)
plot(bl_crop)

# get mean patch area of each class
bl_crop_p_area <- lsm_c_area_mn(bl_crop)
bl_crop_p_area

# describes percentage of landscape belonging to each class, measure of
# composition
bl_pland <- lsm_c_pland(bl_crop, directions = 8)
bl_pland <- bl_pland %>%
  arrange(desc((value)))
bl_pland

# get patches of each class and visualize
test_patch <- landscapemetrics::get_patches(bl_crop, directions = 8)
test_patch
plot(test_patch$layer_1$class_1)
plot(test_patch$layer_1$class_2)
plot(test_patch$layer_1$class_3)

# cohesion of class, this is an aggregation metric that gives info about the
# configuration of the landscape, used to access if patches of the same class are
# located next to each other or appear in isolation
# 0 = if isolated, 100 if cohesion is present (units are percent)
bl_cohesion <- lsm_c_cohesion(bl_crop, directions = 8)
bl_cohesion <- bl_cohesion %>%
  arrange(desc(value))
print(bl_cohesion)


## Try with cropping ag vs non-ag raster with bl watershed
bl_crop_nan <- crop(ag_raster_2008_nan, bl_watershed_trans)
plot(bl_crop_nan)

landscapemetrics::check_landscape(bl_crop_nan)

# mean area of patches by class
bl_crop_p_area_nan <- lsm_c_area_mn(bl_crop_nan)
bl_crop_p_area_nan

# describes percentage of landscape belonging to each class, measure of
# composition
bl_pland_nan <- lsm_c_pland(bl_crop_nan, directions = 8)
bl_pland_nan <- bl_pland_nan %>%
  arrange(desc((value)))
bl_pland_nan

# get patches of each class and visualize
test_patch_nan <- landscapemetrics::get_patches(bl_crop_nan, directions = 8)
plot(test_patch_nan$layer_1$class_0)
plot(test_patch_nan$layer_1$class_1)
plot(test_patch$layer_1$class_3)

# area and edge metric, describes percentage of landscape covered by the largest
# patch of each class. Measure of dominance
bl_lpi <- lsm_c_lpi(bl_crop_nan, directions = 8)
bl_lpi <- bl_lpi %>%
  arrange(desc((value)))
bl_lpi

###############################################################################
# Landscape summary of patch level metrics (using BL watershed all years)
###############################################################################

## Try with cropping ag vs non-ag raster with bl watershed
bl_crop_all <- crop(cdl_all_years, bl_watershed_trans)

# Reclassify the raster values into non-ag = 0, ag = 1, and NaN = NaN
ag_raster_all <- terra::app(bl_crop_all, fun = function(x) {
  ifelse(is.nan(x), NaN, ifelse(x %in% ag_mask, 0, 1))
})

# Define the path where you want to save the file
file_path <- "Intermediate_rasters/ag_raster_all.tif"
# Write the SpatRaster to a file
terra::writeRaster(ag_raster_all, filename = file_path)

# check that raster is compatible with package
landscapemetrics::check_landscape(ag_raster_all)

# mean area of patches in each class
# Initialize a list to store landscape metrics for each year
landscape_metrics_list <- list()

reclassified_raster <- ag_raster_all

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster)) {
  raster_layer <- reclassified_raster[[i]]
  year <- names(cdl_all_years)[i]

  # Calculate landscape metrics
  landscape_metrics <- lsm_c_area_mn(raster_layer)
  landscape_metrics$year <- year
  landscape_metrics_list[[i]] <- landscape_metrics
}

# Combine all metrics into a single data frame
c_area_metrics <- bind_rows(landscape_metrics_list)
c_area_metrics

# Plot changes in mean patch area over time
library(ggplot2)

class_labels <- c("0" = "Non-Ag", "1" = "Ag")
ggplot(c_area_metrics, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Mean Patch Area Over Time",
       x = "Year",
       y = "Mean Patch Area in Hectares",
       color = "Class") +
  scale_color_manual(values = c("blue", "green"), labels = class_labels) +
  theme_minimal()

c_area_metrics_sub <- c_area_metrics %>%
  filter(class %in% 1)
c_area_metrics_sub

ggplot(c_area_metrics_sub, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Mean Patch Area Over Time",
       x = "Year",
       y = "Mean Patch Area in Hectares",
       color = "Class") +
  scale_color_manual(values = "green", labels = class_labels) +
  theme_minimal()

# describes percentage of landscape belonging to each class, measure of
# composition
# Initialize a list to store landscape metrics for each year
landscape_metrics_pland <- list()

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster)) {
  raster_layer <- reclassified_raster[[i]]
  year <- names(cdl_all_years)[i]

  # Calculate landscape metrics
  landscape_metrics <- lsm_c_pland(raster_layer)
  landscape_metrics$year <- year
  landscape_metrics_pland[[i]] <- landscape_metrics
}

# Combine all metrics into a single data frame
pland_metrics <- bind_rows(landscape_metrics_pland)
pland_metrics

# Create the stacked bar plot
ggplot(pland_metrics, aes(x = as.numeric(year), y = value, fill = as.factor(class))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Percent land Cover",
       x = "Year",
       y = "Percent of Land covered by class",
       fill = "Class") +
  scale_fill_manual(values = c("blue", "green"), labels = class_labels) +
  theme_minimal()



# cohesion of class, this is an aggregation metric that gives info about the
# configuration of the landscape, used to access if patches of the same class are
# located next to each other or appear in isolation
# 0 = if isolated, 100 if cohesion is present (units are percent)
# Initialize a list to store landscape metrics for each year
landscape_metrics_coh <- list()

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster)) {
  raster_layer <- reclassified_raster[[i]]
  year <- names(cdl_all_years)[i]

  # Calculate landscape metrics
  landscape_metrics <- lsm_c_cohesion(raster_layer)
  landscape_metrics$year <- year
  landscape_metrics_coh[[i]] <- landscape_metrics
}

# Combine all metrics into a single data frame
coh_metrics <- bind_rows(landscape_metrics_coh)
print(coh_metrics, n =32)


class_labels <- c("0" = "Non-Ag", "1" = "Ag")
ggplot(coh_metrics, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Patch Cohesion Over Time",
       x = "Year",
       y = "Patch Cohesion",
       color = "Class") +
  scale_color_manual(values = c("blue", "green"), labels = class_labels) +
  theme_minimal()


# Initialize a list to store landscape metrics for each year
landscape_metrics_patch <- list()

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster)) {
  raster_layer <- reclassified_raster[[i]]
  year <- names(cdl_all_years)[i]

  # Calculate landscape metrics
  landscape_metrics <- get_patches(raster_layer, to_disk = TRUE,
                                    return_raster = TRUE)
  landscape_metrics$year <- year
  landscape_metrics_patch[[i]] <- landscape_metrics
}

# Combine all metrics into a single data frame
enn_metrics <- bind_rows(landscape_metrics_enn)
enn_metrics


test_patch <- landscapemetrics::get_patches(reclassified_raster[["2008"]], directions = 8, to_disk = TRUE)
plot(test_patch$layer_1$class_0)
plot(test_patch$layer_1$class_1)

# count patches
nrow(terra::unique(test_patch[[1]][[1]]))
nrow(terra::unique(test_patch[[1]][[2]]))

# calculate nearest neighbor

class_0 <- test_patch[[1]][[1]]
class_1 <- test_patch[[1]][[2]]

nn_0 <- get_nearestneighbour(class_0)
nn_1 <- get_nearestneighbour(class_1)
sum(nn_1$dist)/nrow(nn_1)

# average distance btw patches is 117.8123 in 2008

###############################################################################
# Landscape summary of patch level metrics (using BL watershed all years)
# non ag vs major ag classes
###############################################################################

# reclassify into non_ag = 0,  other ag = 1, major_ag = 2, alfalfa = 3,
# and NaN = NaN
ag_raster_combo <- terra::app(bl_crop_all, fun = function(x) {
  ifelse(is.nan(x), NaN,
         ifelse(x %in% major_ag, 2,
                ifelse(x %in% alfalfa, 3,
                       ifelse(x %in% ag_mask, 0, 1))))
})


# Define the path where you want to save the file
file_path <- "Intermediate_rasters/ag_raster_combo.tif"
# Write the SpatRaster to a file
terra::writeRaster(ag_raster_combo, filename = file_path)

# check that raster is compatible with package
landscapemetrics::check_landscape(ag_raster_combo)

# mean area of patches in each class
# Initialize a list to store landscape metrics for each year
landscape_metrics_list <- list()

reclassified_raster <- ag_raster_combo

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster)) {
  raster_layer <- reclassified_raster[[i]]
  year <- names(cdl_all_years)[i]

  # Calculate landscape metrics
  landscape_metrics <- lsm_c_area_mn(raster_layer)
  landscape_metrics$year <- year
  landscape_metrics_list[[i]] <- landscape_metrics
}

# Combine all metrics into a single data frame
c_area_metrics <- bind_rows(landscape_metrics_list)
c_area_metrics

# Plot changes in mean patch area over time
library(ggplot2)
c_area_metrics_sub <- c_area_metrics %>%
  filter(class %in% c(1, 2, 3))
c_area_metrics_sub


class_labels <- c("0" = "Non-Ag", "1" = "Ag", "2" = "Major_Ag", "3" = "Alfalfa")
ggplot(c_area_metrics, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Mean Patch Area Over Time Ag Combo",
       x = "Year",
       y = "Mean Patch Area in Hectares",
       color = "Class") +
  scale_color_manual(values = c("blue", "green", "red", "orange"), labels = class_labels) +
  theme_minimal()

class_labels <- c("1" = "Ag", "2" = "Major_Ag", "3" = "Alfalfa")
ggplot(c_area_metrics_sub, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Mean Patch Area Over Time Ag Combo",
       x = "Year",
       y = "Mean Patch Area in Hectares",
       color = "Class") +
  scale_color_manual(values = c("green", "red", "orange"), labels = class_labels) +
  theme_minimal()

# describes percentage of landscape belonging to each class, measure of
# composition
# Initialize a list to store landscape metrics for each year
reclassified_raster <- ag_raster_combo

landscape_metrics_pland_1 <- list()

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster[[1:8]])) {
  raster_layer <- reclassified_raster[[1:8]][[i]]
  year <- names(cdl_all_years)[i]

  # Calculate landscape metrics
  landscape_metrics <- lsm_c_pland(raster_layer)
  landscape_metrics$year <- year
  landscape_metrics_pland_1[[i]] <- landscape_metrics
}

# Combine all metrics into a single data frame
pland_metrics_1 <- bind_rows(landscape_metrics_pland_1)
print(pland_metrics_1, n =32)

landscape_metrics_pland_2 <- list()

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster[[9:16]])) {
  raster_layer <- reclassified_raster[[9:16]][[i]]
  year <- names(cdl_all_years[[9:16]])[i]

  # Calculate landscape metrics
  landscape_metrics_2 <- lsm_c_pland(raster_layer)
  landscape_metrics_2$year <- year
  landscape_metrics_pland_2[[i]] <- landscape_metrics_2
}

# Combine all metrics into a single data frame
pland_metrics_2 <- bind_rows(landscape_metrics_pland_2)
pland_metrics_2

pland_metrics_combo <- bind_rows(pland_metrics_1, pland_metrics_2)
pland_metrics_combo

# Create the stacked bar plot
ggplot(pland_metrics_combo, aes(x = as.numeric(year), y = value, fill = as.factor(class))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Percent land Cover",
       x = "Year",
       y = "Percent of Land covered by class",
       fill = "Class") +
  scale_fill_manual(values = c("blue", "green", "red", "orange"), labels = class_labels) +
  theme_minimal()

# cohesion of class, this is an aggregation metric that gives info about the
# configuration of the landscape, used to access if patches of the same class are
# located next to each other or appear in isolation
# 0 = if isolated, 100 if cohesion is present (units are percent)
# Initialize a list to store landscape metrics for each year
landscape_metrics_coh <- list()

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster)) {
  raster_layer <- reclassified_raster[[i]]
  year <- names(cdl_all_years)[i]

  # Calculate landscape metrics
  landscape_metrics <- lsm_c_cohesion(raster_layer)
  landscape_metrics$year <- year
  landscape_metrics_coh[[i]] <- landscape_metrics
}

# Combine all metrics into a single data frame
coh_metrics <- bind_rows(landscape_metrics_coh)
print(coh_metrics, n =32)

class_labels <- c("0" = "Non-Ag", "1" = "Ag", "2" = "Major_Ag", "3" = "Alfalfa")
ggplot(coh_metrics, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Patch Cohesion Over Time",
       x = "Year",
       y = "Patch Cohesion",
       color = "Class") +
  scale_color_manual(values = c("blue", "green", "red", "orange"), labels = class_labels) +
  theme_minimal()




