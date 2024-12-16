# Load packages that are included in the R environment
library(dismo)
library(jpeg)
library(maps)
library(raster)
library(rasterVis)
library(readxl)
library(rgbif)
library(rgeos)
library(rJava)
library(sp)
library(svMisc)
library(rgdal)
library(terra)
library(tidyr)

# Check the default library paths
.libPaths()

dir.create("./maxent_demo/lib")

# Prepend the path to your personal library to .libPaths
# This will add your library path (which will be the default path for installing new packages)
# But also keep the default R environment path (to `library` existing packages)
.libPaths(c("./maxent_demo/lib", .libPaths()))

install.packages("ncdf4")
library(ncdf4)



dir.create("./maxent_demo/output")

jar <- paste(system.file(package = "dismo"), "/java/maxent.jar", sep = '') 
if (file.exists(jar)) {
  cat("MaxEnt is available.")
} else {
  cat('MaxEnt is not available!')
}

# Select your species
my_species <- c("Litoria fallax")

# Download GBIF occurrence data
Litoria_fallax_GBIF_raw <- rgbif:: occ_data(scientificName = my_species,
                                            hasCoordinate  = TRUE,
                                            limit          = 500) 

# Return a list of citations for the downloaded GBIF data: 
citations <- rgbif::gbif_citation(Litoria_fallax_GBIF_raw)

# Example of a citation:
print(citations[[1]])


str(Litoria_fallax_GBIF_raw, list.len = 4)

View(head(Litoria_fallax_GBIF_raw$data))


# Select columns, and return a `tibble` with only unique rows.
litoria_fallax <- unique(tibble::as_tibble(Litoria_fallax_GBIF_raw$data [ , c("decimalLongitude", "decimalLatitude",
                                                                              "individualCount", "species", "year",
                                                                              "month", "country", "occurrenceStatus",
                                                                              "coordinateUncertaintyInMeters", "datasetName",
                                                                              "datasetKey")]))

cat("- There are", nrow(litoria_fallax), "unique occurrence records in the tibble.\n")

# You could subset by a chosen dataset
litoria_fallax <- litoria_fallax[litoria_fallax$datasetName == "iNaturalist research-grade observations",]
# Include only records with a particualary uncertainty
litoria_fallax <- litoria_fallax[litoria_fallax$coordinateUncertaintyInMeters < 200,]
# Drop NAs
litoria_fallax <- litoria_fallax[!is.na(litoria_fallax$datasetName), ]

cat("- There are now", nrow(litoria_fallax), "occurrence records in the tibble after filtering.\n")


View(head(litoria_fallax))

write.csv(litoria_fallax, paste0(getwd(),"./maxent_demo/data/Litoria_fallax_filtered.csv"))


# NOTE: added cex (point size) and col so the records are easier to see
map("world", xlim = range(litoria_fallax$decimalLongitude),
    ylim = range(litoria_fallax$decimalLatitude))  
points(litoria_fallax[ , c("decimalLongitude", "decimalLatitude")], pch = ".", cex = 3, col = "blue")


# Read NetCDF data file of predictors
file <- "./maxent_demo/data/Terraclim_EY_E_Aus_orig.nc"
var_names <- c("tmax", "tmin", "ppt", "soil")  

# Visualise one of the variables (tmax)
plot(raster::brick(file, varname = "tmax"))

# Save a CDF file of the mean values for each of the variables, in your directory folder
for (var_name in var_names) {
  
  var_brick <- raster::brick(file, varname = var_name)
  var_mean  <- mean(var_brick)
  
  raster::writeRaster(x         = var_mean, 
                      filename  = paste0("./maxent_demo/data/",var_name, "_mean"),
                      overwrite = TRUE, 
                      format    = 'CDF')
}

mean_files <- list.files("./maxent_demo/data", pattern = "_mean.nc", full.names = TRUE)
predictors <- raster::stack(mean_files)
names(predictors) <- c('Rain_mean', 'Soil_mean', 'MXtemp_mean', 'MNtemp_mean')
plot(predictors)

xy_fallax <- litoria_fallax[, c("decimalLongitude", "decimalLatitude")]
colnames(xy_fallax) <- c("x", "y")
xy_fallax_sp <- sp::SpatialPoints(coords = xy_fallax, proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs"))

xy_fallax_sp <- raster::crop(xy_fallax_sp, predictors[[1]])

plot(predictors[[1]])
points(xy_fallax_sp)


# Run Maxent model

group <- dismo::kfold(xy_fallax_sp, k = 5)
pres_test  <- xy_fallax_sp[group == 1, ]  # 20% of data sample for testing
pres_train <- xy_fallax_sp[group != 1, ]

backg <- dismo::randomPoints(mask = predictors, n = 1000)
colnames(backg) <- c("lon", "lat")

# check the number of NAs
x <- raster::extract(predictors, pres_train) 
y <- na.omit(x)
na_count <- length(y)/length(x)  # need to be 0.5 or more
na_count

maxent_args <- c("removeduplicates=TRUE","jackknife=TRUE")

basic_maxent<- dismo::maxent(predictors,
                             pres_train,
                             path = "./maxent_demo/output",
                             args = maxent_args)

plot(basic_maxent)

map_predictions <- dismo::predict(basic_maxent, predictors)

plot(map_predictions)
points(pres_train)

evaluate_model <- dismo::evaluate(pres_train, pres_test, basic_maxent, predictors)
evaluate_model

evaluate_model@auc

# Save the prediction plot with training points as a `jpeg`
jpeg("./maxent_demo/output/max_prediction.jpeg")
plot(map_predictions)
points(pres_train)
dev.off()

# Save the prediction in an `asc` file

raster::writeRaster(map_predictions,
                    filename  = "./maxent_demo/output/Litoria_fallax_pred.asc",
                    format    = "ascii",
                    overwrite = TRUE)

threshold_model <- dismo::threshold(evaluate_model, 'spec_sens')
threshold_model

m <- c(0, threshold_model, 0,  threshold_model, 1, 1)
reclass <- matrix(m, ncol = 3, byrow = TRUE)
rc <- raster::reclassify(map_predictions, reclass)

jpeg("./maxent_demo/output/pres_absence_map.jpeg")
plot(rc, main = 'presence/absence')
points(pres_train, pch = '+')
dev.off()

dismo::response(basic_maxent)


myspecies <- c("Litoria fallax")
cor <- unname(evaluate_model@cor)
test_data_results <- as.data.frame(list(myspecies,
                                        evaluate_model@np,
                                        evaluate_model@na,
                                        evaluate_model@auc,
                                        cor))

colnames(test_data_results) <- c("species", "presences", "absences", "AUC", "cor")
test_data_results

basic_maxent@results
