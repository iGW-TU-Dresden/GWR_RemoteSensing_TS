library(raster)
library(sf)


setwd("~/TU_Dresden/Masterarbeit/Daten/Bodenfeuchte/SoMo_ml/SoMo.ml_v1_layer2")
# files <- list.files(path="~/TU_Dresden/Masterarbeit/Daten/Bodenfeuchte/SoMo_ml/SoMo.ml_v1_layer2", pattern=".nc")

# Define start and end years
year_start <- 2009
year_end <- 2017
years <- seq(year_start, year_end)
years <- as.character(years)

# Install and load packages
# install.packages("lubridate")
library(lubridate)

# Setting the start and end dates
start_date <- ymd(paste0(year_start,"-01-01"))
end_date <- ymd(paste0(year_end,"-12-31"))
# Calculate the number of days
number_of_days <- as.numeric(end_date - start_date) + 1
sequenz_of_days <- seq(start_date, end_date, by="days")

# Catchment area shapefile
path_to_shapefile <- "~/TU_Dresden/Masterarbeit/Daten/Berthelin/GIS"
polygon <- st_read(dsn = path_to_shapefile, layer = "Catchment_Lauter")

# Loading a raster for coordinate system information
path_to_raster <- "~/TU_Dresden/Masterarbeit/Daten/Bodenfeuchte/SoMo_ml/SoMo.ml_v1_layer2/"
setwd(path_to_raster)
beispiel_rast <- raster("SoMo.ml_v1_layer2_2009.nc")

# Transform to the same CRS
# polygon <- spTransform(polygon, crs(beispiel_rast))

# Create final dataframe
dataframe <- matrix(nrow=number_of_days, ncol=2)
dataframe <- as.data.frame(dataframe)
colnames(dataframe)[1:2] <- c("date", "mean_soil_moisture")
dataframe$date <- sequenz_of_days

# For-loop for the rasters
for (j in c(1:length(years))) {
  
  # j <- 1
  y <- years[j]
  # year <- format(as.Date(j), format="%Y%m%d")
  
  # Loading raster data
  setwd(path_to_raster)
  rast_jahr <- raster(paste0("SoMo.ml_v1_layer2_", y ,".nc"))
  
  # Check how many bands/days are present in this raster/year
  Anzahl_Tage <- rast_jahr@file@nbands
  
  # For loop over all days of the respective year
  for (i in c(1:Anzahl_Tage)) {
    
    # i <- 2
    # Extract raster number i from the annual raster stack
    rast_tag <- raster(paste0("SoMo.ml_v1_layer2_", y ,".nc"), band=i)
    
    # Crop raster to the extent of polygons
    r1 <- crop(rast_tag, polygon, snap="out")
    # Area proportion per pixel in polygon
    area_prop <- rasterize(polygon, r1, silent=F, getCover=TRUE) # /100
    
    # Convert to matrix for faster calculation
    r1 <- as.matrix(r1)
    area_prop <- as.matrix(area_prop)
    
    # Average soil moisture value over the entire catchment area on day i in year y
    result <- sum(r1 * area_prop) / sum(area_prop)
    
    # Date
    date <- ymd(paste(y, "-01-01")) + days(i - 1)
    
    # Get the corresponding row in the dataframe
    row <- which(grepl(date, dataframe$date))
    
    # Fill the dataframe
    dataframe$mean_soil_moisture[row] <- as.numeric(format(round(result, 4), scientific=F))
  }
}

# Save the data series in the target directory
setwd("~/TU_Dresden/Masterarbeit/Daten/Daten_MA")
write.csv2(dataframe, "Soil_moisture_2(Vergleich)_EZG.csv", quote=F)