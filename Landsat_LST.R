
# Calculate land surface temperature (LST) using Landsat 8 or 9 imagery
# based on the protocol outlined in Jeevalakshmi, D., Narayana Reddy, S., and Manikiam, B. 2017. Land Surface Temperature Retrieval from LANDSAT data using Emissivity Estimation. International Journal of Applied Engineering Research 12(20): 9679-9687.
# Download full set of tiff files for a single scene from USGS EarthExplorer, and extract all the files of the scene into a local folder

# Prepared by: Alyssa T. Kullberg
# Last edited: 11/03/2022

## Load packages
#install.packages("raster"")
#install.packages("stringr")
library(raster)
library(stringr)

## Create function to calculate land surface temperature (LST)
est_LST <- function(satellite, path_B4, path_B5, path_B10, path_MTL, ext) {
  
  ## Load the .tif files to be used to calculate and map LST and crop to desired extent
  
  # Band 10 (thermal band) for brightness temperature, and Bands 4 and 5 for NDVI correction
  # Metadata file to extract constants for correction of brightness temperature
  l4 <- crop(raster(paste0(path_B4)), ext1)
  l5 <- crop(raster(paste0(path_B5)), ext1)
  l10 <- crop(raster(paste0(path_B10)), ext1)
  mtl <- read.delim(paste0(path_MTL))
  
  if(satellite == 9){
  # Extract thermal constants K1 and K2 from metadata
  k1 = as.numeric(substr(mtl[266,1], 27, 35))
  k2 = as.numeric(substr(mtl[267,1], 27, 35))
  }else if(satellite == 8){
    # Extract thermal constants K1 and K2 from metadata
    k1 = as.numeric(substr(mtl[207,1], 27, 34))
    k2 = as.numeric(substr(mtl[208,1], 27, 36))
  }
  
  # Calculate brightness temperature 
  t2 = .1+(l10*0.0003342)
  t2 = k2/log((k1/t2 + 1))
  t2 = t2-273.15 #convert K to C
  
  # Calculate NDVI
  ndvi = (l5 - l4)/(l5 + l4)
  
  # Calculate proportion of vegetation
  pv = ((ndvi - 0.2)/(0.5-0.2))^2
  
  # Calculate emissivity
  e = ndvi   #create raster to hold emissivity values 
  e[] = 0.973   #set default values (in the end this represents vegetation)
  e[ndvi<0] = 0.991 #water
  e[ndvi >=0 & ndvi <0.2] = 0.966 #soil
  w = which(ndvi[]>=0.2 & ndvi[]<0.5) #mix of soil and veg >
  e[w] = (0.966 * pv[w]) + (0.966 * (1- pv[w])) + 0.005
  e = mask(e, ndvi)   #mask to get NA NDVI values as NA e values
  
  # Calculate LST
  lst = overlay(t2, e, fun=function(x,y){x / (1 + ((.000010895*x/.01438)*log(y)))})
  
  return(lst)
}


## Provide file paths for Bands 4, 5, and 10, and for metadata text file
# Note: the files may not load correctly if there are spaces anywhere in the file path
my_path_B4 <- "{YOUR FILE PATH HERE}"
my_path_B5 <- "{YOUR FILE PATH HERE}"
my_path_B10 <- "{YOUR FILE PATH HERE}"
my_path_MTL <- "{YOUR FILE PATH HERE}"

## Set the extent you wish to calculate LST.
# This can be the full extent of the scene, e.g., ext1 <- extent(raster(path_B4))
# or a sub-region, in UTM, e.g., ext1 <- extent(540000, 600000, 2800000, 2900000)
# Note: When in the Southern Hemisphere, Landsat 9 uses a false northing of 10,000,000m, so this number must be subtracted from UTM Northing coordinates calculated from most online sources to match up with the Northing coordinates encoded into the raster layers
ext1 <- extent(xmin, xmax, ymin, ymax)

## Run function to estimate LST on your file
LST <- est_LST(satellite = 8, # Define whether Landsat 8 or 9 is being used
               path_B4 = my_path_B4,
               path_B5 = my_path_B5,
               path_B10 = my_path_B10,
               path_MTL = my_path_MTL,
               ext = ext1)
plot(LST) # View results

writeRaster(LST, "{YOUR FILE PATH HERE}")


