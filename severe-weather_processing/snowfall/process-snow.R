#load libraries
library(tidyverse)
library(rgdal)
library(stringr)
library(purrr)
library(fst)
library(tigris)
require(sf)
require(dplyr)
require(readr)
library(raster)

# 0.a Declare Directories 
raw.data.folder <- "~/Desktop/snow-tif/"


# 1a Make a list of all of the files

  setwd(raw.data.folder)
  flist <- list.files(pattern = '12.tif')
  
  
#prep county data 
  ct <- counties(year = 2019)
  ct <- st_transform(ct, crs = st_crs("WGS84"))
  
  # 1b Define the function to read in the data and pull out lat/long/time
  
  compile.data <- function( i ){
    tif_data <- raster(flist[i]) 

    v <- extract(x = tif_data , y= ct, 
                 weights = TRUE, # calculate weights used for averaging (area-weighted mean)
                 normalizeWeights=TRUE, # normalize weights to always add up to 1 if, say, some of the shapefile is not covered by a raster 
                 fun=mean, # take the mean of the values
                 df=TRUE, # return as a dataframe object
                 na.rm=TRUE) # remove NAs before function (mean) applied so that NAs aren't returned)
    
    
    
    weighted.area.national = data.frame(county = paste0(ct$GEOID),snow_data = v[,2], date = substr(names(tif_data ),17,24))
  }
  
  
  # 1d Fill in all of the data and write out
  for (i in 1:length(flist)){ 
    L <- compile.data(i)
    write_fst(L, paste0("~/Desktop/snow-tif/processed/", flist[i], ".fst"))
  }
  


