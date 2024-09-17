#data documentation: https://ghrc.nsstc.nasa.gov/pub/lis/iss/doc/isslis_dataset.pdf

#load libraries
library(tidyverse)
library(rgdal)
library(stringr)
library(purrr)
library(fst)
require(ncdf4)
require(chron)
require(dplyr)
require(readr)
require(fst)

# 0.a Declare Directories 
raw.data.folder <- "~/Desktop/lightning-data-lis/"


# 1a Make a list of all of the files
for(YYYY in c(2017)){
  raw.lis.folder <- paste0(raw.data.folder, YYYY)
  setwd(raw.lis.folder)
  flist <- list.files(pattern = 'FIN.nc')

  
  


  # 1b Define the function to read in the data and pull out lat/long/time
  
  compile.data <- function( i ){
    nc_data <- nc_open(flist[i]) 
     if("lightning_flash_lon" %in% names(nc_data$var)){ #this step ensures that the variable is in the file
    
    lon <- ncvar_get(nc_data, "lightning_flash_lon", verbose = F)
    lat <- ncvar_get(nc_data, "lightning_flash_lat", verbose = F)
    t <- ncvar_get(nc_data, "lightning_flash_TAI93_time") # units: seconds since 1993-01-01 00:00:00.000
   output = data.frame(y = i, flash_latitude = lat, flash_longitude = lon, flash_time = t)}
  }
  

  # 1d Fill in all of the data and write out
  for (i in 1:length(flist)){ 
    L <- compile.data(i)
    write_rds(L, paste0("~/Desktop/lightning-data-lis/lightning-rds/", YYYY, "/lightning-data", i, ".rds"))
  }
  
}

