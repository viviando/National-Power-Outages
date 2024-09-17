# assign flash points to counties

library(tidyverse)
library(rgdal)
library(stringr)
library(purrr)
library(fst)
require(ncdf4)
require(chron)
require(dplyr)
require(tigris)
require(sf)
require(lubridate)
# setwd

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/extreme weather data/lightning-data-lis/03 final-data")
dta <- read_fst("lighting-flashes-2017-2020.fst")




################################################################################
# 1. read in utility boundaries
################################################################################
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/e_locality")

utility <- st_read("e_locality.shp")
  
  
utility_wgs <- utility %>% sf::st_as_sf()%>% sf::st_transform("WGS84")

st_crs(utility_wgs)
################################################################################
# 2. make lightning data spatial:
################################################################################

dta_sf <- sf::st_as_sf(dta, coords = c("flash_longitude", "flash_latitude"),crs = "WGS84") # is this the correct projection?

st_crs(dta_sf)

################################################################################
# 3. intersect and create exposure variable
################################################################################
#sf_use_s2(FALSE)

utility_wgs <- st_make_valid(utility_wgs)

lighting_utility <- st_join( dta_sf,utility_wgs,  join = st_intersects) %>%
  dplyr::select(time_utc,PRIME_DPS_, NAME) %>%
  dplyr::mutate(lightning_exposure = ifelse(is.na(PRIME_DPS_) == FALSE, 1, 0)) %>% 
  dplyr::mutate(time_utc = as.POSIXct(time_utc, tz = "UTC", format = "%Y/%m/%d %H:%M:%S")) %>% 
  dplyr::mutate(time_utc_hourly = strftime(time_utc, tz = "UTC", format = "%Y-%m-%d %H"))%>%
  dplyr:: mutate(time_utc_daily = strftime(time_utc, tz = "UTC", format = "%Y-%m-%d"))



#create hourly exposure metric:
lighting_utility_hourly <- lighting_utility %>%
  dplyr::group_by(PRIME_DPS_, NAME,time_utc_hourly) %>% 
  dplyr::summarize(num_hourly_exposure = sum(lightning_exposure)) %>% 
  dplyr::ungroup() %>% 
  dplyr:: mutate(bin_exposure_hourly = ifelse(num_hourly_exposure > 0, 1, 0))
  
  
hourly_lightning_exposure_complete <- as.data.frame(lighting_utility_hourly) %>% 
    dplyr::select(PRIME_DPS_, NAME, time_utc_hourly, num_hourly_exposure, bin_exposure_hourly) %>%
    dplyr::mutate(time_utc_hourly = as.character(as.POSIXct(time_utc_hourly, tz = "UTC", format = "%Y-%m-%d %H")))%>%
    dplyr::group_by(PRIME_DPS_, NAME) %>% 
    complete(time_utc_hourly = as.character(c(
      seq.POSIXt(
        from = as.POSIXct("2017-03-01", tz ="UTC"),
        to = as.POSIXct("2020-12-31", tz ="UTC"),
        by = "hour")))) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(num_hourly_exposure = ifelse(is.na(num_hourly_exposure) == TRUE, 0, num_hourly_exposure)) %>%
    dplyr::mutate(bin_exposure_hourly = ifelse(is.na(bin_exposure_hourly) == TRUE, 0, bin_exposure_hourly))

utility_hour <- utility %>% as.data.frame() %>%
  dplyr::select(PRIME_DPS_, NAME)%>%
  dplyr::group_by(PRIME_DPS_, NAME) %>% 
  dplyr::mutate(time_utc_hourly = NA) %>% 
  complete(time_utc_hourly = as.character(c(
    seq.POSIXt(
      from = as.POSIXct("2017-03-01", tz ="UTC"),
      to = as.POSIXct("2020-12-31", tz ="UTC"),
      by = "hour"))))

hourly_lightning_exposure_complete<- full_join(utility_hour, hourly_lightning_exposure_complete)%>% 
  dplyr::mutate(num_hourly_lightning_exposure = ifelse(is.na(num_hourly_exposure) == TRUE, 0, num_hourly_exposure)) %>%
  dplyr::mutate(bin_lightning_exposure_hourly = ifelse(is.na(bin_exposure_hourly) == TRUE, 0, bin_exposure_hourly))

#create daily exposure metric:
lighting_utility_daily <- lighting_utility %>%
  dplyr::group_by(PRIME_DPS_, NAME, time_utc_daily) %>% 
  dplyr::summarize(num_daily_exposure = sum(lightning_exposure)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(bin_exposure_daily = ifelse(num_daily_exposure > 0, 1, 0))%>%
  dplyr::ungroup()



daily_lightning_exposure_complete <- as.data.frame(lighting_utility_daily) %>% 
  dplyr::select(PRIME_DPS_, NAME, time_utc_daily, num_daily_exposure, bin_exposure_daily) %>% 
  dplyr::group_by(PRIME_DPS_, NAME) %>% 
  complete(time_utc_daily = as.character(c(
    seq.POSIXt(
      from = as.POSIXct("2017-03-01", tz ="UTC"),
      to = as.POSIXct("2020-12-31", tz ="UTC"),
      by = "day")))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(num_daily_exposure = ifelse(is.na(num_daily_exposure) == TRUE, 0, num_daily_exposure)) %>%
  dplyr::mutate(bin_exposure_daily = ifelse(is.na(bin_exposure_daily) == TRUE, 0, bin_exposure_daily))
  
utility_day <- utility %>% as.data.frame() %>%
  dplyr::select(PRIME_DPS_, NAME)%>%
  dplyr::group_by(PRIME_DPS_, NAME) %>% 
  dplyr::mutate(time_utc_daily = NA) %>% 
  complete(time_utc_daily = as.character(c(
    seq.POSIXt(
      from = as.POSIXct("2017-03-01", tz ="UTC"),
      to = as.POSIXct("2020-12-31", tz ="UTC"),
      by = "day"))))

daily_lightning_exposure_complete<- full_join(utility_day, daily_lightning_exposure_complete)%>% 
  dplyr::mutate(num_daily_lightning_exposure = ifelse(is.na(num_daily_exposure) == TRUE, 0, num_daily_exposure)) %>%
  dplyr::mutate(bin_exposure_lightning_daily = ifelse(is.na(bin_exposure_daily) == TRUE, 0, bin_exposure_daily))

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/extreme weather data/lightning-points")
write.fst(daily_lightning_exposure_complete, "lightning-daily-utility.fst")
write.fst(hourly_lightning_exposure_complete,"lightning-hourly-utility.fst")


