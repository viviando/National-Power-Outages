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

setwd("~/Desktop/lightning-data-lis/03 final-data")
dta <- read_fst("lighting-flashes-2017-2020.fst")


#only need 2018-2020

dta <- dta %>% filter(year(time_utc) != 2017)


################################################################################
# 1. read in county boundaries
################################################################################

US_counties <- counties(year = 2019) 
US_counties_sf <- US_counties %>% sf::st_as_sf()%>% sf::st_transform("WGS84")

st_crs(US_counties_sf)
################################################################################
# 2. make lightning data spatial:
################################################################################

dta_sf <- sf::st_as_sf(dta, coords = c("flash_longitude", "flash_latitude"),crs = "WGS84") # is this the correct projection?

st_crs(dta_sf)

################################################################################
# 3. intersect
################################################################################

lighting_county <- st_join( dta_sf,US_counties_sf,  join = st_intersects) %>%
  select(time_utc, GEOID, COUNTYFP, NAME)

lighting_county_nona <- lighting_county %>% 
  na.omit() %>%
  as.data.frame()%>%
  select(-geometry)

write.csv(lighting_county_nona, "lightning-counties.csv")



