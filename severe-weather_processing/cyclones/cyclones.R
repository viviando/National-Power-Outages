####**************************
# Code Description ####
#*Goal: Obtain Daily Hurricane Exposure, 2018-2020 
#*Created 10/17/2022
####**************************

require(tigris)
options(tigris_use_cache = TRUE)

# Load Shapefile 

tracks <- read_sf(here("data","1_raw","ibtracs","IBTrACS.NA.list.v04r00.lines")) %>% 
  janitor::clean_names() %>% 
  filter(season == 2018 | season == 2019 | season == 2020) # Get the years of interest 

# Get to common CRS 

tracks <- st_transform(tracks, 4269)

# Get ISO_TIME in datetime format 
# Confirmd to be in UTC: https://www.ncei.noaa.gov/sites/default/files/2021-07/IBTrACS_v04_column_documentation.pdf

tracks$iso_time = as_datetime(tracks$iso_time, tz = "UTC") # In UTC per documentation 
tracks$iso_time = as_datetime(tracks$iso_time, tz = "Etc/GMT-12")

# Create buffer (100km)

tracks_buf <- tracks %>% 
  st_buffer(dis = 100000) %>%   
  dplyr::select(name, season, iso_time, nature, lat, lon, geometry)

# Download counties 

counties <- counties(year = 2019) %>% 
  janitor::clean_names() 
  
counties <- counties %>% 
  sf::st_as_sf()%>% 
  sf::st_transform(4269) %>%
  dplyr::mutate(fips = paste(statefp, countyfp, sep=""))

# Check counties 

st_crs(counties) == st_crs(tracks_buf) # TRUE 

# Create an exposure variable 

cyclone_exposure <- counties %>% st_join(tracks_buf, left=TRUE) %>% 
  dplyr::select(statefp,countyfp,fips,iso_time,season) %>%
  dplyr::mutate(cyclone_exposure = ifelse(is.na(season) == FALSE, 1, 0)) %>% 
  dplyr::mutate(date = date(iso_time)) %>% 
  group_by(fips,date) %>% 
  dplyr::summarize(exposure = sum(cyclone_exposure)) %>% 
  ungroup() %>% 
  dplyr::mutate(cyclone = ifelse(exposure > 0, 1, 0))

cyclone_exposure_complete <- as.data.frame(cyclone_exposure) %>% 
  select(fips, date, cyclone) %>% 
  group_by(fips) %>% 
  complete(date = c(
    seq.POSIXt(
      from = as.POSIXct("2018-01-01", tz ="UTC"),
      to = as.POSIXct("2020-12-31", tz ="UTC"),
      by = "day"))) %>% 
  ungroup() %>% 
  dplyr::mutate(cyclone = ifelse(is.na(cyclone) == TRUE, 0, cyclone)) %>% 
  dplyr::rename(day = date) %>% 
  na.omit() # counties with no cylcone exposure

write_fst(cyclone_exposure_complete, here("data","2_processed","cyclones","cyclone_exposure.fst"))
