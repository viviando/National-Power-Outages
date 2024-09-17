####**************************
# Code Description ####
#*Goal: Obtain Daily Wildfire Exposure, 2018-2020 
#*Created: 10/17/2022
#*Updated: 11/2/2022
####**************************

# Read in with read_libraries.R 

library(units)

##################################################
# STEP 1: Create a definitive list of fires      #
##################################################

# Source @@@: https://data-nifc.opendata.arcgis.com/datasets/nifc::interagencyfireperimeterhistory-all-years-view/explore?location=32.012796%2C-122.087025%2C3.96

sf_use_s2(FALSE)

# WFS (Corroborated by Ben as the best source for national fires)

wfs <- read_sf("/Users/alexnorthrop/Documents/Research/Projects/National Power Outages/data/1_raw/nifc/InterAgencyFirePerimeterHistory_All_Years_View") %>% 
  janitor::clean_names() %>% 
  filter(fire_year == "2018" | fire_year == "2019" | fire_year == "2020" |
           fire_year_2 == 2018 | fire_year_2 == 2019 | fire_year_2 == 2020 )  %>% 
  mutate(area = st_area(geometry)) %>% 
  mutate(area = drop_units(area)) %>% # originally in m^2 
  filter(area > 1000000) %>% # Hypothesize that only fires > 1km2 would disrupt power
  filter(agency != "AFS") %>% # Exclude data from AK 
  filter(feature_ca != "Prescribed Fire") %>% # Exclude prescribed fires 
  filter(objectid != "19412" & objectid != "19416" & objectid != "19541") %>% # Avoids geo_id duplicates since these are the unique identifiers for this dataset with no NA values 
  select(incident,fire_year,irwinid,unqe_fire,geo_id,source,agency,geometry) # Note: Above, AICC Final Wildfire Perimeters were selected vs. BIA perimeters since they were finalized
                                                                             # And because the AICC fires were larger indicating a more severe exposure of interest 
counties <- read_sf(here("data","shapefiles","cb_2015_us_county_500k")) %>% 
  janitor::clean_names() %>% 
  rename(fips = geoid)

# Full Fire History (With Dates)
# Note: Documentation states that these records may be incomplete; as such, 
# data were systematically reviewed if missing 

fires <- read_csv(here("data","1_raw","nifc","WFIGS_-_Wildland_Fire_Locations_Full_History.csv")) %>% 
  janitor::clean_names()

## Compile a list with the greatest number of matches (1734 of 1879)

wildfire_matches <- wfs %>% 
  filter(irwinid %in% fires$irwin_id | unqe_fire %in% fires$unique_fire_identifier | incident %in% fires$incident_name) 

wildfire_misses <- as.data.frame(wfs) %>% 
  filter(!irwinid %in% fires$irwin_id & !unqe_fire %in% fires$unique_fire_identifier & !incident %in% fires$incident_name) %>% 
  select(-geometry)

write_csv(wildfire_misses, here("data","wild_fire_misses.csv"))

as.data.frame(wildfire_misses) %>% 
  group_by(source) %>% 
  count() %>% 
  arrange(n)

##########################################################
# Step 2: Index of Sources, Missing Fires, and Data Link #
##########################################################
# 
#####  WFDSS: 1 

# LINK: Unknown 
# STATUS: Pending

#####  NPS: 11, 

# LINK: https://nifc.maps.arcgis.com/home/item.html?id=098ebc8e561143389ca3d42be3707caa#data
# STATUS: Pending. Data seem available but unable to pull the data directly; manual pull needed. (arcGIS) 

#####  USFS: 11 
#
# LINK: https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_FireOccurrenceAndPerimeter_01/MapServer
# STATUS: Pending. Data are there, but unsure how to pull. (arcGIS) 

##### CalFire FRAP: 16

# https://frap.fire.ca.gov/mapping/gis-data 
# STATUS: Complete (see next section)

##### BIA: 17

# LINK: Unknown
# STATUS: Pending. Data not located. 

#####  FWS: 42

# LINK: https://services.arcgis.com/QVENGdaPbd4LUkLV/arcgis/rest/services/USFWS_Wildfire_History_gdb/FeatureServer
# STATUS: Pending. Data should be on above link, but requires a token. (arcGIS) 

#####  BLM 74

# LINK: https://gis.blm.gov/arcgis/rest/services/fire/BLM_Natl_FirePerimeter/MapServer 
# STATUS: Pending. Data should be available on above link. (arcGIS) 

# Check CalFire for Matches 

ca_fires <- read_sf(here("data","1_raw","nifc","cal_fire","fire21_1.gdb")) %>% 
  janitor::clean_names() %>% 
  filter(year == "2018" | year == "2019" | year == "2020")

ca_misses <- ca_fires %>% 
  filter(fire_name %in% wildfire_misses$incident) %>% 
  select(fire_name, alarm_date, cont_date)

#############################################################
# Step 3: Combine data sources to get dates for spatial data#
#############################################################


# First get the WFS data into a cleaner format

wfs <- wfs %>% 
  rename(fire_name = incident) %>% 
  select(fire_name, fire_year, irwinid, unqe_fire,geo_id, geometry)

# Then fires into a cleaner format 

fires <- fires %>% 
  rename(irwinid = irwin_id) %>% 
  rename(unqe_fire = unique_fire_identifier) %>% 
  rename(fire_name = incident_name) 

# Create data frames to join

fire_1 <- fires %>% 
  mutate(join_source = "irwin") %>% 
  select(join_source, fire_name, irwinid, unqe_fire, fire_discovery_date_time, containment_date_time, control_date_time, fire_out_date_time)

fire_2 <- fires %>% 
  mutate(join_source = "fire_name") %>% 
  select(join_source, fire_name, irwinid, unqe_fire, fire_discovery_date_time, containment_date_time, control_date_time, fire_out_date_time)

fire_3 <- fires %>% 
  mutate(join_source = "unique_id") %>% 
  select(join_source, fire_name, irwinid, unqe_fire, fire_discovery_date_time, containment_date_time, control_date_time, fire_out_date_time)

fire_4 <- as.data.frame(ca_misses) %>% 
  mutate(join_source = "fire_name_ca") %>% 
  select(fire_name, join_source, alarm_date,cont_date) %>% 
  rename(fire_discovery_date_time = alarm_date) %>% 
  rename(containment_date_time = cont_date) %>% 
  mutate(irwinid = NA,
         unqe_fire = NA,
         control_date_time = NA,
         fire_out_date_time = NA) %>% 
select(join_source, fire_name, irwinid, unqe_fire, fire_discovery_date_time, containment_date_time, control_date_time, fire_out_date_time)


wf_a <- wfs %>%
  left_join(fire_1, by = "irwinid") %>% 
  filter(!is.na(join_source)) %>% 
  mutate(date_check = as.Date(fire_discovery_date_time)) %>% 
  mutate(year = year(date_check)) %>% 
  filter(year > 2017 & year < 2021) %>% 
  mutate(irwind_alt = NA) %>% 
  rename(fire_name = fire_name.x,
         fire_name_alt = fire_name.y,
         unqe_fire = unqe_fire.x,
         unqe_fire_alt = unqe_fire.y)

wf_b <- wfs %>% 
  left_join(fire_2, by = "fire_name") %>% 
  filter(!is.na(join_source)) %>% 
  mutate(date_check = as.Date(fire_discovery_date_time)) %>% 
  mutate(year = year(date_check)) %>% 
  filter(year > 2017 & year < 2021) %>% 
  mutate(fire_name_alt = NA) %>% 
  rename(irwinid = irwinid.x,
         irwind_alt = irwinid.y,
         unqe_fire = unqe_fire.x,
         unqe_fire_alt = unqe_fire.y)

wf_c <- wfs %>% 
  left_join(fire_3, by = "unqe_fire") %>% 
  filter(!is.na(join_source)) %>% 
  mutate(date_check = as.Date(fire_discovery_date_time)) %>% 
  mutate(year = year(date_check)) %>% 
  filter(year > 2017 & year < 2021) %>% 
  mutate(unqe_fire_alt = NA) %>% 
  rename(fire_name = fire_name.x,
         fire_name_alt = fire_name.y,
         irwinid = irwinid.x,
         irwind_alt = irwinid.y)

wf_d <- wfs %>% 
  left_join(fire_4, by = "fire_name") %>% 
  filter(!is.na(join_source)) %>% 
  mutate(date_check = as.Date(fire_discovery_date_time)) %>% 
  mutate(year = year(date_check)) %>% 
  filter(year > 2017 & year < 2021) %>% 
  mutate(fire_name_alt = NA) %>% 
  rename(irwinid = irwinid.x,
         irwind_alt = irwinid.y,
         unqe_fire = unqe_fire.x,
         unqe_fire_alt = unqe_fire.y)

wfs_long <- base::rbind(wf_a, wf_b, wf_c, wf_d)

#  Go through dates to ensure the best versions are included for the final data frame 
wfs_clean <- wfs_long %>% 
  filter(!is.na(containment_date_time)) %>% # Filters out instances where containment time is not found 
  filter(!duplicated(geo_id))

wfs_some_dates <- wfs_long %>% 
  filter(!geo_id %in% wfs_clean$geo_id) %>% 
  filter(!is.na(fire_out_date_time) | !is.na(control_date_time)) %>% 
  filter(!duplicated(geo_id))

wfs_no_dates <- wfs_long %>% 
  filter(!geo_id %in% wfs_clean$geo_id) %>% 
  filter(!geo_id %in% wfs_some_dates$geo_id) %>% 
  filter(!duplicated(geo_id))

wfs_mixed <- base::rbind(wfs_clean, wfs_some_dates, wfs_no_dates)

# Fill in containment dates with approximations if data not available (45 dates are approximated this way) 

wf_aprox <- wfs_mixed %>% 
  mutate(containment_date_time = ifelse(is.na(containment_date_time) == F, containment_date_time, control_date_time)) %>% 
  mutate(containment_date_time = ifelse(is.na(containment_date_time) == F, containment_date_time, fire_out_date_time)) 

# Prepare dataset for imputation 
# sum(is.na(wf_aprox$containment_date_time)) # 69 of 1717 require imputation 
# Note: I haven't found where several of the records are lost from 1734 to 1717 

wf_to_impute <- wf_aprox %>% 
  mutate(area = st_area(geometry)) %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  mutate(fire_discovery_date_time = as.numeric(as.Date(fire_discovery_date_time))) %>% 
  mutate(containment_date_time = as.numeric(as.Date(containment_date_time))) %>% 
  mutate(fire_days = (containment_date_time - fire_discovery_date_time)) %>% 
  select(geo_id, area, fire_days) 

# Create to link back later 
wf_start <- as.data.frame(wf_aprox) %>% 
  select(geo_id, fire_discovery_date_time)
  
#############################################################
# Step 4: Process imputation for missing time data          #
#############################################################

library(mice)

set.seed(7)

imputation <- mice(wf_to_impute, method = "cart", maxit=100)

wf_imputed <- complete(imputation) 

# No cases where this method failed (i.e. imputation was less than 0)

wf_imputed %>% 
 filter(fire_days < 0)

# Add back data  

wf_data <- wf_imputed %>% 
  select(geo_id, fire_days) %>% 
  left_join(wfs, by = "geo_id") %>% 
  left_join(wf_start, by = "geo_id") %>% 
  mutate(fire_start_n = as.numeric(as.Date(fire_discovery_date_time)),
         fire_end = as.Date(fire_start_n+fire_days),
         fire_start = as.Date(fire_discovery_date_time)) %>% 
  select(geo_id,fire_start,fire_end,geometry)

#############################################################
# Step 5: Link data to counties                             #
#############################################################

counties <- st_as_sf(counties)
wf_data <- st_as_sf(wf_data)

# Set CRS 

wf_data <- st_transform(wf_data, 4269)

st_crs(wf_data) == st_crs(counties)

# Join with counties 

wf_counties <- counties %>% 
  st_join(wf_data, left = TRUE) %>% 
  select(statefp,countyfp,fips,fire_start,fire_end) %>% 
  filter(!is.na(fire_start)) %>% 
  mutate(wf_date = map2(fire_start, fire_end, seq, by = 'day')) %>%
  unnest(wf_date) 

wf_counties <- as.data.frame(wf_counties) %>% 
  select(-geometry)

counties <- as.data.frame(counties) %>% 
  select(fips)

total_wf_counties <- counties %>% 
  left_join(wf_counties, by = "fips")

# Complete data as time series 

wf_exposure_complete <- total_wf_counties %>% 
  mutate(date = wf_date) %>% 
  select(fips, wf_date, date) %>% 
  group_by(fips) %>% 
  complete(date = c(
    seq.POSIXt(
      from = as.POSIXct("2018-01-01", tz ="UTC"),
      to = as.POSIXct("2020-12-31", tz ="UTC"),
      by = "day"))) %>% 
  ungroup() %>% 
  dplyr::mutate(wf = ifelse(is.na(wf_date) == TRUE, 0, 1)) %>% 
  rename(day = date) %>% 
  select(fips, day, wf) %>% 
  group_by(day, fips) %>% # Ensure there are no duplicate values 
  filter(row_number() == 1) %>% 
  ungroup()

# Write out 

write_fst(wf_exposure_complete, here("data","2_processed","wildfires","wf_exposure.fst"))

#############################################################
# Step 6: Create resource for manual data pull              #
#############################################################


wfs <- read_sf("/Users/alexnorthrop/Documents/Research/Projects/National Power Outages/data/1_raw/nifc/InterAgencyFirePerimeterHistory_All_Years_View") %>% 
  janitor::clean_names() %>% 
  filter(fire_year == "2018" | fire_year == "2019" | fire_year == "2020" |
           fire_year_2 == 2018 | fire_year_2 == 2019 | fire_year_2 == 2020 )  %>% 
  mutate(area = st_area(geometry)) %>% 
  mutate(area = drop_units(area)) %>% # originally in m^2 
  filter(area > 1000000) %>% # Hypothesize that only fires > 1km2 would disrupt power
  filter(agency != "AFS") %>% # Exclude data from AK 
  filter(feature_ca != "Prescribed Fire") %>% # Exclude prescribed fires 
  filter(!duplicated(geo_id)) %>% # Avoids geo_id duplicates since these are the unique identifiers for this dataset with no NA values 
  select(incident,fire_year,irwinid,unqe_fire,geo_id,source,agency,geometry) 

missing_fires <- as.data.frame(wfs) %>% 
  filter(!geo_id %in% wf_aprox$geo_id) %>% 
  mutate(fire_discovery_date = NA,
         fire_containment_date = NA) %>% 
  rename(fire_name = incident) %>% 
  select(fire_name, fire_year, fire_discovery_date, fire_containment_date, source, agency, unqe_fire, geo_id)

write_excel_csv(missing_fires, here("missing_fires_to_share.csv"))

