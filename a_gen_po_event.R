####***********************
#### Code Description ####
# Author: Vivian
# Date: 6/10/2021
# Goal: Given a cleaned POUS dataset, we aim to do the following
# (1) Generate power outage event indicator
#DATA NOTE: WHEN MERGING COUNTY_POP_CUST WITH ANY OF THE POUS DATA, YOU SHOULD STR_TO_TITLE(COUNTY_NAME) THE POUS DATA
####***********************

####***********************
# Part 0: read in libraries, set directories, etc ####
####***********************
library(raster)
library(openintro)
library(forcats)
library(stringi)
library(dplyr)
library(stringr) 
library(lubridate)
library(readxl)
library(data.table)
library(ggplot2)
library(carData)
library(usmap)
library(covidcast)
library(RColorBrewer)
library(readr)
library(tidyverse)
library(here)
options(scipen = 999)

# set a specific directory but otherwise use here()
out_threshold_county <- "~/Desktop/0_PhD/0_Research Projects/National Power Outages/Data/Output Tmp/cust_county_thresh_dta/"

####***********************
#### Part 1: Create variables of interest for all state-county-years at hourly level & combine ####
####* This is pretty involved and requires a loop but does aggregate to hourly level information
####*  Also combine all these datasets because they are currently separated out (due to being too large)
####***********************

#READ IN ALL THE CUSTOMER TIME DATA (WITH SUFFIX OF 1-5) - THIS IS A LOT OF DATA
out_state <- "~/Desktop/0_PhD/0_Research Projects/National Power Outages/Data/Output Tmp/state_ct_long_dta/"
file_list_pt <- list.files(path = out_state, pattern = "df_state_pt_long_")
for (data_name in 1:length(file_list_pt)){
  print(substr(file_list_pt[data_name], 1, 18))
  assign(substr(file_list_pt[data_name], 1, 18), read_csv(paste0(out_state, file_list_pt[data_name])))
}

#Adjust for ADDITIONAL inconsistencies with names in LA
#these values should match with key_state_county_fips key
df_state_pt_long_2 <- df_state_pt_long_2 %>% 
  mutate(county_name = ifelse(state_name == "Louisiana" & str_detect(county_name, "Desoto"),  str_replace(county_name, "Desoto", "De Soto"), county_name),
         county_name = ifelse(state_name == "Louisiana" & str_detect(county_name, "E. "),  str_replace(county_name, "E. ", "East "), county_name),
         county_name = ifelse(state_name == "Louisiana" & str_detect(county_name, "W. "),  str_replace(county_name, "W. ", "West "), county_name),
         county_name = ifelse(state_name == "Louisiana" & str_detect(county_name, "La Salle"),  str_replace(county_name, "La Salle", "Lasalle"), county_name))

# EVALUATE DISTRIBUTION OF % CUSTOMERS OUT FOR ALL STATE-COUNTY-YEAR
# THIS WILL TELL US WHICH THRESHOLDS MIGHT BE VIABLE FOR PO THRESHOLD METHOD
#merge with the "county_pop_cust.csv"
county_pop_cust <- read_csv(here::here("Data", "Outputs", "county_pop_cust.csv")) %>%
  mutate(state = abbr2state(state),
         fips = as.character(fips))


#*CREATE FUNCTION TO MERGE COUNTY POP WITH DF STATE AND CONVERT TO HOURLY LEVEL
#*THIS WILL ULTIMATELY OUTPUT CSV FILES SO WE CAN KEEP THEM AND REDOWNLOAD AS NEEDED
#*TO GET THE % CUSTOMERS AND GET THE DISTRIBUTION OF % OUT GENERAL AND IN SMALL/LARGE PLACES

#####ADD IN THE VARS HERE BEFORE THIS FUNCTION OR IN THIS FUNCTION
#merge with the state county key
key_state_county_fips <- read_csv(here::here("Data", "Keys", "key_state_county_fips.csv"))

f_merge_cust_county_hr <- function(df_state_name){
  
  df_mutated_tmp <- df_state_name %>%
    mutate(year = year(recorded_date_time),
           county_fips = as.character(county_fips),
           county_name = str_to_title(county_name)) %>%
    filter(year != 2017 | year != "2017") 
  
  #attach the read in state data file with the state-county-fips key because some get lost in 3_gen_clean_count_dta (ie - west virginia)
  df_mutated_tmp <- left_join(df_mutated_tmp, key_state_county_fips, by = c("state_name", "county_name")) %>%
    mutate(fips = ifelse(!is.na(county_fips.x), county_fips.x, county_fips.y)) 

  merged_hr_pt_cust <- left_join(df_mutated_tmp, county_pop_cust, by = c("year" = "pous_year", "fips" = "fips")) %>%
    select(utility_name, state_name, county_name, fips, city_name, customers_out, recorded_date_time, tim_diff, customer_time, cust_denominator) %>%
    mutate(hour_collapser = floor_date(ymd_hms(recorded_date_time), unit = "hour"),
           customers_out = as.numeric(customers_out),
           year = year(recorded_date_time)) %>%
    group_by(year, state_name, county_name) %>%
    mutate(n_utilities = n_distinct(utility_name)) %>%
    group_by(state_name, county_name, city_name, hour_collapser) %>%
    mutate(subcounty_hourly_avg = mean(customers_out)) %>%
    filter(row_number() == 1) %>%
    group_by(state_name, county_name, hour_collapser) %>%
    mutate(cust_out_hr = sum(subcounty_hourly_avg)) %>%
    filter(row_number() == 1) %>%
    mutate(pct_cust_out_hr = cust_out_hr/cust_denominator*100) %>%
    select(c(utility_name, state_name, county_name, fips, city_name, customers_out, recorded_date_time,
             tim_diff, customer_time, cust_denominator, hour_collapser, year, n_utilities,
             subcounty_hourly_avg, cust_out_hr, pct_cust_out_hr))
  
  write_csv(merged_hr_pt_cust, here::here("Data", "Output Tmp", "cust_county_thresh_dta", paste0("cust_county_hr_", str_sub(as.character(deparse(substitute(df_state_name))), -1, -1), ".csv")))
}

#apply the function to our long datasets to obtain variables of interest at the hourly level
#running all the below takes ~1 hour
f_merge_cust_county_hr(df_state_pt_long_1)
f_merge_cust_county_hr(df_state_pt_long_2)
f_merge_cust_county_hr(df_state_pt_long_3)
f_merge_cust_county_hr(df_state_pt_long_4)
f_merge_cust_county_hr(df_state_pt_long_5)

rm(list=ls(pattern="df_state_pt_long"))

#####NEXT STEPS: GET THE DISTRIBUTION OF THRESHOLDS FOR ALL THE DATA
####(1) READ IN ALL THE "EVAL_*" DATASETS AND EVALUATE THE MIN, MEAN, AND MAX FOR ALL STATES --> THEN DECIDE ON THRESHOLD FROM THERE
out_threshold_county <- "~/Desktop/0_PhD/0_Research Projects/National Power Outages/Data/Output Tmp/cust_county_thresh_dta/"
file_list_cust_county_hr <- list.files(path = out_threshold_county, pattern = "cust_county_hr_")

#read all the evaluations and then save to one datafile
county_cust_hr_all <- do.call("rbind", lapply(paste0(out_threshold_county, file_list_cust_county_hr), read_csv))

#get total unique utilities operating in our dataset before any exclusions
length(unique(county_cust_hr_all$utility_name)) 
length(unique(county_cust_hr_all$fips)) 

#* When working with the county_cust_hr_all dataset, we must remove/adjust several utilities
#* this is because I noticed some utilities in POUS not recorded in the theoretically global EIA dataset
#* after reaching out to POUS about these specific utilities, they provided information about how to move forward
#* By removing these utilities, we move from 3006 fips found in our data to 2975 fips codes (many of which are rural in NE)
county_cust_hr_all <- county_cust_hr_all %>% #this HAS fips for brooklyn and wv
  filter(utility_name != "Meeker Co-op Light & Power",
         utility_name != "Nebraska Public Power District") %>% 
  mutate(county_name = ifelse(utility_name == "Pennyrile Electric" | utility_name == "Blue Ridge Electric Cooperative", "Unknown", county_name)) %>% 
  arrange(fips, hour_collapser)

#get total unique utilities operating in our dataset AFTER exclusions of confirmed unreliable data
length(unique(county_cust_hr_all$utility_name)) #692
length(unique(county_cust_hr_all$fips)) #2976

# write_csv(county_cust_hr_all, (here::here("Data", "Outputs", "county_cust_hr_all.csv")))
# county_cust_hr_all <- read_csv(here::here("Data", "Outputs", "county_cust_hr_all.csv"))


####***********************
####* Part 2: Adjust time from UTC ####
####* Need to account for Daylight savings time (dls) and convert from UTC to local time
####***********************
#* use the excel file I created titled county_time_zones (ask Vivian if you need this)
#* helpful site: https://www.timetemperature.com/tzus/time_zone_boundaries.shtml 
df_county_time_zones <- read_xlsx(here::here("Data", "Time", "county_time_zones.xlsx")) %>% 
  mutate(county_name = str_replace(county_name, " County", ""))

ptz <- c("California", "Nevada", "Washington", "Oregon", "Idaho")
mtz <- c("Montana", "Idaho", "Wyoming", "Utah", "Colorado", "Arizona", "New Mexico", 
         "North Dakota", "South Dakota", "Nebraska", "Kansas", "Texas")
ctz <- c("North Dakota", "South Dakota", "Nebraska", "Kansas", "Texas", "Oklahoma", 
         "Minnesota", "Idaho", "Missouri", "Arkansas", "Louisiana", "Michigan", 
         "Indiana", "Kentucky", "Tennessee", "Mississippi", "Alabama", "Florida")

county_cust_hr_all_dls <- left_join(county_cust_hr_all, df_county_time_zones, by = c("state_name", "county_name")) %>% 
  filter(year(hour_collapser) != 2017) %>% 
  mutate(time_zone_county = ifelse(!is.na(time_zone), time_zone, case_when(state_name %in% ptz ~ "Pacific Time Zone",
                                                                           state_name %in% mtz ~ "Mountain Time Zone",
                                                                           state_name %in% ctz ~ "Central Time Zone",
                                                                           state_name == "Hawaii" ~ "Hawaii Aleutian Time Zone",
                                                                           state_name == "Alaska" ~ "Alaska Time Zone",
                                                                           TRUE ~ "Eastern Time Zone")),
         updated_time = case_when(time_zone_county == "Pacific Time Zone" ~ hour_collapser - 8*(60*60),
                                  time_zone_county == "Mountain Time Zone" ~ hour_collapser - 7*(60*60),
                                  time_zone_county == "Central Time Zone" ~ hour_collapser - 6*(60*60),
                                  time_zone_county == "Eastern Time Zone" ~ hour_collapser - 5*(60*60),
                                  time_zone_county == "Alaska Time Zone" ~ hour_collapser - 9*(60*60),
                                  time_zone_county == "Hawaii Aleutian Time Zone" ~ hour_collapser - 10*(60*60))) %>% 
  rename(hour_collapser_utc = hour_collapser,
         hour_collapser = updated_time)

#* we also need to account for daylight savings 
#* use this website for guidance: https://www.timeanddate.com/time/change/usa
#* create df_dls because for some reason, when I try using the actual hour_collapser variable
#* the corresponding time for 2:00:00 actually points to 5:00:00; extra complications but necessary for accuracy
#* Arizona and Hawaii do not do dls!!!
df_dls <- county_cust_hr_all_dls %>% 
  group_by(hour_collapser) %>% 
  filter(row_number() == 1) %>% 
  filter(year == 2018 & month(hour_collapser) == 3 & day(hour_collapser) == 11 & hour(hour_collapser) == 2 |
           year == 2018 & month(hour_collapser) == 11 & day(hour_collapser) == 4 & hour(hour_collapser) == 2 |
           year == 2019 & month(hour_collapser) == 3 & day(hour_collapser) == 10 & hour(hour_collapser) == 2 |
           year == 2019 & month(hour_collapser) == 11 & day(hour_collapser) == 3 & hour(hour_collapser) == 2 |
           year == 2020 & month(hour_collapser) == 3 & day(hour_collapser) == 8 & hour(hour_collapser) == 2 |
           year == 2020 & month(hour_collapser) == 11 & day(hour_collapser) == 1 & hour(hour_collapser) == 2) %>% 
  mutate(num_dls = as.numeric(hour_collapser)) %>% 
  dplyr::select(hour_collapser, num_dls) %>% 
  arrange(hour_collapser)

v_dls <- as.vector(df_dls$num_dls)       

county_cust_hr_all_dls <- county_cust_hr_all_dls %>% 
  mutate(hour_collapser_dls = case_when(hour_collapser < v_dls[1] ~ hour_collapser,
                                        v_dls[1] <= hour_collapser & hour_collapser < v_dls[2] ~ hour_collapser + (60*60),
                                        v_dls[2] <= hour_collapser & hour_collapser < v_dls[3] ~ hour_collapser - (60*60),
                                        v_dls[3] <= hour_collapser & hour_collapser < v_dls[4] ~ hour_collapser + (60*60),
                                        v_dls[4] <= hour_collapser & hour_collapser < v_dls[5] ~ hour_collapser - (60*60),
                                        v_dls[5] <= hour_collapser & hour_collapser < v_dls[6] ~ hour_collapser + (60*60),
                                        v_dls[6] <= hour_collapser ~ hour_collapser - (60*60)),
         hour_collapser_final = case_when(state_name != "Arizona" ~ hour_collapser_dls,
                                          state_name != "Hawaii" ~ hour_collapser_dls,
                                          TRUE ~ hour_collapser)) #for some reason, the regular ifelse did not work

# write_csv(county_cust_hr_all_dls, (here::here("Data", "Outputs", "county_cust_hr_all_dls.csv")))
county_cust_hr_all_dls <- read_csv((here::here("Data", "Outputs", "county_cust_hr_all_dls.csv")))


####***********************
#### Part 3: Investigate threshold to be used for pct customers out ####
####***********************
#Below investigates the threshold that should be used for pct customers out
# county_cust_hr_all_dls <- read_csv(paste0(out_threshold_summarized, "county_cust_hr_all_dls.csv"))
tmp_cust_quant <- county_cust_hr_all_dls %>% filter(pct_cust_out_hr != 0)
quantile(tmp_cust_quant$pct_cust_out_hr, probs = c(50, 75, 76, 80, 85, 90, 95)/100, na.rm = TRUE)
q1 <- unname(quantile(tmp_cust_quant$pct_cust_out_hr, prob=c(0.9, 0.95), na.rm = TRUE)[1])
q2 <- unname(quantile(tmp_cust_quant$pct_cust_out_hr, prob=c(0.9, 0.95), na.rm = TRUE)[2])
rm(tmp_cust_quant)

####***********************
#### Part 4: Add threshold var to create PO event ####
###* Based on the outputs from part 3
####***********************
county_cust_hr_all_new_denominator <- county_cust_hr_all_dls %>%
  arrange(state_name, county_name, hour_collapser_final) %>%
  group_by(year, state_name, county_name) %>%
  mutate(cust_out_hr = as.numeric(cust_out_hr),
         final_cust_denominator = as.numeric(cust_denominator)) %>%
  ungroup() %>% 
  select(-c(recorded_date_time, subcounty_hourly_avg)) %>%
  mutate(hr_diff_uncollapsed = as.numeric(difftime(hour_collapser_final, lag(hour_collapser_final), units = "hours")),
         i_gt_90pctle = ifelse(as.numeric(pct_cust_out_hr) > q1, 1, 0), #90th percentile),
         i_gt_95pctle = ifelse(as.numeric(pct_cust_out_hr) > q2, 1, 0)) %>%  #95th percentile
  drop_na(fips) %>% #can actually drop because fips = NA means county == Unknown OR a state-county pairing that doesn't exist
  arrange(state_name, county_name, year, hour_collapser_final) 

write_csv(county_cust_hr_all_new_denominator, here::here("Data", "Outputs", "county_cust_hr_all_new_denominator.csv"))

####***********************
#### Part 5: Compute duration of PO event using a function ####
####***********************

rm(list=ls(pattern="county_cust"))
county_cust_hr_all_new_denominator <- read_csv(here::here("Data", "Outputs", "county_cust_hr_all_new_denominator.csv"))

#* Function used to count the number of distinct power outage events
#* The reasons an outage is identified as new within a county are below
#* (1) 0-1 - if there is no outage but there is an outage
#* (2) 1-1 - if there is a duration time greater than 48 hours in between
f_outage_tracker <- function(outage_threshold){
  
  county <- eval(substitute(county_name), county_cust_hr_all_new_denominator)
  hr_diff_uncollapsed <- eval(substitute(hr_diff_uncollapsed), county_cust_hr_all_new_denominator)
  ind_outage <- eval(substitute(outage_threshold), county_cust_hr_all_new_denominator)
  
  j = 1
  out_track <- rep(0, nrow(county_cust_hr_all_new_denominator))
  for (i in 1:nrow(county_cust_hr_all_new_denominator)){
    
    #deal with the first row
    print(i)
    
    if (i == 1){
      if (ind_outage[[i]] == 0){
        out_track[[i]] = 0
      }
      else{
        out_track[[i]] = 1
      }
    }
    #deal with non-first row
    else{
      #if the consecutive row is 1-0 then set to 0 and increment the counter
      if (ind_outage[[i]] == 0 &
          ind_outage[[i - 1]] == 1){
        out_track[[i]] = 0
        j = j+1
      }
      #if the consecutive row is 0-1 then increment counter and set to new counter value
      else if (ind_outage[[i]] == 1 &
               ind_outage[[i - 1]] == 0){
        out_track[[i]] = j
      }
      #if the current row is 1-1 and the county previously are the same then set to counter
      else if (ind_outage[[i]] == 1 &
               ind_outage[[i - 1]] == 1 &
               county[[i]] == county[[i - 1]] &
               hr_diff_uncollapsed[[i]] <= 48 &
               hr_diff_uncollapsed[[i]] > 0){
        out_track[[i]] = j
      }
      #if the consecutive row is 1-1 but county is different or there is a long duration between outages, then increase counter
      else if (ind_outage[[i]] == 1 &
               ind_outage[[i - 1]] == 1 &
               (county[[i]] != county[[i - 1]] |
                hr_diff_uncollapsed[[i]] > 48 |
                hr_diff_uncollapsed[[i]] < 0)){
        j = j+1
        out_track[[i]] = j
      }
      #all other situations should be 0
      else{
        out_track[[i]] = 0
      }
    }
  }
  
  out_track
}

#run the functions and bind the results into one dataframe
out_track_all <- 
  bind_cols(
    f_outage_tracker(i_gt_90pctle),
    f_outage_tracker(i_gt_95pctle)
  ) %>% 
  rename(
    outage_90pctle = "...1",
    outage_95pctle = "...2"
  )

outage_event_expanded <- cbind(county_cust_hr_all_new_denominator, out_track_all) 
rm(out_track_all)
write_csv(outage_event_expanded, here::here("Data", "Outputs", "outage_event_expanded.csv")) 


####***********************
#### Part 6: Condense expanded PO data to obtain hourly counts of 1+ hour and 8+ hour PO ####
# We want to collapse expanded PO data to be shorter so we have information at a county-year-hour level of outage #s
####***********************
# outage_event_expanded <- read_csv(here::here("Data", "Outputs", "outage_event_expanded.csv"))
outage_event_90pctle <- outage_event_expanded %>% 
  select(-c(outage_95pctle)) %>%
  filter(outage_90pctle != 0) %>%
  group_by(outage_90pctle) %>%
  slice(c(1, n())) %>%
  mutate(po_duration_hr = as.numeric(difftime(lead(hour_collapser_final), hour_collapser_final, units = "hours")),
         po_duration_hr = ifelse(po_duration_hr == 0, 0.5, po_duration_hr)) %>%
  filter(row_number() == 1)

tmp <- outage_event_expanded %>% 
  filter(outage_90pctle != 0)

outage_event_95pctle <- outage_event_expanded %>% 
  select(-c(outage_90pctle)) %>%
  filter(outage_95pctle != 0) %>%
  group_by(outage_95pctle) %>%
  slice(c(1, n())) %>%
  mutate(po_duration_hr = as.numeric(difftime(lead(hour_collapser_final), hour_collapser_final, units = "hours")),
         po_duration_hr = ifelse(po_duration_hr == 0, 0.5, po_duration_hr)) %>% #do this step to remind myself that 0 po_duration_hr does not mean there is no outage; means the outage < 1 hr
  filter(row_number() == 1)

write_csv(outage_event_90pctle, here::here("Data", "Outputs", "outage_event_90pctle.csv")) 
write_csv(outage_event_95pctle, here::here("Data", "Outputs", "outage_event_95pctle.csv")) 

#The purpose of below is to add back counties still in data but with 0 PO events 
# write_csv(all_fips, here::here("Data", "Outputs", "all_fips.csv"))
all_fips <- read_csv(here::here("Data", "Outputs", "all_fips.csv"))

#*ENSURE THAT ALL COUNTIES HAVE 3 YEARS
#*IF THEY DONT HAVE ALL 3 YEARS OF DATA THEN FOLLOW THE FOLLOWING RULES
#*(1) 1 YEAR OF DATA: ASSUME OTHER YEARS HAVE THE SAME OUTPUT, IN THIS CASE, POWER OUTAGE EVENTS
#*(2) 2 YEARS OF DATA: TAKE THE AVERAGE OF OTHER YEARS AND IMPUTE

#90th percentile
outage_event_90pctle <- read_csv(here::here("Data", "Outputs", "outage_event_90pctle.csv"))

tmp_outage90 <- outage_event_90pctle %>% 
  group_by(year, fips) %>% 
  mutate(tot_po_yr = n(),
         tmp_ind_po4 = ifelse(po_duration_hr >= 4, 1, 0),
         tmp_ind_po8 = ifelse(po_duration_hr >= 8, 1, 0),
         tmp_ind_po24 = ifelse(po_duration_hr >= 24, 1, 0),
         tot_po4 = sum(tmp_ind_po4),
         tot_po8 = sum(tmp_ind_po8),
         tot_po24 = sum(tmp_ind_po24)) %>% 
  # mutate_at(vars(starts_with("tot")), sum) %>% 
  filter(row_number() == 1) %>% 
  group_by(fips) %>% 
  mutate(n_yr = n()) %>% 
  select(state_name, county_name, fips, final_cust_denominator, n_yr, starts_with("tot_po"))

tmp_outage90_2yrs <- tmp_outage90 %>% 
  filter(n_yr == 2) %>% 
  group_by(fips) %>% 
  mutate(tot_po_yr = mean(tot_po_yr),
         tot_po4 = mean(tot_po4),
         tot_po8 = mean(tot_po8),
         tot_po24 = mean(tot_po24)) %>% 
  # mutate_at(vars(starts_with("tot")), mean) %>% 
  filter(row_number() == 1)

tmp_outage90_1yr <- tmp_outage90 %>% 
  filter(n_yr == 1) #want to add this dataset 2x to the original

tmp_outage90_none_fips <- anti_join(all_fips, outage_event_90pctle, by = "fips")

replace_0 <- function(x)(ifelse(is.na(x), 0, x))
final_outage90 <- rbind(tmp_outage90, tmp_outage90_2yrs, tmp_outage90_1yr, tmp_outage90_1yr, tmp_outage90_none_fips) %>% 
  mutate_at(vars(starts_with(("tot"))), replace_0)

rm(list=ls(pattern="tmp_"))


write_csv(final_outage90, here::here("Data", "Outputs", "final_outage90.csv")) 













