####***********************
#### Code Description ####
# Author: Vivian
# Date: 7/12/2021
# Goal: Create person time values using the scaled down county estimates
####***********************

####***********************
# PART 0: read in libraries, set directories, etc ####
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
library(tidyverse)
options(scipen = 999)

# out_state <- "~/Desktop/0_PhD/Spring 2021/Rotations - Joan/Power Outages/poweroutage/output/state_df/"
# out_threshold <- "~/Desktop/0_PhD/Spring 2021/Rotations - Joan/Power Outages/poweroutage/output/threshold/"
# out_threshold_summarized <- "~/Desktop/0_PhD/Spring 2021/Rotations - Joan/Power Outages/poweroutage/output/threshold/summarized/"
# out_path <- "~/Desktop/0_PhD/Spring 2021/Rotations - Joan/Power Outages/poweroutage/output/"
# out_customer_time <- "~/Desktop/0_PhD/Spring 2021/Rotations - Joan/Power Outages/poweroutage/output/customer_time"

####***********************
####* Create variable that considers reported customers out and estimated county pop ####
####* Note that estimated county pop is households and not population
####***********************

outage_event_expanded <- read_csv(here::here("Data", "Outputs", "outage_event_expanded_new_dta.csv"))

# customer time non standardized ------------------------------------------

# take all known year information
customer_time_nonstd <- outage_event_expanded %>% 
  select(clean_state_name, clean_county_name, fips, year, downscaled_county_estimate, customers_out_total) %>%
  dplyr::group_by(year, fips) %>% 
  mutate(sum_customer_time = sum(customers_out_total, na.rm = TRUE)) %>% 
  filter(row_number() == 1) %>% 
  dplyr::group_by(fips) %>% 
  mutate(avg_customers_out = mean(sum_customer_time, na.rm = TRUE)) %>% 
  filter(row_number() == 1) %>% 
  select(-sum_customer_time)

write_csv(customer_time_nonstd, here::here("Data", "Outputs", "customer_time_nonstd_w_new_dta_all_yrs.csv")) 

# outage information assuming less than 3 years reflects other years

tmp_customer_time_expanded <- outage_event_expanded %>%
  select(clean_state_name, clean_county_name, fips, year, downscaled_county_estimate, customers_out_total) %>%
  group_by(year, fips) %>%
  mutate(sum_customer_time = sum(customers_out_total, na.rm = TRUE)) %>% 
  filter(row_number() == 1) %>%
  group_by(fips) %>% 
  mutate(n_year_available = n())

tmp_pt_avail_2yr <- tmp_customer_time_expanded %>% 
  filter(n_year_available == 2) %>% 
  group_by(fips) %>% 
  mutate(sum_customer_time = mean(sum_customer_time)) %>% #replace this missing year with the mean of previous years
  filter(row_number() == 1) #keep only one row when there are 2 years of data

tmp_pt_avail_1yr <- tmp_customer_time_expanded %>% 
  filter(n_year_available == 1) %>% 
  group_by(fips) %>% 
  mutate(sum_customer_time = mean(sum_customer_time)) #want to add this dataset 2x to the original

customer_time_expanded <- rbind(tmp_customer_time_expanded, tmp_pt_avail_2yr, tmp_pt_avail_1yr, tmp_pt_avail_1yr) %>%
  arrange(fips)

customer_time_nonstd_yrs_assumed <- customer_time_expanded %>%
  group_by(fips) %>%
  mutate(avg_customer_time = mean(sum_customer_time)) %>%
  select(clean_state_name, clean_county_name, fips, avg_customer_time) %>%
  filter(row_number() == 1)

write_csv(customer_time_nonstd_yrs_assumed, here::here("Data", "Outputs", "customer_time_nonstd_w_new_dta_yrs_assumed.csv")) 

# keep information for only 3 years
tmp_pt_avail_3yr <- tmp_customer_time_expanded %>% 
  filter(n_year_available == 3) %>% 
  group_by(fips) %>% 
  mutate(sum_customer_time = mean(sum_customer_time)) %>% 
  filter(row_number() == 1)

write_csv(tmp_pt_avail_3yr, here::here("Data", "Outputs", "customer_time_nonstd_w_new_dta_3yr_only.csv")) 



# keep information for 2 3 years
tmp_pt_avail_23yr <- tmp_customer_time_expanded %>% 
  filter(n_year_available == 3 |n_year_available == 2) %>% 
  group_by(fips) %>% 
  mutate(sum_customer_time = mean(sum_customer_time)) %>% 
  filter(row_number() == 1)

write_csv(tmp_pt_avail_23yr, here::here("Data", "Outputs", "customer_time_nonstd_w_new_dta_23yr_only.csv")) 


# customer time  standardized ------------------------------------------

#all year information
customer_time_std <- outage_event_expanded %>%
  group_by(fips, year) %>% 
  mutate(sum_customer_time = sum(customers_out_total, na.rm = TRUE),
         customer_time_std = sum_customer_time/downscaled_county_estimate) %>% 
  filter(row_number() == 1) %>% 
  group_by(fips) %>%
  mutate(avg_customer_time_std = mean(customer_time_std)) %>%
  select(clean_state_name, clean_county_name, fips, avg_customer_time_std) %>%
  filter(row_number() == 1)

write_csv(customer_time_std, here::here("Data", "Outputs", "customer_time_std_w_new_dta_all_yrs.csv"))

# outage information assuming less than 3 years reflects other years
customer_time_std_yrs_assumed <- customer_time_expanded %>%
  mutate(customer_time_std = sum_customer_time/downscaled_county_estimate) %>%
  group_by(fips) %>%
  mutate(avg_customer_time_std = mean(customer_time_std)) %>%
  select(clean_state_name, clean_county_name, fips, avg_customer_time_std) %>%
  filter(row_number() == 1)

write_csv(customer_time_std, here::here("Data", "Outputs", "customer_time_std_w_new_dta_yrs_assumed.csv"))

# keep information for only 3 years
customer_time_std <- tmp_pt_avail_3yr %>%
  mutate(customer_time_std = sum_customer_time/downscaled_county_estimate) %>%
  group_by(fips) %>%
  mutate(avg_customer_time_std = mean(customer_time_std)) %>%
  select(clean_state_name, clean_county_name, fips, avg_customer_time_std) %>%
  filter(row_number() == 1)

write_csv(customer_time_std, here::here("Data", "Outputs", "customer_time_std_w_new_dta_3yrs_only.csv"))

# keep information for 2 3 years
customer_time_std_3 <- tmp_pt_avail_3yr %>%
  mutate(customer_time_std = sum_customer_time/downscaled_county_estimate) %>%
  group_by(fips) %>%
  mutate(avg_customer_time_std = mean(customer_time_std)) %>%
  select(clean_state_name, clean_county_name, fips, avg_customer_time_std) %>%
  filter(row_number() == 1)


customer_time_std_2 <- tmp_pt_avail_2yr %>%
  mutate(customer_time_std = sum_customer_time/downscaled_county_estimate) %>%
  group_by(fips) %>%
  mutate(avg_customer_time_std = mean(customer_time_std)) %>%
  select(clean_state_name, clean_county_name, fips, avg_customer_time_std) %>%
  filter(row_number() == 1) %>% 
  rbind(., customer_time_std_3)

write_csv(customer_time_std_2, here::here("Data", "Outputs", "customer_time_std_w_new_dta_23yrs_only.csv"))

# list of counties with less than 3 years of data
counties_lt_3yrs <- outage_event_expanded %>%
  select(clean_state_name, clean_county_name, fips, year, downscaled_county_estimate, customers_out_total) %>%
  group_by(fips, year) %>% 
  filter(row_number() == 1) %>% 
  group_by(fips) %>% 
  mutate(n_year_available = n()) %>% 
  group_by(fips) %>% 
  filter(row_number() == 1) %>% 
  filter(n_year_available != 3) %>% 
  select(fips)

write_csv(counties_lt_3yrs, here::here("Data", "Outputs", "counties_lt_3yrs.csv"))

nrow(counties_lt_3yrs)

counties_lt_2yrs <- outage_event_expanded %>%
  select(clean_state_name, clean_county_name, fips, year, downscaled_county_estimate, customers_out_total) %>%
  group_by(fips, year) %>% 
  filter(row_number() == 1) %>% 
  group_by(fips) %>% 
  mutate(n_year_available = n()) %>% 
  group_by(fips) %>% 
  filter(row_number() == 1) %>% 
  filter(n_year_available < 2) %>% 
  select(fips)

write_csv(counties_lt_2yrs, here::here("Data", "Outputs", "counties_lt_2yrs.csv"))

counties_n_years <- outage_event_expanded %>%
  select(clean_state_name, clean_county_name, fips, year, downscaled_county_estimate, customers_out_total) %>%
  group_by(fips, year) %>% 
  filter(row_number() == 1) %>% 
  group_by(fips) %>% 
  mutate(n_year_available = n()) %>% 
  group_by(fips) %>% 
  filter(row_number() == 1) %>% 
  select(fips, n_year_available)

write_csv(counties_n_years, here::here("Data", "Outputs", "counties_n_years.csv"))



