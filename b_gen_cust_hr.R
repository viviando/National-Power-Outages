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

####***********************
####* Part 1: Create variable that considers reported customers out and estimated county pop ####
####* Note that estimated county pop is households and not population
####***********************

outage_event_expanded <- read_csv(here::here("Data", "Outputs", "outage_event_expanded.csv"))

tmp_customer_time_expanded <- outage_event_expanded %>%
  select(state_name, county_name, fips, year, final_cust_denominator, cust_out_hr, customer_time) %>%
  group_by(year, fips) %>%
  mutate(sum_customer_time = sum(cust_out_hr, na.rm = TRUE)) %>% 
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

customer_time_nonstd <- customer_time_expanded %>%
  group_by(fips) %>%
  mutate(avg_customer_time = mean(sum_customer_time)) %>%
  select(state_name, county_name, fips, avg_customer_time) %>%
  filter(row_number() == 1)

write_csv(customer_time_nonstd, here::here("Data", "Outputs", "customer_time_nonstd.csv")) 

customer_time_std <- customer_time_expanded %>%
  mutate(customer_time_std = sum_customer_time/final_cust_denominator) %>%
  group_by(fips) %>%
  mutate(avg_customer_time_std = mean(customer_time_std)) %>%
  select(state_name, county_name, fips, avg_customer_time_std) %>%
  filter(row_number() == 1)

write_csv(customer_time_std, here::here("Data", "Outputs", "customer_time_std.csv"))
