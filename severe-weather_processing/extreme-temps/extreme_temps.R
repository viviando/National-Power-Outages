# title: "Mapping anomalously hot days_2018_2020"
# author: "Brittany Shea" (Adapted by Alex Northrop)
# date: "2022-10-21"

# Download temperature data: https://github.com/rmp15/PRISM-grids-into-FIPS-ZIP-censustract-USA/tree/main/output/fips/tmean

library(viridis)
library(tmap)
library(tidyverse)
library(lubridate)
library(sf)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(raster)
library(sp)
library(plyr)
library(graticule)
library(zoo)
library(dplyr)
library(MetBrewer)
library(conflicted)
library(here)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("here", "here")

# Load data

setwd("/Users/alexnorthrop/Documents/Research/Projects/National Power Outages/data/1_raw/prism/tmean")
file_list <- list.files(path="/Users/alexnorthrop/Documents/Research/Projects/National Power Outages/data/1_raw/prism/tmean") # just update this with where your data is stored
tdat <- lapply(file_list, readRDS)
temp_data <- as.data.frame(do.call(rbind, tdat))



# Change format of date columns

temp_data  <- temp_data  %>%
  select(date, fips, tmean) %>%
  dplyr::mutate(date = lubridate::dmy(date)) %>%
  dplyr::mutate(week_num = lubridate::epiweek(date))

temp_data53  <- temp_data  %>% filter(week_num == 53) %>% select(date) %>% unique() # interesting - these only appear on some years
temp_data$week_num[which(temp_data$week_num == "53")] = "52"

temp_data_1981_2010 <- temp_data %>% filter(date > "1980-12-30" & date < "2011-01-01")
temp_data_2018_2020 <- temp_data %>% filter(date > "2017-12-30")


# Group "bind_weighted_area_raster_fips_tmean_daily_1981_2010" by week and county, then calculate 85th and 15th percentile using weekly long-term average

dname = 'hot_85th'
cname = 'cold_15th'

fips85thpercentile = temp_data_1981_2010 %>% 
  group_by(week_num,fips) %>%
  dplyr::summarise(hot_85th = quantile(tmean, probs = 0.85)) %>%
  dplyr::mutate(fips = as.numeric(as.character(fips)))

fips15thpercentile = temp_data_1981_2010 %>% 
  group_by(week_num,fips) %>%
  dplyr::summarise(cold_15th = quantile(tmean, probs = 0.15)) %>%
  dplyr::mutate(fips = as.numeric(as.character(fips)))

# Group "bind_weighted_area_raster_fips_tmean_daily_2018_2020" by week and county, then find count of number of days over a threshold (>27°C) and under (<5°C)

dname = 'tmean'
cname = 'tmean'

threshold_h = 24 # Changed this threshold to account for power lines: https://ieeexplore.ieee.org/abstract/document/9123900
threshold_c = 0 # Since using tmean, this likely includese freezing temperature during some point of the day 

over24_tmean_daily_2018_2020 <- temp_data_2018_2020 %>%
  dplyr::mutate(over_24 = ifelse(get(dname) > threshold_h,1,0)) %>%
  dplyr::group_by(fips) %>%
  dplyr::mutate(fips = as.numeric(as.character(fips)))

under0_tmean_daily_2018_2020 <- temp_data_2018_2020 %>%
  dplyr::mutate(under_0 = ifelse(get(dname) < threshold_c,1,0)) %>%
  dplyr::group_by(fips) %>%
  dplyr::mutate(fips = as.numeric(as.character(fips)))

# Perform left join

join_tmean_daily_over24_85th_2018_2020 <- over24_tmean_daily_2018_2020 %>%
  left_join(fips85thpercentile, by = c("week_num" = "week_num", "fips" = "fips"))

join_tmean_daily_under0_15th_2018_2020 <- under0_tmean_daily_2018_2020 %>%
  left_join(fips15thpercentile, by = c("week_num" = "week_num", "fips" = "fips"))

# Create new column to determine whether tmean_daily meets 2 thresholds

join_tmean_daily_over24_85th_2018_2020 <- join_tmean_daily_over24_85th_2018_2020 %>%
  dplyr::mutate(anomhot = ifelse(over_24 == 1 & tmean >= hot_85th, 1,0))

join_tmean_daily_under0_15th_2018_2020 <- join_tmean_daily_under0_15th_2018_2020 %>%
  dplyr::mutate(anomcold = ifelse(under_0 == 1 & tmean <= cold_15th, 1,0))

# Find number of hot days by county over time

county_count_hot <- join_tmean_daily_over24_85th_2018_2020 %>% 
  group_by(fips) %>%
  dplyr::summarize(total_hot = sum(anomhot))

county_count_cold <- join_tmean_daily_under0_15th_2018_2020 %>% 
  group_by(fips) %>%
  dplyr::summarize(total_hot = sum(anomcold))

# Pad for a common FIPS 

join_tmean_daily_over24_85th_2018_2020$fips <- str_pad(join_tmean_daily_over24_85th_2018_2020$fips, width=5, side="left", pad="0")
join_tmean_daily_under0_15th_2018_2020$fips <- str_pad(join_tmean_daily_under0_15th_2018_2020$fips, width=5, side="left", pad="0")

heat_exposure <- join_tmean_daily_over24_85th_2018_2020 %>% 
  select(fips, date, anomhot) %>% 
  dplyr::rename(day = date)

cold_exposure <- join_tmean_daily_under0_15th_2018_2020 %>% 
  select(fips, date, anomcold) %>% 
  dplyr::rename(day = date)

write_fst(heat_exposure, "/Users/alexnorthrop/Documents/Research/Projects/National Power Outages/data/2_processed/heat/anomhot.fst")

write_fst(cold_exposure, "/Users/alexnorthrop/Documents/Research/Projects/National Power Outages/data/2_processed/heat/anomcold.fst")
