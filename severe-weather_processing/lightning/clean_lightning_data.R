# clean for merge with power outage data

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

lightning_strikes <- read_csv(here("data","2_processed","lightning","lightning-counties.csv")) %>% 
  janitor::clean_names() %>% 
  dplyr::rename(fips = geoid) %>% 
  dplyr::mutate(time_utc = as_datetime(time_utc, tz = "UTC")) %>%  # ensure that date time is read as UTC 
  dplyr::mutate(time_bit = as_datetime(time_utc, tz = "Etc/GMT-12")) %>% 
  dplyr::mutate(day = as_date(time_bit, tz = "Etc/GMT-12")) %>%
  dplyr::select(day, fips) %>% 
  dplyr::group_by(day, fips) %>%
  dplyr::filter(row_number() == 1) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(lightning = 1) %>% 
  dplyr::group_by(fips) %>% 
  tidyr::complete(day = c(
    seq.POSIXt(
      from = as.POSIXct("2018-01-01", tz = "Etc/GMT-12"),
      to = as.POSIXct("2020-12-31", tz ="Etc/GMT-12"),
      by = "day"))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(lightning = ifelse(is.na(lightning) == TRUE, 0, lightning)) %>% 
  dplyr::mutate(day = as_date(day, tz = "Etc/GMT-12"))

n_distinct(lightning_strikes$fips) # 3096: Note, there are several "missing counties", but this might be due to several reasons 

# Write CSV

write_csv(lightning_strikes, here("data","2_processed","lightning","lightning_strikes.csv"))

  