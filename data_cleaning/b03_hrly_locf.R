# Do LOCF on missing observations where the gaps are small - less than
# a certain number of hours.
# Goal is to impute data that we think might be randomly missing and that
# we have reasonable confidence in replacing, but not other stuff. 


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(zoo)
library(lubridate)
library(here)

# Constants ---------------------------------------------------------------

hour_thrshld <- dhours(4)
hour_num <- 4

# Helper ------------------------------------------------------------------

add_hourly_locf <- function(utility_county_city_df) {
  
  new_locf_rep <- utility_county_city_df$customers_out_api_on
  
  i = 1
  while (i < dim(utility_county_city_df)[1]) {
    j = i + 1
    if (is.na(utility_county_city_df$customers_out_api_on[j])) {
      while (is.na(utility_county_city_df$customers_out_api_on[j]) &
             (j < length(utility_county_city_df$customers_out_api_on))) {
        j = j + 1
      }
      k = j - 1 # index to replace to
      l = i + 1 # index to replace from
      m = l + (hour_num*6) - 1 # index if gap is greater than X hours, then we can carry forward to X hrs only
      if (utility_county_city_df$date[k] -
          utility_county_city_df$date[l] < hour_thrshld) {
        new_locf_rep <-
          replace(new_locf_rep,
                  l:k,
                  utility_county_city_df$customers_out_api_on[i])
      }
      if (utility_county_city_df$date[k] -
          utility_county_city_df$date[l] >= hour_thrshld
      ) {
        new_locf_rep <- 
          replace(new_locf_rep,
                  l:m,
                  utility_county_city_df$customers_out_api_on[i])
      }
      i = j
    }
    else{
      i = i + 1
    }
  }
  
  utility_county_city_df$new_locf_rep <- new_locf_rep
  utility_county_city_df
}

# Do ----------------------------------------------------------------------

# list files in county folder
counties <-
  list.files(here("data_proc", "data", "rand_sample_working"),
             full.names = TRUE)

start_time <- Sys.time()
for (file in counties) {
  county <- read_rds(file)
  
  unique_combos <- county %>%
    arrange(utility_name,
            clean_state_name,
            clean_county_name,
            city_name,
            fips) %>%
    select(utility_name,
           clean_state_name,
           clean_county_name,
           city_name,
           fips) %>%
    distinct()
  
  hourly_LOCFs <- vector("list", NROW(unique_combos))
  
  for (i in 1:NROW(unique_combos)) {
    hourly_LOCFs[[i]] <- county %>%
      filter(
        utility_name == unique_combos$utility_name[i],
        clean_state_name == unique_combos$clean_state_name[i],
        clean_county_name == unique_combos$clean_county_name[i],
        city_name == unique_combos$city_name[i],
        fips == unique_combos$fips[i]
      )
  }
  
  for (i in 1:NROW(unique_combos)) {
    hourly_LOCFs[[i]] <-
      add_hourly_locf(utility_county_city_df = hourly_LOCFs[[i]])
  }
  
  county <- bind_rows(hourly_LOCFs)
  
  # clause for removing a county if the js don't differ
  counts <- county %>%
    mutate(year = year(date)) %>%
    select(
      utility_name,
      clean_county_name,
      clean_state_name,
      city_name,
      fips,
      year,
      new_locf_rep
    ) %>%
    distinct()
  
  counts <- counts %>%
    group_by(utility_name,
             clean_county_name,
             clean_state_name,
             city_name,
             fips,
             year) %>%
    summarise(n = n())
  
  counts <-
    counts %>%
    filter(n == 1) %>%
    mutate(city_year = paste0(city_name, year))
  
  county <-
    county %>% 
    mutate(year = year(date)) %>%
    mutate(city_year = paste0(city_name, year)) %>%
    filter(!(city_year %in% counts$city_year))
    
  if (dim(county)[1] > 0){
    write_rds(county, file)
  }
  print(unique(county$clean_county_name))
  
  
}
stop_time <- Sys.time()




