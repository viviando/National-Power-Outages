# Expand Out Power Outages

# This script expands power outage data into a time series, from its current
# form where the dataset only includes entries for changes in customers_out.
# It does this one county at a time, and saves each expanded county
# in the 'expanded counties' folder in the 'data' folder.

# It keeps the groupings we've made.
# This script uses code from Matt's "example_for_shenyue".


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(zoo)
library(here)
library(lubridate)

set.seed(7)


# Data --------------------------------------------------------------------
raw_counties <-
  read_rds(here("data_proc", "data", "raw_with_fips.RDS")) %>% 
  ungroup()

# Helpers -----------------------------------------------------------------

# Flag missing values
flag_missing_values <- function(df) {
  df <-
    df %>% group_by(
      clean_state_name,
      clean_county_name,
      city_name,
      utility_name,
      fips,
      recorded_date_time
    ) %>%
    arrange(
      clean_state_name,
      clean_county_name,
      city_name,
      utility_name,
      fips,
      recorded_date_time,
      desc(customers_out)
    ) %>%
    mutate(missing = 0:(n() - 1)) %>%
    ungroup() %>%
    mutate(
      customers_out = ifelse(missing == 1,-99, customers_out),
      datetime = ifelse(
        missing == 1,
        recorded_date_time + lubridate::minutes(10),
        recorded_date_time
      )
    ) %>%
    mutate(datetime = lubridate::as_datetime(datetime))
  df
}

# Make ten-minute time intervals
ten_min_int <- function(df) {
  df <- df %>%
    mutate(
      year = lubridate::year(datetime),
      month = lubridate::month(datetime),
      day = lubridate::day(datetime),
      hour = lubridate::hour(datetime),
      minute = 10 * floor(lubridate::minute(datetime) / 10)
    ) %>%
    mutate(date = lubridate::ymd_hm(sprintf(
      "%s-%s-%s %s:%s",
      year,
      month,
      day,
      hour,
      minute
    ))) %>%
    select(-datetime,-hour,-day,-month,-year,-missing)
  # eliminate duplicate rows - take later observation to minimize error
  df <- df %>%
    group_by(utility_name,
             clean_state_name,
             clean_county_name,
             city_name,
             date) %>%
    slice_max(recorded_date_time, with_ties = FALSE)
  
  df
}

# Total minutes
tot_mins <- function(df) {
  df <- df %>%
    group_by(utility_name,
             clean_state_name,
             clean_county_name,
             city_name,
             fips) %>%
    mutate(total_minutes = as.integer(difftime(lead(date), date))) %>%
    mutate(total_minutes = ifelse(is.na(total_minutes), 
                                  # fix for end time problem
                                  as.integer(difftime(
                                    lubridate::ymd_hm("2021-01-01 00:00"),
                                    date
                                  )),
                                  total_minutes)) %>%
    ungroup()
  df
}

# Long dataframe with last observation carried forward
# Function from Matt. Says that LOCF needs to be checked for accuracy.
# May carry forward observation from another group incorrectly?

return_county_longform <- function(df) {
  unique_combos <- df %>%
    select(utility_name,
           clean_state_name,
           clean_county_name,
           city_name,
           fips) %>%
    distinct()
  
  customers_outLOCF <- vector("list", NROW(unique_combos))
  
  for (i in 1:NROW(unique_combos)) {
    single_city <- df %>%
      filter(
        utility_name == unique_combos$utility_name[i],
        clean_state_name == unique_combos$clean_state_name[i],
        clean_county_name == unique_combos$clean_county_name[i],
        city_name == unique_combos$city_name[i],
        fips == unique_combos$fips[i]
      )
    
    last_cty_date <- tail(single_city$recorded_date_time, n=1)
    
    single_city_frame <- single_city %>% expand(
      nesting(
        utility_name,
        clean_state_name,
        clean_county_name,
        city_name,
        fips
      ),
      date = seq(
        from = floor_date(as.POSIXct(min(
          df$recorded_date_time
        ), tz = "UTC"), unit = '10 minutes'),
        to = floor_date(as.POSIXct(max(
          df$recorded_date_time
        ), tz = "UTC"), unit = '10 minutes'),
        by = "10 min"
      )
    )
    
    to_join <- single_city %>%
      select(
        utility_name,
        clean_state_name,
        clean_county_name,
        city_name,
        fips,
        customers_out,
        date
      ) %>%
      arrange(clean_state_name,
              clean_county_name,
              city_name,
              utility_name,
              fips,
              date)
    
    single_city_frame <- single_city_frame %>% left_join(to_join)
    
    customers_outLOCF[[i]] <- single_city_frame %>%
      mutate(customers_out_NA = ifelse(date < last_cty_date, 
                                       zoo::na.locf(single_city_frame$customers_out, na.rm = FALSE),
                                       NA))
    
  }
  
  
  all_cities <- bind_rows(customers_outLOCF)
  all_cities <-
    all_cities %>% mutate(customers_out_api_on =
                            ifelse(customers_out_NA == -99, NA, 
                                   customers_out_NA))
  all_cities
}


# Actual expansion and writing --------------------------------------------

l <- raw_counties

k <- sort(unique(l$state_county))

for (j in 1:length(k)) {
  # process expansion
  print(j)
  county_df <- raw_counties %>% filter(state_county == k[[j]])
  county_df <- flag_missing_values(county_df)
  county_df <- ten_min_int(county_df)
  county_df <- tot_mins(county_df)
  county_df_long <- return_county_longform(county_df)
  # write
  write_rds(county_df_long,
            here(
              "data_proc",
              "data",
              "rand_sample_working",
              paste0("expanded_", k[[j]], ".RDS")
            ))
}



