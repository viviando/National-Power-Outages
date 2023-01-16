# Going to join customers served specifications so that we can estimate
# power outages more quickly.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)


# Do ----------------------------------------------------------------------

# first calculate customers served estimates.
# going to do functions cause that's like, county-level
all_counties <-
  readRDS(here("data_proc", "data", "raw_with_fips.RDS"))

# list files in the county folder:
counties <-
  list.files(here("data_proc", "data", "rand_sample_working"),
             full.names = TRUE)

for (file in counties) {
  county_df <- readRDS(file)
  print(file)
  
  if (dim(county_df)[1] > 0) {
    single_county_estimates <- all_counties %>%
      mutate(year = year(recorded_date_time)) %>%
      filter(
        clean_state_name == unique(county_df$clean_state_name),
        clean_county_name == unique(county_df$clean_county_name)
      ) %>%
      group_by(clean_state_name, clean_county_name, city_name, year) %>%
      summarise(
        customers_tracked = max(customers_tracked, na.rm = TRUE),
        customers_out = max(customers_out, na.rm = TRUE)
      )
    
    single_county_estimates <- single_county_estimates %>%
      group_by(clean_state_name, clean_county_name, city_name, year) %>%
      mutate(estimate_to_use = max(customers_tracked, customers_out, na.rm = TRUE))
    
    single_county_estimates <-
      single_county_estimates %>% select(clean_state_name,
                                         clean_county_name,
                                         city_name,
                                         year,
                                         customers_served = estimate_to_use)
    
    write_csv(
      single_county_estimates,
      here(
        "data_proc",
        "data",
        "county_cust_estimates",
        paste0(
          unique(county_df$clean_state_name),
          unique(county_df$clean_county_name),
          '_estimates_by_city_county.csv'
        )
      )
    )
  }
}

