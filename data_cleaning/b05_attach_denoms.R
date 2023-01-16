# Join customer estimates from EIA to the live customer estimate data,
# calculate person-coverage, and then eliminate counties with insufficient
# coverage.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(zoo)


# Do ----------------------------------------------------------------------

counties <-
  list.files(here("data_proc", "data", "hourly_county"),
             full.names = TRUE)

counties <- lapply(FUN = readRDS, X = counties)
counties <- bind_rows(counties) %>%
  mutate(
    state_fips = str_sub(
      string = fips,
      start = 1,
      end = 2
    ),
    county_fips = str_sub(
      string = fips,
      start = 3,
      end = 5
    )
  ) %>%
  mutate(year = year(hour))

eia_estimates <-
  read_csv(here(
    "data_proc",
    "data",
    "downscaled_county_customer_estimates.csv"
  )) %>%
  mutate(state_fips = str_pad(state_fips, 2, pad = "0"))


# Do a error correction procedure -----------------------------------------

errors <- counties %>%
  mutate(yearmon = zoo::as.yearmon(hour)) %>%
  group_by(clean_county_name, yearmon) %>%
  summarise(m = min(customers_out_total))

counties <- counties %>%
  mutate(yearmon = zoo::as.yearmon(hour)) %>%
  left_join(errors)

counties <-
  counties %>% mutate(customers_out_total = customers_out_total - m)


# Add coverage estimates --------------------------------------------------

counties <- counties %>%
  left_join(eia_estimates) %>%
  select(
    clean_state_name,
    clean_county_name,
    fips,
    year,
    hour,
    customers_out_total,
    customers_served_total,
    downscaled_county_estimate
  )


# Exclude low person coverage ---------------------------------------------

coverage_estimates <- counties %>%
  group_by(clean_state_name, clean_county_name, fips, year) %>%
  summarise(
    customers_served_total = max(customers_served_total),
    downscaled_county_estimate = max(downscaled_county_estimate)
  ) %>%
  mutate(pc = customers_served_total / downscaled_county_estimate) %>%
  mutate(pc = case_when(pc > 1 ~ 1,
                        TRUE ~ pc))

coverage_estimates <- coverage_estimates %>%
  filter(!is.na(pc)) %>%
  select(-customers_served_total,-downscaled_county_estimate)

counties <- counties %>%
  left_join(coverage_estimates)

# counties <- counties %>%
#   filter(pc > 0.5) %>%
#   ungroup()

# can use this filtering function in the final version

# Write -------------------------------------------------------------------

write_rds(counties,
          here("data_proc", "data", "data_with_coverage_exclusions_sample.RDS"))
