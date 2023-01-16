# Find the number of households and establishments to get census customer
# estimates by year.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)


# Read --------------------------------------------------------------------

# Households --------------------------------------------------------------

# 2017 occupied housing units
hh_2017 <-
  read_csv(
    here(
      "data_proc",
      "raw_data",
      "households_census_data",
      "nhgis0006_ds233_20175_county_E.csv"
    )
  ) %>%
  select(
    state = STATE,
    state_fips = STATEA,
    county = COUNTY,
    county_fips = COUNTYA,
    total_occupied_hh = AH37E001
  ) %>%
  mutate(year = 2017)

# 2018 occupied hh
hh_2018 <-
  read_csv(
    here(
      "data_proc",
      "raw_data",
      "households_census_data",
      "nhgis0006_ds239_20185_county_E.csv"
    )
  ) %>%
  select(
    state = STATE,
    state_fips = STATEA,
    county = COUNTY,
    county_fips = COUNTYA,
    total_occupied_hh = AJ1UE001
  ) %>%
  mutate(year = 2018)

# 2019 occupied hh
hh_2019 <-
  read_csv(
    here(
      "data_proc",
      "raw_data",
      "households_census_data",
      "nhgis0006_ds244_20195_county_E.csv"
    )
  ) %>%
  select(
    state = STATE,
    state_fips = STATEA,
    county = COUNTY,
    county_fips = COUNTYA,
    total_occupied_hh = ALZLE001
  ) %>%
  mutate(year = 2019)

# 2019 occupied hh
hh_2020 <-
  read_csv(
    here(
      "data_proc",
      "raw_data",
      "households_census_data",
      "nhgis0006_ds249_20205_county_E.csv"
    )
  ) %>%
  select(
    state = STATE,
    state_fips = STATEA,
    county = COUNTY,
    county_fips = COUNTYA,
    total_occupied_hh = AMUFE001
  ) %>%
  mutate(year = 2020)

# join all yrs
hh_all_years <- bind_rows(hh_2018, hh_2019, hh_2020)


# Establishments ----------------------------------------------------------

estab_2018 <-
  read_csv(
    here(
      "data_proc",
      "raw_data",
      "establishments_census_data",
      "CBP2018.CB1800CBP_data_with_overlays_2022-07-22T114055.csv"
    ),
    skip = 1
  ) %>%
  select(
    id,
    county = `Geographic Area Name`,
    naics_code = `Meaning of NAICS code`,
    size = `Meaning of Employment size of establishments code`,
    num_of_estab = `Number of establishments`
  )

estab_2018 <- estab_2018 %>%
  filter(naics_code == 'Total for all sectors' &
           size == 'All establishments') %>%
  mutate(
    state_fips = str_sub(id, start = 10, end = 11),
    county_fips = str_sub(id, start = 12, end = 14)
  ) %>%
  select(county_long = county, state_fips, county_fips, num_of_estab) %>%
  mutate(year = 2018)


estab_2019 <-
  read_csv(
    here(
      "data_proc",
      "raw_data",
      "establishments_census_data",
      "CBP2019.CB1900CBP_data_with_overlays_2022-07-22T114003.csv"
    ),
    skip = 1
  ) %>%
  select(
    id,
    county = `Geographic Area Name`,
    naics_code = `Meaning of NAICS code`,
    size = `Meaning of Employment size of establishments code`,
    num_of_estab = `Number of establishments`
  )

estab_2019 <- estab_2019 %>%
  filter(naics_code == 'Total for all sectors' &
           size == 'All establishments') %>%
  mutate(
    state_fips = str_sub(id, start = 10, end = 11),
    county_fips = str_sub(id, start = 12, end = 14)
  ) %>%
  select(county_long = county, state_fips, county_fips, num_of_estab) %>%
  mutate(year = 2019)

estab_2020 <- read_csv(
  here(
    "data_proc",
    "raw_data",
    "establishments_census_data",
    "CBP2020.CB2000CBP_data_with_overlays_2022-06-22T143028.csv"
  ),
  skip = 1
) %>%
  select(
    id,
    county = `Geographic Area Name`,
    naics_code = `Meaning of NAICS code`,
    size = `Meaning of Employment size of establishments code`,
    num_of_estab = `Number of establishments`
  )

estab_2020 <- estab_2020 %>%
  filter(naics_code == 'Total for all sectors' &
           size == 'All establishments') %>%
  mutate(
    state_fips = str_sub(id, start = 10, end = 11),
    county_fips = str_sub(id, start = 12, end = 14)
  ) %>%
  select(county_long = county, state_fips, county_fips, num_of_estab) %>%
  mutate(year = 2020)

all_estab <- bind_rows(estab_2018, estab_2019, estab_2020)

census_estimates <- hh_all_years %>% 
  left_join(all_estab)

# replace missing establishments with "0" because these are for small counties
# reasonable assumption bc very small counties likely have no establishments
census_estimates <- census_estimates %>% 
  mutate(num_of_estab = ifelse(is.na(num_of_estab), 0, num_of_estab)) %>% 
  mutate(census_estimate_customers = total_occupied_hh + num_of_estab) 

write_csv(
  census_estimates,
  here(
    "data_proc",
    "data",
    "county_level_census_cust_estimates.csv"
  )
)
