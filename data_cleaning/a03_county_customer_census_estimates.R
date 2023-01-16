# Get estimates of county customers based on EIA down scaling


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)

# Read --------------------------------------------------------------------

# customer estimates
census_cust_estimates <-
  read_csv(
    here(
      "data_proc",
      "data",
      "county_level_census_cust_estimates.csv"
    ),
    col_types = cols(
      state = col_character(),
      state_fips = col_character(),
      county = col_character(),
      county_fips = col_character(),
      total_occupied_hh = col_double(),
      year = col_double(),
      county_long = col_character(),
      num_of_estab = col_double(),
      census_estimate_customers = col_double()
    )
  ) %>%
  mutate(state_fips = as.numeric(state_fips))


# eia totals
eia_totals <-
  read_csv(here("data_proc", "data", "eia_state_total_customers_by_year.csv"))


# state name abbrev
state_abbrev <-
  read_csv(
    here("data_proc", "data", "state_names_abbrv.csv"),
    col_names = FALSE,
    col_types = cols(X1 = col_character(),
                     X2 = col_character(),
                     X3 = col_double())
  )
colnames(state_abbrev) <- c("state_name", "state_ab", "state_fips")


# Do ----------------------------------------------------------------------

downscaled <- eia_totals %>%
  left_join(state_abbrev) %>%
  left_join(census_cust_estimates) %>%
  select(
    year,
    state,
    county,
    state_fips,
    county_fips,
    eia_state_total_cust,
    census_estimate_customers
  )

downscaled <- downscaled %>%
  group_by(year, state) %>%
  mutate(census_est_state_total = sum(census_estimate_customers, 
                                      na.rm = TRUE)) %>%
  ungroup()

downscaled <-
  downscaled %>%
  mutate(p_of_state = census_estimate_customers / census_est_state_total)

downscaled <- downscaled %>%
  mutate(downscaled_county_estimate = p_of_state * eia_state_total_cust) %>%
  select(year,
         state,
         county,
         state_fips,
         county_fips,
         downscaled_county_estimate)

# Write -------------------------------------------------------------------

write_csv(downscaled,
          here(
            "data_proc",
            "data",
            "downscaled_county_customer_estimates.csv"
          ))






