# Read in raw power outage data, clean names and add FIPS codes to state and
# county combinations. Goal is to get as many accurate county FIPS as possible.

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(tidytext)
library(snakecase)
library(readxl)


# Read --------------------------------------------------------------------

# raw data from POUS
raw_pous_read <-
  read_csv(
    here(
      "data_proc",
      "raw_data",
      "POUS_Export_Raw_CityByUtility_20170101_20201231.csv"
    )
  )#, n_max = 10000)

# FIPS crosswalk
fips_xwalk <-
  read_csv(here("data_proc", "raw_data", "nhgis0009_ds244_20195_county.csv"))

# manual name spelling correction file
spell_correction_file <-
  read_xlsx(here("data_proc", "data", "missing_county_key.xlsx")) %>%
  select(clean_state_name = clean_state, clean_county_name, correct_county)

# state reassignment correction file
state_reassignment <-
  read_csv(here("data_proc", "data", "state_reassignment_fix.csv"))


# Tidy --------------------------------------------------------------------

# clean column names and also get clean names of states and counties in POUS
# remove punctuation and other threats to matching like stop words
# 'municipality', 'township', etc.

raw_pous <- raw_pous_read %>%
  clean_names()

clean_pous_state_names <-
  data.frame(state_name = c(unique(raw_pous$state_name))) %>%
  mutate(clean_state_name = str_replace_all(state_name, "[^[:alnum:]]", "")) %>%
  mutate(clean_state_name = str_replace_all(clean_state_name, "[:space:]", "")) %>%
  mutate(clean_state_name = tolower(clean_state_name))

clean_pous_county_names <-
  data.frame(county_name = c(unique(raw_pous$county_name))) %>%
  mutate(clean_county_name = str_replace_all(county_name, "[^[:alnum:]]", "")) %>%
  mutate(clean_county_name = str_replace_all(clean_county_name, "[:space:]", "")) %>%
  mutate(clean_county_name = tolower(clean_county_name))

# add clean names to pous data
raw_pous <- raw_pous %>%
  left_join(clean_pous_state_names) %>%
  left_join(clean_pous_county_names)

# # check that they are unique
# s <- raw_pous %>%
#   select(state_name, county_name, clean_state_name, clean_county_name) %>%
#   distinct() %>%
#   group_by(clean_state_name, clean_county_name) %>%
#   mutate(n = n())

# clean column names and get clean names of states and counties in xwalk
fips_xwalk <-
  fips_xwalk %>%
  mutate(fips = paste0(substr(GISJOIN, 2, 3), substr(GISJOIN, 5, 7))) %>%
  select(state_name = STATE, county_name = COUNTY, fips)

xwalk_state_names <-
  data.frame(state_name = c(unique(fips_xwalk$state_name))) %>%
  mutate(clean_state = str_replace_all(state_name, "[^[:alnum:]]", "")) %>%
  mutate(clean_state = str_replace_all(clean_state, "[:space:]", "")) %>%
  mutate(clean_state = tolower(clean_state))

xwalk_county_names <-
  data.frame(county_name = c(unique(fips_xwalk$county_name))) %>%
  mutate(clean_county = str_replace(county_name, " [Cc]{1}ounty", "")) %>%
  mutate(clean_county = str_replace(clean_county, " [Pp]{1}arish", "")) %>%
  mutate(clean_county = str_replace(clean_county, " [Bb]{1}orough", "")) %>%
  mutate(clean_county = str_replace(clean_county, " Census Area", "")) %>%
  mutate(clean_county = str_replace(clean_county, " [Mm]{1}unicipality", "")) %>%
  mutate(clean_county = str_replace_all(clean_county, "[^[:alnum:]]", ""))

xwalk_county_names <- xwalk_county_names  %>%
  mutate(clean_county = str_replace_all(clean_county, "[:space:]", "")) %>%
  mutate(clean_county = tolower(clean_county))

fips_xwalk <-
  fips_xwalk %>%
  left_join(xwalk_state_names) %>%
  left_join(xwalk_county_names) %>%
  select(clean_county_name = clean_county,
         clean_state_name = clean_state,
         fips)

# # check that they are unique
# s <- fips_xwalk %>%
#   select(state_name, county_name, clean_state, clean_county) %>%
#   distinct() %>%
#   group_by(clean_state, clean_county) %>%
#   mutate(n = n())

# save which state-county combos appear in the FIPS file
state_county_combos <- fips_xwalk %>%
  mutate(state_county = paste0(clean_state_name, clean_county_name)) %>%
  select(state_county)


# REVISE??
# want to eliminate Canada and US territories; keep only continental states
# this happends to be exactly what is in xwalk file
# (although Puerto Rico is in the xwalk but not the POUS dataset - that
# doesn't matter though)
continental <- unique(fips_xwalk$clean_state_name)
raw_pous <- raw_pous %>%
  filter(clean_state_name %in% continental)

# County names ------------------------------------------------------------

# want to clean county names so they match our FIPS crosswalk file
# replace names spelled differently with good spelling
raw_pous <-
  raw_pous %>%
  mutate(state_county = paste0(clean_state_name, clean_county_name))

raw_pous <- raw_pous %>%
  left_join(spell_correction_file) %>%
  mutate(
    clean_county_name = case_when(
      state_county %in% state_county_combos$state_county ~ clean_county_name,
      (!is.na(correct_county)) ~ correct_county,
      TRUE ~ clean_county_name
    )
  ) %>%
  select(-c(correct_county, state_county))

# replace fake combinations based on data errors with the correct state and
# county names
raw_pous <- raw_pous %>%
  left_join(state_reassignment) %>%
  mutate(clean_state_name = case_when(
    !is.na(new_clean_state_name) ~ new_clean_state_name,
    TRUE ~ clean_state_name
  )) %>%
  select(-c(new_state_name, new_clean_state_name))

# add FIPS codes based on good spelling and state reassignment
raw_pous <-
  raw_pous %>%
  left_join(fips_xwalk) %>%
  mutate(state_county = paste0(clean_state_name, clean_county_name))

raw_pous <-
  raw_pous %>% filter(state_county %in% state_county_combos$state_county)

# select out relevant cols
raw_pous <-
  raw_pous %>% select(
    utility_name,
    clean_state_name,
    clean_county_name,
    city_name,
    recorded_date_time,
    customers_tracked,
    customers_out,
    state_county,
    fips
  ) %>% 
  ungroup() 

# Write -------------------------------------------------------------------

saveRDS(raw_pous, here("data_proc", "data", "raw_with_fips.RDS"))
