# Want to estimate the number of customers by state so eventually we can get
# county customer estimates and coverage information.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)


# Read --------------------------------------------------------------------

# read customer counts

# list eia dataset names
eia_sets <-
  list.files(here("data_proc", "raw_data", "EIA"), full.names = TRUE)

# read into a list for easy processing
cust_counts <-
  lapply(eia_sets,
         read_excel,
         skip = 3,
         col_names = F)

# set column types so that we can bind rows
cust_counts <-
  lapply(X = cust_counts,
         FUN = mutate,
         across(everything(), as.character))

# remove column that weirdly appears in only one year so we can bind rows
# i guess 2019 was a long form year for the short-form utilities
cust_counts[[2]] <- cust_counts[[2]] %>% select(-c("...10"))
colnames(cust_counts[[2]]) <- colnames(cust_counts[[1]])

# get columns we want to count customers by state and year
cust_counts <- lapply(
  X = cust_counts,
  FUN = select,
  year = "...1",
  utility_name = "...3",
  part = "...4",
  customer_counts = "...24",
  state_ab = "...7"
)

# set types for adding customer counts
cust_counts <-
  bind_rows(cust_counts) %>%
  mutate(customer_counts = as.numeric(na_if(x = customer_counts, y = '.'))) %>%
  drop_na(customer_counts)

# get final customer counts by state and year
cust_counts <- cust_counts %>%
  filter(part != 'C') %>%
  group_by(state_ab, year) %>%
  summarise(eia_state_total_cust = sum(as.numeric(customer_counts), 
                                       na.rm = TRUE))


# Write -------------------------------------------------------------------

write_csv(cust_counts,
          here("data_proc", "data", "eia_state_total_customers_by_year.csv"))

