# Libraries ---------------------------------------------------------------
library(stringr)
library(spdep)
library(rgdal)
library(magrittr)
library(ggplot2)
library(sf) 
library(tidyverse)
library(dplyr)
library(cdlTools) #this is for fips to statename conversion
library(cowplot)
library(ggthemes)
library(here)

# Read in data ------------------------------------------------------------
outage90 <- read_csv(file = here::here("Data", "Outputs", "fips_w_po_and_0_po.csv")) %>%
  group_by(fips) %>% 
  mutate_at(vars(starts_with("tot")), mean) %>% 
  mutate(n_years_available = n()) %>% 
  filter(row_number() == 1) 

#* outage90_rounded is used to obtain information about yearly averages of PO frequency among states and counties 
#* this differs from outage90 because we want the ROUNDED reports since 5.3 1+ hour outages/year doesn't mean anything. 
#* What is .3 of an outage? It makes more sense to round for interpretation
outage90_rounded <- outage90 %>% 
  mutate_at(vars(starts_with("tot")), round, 0) %>% 
  select(-year)


# Prep outage data for alpha map ------------------------------------------

# 1+ hour outage
df_gt_0_po <- subset(outage90_rounded, tot_po_yr > 0)
df_gt_0_po$decile <- Hmisc::cut2(df_gt_0_po$tot_po_yr, g=4)
table(df_gt_0_po$decile)
df_antijoin_po <- anti_join(outage90_rounded, df_gt_0_po, by = "fips")

outage90_rounded_map <- rbind(df_gt_0_po, df_antijoin_po) %>% 
  mutate(decile_v2 = ifelse(tot_po_yr == 0, "0", as.character(decile)))
unique(outage90_rounded_map$decile)
unique(outage90_rounded_map$decile_v2)



#version 2 var contains counties with 0 outages
outage90_rounded_map$decile_v2 <- factor(outage90_rounded_map$decile_v2, levels = c("0",
                                                                                                        "[  1, 49)",
                                                                                                        "[ 49, 76)",
                                                                                                        "[ 76,107)",
                                                                                                        "[107,414]"))

decile_list <- as.list(sort(unique(as.character(outage90_rounded_map$decile_v2))))
outage90_rounded_map <- outage90_rounded_map %>% 
  mutate(ntile = case_when(decile_v2 == "0" ~ 0,
                           decile_v2 == decile_list[1] ~ 1,
                           decile_v2 == decile_list[2] ~ 2,
                           decile_v2 == decile_list[3] ~ 3,
                           decile_v2 == decile_list[4] ~ 4))

write_csv(outage90_rounded_map, here("Data", "Outputs", "bivar_mapping", "po1_map_dta.csv"))

# 8 hour outage
df_gt_0_po <- subset(outage90_rounded, tot_po8 > 0)
df_gt_0_po$decile <- Hmisc::cut2(df_gt_0_po$tot_po8, g=4)
table(df_gt_0_po$decile)
df_antijoin_po <- anti_join(outage90_rounded, df_gt_0_po, by = "fips")

outage90_rounded_map <- rbind(df_gt_0_po, df_antijoin_po) %>% 
  mutate(decile_v2 = ifelse(tot_po8 == 0, "0", as.character(decile)))
unique(outage90_rounded_map$decile)
unique(outage90_rounded_map$decile_v2)

#version 2 var contains counties with 0 outages
outage90_rounded_map$decile_v2 <- factor(outage90_rounded_map$decile_v2, levels = c("0",
                                                                                    "[1, 3)",
                                                                                    "3",
                                                                                    "[4, 7)",
                                                                                    "[7,35]"))

decile_list <- as.list(levels(outage90_rounded_map$decile_v2))
outage90_rounded_map <- outage90_rounded_map %>% 
  mutate(ntile = case_when(decile_v2 == "0" ~ 0,
                           decile_v2 == decile_list[2] ~ 1,
                           decile_v2 == decile_list[3] ~ 2,
                           decile_v2 == decile_list[4] ~ 3,
                           decile_v2 == decile_list[5] ~ 4))

write_csv(outage90_rounded_map, here("Data", "Outputs", "bivar_mapping", "po8_map_dta.csv"))

# Customers out non standardized
counties_n_years <- read_csv(here::here("Data", "Outputs", "counties_n_years.csv"))

customer_time_nonstd <- read_csv(here::here("Data", "Outputs", "customer_time_nonstd_w_new_dta_all_yrs.csv")) %>% 
  left_join(., counties_n_years, by = "fips") %>% 
  rename(n_years_available = n_year_available) %>% 
  mutate(avg_customers_out = round(avg_customers_out, 0))

df_gt_0_custout <- subset(customer_time_nonstd, avg_customers_out > 0)
hist(df_gt_0_custout$avg_customers_out)
summary(df_gt_0_custout$avg_customers_out)
df_gt_0_custout$decile <- Hmisc::cut2(round(df_gt_0_custout$avg_customers_out, 3), g=4)
table(df_gt_0_custout$decile)
df_antijoin_custout <- anti_join(customer_time_nonstd, df_gt_0_custout, by = "fips") %>% 
  mutate(decile = ifelse(avg_customers_out == 0, "0", as.character(decile)))

customer_time_nonstd_map <- rbind(df_gt_0_custout, df_antijoin_custout)
unique(customer_time_nonstd_map$decile)


#version 2 var contains counties with 0 outages
customer_time_nonstd_map$decile_v2 <- factor(customer_time_nonstd_map$decile, levels = c("0",
                                                                                    "[     1,    3169)",
                                                                                    "[  3169,   39825)",
                                                                                    "[ 39825,  163281)",
                                                                                    "[163281,10045051]"))

decile_list <- as.list(sort(unique(as.character(customer_time_nonstd_map$decile_v2))))
customer_time_nonstd_map <- customer_time_nonstd_map %>% 
  mutate(ntile = case_when(decile_v2 == "0" ~ 0,
                           decile_v2 == decile_list[1] ~ 1,
                           decile_v2 == decile_list[2] ~ 2,
                           decile_v2 == decile_list[3] ~ 3,
                           decile_v2 == decile_list[4] ~ 4))

write_csv(customer_time_nonstd_map, here::here("Data", "Outputs", "bivar_mapping", "custoutnonstd_map_dta.csv"))
 
# Customers out standardized
customer_time_std <- read_csv(here::here("Data", "Outputs", "customer_time_std_w_new_dta_all_yrs.csv")) %>% 
  left_join(., counties_n_years, by = "fips") %>% 
  rename(n_years_available = n_year_available) %>% 
  mutate(avg_customer_time_std = round(avg_customer_time_std, 0))

# df_gt_0_custout <- subset(customer_time_std, round(avg_customer_time_std, 0) > 0)
# summary(df_gt_0_custout$avg_customer_time_std)
# hist(df_gt_0_custout$avg_customer_time_std)
# 
# sum(df_gt_0_custout$avg_customer_time_std < 1) #853
# sum(df_gt_0_custout$avg_customer_time_std > 1 & df_gt_0_custout$avg_customer_time_std < 5) #754
# sum(df_gt_0_custout$avg_customer_time_std >= 5 & df_gt_0_custout$avg_customer_time_std < 20) #643
# sum(df_gt_0_custout$avg_customer_time_std >= 20) #151
# 
# df_gt_0_custout$decile <- Hmisc::cut2(round(df_gt_0_custout$avg_customer_time_std, 2), g=4)
# table(df_gt_0_custout$decile)
# df_antijoin_custout <- anti_join(customer_time_std, df_gt_0_custout, by = "fips")
# 
# customer_time_std_map <- dplyr::bind_rows(df_gt_0_custout, df_antijoin_custout) %>% 
#   mutate(decile = ifelse(round(avg_customer_time_std, 2) == 0, "0", as.character(decile)))
# unique(customer_time_std_map$decile)

customer_time_std_map <- customer_time_std %>% 
  mutate(decile = case_when(avg_customer_time_std == 0 ~ "0",
                            avg_customer_time_std == 1 ~ "1",
                            avg_customer_time_std > 1 & avg_customer_time_std < 5 ~ "(   1, 5)",
                            avg_customer_time_std >= 5 & avg_customer_time_std < 20 ~"[   5, 20)",
                            avg_customer_time_std >= 20 ~ "[20,251]"))


#version 2 var contains counties with 0 outages
customer_time_std_map$decile_v2 <- factor(customer_time_std_map$decile, levels = c("0",
                                                                                   "1",
                                                                                   "(   1, 5)",
                                                                                   "[   5, 20)",
                                                                                   "[20,251]"))

decile_list <- as.list(levels((customer_time_std_map$decile_v2)))
customer_time_std_map <- customer_time_std_map %>% 
  mutate(ntile = case_when(decile_v2 == "0" ~ 0,
                           decile_v2 == decile_list[2] ~ 1,
                           decile_v2 == decile_list[3] ~ 2,
                           decile_v2 == decile_list[4] ~ 3,
                           decile_v2 == decile_list[5] ~ 4))

unique(customer_time_std_map$ntile)
write_csv(customer_time_std_map, here::here("Data", "Outputs", "bivar_mapping", "custoutstd_map_dta.csv"))


