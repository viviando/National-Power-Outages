####***********************
#### Code Description ####
# Author: Vivian
# Date: 10/17/2021
# Goal: Create tables for manuscript
# contour plot link: https://plotly.com/ggplot2/contour-plots/
# https://r-graphics.org/recipe-legend-label-text 
####***********************

####***********************
#PART 0: read in libraries, set directories, etc ####
####***********************
library(raster)
library(openintro)
library(forcats)
library(stringi)
library(dplyr)
library(stringr) 
library(lubridate)
library(readxl)
library(data.table)
library(ggplot2)
library(carData)
library(usmap)
library(covidcast)
library(viridis)
library(RColorBrewer)
library(readr)
library(tidyverse)
library(here)
library(writexl)
options(scipen = 999)

#libraries for contour plot
library(plotly)
library(metR)
library(devtools)
# install_github("geanders/countytimezones")
library(countytimezones)

####***********************
# Prep outage data ####
# set the regions and round outage data because what does it mean to have 0.33 of an outage per year --> nothing
####***********************
mountain_west <- c("Arizona", "Colorado", "Idaho", "Newmexico", "Montana", "Utah", "Nevada", "Wyoming")
pacific_west <- c("Alaska", "Hawaii", "California", "Oregon", "Washington")
south_atlantic <- c("Districtofcolumbia", "Delaware", "Florida", "Georgia", "Maryland", "Northcarolina",
                    "Southcarolina", "Virginia", "Westvirginia")
east_south_central <- c("Alabama", "Kentucky", "Mississippi", "Tennessee")
west_south_central <- c("Arkansas", "Louisiana", "Oklahoma", "Texas")
east_north_central <- c("Indiana", "Illinois", "Michigan", "Ohio", "Wisconsin")
west_north_central <- c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "Northdakota", "Southdakota")
new_england <- c("Connecticut", "Maine", "Massachusetts", "Newhampshire", "Rhodeisland", "Vermont")
middle_atlantic <- c("Newjersey", "Newyork", "Pennsylvania")

counties_n_years <- read_csv(here::here("Data", "Outputs", "counties_n_years.csv"))


#* outage_event_90pctle should provide the start time visuals
outage_event_90pctle <- read_csv(here::here("Data", "Outputs", "outage_event_90pctle_w_new_dta.csv")) %>%
  left_join(., counties_n_years, by = "fips") %>% 
  rename(hour_collapser = hour,
         state_name = clean_state_name,
         county_name = clean_county_name) %>% 
  mutate(state_name = tools::toTitleCase(state_name)) 

local_time <- calc_local_time(date_time = outage_event_90pctle$hour_collapser, fips = outage_event_90pctle$fips)

outage_event_90pctle <- cbind(outage_event_90pctle, local_time) %>% 
  rename(hour_collapser_org = hour_collapser,
         hour_collapser = local_time) 

outage_event_90pctle <- outage_event_90pctle %>% 
  mutate(hour_collapser = as.POSIXct(hour_collapser),
         ind_day_outage = ifelse(hour(hour_collapser) >= 7 & hour(hour_collapser) <= 19, 1, 0),
         ind_night_outage = ifelse(ind_day_outage != 1, 1, 0),
         ind_weekend = ifelse(weekdays(hour_collapser) == "Saturday" | weekdays(hour_collapser) == "Sunday", 1, 0),
         ind_weekday = ifelse(ind_weekend != 1, 1, 0),
         time_of_outage = hour(hour_collapser),
         census_division = case_when(
           state_name %in% mountain_west ~ "Mountain West",
           state_name %in% pacific_west ~ "Pacific West",
           state_name %in% south_atlantic ~ "South Atlantic",
           state_name %in% east_south_central ~ "East South Central",
           state_name %in% west_south_central ~ "West South Central",
           state_name %in% east_north_central ~ "East North Central",
           state_name %in% west_north_central ~ "West North Central",
           state_name %in% new_england ~ "New England",
           state_name %in% middle_atlantic ~ "Middle Atlantic"),
         census_region = case_when(
           census_division %in% c("Mountain West", "Pacific West") ~ "West",
           census_division %in% c("South Atlantic", "East South Central", "West South Central") ~ "South",
           census_division %in% c("East North Central", "West North Central") ~ "Midwest",
           census_division %in% c("New England", "Middle Atlantic") ~ "Northeast"
         ))


#* outage90 dataset is used to obtain the really basic descriptive statistics about PO events
#* ie - how many there are across the US during our study period
#* in this case, it is okay to have non-whole numbers reported like 0.3 1+ hour outage/year because this still tells us that this 
#* particular county experienced at least 1 1+ hour outage.
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


#* generate values for counties with 2 or 3 years available for figures
glimpse(outage_event_90pctle)

outage_event_90pctle_23 <- outage_event_90pctle %>% 
  filter(n_year_available != 1)
  
outage90_23 <- outage90 %>% 
  filter(n_years_available != 1)

outage90_rounded_23 <- outage90 %>% 
  filter(n_years_available != 1) %>% 
  mutate_at(vars(starts_with("tot")), round, 0) %>% 
  select(-year)


####***********************
# Basic descriptive statistics about our data ####
# how many counties in our dataset, how many counties have 1+ hour and 8+ hour outages etc
####***********************

#the total number of counties in our study
all_counties_in_study <- readRDS(here("Data", "Updated_PO", "data_with_coverage_exclusions_sample.RDS")) %>% 
  filter(pc > 0.5) %>% 
  dplyr::select(clean_state_name, fips) %>% 
  group_by(fips) %>% 
  filter(row_number() == 1) %>% 
  drop_na(fips) #remember to drop the na fips because those include county = Unknown or nonsensical entries like "zip"
dim(all_counties_in_study) # fips in our study

#

#number of counties with AT LEAST 1+ hour outage ever
nrow(outage90_23 %>% filter(tot_po_yr > 0)) # even if there are 8.3 annual average outages, it still counts as > 1 
nrow(outage_event_90pctle_23 %>% filter(po_duration_hr > 0) %>% group_by(fips) %>% filter(row_number() == 1)) 

#number of counties with AT LEAST 8+ hour outage ever
nrow(outage90_23 %>% filter(tot_po8 > 0)) 
nrow(outage_event_90pctle_23 %>% filter(po_duration_hr >= 8) %>% group_by(fips) %>% filter(row_number() == 1))


#total 1+ hour outages and total 8+ hour outages
outage_counts <- outage90_23

sum(outage_counts$tot_po_yr) #<- the total for all average yearly 1+ hours for fips
nrow(outage_event_90pctle_23 %>% filter(po_duration_hr >= 1)) # <- total of all 1+ hour outages
sum(outage_counts$tot_po8) #24708 <- the total for all average yearly 8+ hours for fips
nrow(outage_event_90pctle_23 %>% filter(po_duration_hr >= 8)) # <- the total of all 8+ hour outages

#top 10 states with highest number of outages for 1+ hour outages
top10_states_num_any_outage <- outage90_rounded_23 %>% 
  group_by(state_name) %>% 
  mutate(sum_any_outage = sum(tot_po_yr)) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(sum_any_outage)) %>% 
  dplyr::select(state_name, sum_any_outage)

write_xlsx(top10_states_num_any_outage, here::here("Data", "Manuscript Results", "table_top10_states_num_any_outage.xlsx"))

#top 10 states with highest proportion of top decile outages
top_decile_threshold <- unname(quantile(outage90_rounded$tot_po_yr, probs = seq(.1, .9, by = .1), na.rm = TRUE))[9]
top10_states_prop_any_outage <- outage90_rounded %>% 
  group_by(state_name) %>% 
  mutate(tot_counties = n(),
         ind_top_decile = ifelse(tot_po_yr > top_decile_threshold, 1, 0),
         sum_top_decile = sum(ind_top_decile),
         prop_top_decile = sum_top_decile/tot_counties) %>%
  filter(row_number() == 1) %>% 
  dplyr::select(state_name, tot_counties, sum_top_decile, prop_top_decile) %>% 
  arrange(desc(prop_top_decile))

write_xlsx(top10_states_prop_any_outage, here::here("Data", "Manuscript Results", "table_top10_states_prop_any_outage.xlsx"))

#top 10 states with the greatest number of top decile outages
top10_states_prop_any_outage <- top10_states_prop_any_outage %>% 
  arrange(desc(sum_top_decile))

write_xlsx(top10_states_prop_any_outage, here::here("Data", "Manuscript Results", "table_top10_states_numdec_any_outage.xlsx"))

#top counties with highest number of 1+ hour outages
top_counties_any_outage <- outage_event_90pctle_23 %>% 
  group_by(fips) %>% 
  mutate(sum_any_outage = n()) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(sum_any_outage)) %>% 
  dplyr::select(state_name, county_name, fips, sum_any_outage)

write_xlsx(top_counties_any_outage, here::here("Data", "Manuscript Results", "table_top_counties_any_outage.xlsx"))

####***********************
# County total customer-time non-standardized ####
####***********************
customer_time_nonstd <- read_csv(here::here("Data", "Outputs", "customer_time_nonstd_w_new_dta_all_yrs.csv"))

#avg_customers_out in the customer_time_nonstd is annual average of counties so we now take the annual average
#of states
top_states_customer_time <- customer_time_nonstd %>% 
  rename(state_name = clean_state_name) %>% 
  group_by(state_name) %>% 
  mutate(sum_avg_customer_time = sum(avg_customers_out)) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(sum_avg_customer_time))

sum(top_states_customer_time$sum_avg_customer_time) #sum of the annual customer time per state; 673088444 (new = 520213522)

write_xlsx(top_states_customer_time, here::here("Data", "Manuscript Results", "table_top_states_customer_time.xlsx"))
# write_xlsx(top_states_person_time, here::here("Data", "Manuscript Results",  "test.xlsx"))


top_counties_person_time <- customer_time_nonstd %>% 
  arrange(desc(avg_customers_out))

write_xlsx(top_counties_person_time, here::here("Data", "Manuscript Results", "table_top_counties_person_time.xlsx"))


####***********************
# County average total custoer-time ####
####***********************

# has fips with 1 year, 2 years, and 3 years
customer_time_std <- read_csv(here::here("Data", "Outputs", "customer_time_std_w_new_dta_all_yrs.csv"))
names(customer_time_std)

#avg_customer_time_std in the customer_time_std is annual average of counties so we now take the annual average
#of states
top_states_customer_time <- customer_time_std %>% 
  rename(county_name = clean_county_name) %>%
  group_by(county_name) %>% 
  mutate(sum_avg_customer_time_std = sum(avg_customer_time_std)) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(sum_avg_customer_time_std))


####***********************
# 8+ hour outage ####
# data creation, limit to 8+ hour duration, create those deciles, and round
####***********************
#below is an important cutoff for counties with at least one 8+ hour outages
outage90_rounded_8_23 <- outage90_rounded_23 %>% 
  filter(tot_po8 > 0)


outage90_rounded_8_23$decile_tot_po8 <- Hmisc::cut2(outage90_rounded_8_23$tot_po8, g=10) #gte 8 is the cutoff
top_decile_threshold <- 8


#summary and distribution of 8+ hour outages among all counties with 2,3 years of data and at least 1 8 hour outage
summary(outage90_rounded_8_23$tot_po8)
IQR(outage90_rounded_8_23$tot_po8)
hist(outage90_rounded_8_23$tot_po8)
sum(outage90_rounded_8_23$tot_po8) #6156

#top 10 states with highest number of outages
top10_states_num_8hr_outage <- outage90_rounded_23 %>% 
  group_by(state_name) %>% 
  mutate(sum_8hr_outage = sum(tot_po8)) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(sum_8hr_outage)) %>% 
  dplyr::select(state_name, sum_8hr_outage)

write_xlsx(top10_states_num_8hr_outage, here::here("Data", "Manuscript Results", "table_top10_states_num_8hr_outage.xlsx"))

# #top 10 states with highest proportion of top category outages
top_decile_threshold <- unname(quantile(outage90_rounded_23$tot_po8, probs = seq(.1, .9, by = .1), na.rm = TRUE))[9]

top10_states_prop_8hr_outage <- outage90_rounded_23 %>% 
  group_by(state_name) %>% 
  mutate(tot_counties = n(),
         ind_top_decile = ifelse(tot_po8 >= top_decile_threshold, 1, 0),
         sum_top_decile = sum(ind_top_decile),
         prop_top_decile = sum_top_decile/tot_counties) %>%
  filter(row_number() == 1) %>% 
  dplyr::select(state_name, tot_counties, sum_top_decile, prop_top_decile) %>% 
  arrange(desc(prop_top_decile))

write_xlsx(top10_states_prop_8hr_outage, here::here("Data", "Manuscript Results", "table_top10_states_prop_8hr_outage.xlsx"))

#top 10 states with the greatest number of top decile outages
top10_states_prop_8hr_outage <- top10_states_prop_8hr_outage %>% 
  arrange(desc(sum_top_decile))

write_xlsx(top10_states_prop_8hr_outage, here::here("Data", "Manuscript Results", "table_top10_states_numdec_8hr_outage.xlsx"))

#top counties with highest number of 8+ hour duration outages
top_counties_8hr_outage <- outage_event_90pctle_23 %>% 
  group_by(fips) %>% 
  filter(po_duration_hr >= 8) %>% 
  mutate(sum_8hr_outage = n()) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(sum_8hr_outage)) %>% 
  dplyr::select(state_name, county_name, fips, sum_8hr_outage)

write_xlsx(top_counties_8hr_outage, here::here("Data", "Manuscript Results", "table_top_counties_8hr_outage.xlsx"))

####***********************
# 8+ hour outages & SVI analysis ####
####***********************
outage90_23$decile_tot_po8 <- Hmisc::cut2(outage90_23$tot_po8, g=10)

#create svi and dme use quartiles
po_w_social_vars <- read_csv(here::here("Data", "Outputs", "po_w_social_vars.csv")) %>% 
  mutate(dme_bene_per_1000 = (mdcr_dme/mdcr_bene)*1000)

q1 <- unname(quantile(po_w_social_vars$dme_bene_per_1000, prob=c(0.25, 0.5, 0.75), na.rm = TRUE)[1])
q2 <- unname(quantile(po_w_social_vars$dme_bene_per_1000, prob=c(0.25, 0.5, 0.75), na.rm = TRUE)[2])
q3 <- unname(quantile(po_w_social_vars$dme_bene_per_1000, prob=c(0.25, 0.5, 0.75), na.rm = TRUE)[3])
q4 <- unname(quantile(po_w_social_vars$dme_bene_per_1000, prob=c(0.25, 0.5, 0.75, 1), na.rm = TRUE)[4])


quantile(po_w_social_vars$dme_bene_per_1000, na.rm = TRUE)

#create quartiles of SVI according to the counties in our dataset with 2+ years of data
svi_q1 <- unname(quantile(po_w_social_vars$svi_rank, prob=c(0.25, 0.5, 0.75), na.rm = TRUE)[1])
svi_q2 <- unname(quantile(po_w_social_vars$svi_rank, prob=c(0.25, 0.5, 0.75), na.rm = TRUE)[2])
svi_q3 <- unname(quantile(po_w_social_vars$svi_rank, prob=c(0.25, 0.5, 0.75), na.rm = TRUE)[3])
svi_q4 <- unname(quantile(po_w_social_vars$svi_rank, prob=c(0.25, 0.5, 0.75, 1), na.rm = TRUE)[4])

quantile(po_w_social_vars$svi_rank, na.rm = TRUE)
glimpse(po_w_social_vars)

po_w_social_vars <- po_w_social_vars %>% 
  mutate(
         # svi_rank_level = case_when(svi_rank <= 0.25 ~ "Quartile 1 [0 - 0.25]",
         #                            0.25 < svi_rank & svi_rank <= 0.5 ~ "Quartile 2 (0.25 - 0.5]",
         #                            0.5 < svi_rank & svi_rank <= 0.75 ~ "Quartile 3 (0.5 - 0.75]",
         #                            svi_rank > 0.75 ~ "Quartile 4 (0.75 - 1]"),
         svi_rank_level = case_when(svi_rank <= svi_q1 ~ paste0("1st Quartile", " [0 - ", round(svi_q1, digits = 2), "]"),
                                    svi_q1 < svi_rank & svi_rank <= svi_q2 ~ paste0("2nd Quartile", " (", round(svi_q1, digits = 2), " - ", round(svi_q2, digits = 2), "]"),
                                    svi_q2 < svi_rank & svi_rank <= svi_q3 ~ paste0("3rd Quartile", " (", round(svi_q2, digits = 2), " - ", round(svi_q3, digits = 2), "]"),
                                    svi_q3 < svi_rank ~ paste0("4th Quartile", " (", round(svi_q3, digits = 2), " - ", round(svi_q4, digits = 2), "]")),
         dme_rate_level = case_when(dme_bene_per_1000 <= q1 ~ paste0("1st Quartile", " [0 - ", round(q1, digits = 0), "]"),
                                    q1 < dme_bene_per_1000 & dme_bene_per_1000 <= q2 ~ paste0("2nd Quartile", " (", round(q1, digits = 0), " - ", round(q2, digits = 0), "]"),
                                    q2 < dme_bene_per_1000 & dme_bene_per_1000 <= q3 ~ paste0("3rd Quartile", " (", round(q2, digits = 0), " - ", round(q3, digits = 0), "]"),
                                    q3 < dme_bene_per_1000 ~ paste0("4th Quartile", " (", round(q3, digits = 0), " - ", round(q4, digits = 0), "]")),
         # svi_rank_level = factor(svi_rank_level, levels = c("Quartile 1 [0 - 0.25]", "Quartile 2 (0.25 - 0.5]", "Quartile 3 (0.5 - 0.75]", "Quartile 4 (0.75 - 1]"))
         ) %>% 
  dplyr::select(fips, svi_rank_level, dme_rate_level)

joined_keep_vars <- c("state_name", "county_name", "fips", "tot_po_yr", "tot_po8", "svi_rank_level", "dme_rate_level")
df_joined <- left_join(outage90_23, po_w_social_vars, by = "fips") %>% 
  dplyr::select(all_of(joined_keep_vars)) 

####***********************
####* 8+ hour outages & SVI/DME nonparametric tests ####
####***********************

#FOLLOW UP WITH JOAN BUT THE WILCOX RANK SUM JUST TESTS IF THE MEDIANS ARE DIFFERENT (NOT IF ONE IS GREATER THAN OTHER)
df_nonparam_test <- df_joined %>% 
  mutate(svi_rank_level_cat2 = ifelse(svi_rank_level == "4th Quartile (0.77 - 1]" | svi_rank_level == "3rd Quartile (0.53 - 0.76]", "High SVI", "Lower SVI"),
         dme_rate_level_cat2 = ifelse(dme_rate_level == "4th Quartile (74 - 478]" | dme_rate_level == "3rd Quartile (58 - 74]", "High DME", "Lower DME"))

#https://data.library.virginia.edu/the-wilcoxon-rank-sum-test/
wilcox.test(tot_po_yr ~ svi_rank_level_cat2, data = df_nonparam_test, exact = FALSE)
wilcox.test(tot_po_yr ~ dme_rate_level_cat2, data = df_nonparam_test, exact = FALSE)

wilcox.test(tot_po8 ~ svi_rank_level_cat2, data = df_nonparam_test, exact = FALSE)
wilcox.test(tot_po8 ~ dme_rate_level_cat2, data = df_nonparam_test, exact = FALSE)


#we prefer wilcoxon rank sum tests because it deals specifically with medians
chisq.test(df_joined$tot_po_yr, df_joined$svi_rank_level)
chisq.test(df_joined$tot_po_yr, df_joined$dme_rate_level)

chisq.test(df_joined$tot_po8, df_joined$svi_rank_level)
chisq.test(df_joined$tot_po8, df_joined$dme_rate_level)


####***********************
# High SVI information ####
####***********************
keep_svi_names <- c("state_name", "county_name", "fips", "tot_po_yr", "tot_po8", "svi_rank_level")
df_svi <- left_join(outage90_rounded_23, po_w_social_vars, by = "fips") %>% 
  dplyr::select(all_of(keep_svi_names))

nrow(df_svi %>% filter(!is.na(svi_rank_level))) #N value for wilcoxon rank sum test

#information about characteristics of counties with high DME rate
df_svi_po_dist <- df_svi %>% 
  group_by(svi_rank_level) %>% 
  mutate(rank_tot_po1 = sum(tot_po_yr),
         rank_tot_po8 = sum(tot_po8),
         rank_mean_po1 = mean(tot_po_yr),
         rank_mean_po8 = mean(tot_po8),
         rank_sd_po1 = sd(tot_po_yr),
         rank_sd_po8 = sd(tot_po8),
         rank_min_po1 = min(tot_po_yr),
         rank_min_po8 = min(tot_po8),
         rank_max_po1 = max(tot_po_yr),
         rank_max_po8 = max(tot_po8),
         rank_med_po1 = median(tot_po_yr),
         rank_med_po8 = median(tot_po8),
         rank_iqr_po1 = IQR(tot_po_yr),
         rank_iqr_po8 = IQR(tot_po8)) %>% 
  filter(row_number() == 1) %>% 
  dplyr::select(svi_rank_level, starts_with("rank")) %>% 
  mutate_at(vars(starts_with("rank")), round)



####***********************
# High DME information ####
####***********************
keep_dme_names <- c("state_name", "county_name", "fips", "tot_po_yr", "tot_po8", "dme_rate_level")
df_dme <- left_join(outage90_rounded_23, po_w_social_vars, by = "fips") %>% 
  dplyr::select(all_of(keep_dme_names))

nrow(df_dme %>% filter(!is.na(dme_rate_level))) #N value for wilcoxon rank sum test

# df_dme %>% filter(is.na(dme_rate_level)) #there is a Lakota county that doesn't have DME (46102)
df_dme_po_dist <- df_dme %>% 
  group_by(dme_rate_level) %>% 
  mutate(rank_tot_po1 = sum(tot_po_yr),
         rank_tot_po8 = sum(tot_po8),
         rank_mean_po1 = mean(tot_po_yr),
         rank_mean_po8 = mean(tot_po8),
         rank_sd_po1 = sd(tot_po_yr),
         rank_sd_po8 = sd(tot_po8),
         rank_min_po1 = min(tot_po_yr),
         rank_min_po8 = min(tot_po8),
         rank_max_po1 = max(tot_po_yr),
         rank_max_po8 = max(tot_po8),
         rank_med_po1 = median(tot_po_yr),
         rank_med_po8 = median(tot_po8),
         rank_iqr_po1 = IQR(tot_po_yr),
         rank_iqr_po8 = IQR(tot_po8)) %>% 
  filter(row_number() == 1) %>% 
  dplyr::select(dme_rate_level, starts_with("rank")) %>% 
  mutate_at(vars(starts_with("rank")), round)

#output both svi and dme
write_xlsx(rbind(df_svi_po_dist, df_dme_po_dist), here::here("Data", "Manuscript Results", "df_svi_dme_po_dist.xlsx"))

####***********************
#FIGURES ####
####***********************

####***********************
# Contour plots of start time GENERAL ####
####***********************

contour_keep_vars <- c("state_name", "county_name", "fips", "year", "census_division", "hour_collapser", "po_duration_hr", "time_of_outage")

#why aren't there any in june or july from 16-20 hours
# names(outage_event_90pctle)
# a_tmp <- outage_event_90pctle %>% 
#   dplyr::select(all_of(contour_keep_vars)) %>% 
#   filter(po_duration_hr >= 8)
# a_tmp2 <- a_tmp %>% 
#   filter(month(hour_collapser_final) %in% c("6", "7"), hour(hour_collapser_final) %in% c("16", "17", "18", "19"))
# a_tmp3 <- df_contour %>% 
#   select(state_name, time_of_outage, month, sum_outages_time_month)
library(MetBrewer)
hiroshige_colors <- c("firebrick3", "#e76254", "#ef8a47", "#f7aa58", "#ffd06f", "#ffe6b7", "#aadce0", "#72bcd5", "#528fad", "#376795", "#1e466e")

df_contour <- outage_event_90pctle_23 %>% 
  dplyr::select(all_of(contour_keep_vars)) %>% 
  filter(po_duration_hr >= 8) %>% 
  group_by(time_of_outage, month(hour_collapser)) %>% 
  mutate(sum_outages_time_month = n(),
         month = as.factor(month(hour_collapser))) %>% 
  filter(row_number() == 1) 

scico_palette <- scico(8, palette = 'batlow')
show_col(scico_palette)
color_list_3yrs <- c(scico_palette[1:6], "#F4BB44", "#F0E68C")
show_col(color_list_3yrs)
ggplot(df_contour, aes(month(hour_collapser), time_of_outage)) +
  # geom_contour_filled(aes(z = sum_outages_time_month)) +
  stat_contour_filled(aes(z = sum_outages_time_month)) +
  # scale_fill_distiller(super = metR::ScaleDiscretised, palette = "RdYlBu") +
  # scale_fill_manual(values = rev(hiroshige_colors)) +
  # scale_fill_distiller(super = metR::ScaleDiscretised, palette = "RdYlBu") +
  # scale_fill_scico_d(palette = "batlow", direction = -1) +
  # scale_fill_manual(values = rev(color_list_3yrs)) +
  scale_fill_manual(values= rev(met.brewer("Hiroshige", 8))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(#title = "Start Time of 8+ Hour Outages by Month",
       fill = "Count of Outages") +
  # xlab("Month") +
  ylab("Time of Outage (Local Military Time)") +
  scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June",
                              "July", "August", "September", "October", "November", "December"),
                   expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 23, by = 1), limits = c(0, 23), 
                     expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.key.height= unit(1, 'cm'),
        plot.title = element_text(size = 15, hjust = 0.5)) 

ggsave(path = here::here("Visuals"),
       filename = "contour_all_po8.jpg", 
       dpi=300,
       height=5, width=7, units="in")

scico_palette <- scico(13, palette = 'batlow')
show_col(scico_palette)
color_list_3yrs <- c(scico_palette[1:9], "#E49B0F", "#FFC000", "#F4BB44", "#F0E68C")
show_col(color_list_3yrs)



region_keep_vars <- c("state_name", "county_name", "fips", "year", "census_division", "census_region", "hour_collapser", "po_duration_hr", "time_of_outage")
df_contour_region <- outage_event_90pctle_23 %>% 
  dplyr::select(all_of(region_keep_vars)) %>% 
  filter(po_duration_hr >= 8) %>% 
  group_by(census_region, time_of_outage, month(hour_collapser)) %>% 
  mutate(sum_outages_time_month = n()) %>% 
  filter(row_number() == 1) 

ggplot(df_contour_region, aes(month(hour_collapser), time_of_outage, z = sum_outages_time_month)) +
  stat_contour_filled() +
  # scale_fill_distiller(super = metR::ScaleDiscretised, palette = "RdYlBu") +
  scale_fill_manual(values= rev(met.brewer("Hiroshige", 13))) +
  # scale_fill_scico_d(palette = "batlow", direction = -1) +
  # scale_fill_manual(values = rev(color_list_3yrs)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_wrap(~census_region) +
  labs(#title = "Census Region Start Time of 8+ Hour Outages by Month",
       fill = "Counts of Outages") +
  ylab("Time of Outage Military Time") +
  scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", 
                              "July", "August", "September", "October", "November", "December"), 
                   expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 23, by = 4), limits = c(0, 23), 
                     expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.key.height= unit(0.5, 'cm'),
        plot.title = element_text(size = 15, hjust = 0.5)) +
  theme(panel.spacing.x = unit(1, "lines")) # Change horizontal spacing between facets


ggsave(path = here::here("Visuals"),
       filename = "contour_regional_po8.jpg", 
       dpi=300,
       height=4, width=7, units="in")

####***********************
###1+ hour outages plots ####
####***********************
names(outage90)

#summary and distribution of 1+ hour outages
summary(outage90_rounded_23$tot_po_yr)
sd(outage90_rounded_23$tot_po_yr)
sum(outage90_rounded_23$tot_po_yr) 
IQR(outage90_rounded_23$tot_po_yr)

#8+ hour
summary(outage90_rounded_23$tot_po8)
sd(outage90_rounded_23$tot_po8)
sum(outage90_rounded_23$tot_po8) 
IQR(outage90_rounded_23$tot_po8)

#number of counties with 1+ hour outage ROUNDED
nrow(outage90_rounded_23 %>% 
       filter(tot_po_yr > 0)) 

#number of counties with 8+ hour outage ROUNDED
nrow(outage90_rounded_23 %>% filter(tot_po8 > 0)) 


df_gt_0_po <- subset(outage90_rounded_23, tot_po_yr > 0)
df_gt_0_po$decile_tot_po_yr <- Hmisc::cut2(df_gt_0_po$tot_po_yr, g=10)
# df_gt_0_po$ntile <- ntile(df_gt_0_po$tot_po_yr, 11)
table(df_gt_0_po$decile_tot_po_yr)
df_antijoin_po <- anti_join(outage90_rounded_23, df_gt_0_po, by = "fips")

outage90_rounded_map <- rbind(df_gt_0_po, df_antijoin_po) %>% 
  mutate(decile_tot_po_yr_v2 = ifelse(tot_po_yr == 0, "0", as.character(decile_tot_po_yr)))
unique(outage90_rounded_map$decile_tot_po_yr)
unique(outage90_rounded_map$decile_tot_po_yr_v2)

#version 2 contains counties with 0 outages
outage90_rounded_map$decile_tot_po_yr_v2 <- factor(outage90_rounded_map$decile_tot_po_yr_v2, levels = c("0",
                                                                                                        "[  1, 22)",
                                                                                                        "[ 22, 43)",
                                                                                                        "[ 43, 56)",
                                                                                                        "[ 56, 66)",
                                                                                                        "[ 66, 76)",
                                                                                                        "[ 76, 88)",
                                                                                                        "[ 88,100)",
                                                                                                        "[100,116)",
                                                                                                        "[116,141)",
                                                                                                        "[141,414]"))

decile_list <- as.list(sort(unique(as.character(outage90_rounded_map$decile_tot_po_yr_v2))))
outage90_rounded_map <- outage90_rounded_map %>% 
  mutate(ntile = case_when(decile_tot_po_yr_v2 == "0" ~ 0,
                           decile_tot_po_yr_v2 == decile_list[1] ~ 1,
                           decile_tot_po_yr_v2 == decile_list[2] ~ 2,
                           decile_tot_po_yr_v2 == decile_list[3] ~ 3,
                           decile_tot_po_yr_v2 == decile_list[4] ~ 4,
                           decile_tot_po_yr_v2 == decile_list[5] ~ 5,
                           decile_tot_po_yr_v2 == decile_list[6] ~ 6,
                           decile_tot_po_yr_v2 == decile_list[7] ~ 7,
                           decile_tot_po_yr_v2 == decile_list[8] ~ 8,
                           decile_tot_po_yr_v2 == decile_list[9] ~ 9,
                           decile_tot_po_yr_v2 == decile_list[10] ~ 10))

unique(outage90_rounded_map$decile_tot_po_yr_v2)




map_avg_any <- plot_usmap(regions = "counties",
                          data = outage90_rounded_map,
                          values = "decile_tot_po_yr_v2",
                          color = "grey",
                          size=0.2) + 
  labs(title = "County Yearly Average of 1+ Hour Outage Events",
       # subtitle = "90th percentile threshold (0.5%)",
       fill = "Deciles of Outage Counts") +
  scale_fill_viridis(discrete = TRUE,
                     option = "inferno",
                     direction = -1) +
  scale_alpha(outage90_rounded_map$n_years_available) +
  # scale_fill_brewer(palette="RdYlBu", direction = -1, na.value = "grey70") +
  theme(legend.text = element_text(size=10.5),
        legend.title = element_text(size=10.5),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 15)) +
  labs(fill = "Outage Event Frequency")

map_avg_any

ggsave(path = here::here("Visuals"),
       filename = "nat_map_po1.jpg", 
       dpi=300,
       height=4, width=7, units="in")

####***********************
#### 8+ hour outages ####
# rounded data
####***********************
# AVERAGE YEARLY COUNTS OF 8+ HOUR POWER OUTAGES 
outage90_rounded_8 <- outage90_rounded %>% 
  filter(tot_po8 > 0)

outage90_rounded_8$decile_tot_po8 <- Hmisc::cut2(outage90_rounded_8$tot_po8, g=10)
outage90_rounded$decile_tot_po8 <- Hmisc::cut2(outage90_rounded$tot_po8, g=10)

#NOTE THAT THIS MAP INCLUDES COUNTIES WITH 0 8+ HOUR OUTAGES
map_avg_8 <- plot_usmap(regions = "counties",
                        data = outage90_rounded,
                        values = "decile_tot_po8",
                        color = "grey",
                        size=0.2) + 
  labs(title = "County Yearly Average of 8+ Hour Outage Events") +
  scale_fill_viridis(discrete = TRUE,
                     option = "inferno",
                     direction = -1) +
  # scale_fill_brewer(palette="RdYlBu", direction = -1, na.value = "grey70") +
  theme(legend.text = element_text(size=10.5),
        # legend.title = element_blank(),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 15)) +
  labs(fill = "Outage Event Frequency")

map_avg_8

ggsave(path = here::here("Visuals"),
       filename = "nat_map_po8.jpg", 
       dpi=300,
       height=4, width=7, units="in")

###HOW THE HECK DO WE PLOT TOGETHER?
# map_avg_any + map_avg_8

####***********************
#### Customer hours national map - ABSOLUTE VALUES ####
####***********************
customer_time_nonstd$dec_avg_pt <- Hmisc::cut2(customer_time_nonstd$sum_customer_time, g=10)

map_pt_std <- plot_usmap(regions = "counties",
                         data = customer_time_nonstd,
                         values = "dec_avg_pt",
                         color = "grey",
                         size = 0.2
) + 
  labs(title = "County Yearly Averaged Totals of Customer Hours",
       # subtitle = "90th Percentile Threshold (0.5%)",
       fill = "Decile Categories") +
  # scale_fill_manual(values = mycolors,
  #                   name="Decile \nCategories",
  #                   na.value = "grey"
  # ) +
  scale_fill_viridis(discrete = TRUE,
                     option = "inferno",
                     direction = -1) +
  # scale_fill_brewer(palette="RdYlBu", direction = -1, na.value = "grey70") +
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 15)) +
  labs(fill = "Customer-Hours")

map_pt_std

ggsave(path = here::here("Visuals"),
       filename = "nat_map_customer_hours.jpg", 
       dpi=300,
       height=4, width=7, units="in")

####***********************
#### Customer hours national map - STANDARDIZED BY CUSTOMERS ####
####***********************
customer_time_std$dec_avg_pt <- Hmisc::cut2(customer_time_std$avg_customer_time_std, g=10)

map_pt_std <- plot_usmap(regions = "counties",
                         data = customer_time_std,
                         values = "dec_avg_pt",
                         color = "grey",
                         size = 0.2
) + 
  labs(title = "County Yearly Averaged Hours Per Customer",
       # subtitle = "90th Percentile Threshold (0.5%)",
       fill = "Decile Categories") +
  # scale_fill_manual(values = mycolors,
  #                   name="Decile \nCategories",
  #                   na.value = "grey"
  # ) +
  scale_fill_viridis(discrete = TRUE,
                     option = "inferno",
                     direction = -1) +
  # scale_fill_brewer(palette="RdYlBu", direction = -1, na.value = "grey70") +
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 15)) +
  labs(fill = "Customer-Hours")

map_pt_std

ggsave(path = here::here("Visuals"),
       filename = "nat_map_customer_hours_std_by_cust_pop.jpg", 
       dpi=300,
       height=4, width=7, units="in")

####***********************
#### SVI and DME distribution (all) counties by quartile ####
####***********************

#svi map (all)
map_svi_invest <- plot_usmap(regions = "counties",
                             data = po_w_social_vars,
                             values = "svi_rank_level",
                             color = "black") +
  labs(#title = "SVI Designation",
       fill = "SVI Quartile") +
  scale_fill_manual(values=c("#0072B2", "#56B4E9", "#F0E442", "red"),
                    labels = c("1st Quartile [0-0.28]", "2nd Quartile (0.28-0.53]", "3rd Quartile (0.53-0.77]", "4th Quartile (0.77-1]")) +
  # scale_colour_brewer(palette = "YlOrRd", direction = - 1) + 
  theme(legend.text = element_text(size=10.5),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 15)) 

map_svi_invest
ggsave(path = here::here("Visuals"),
       filename = "nat_map_svi.jpg", 
       dpi=300,
       height=4, width=7, units="in")

#dme map (all)
map_dme_invest <- plot_usmap(regions = "counties",
                             data = po_w_social_vars,
                             values = "dme_rate_level",
                             color = "black") +
  labs(#title = "DME Quartile Designation",
       fill = "DME Quartile") +
  scale_fill_manual(values=c("#0072B2", "#56B4E9", "#F0E442", "red"),
                    labels = c("1st Quartile [0-45]", "2nd Quartile (45-58]", "3rd Quartile (58-74]", "4th Quartile (74-478]")) +
  theme(legend.text = element_text(size=10.5),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 15)) 

map_dme_invest
ggsave(path = here::here("Visuals"),
       filename = "nat_map_dme.jpg", 
       dpi=300,
       height=4, width=7, units="in")


names(tmp)
names(outage90_rounded_8)

####***********************
#### something about colored distribution of 8+ hour outage STATE LEVEL ####
####***********************
tmp <- outage90_rounded %>% 
  dplyr::select(state_name, county_name, tot_po8, decile_tot_po8) %>% 
  mutate(rounded_po = round(tot_po8)) %>% 
  group_by(rounded_po) %>% 
  mutate(freq = n()) %>% 
  filter(row_number() == 1) 

tmp <- outage90_rounded %>% 
  dplyr::select(county_name, tot_po8, decile_tot_po8) %>% 
  mutate(rounded_po = round(tot_po8)) %>% 
  group_by(rounded_po) %>% 
  mutate(freq = n()) %>% 
  filter(row_number() == 1) 

ggplot(tmp, aes(x = rounded_po, y = freq, fill = decile_tot_po8)) +
  geom_bar(stat = "identity") +
  ggtitle("Distribution of Average Yearly 8+ Hour Outages") +
  labs(x = "Average Yearly Count",
       y = "Frequency") +
  theme_classic() +
  theme(legend.title = element_blank())

tmp <- tmp %>% 
  arrange(desc(tot_po8))

write_xlsx(tmp, here::here("Data", "Manuscript Results", "table_top_places_8hr_outage.xlsx"))


####***********************
#### Traectories line plot of states ####
####***********************

###TRAJECTORIES
traj_keep_vars <- c("state_name", "county_name", "fips", "year", "census_division", "hour_collapser", "po_duration_hr")
df_traj_8hr <- outage_event_90pctle_23 %>% 
  dplyr::select(traj_keep_vars) %>% 
  filter(po_duration_hr >= 8) %>% 
  group_by(state_name, month(hour_collapser)) %>% 
  mutate(sum_outage_state_month = n()) %>% 
  filter(row_number() == 1) %>% 
  rename(month = "month(hour_collapser)") %>% 
  dplyr::select(state_name, month, sum_outage_state_month, census_division) %>% 
  mutate(state_name = ifelse(state_name == "Newyork", "New York", 
                             ifelse(state_name == "Northcarolina", "North Carolina", state_name)))

tmp_8hr <- df_traj_8hr %>% 
  group_by(state_name) %>% 
  mutate(sum_state = sum(sum_outage_state_month)) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(sum_state))  

table_top10_state_8hr_outage <- tmp_8hr %>% 
  dplyr::select(state_name, sum_state)

# install.packages('writexl')
# library(writexl)
# write_xlsx(table_top10_state_8hr_outage, paste0(out_figures, "table_top10_state_8hr_outage.xlsx"))

list_tmp <- table_top10_state_8hr_outage$state_name[1:10]

#create color palette of interest
palette <- c("red",
             "skyblue",
             "purple",
             "orange",
             "blue",
             "salmon1",
             "pink",
             "slateblue2",
             "brown",
             "tan",
             "grey90") #you can play around with this to find a color scheme you like-- this was just random


#Create vector for states of interest

vec <- table_top10_state_8hr_outage$state_name[1:10] #these would be the ones you dont want greyed out


#make a new column with all other counties assigned as other
df_traj2 <- df_traj_8hr %>%
  mutate(county_5 = ifelse(state_name %in% vec, state_name, "Other"))

#order new column other is grey (will be assigned colors from palette in order)
df_traj2$county_5 <- factor(df_traj2$county_5, levels = c(vec,"Other")) 

#KEEP FINAL
traj_urban <- ggplot(data = df_traj2, aes(x = month,
                                          y = sum_outage_state_month, 
                                          group = state_name, 
                                          color = county_5)) + #group should be all counties and color should be new county column
  geom_line() + 
  labs(title = "Total Monthly Counts of 8+ Hour Outages") +
  ylab("Counts of 8+ Hour Outages") +
  scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", 
                              "July", "August", "September", "October", "November", "December"), 
                   expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5),
        legend.title = element_blank()) +
  scale_color_manual(values= palette)

traj_urban 
ggsave(path = here::here("Visuals"),
       filename = "top_10states_po8.jpg", 
       dpi=300,
       height=4, width=7, units="in")








# Additional manuscript image ---------------------------------------------

glimpse(outage_event_90pctle_23)
cust_dist <- read_csv(file = here::here("Data", "Outputs", "fips_w_po_and_0_po.csv")) %>%
  group_by(fips) %>% 
  mutate_at(vars(starts_with("tot")), mean) %>% 
  mutate(n_years_available = n()) %>% 
  filter(row_number() == 1)  %>% 
  mutate_at(vars(starts_with("tot")), round, 0) %>% 
  dplyr::select(-year) %>% 
  filter(n_years_available != 1)

data <- readRDS(here("Data", "Updated_PO", "data_with_coverage_exclusions_sample.RDS")) 
length(unique(data$fips))

tmp <- data %>% 
  select(clean_county_name, fips, year, customers_served_total) %>% 
  filter(fips %in% cust_dist$fips) %>% 
  group_by(fips, year) %>% 
  filter(row_number() == 1) %>% 
  group_by(fips) %>% 
  mutate(avg_cust = round(mean(customers_served_total, na.rm = TRUE), 0)) %>% 
  filter(row_number() == 1) %>% 
  select(fips, avg_cust) %>% 
  mutate(binned_avg_cust = case_when(avg_cust < 10000 ~ 1,
                                     avg_cust >= 10000 & avg_cust < 50000 ~ 2,
                                     avg_cust >= 50000 & avg_cust < 100000 ~ 3,
                                     avg_cust >= 100000 & avg_cust < 150000 ~ 4,
                                     avg_cust >= 150000 ~ 5))

tmp$binned_avg_cust <- factor(tmp$binned_avg_cust, levels = c(1, 2, 3, 4, 5))
unique(tmp$binned_avg_cust)


length(unique(tmp$fips))

plot_usmap(regions = "counties",
           data = tmp,
           values = "binned_avg_cust",
           color = "black",
           size = 0.2) +
  labs(title = "Estimated County Customers",
       fill = "Customers out threshold") +
  # scale_fill_manual(values= rev(met.brewer("Hiroshige", 5)),
  #                   na.value = "white") +
  scale_fill_scico_d(palette = "batlow", 
                     direction = -1, 
                     na.value = "white",
                     labels = c("1-10",
                                "11-50",
                                "51-100",
                                "101-150",
                                "150+")) +
  theme(legend.position = c(-0.12, 0.1),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.35, "cm"))
  
  # scale_fill_brewer(palette="Accent", direction = -1, na.value = "white") 

ggsave(path = here::here("Visuals"),
       filename = "map_county_cust.jpg", 
       dpi=300,
       height=4, width=7, units="in")
head(tmp)



####***********************
####CONTOUR PLOTS SVI AND DME ####
####CREATE CONTOUR PLOT OF HOUR AND STUFF BUT NEED FROM ANOTHER FILE 
####***********************
scico_palette <- scico(8, palette = 'batlow')
show_col(scico_palette)
color_list_3yrs <- c(scico_palette[1:6], "#FFDB58", "#F8DE7E")
show_col(color_list_3yrs)

joined_keep_vars <- c("state_name", "county_name", "fips", "po_duration_hr", "hour_collapser", "time_of_outage", "svi_rank_level", "dme_rate_level", "census_region")
df_contour_svi_dme <- left_join(outage_event_90pctle_23, po_w_social_vars, by = "fips") %>% 
  dplyr::select(all_of(joined_keep_vars)) %>% 
  filter(po_duration_hr >= 8) %>% 
  mutate(month = month(hour_collapser)) %>% 
  group_by(month, time_of_outage, svi_rank_level) %>% 
  mutate(sum_outage_svi = n()) %>% 
  group_by(month, time_of_outage, dme_rate_level) %>%
  mutate(sum_outage_dme = n())

svi_counties_alloutages <- left_join(outage_event_90pctle_23, po_w_social_vars, by = "fips") %>% 
  dplyr::select(all_of(joined_keep_vars)) %>% 
  group_by(fips) %>% 
  filter(row_number() == 1) %>% 
  dplyr::select(state_name, county_name, svi_rank_level, census_region)
prop.table(table(svi_counties_alloutages$svi_rank_level))

svi_counties_8hr <- df_contour_svi_dme %>% 
  group_by(fips) %>% 
  filter(row_number() == 1) %>% 
  dplyr::select(state_name, county_name, svi_rank_level, census_region)
prop.table(table(svi_counties_8hr$svi_rank_level)) 
# check to make sure that those with 8 hour outages have same distribution across SVI levels 
# high SVI increased by 1% and low SVI decreased by 1%; not too drastic though

df_outage_time_svi <- df_contour_svi_dme %>% 
  group_by(month, time_of_outage, svi_rank_level) %>% 
  filter(po_duration_hr >= 8) %>% 
  filter(row_number() == 1) %>%
  dplyr::select(-contains("dme"))

df_outage_time_dme <- df_contour_svi_dme %>% 
  group_by(month, time_of_outage, dme_rate_level) %>% 
  filter(po_duration_hr >= 8) %>% 
  filter(row_number() == 1) %>% 
  dplyr::select(-contains("svi"))

#KEEP FINAL
ggplot(df_outage_time_svi, aes(month, time_of_outage, z = sum_outage_svi)) +
  stat_contour_filled() +
  # scale_fill_distiller(super = metR::ScaleDiscretised, palette = "RdYlBu") +
  scale_fill_manual(values= rev(met.brewer("Hiroshige", 15))) +
  # scale_fill_scico_d(palette = "batlow", direction = -1) +
  # scale_fill_manual(values = rev(color_list_3yrs)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_wrap(~svi_rank_level) +
  labs(title = "Start Time of 8+ Hour Outage by County SVI Quartiles",
       fill = "Counts of Outages") +
  ylab("Time of Outage (Military Time)") +
  scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", 
                              "July", "August", "September", "October", "November", "December"), 
                   expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 23, by = 4), limits = c(0, 23), 
                     expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.key.height= unit(0.5, 'cm'),
        plot.title = element_text(size = 15, hjust = 0.5)) +
  theme(panel.spacing.x = unit(1, "lines")) # Change horizontal spacing between facets

#interestingly, the medium SVI rank has the highest number of outages (occurring at night) --> could be that they are citi3s
#check what types of places high vs medium svi is
ggsave(path = here::here("Visuals"),
       filename = "contour_svi.jpg", 
       dpi=300,
       height=4, width=7, units="in")


#KEEP FINAL (NEW DOUBLE FACET)
ggplot(df_contour_svi_dme, aes(month, time_of_outage, z = sum_outage_svi)) +
  stat_contour_filled() +
  # scale_fill_distiller(super = metR::ScaleDiscretised, palette = "RdYlBu") +
  scale_fill_manual(values= rev(met.brewer("Hiroshige", 15))) +
  # scale_fill_scico_d(palette = "batlow", direction = -1) +
  # scale_fill_manual(values = rev(color_list_3yrs)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_wrap(~svi_rank_level + census_region) +
  labs(#title = "Start Time of 8+ Hour Outage by County SVI Quartiles and Census Region",
       fill = "Counts of Outages") +
  ylab("Time of Outage (Military Time)") +
  scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", 
                              "July", "August", "September", "October", "November", "December"), 
                   expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 23, by = 4), limits = c(0, 23), 
                     expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 5),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.key.height= unit(0.5, 'cm'),
        plot.title = element_text(size = 12, hjust = 0.5),
        strip.text.x = element_text(size = 5),
        strip.text.y = element_text(size = 5)) +
  theme(panel.spacing.x = unit(1, "lines")) # Change horizontal spacing between facets

#interestingly, the medium SVI rank has the highest number of outages (occurring at night) --> could be that they are citi3s
#check what types of places high vs medium svi is
ggsave(path = here::here("Visuals"),
       filename = "contour_svi_region.jpg", 
       dpi=300,
       height=4, width=7, units="in")


scico_palette <- scico(13, palette = 'batlow')
show_col(scico_palette)
color_list_3yrs <- c(scico_palette[1:9], "#F4BB44", "#FFDB58", "#F8DE7E", "#F0E68C")
show_col(color_list_3yrs)

ggplot(df_outage_time_dme, aes(month, time_of_outage, z = sum_outage_dme)) +
  stat_contour_filled() +
  # scale_fill_distiller(super = metR::ScaleDiscretised, palette = "RdYlBu") +
  scale_fill_manual(values= rev(met.brewer("Hiroshige", 12))) +
  # scale_fill_scico_d(palette = "batlow", direction = -1) +
  # scale_fill_manual(values = rev(color_list_3yrs)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_wrap(~dme_rate_level) +
  labs(title = "Start Time of 8+ Hour Outage by Medicare DME user prevalence per 1000",
       fill = "Counts of Outages") +
  ylab("Time of Outage (Military Time)") +
  scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", 
                              "July", "August", "September", "October", "November", "December"), 
                   expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 23, by = 4), limits = c(0, 23), 
                     expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.key.height= unit(0.5, 'cm'),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(panel.spacing.x = unit(1, "lines")) # Change horizontal spacing between facets

ggsave(path = here::here("Visuals"),
       filename = "contour_dme.jpg", 
       dpi=300,
       height=4, width=7, units="in")
###END OF OFFICIAL VISUALS FOR PO MANUSCRIPT


























#side project - figure out which breaks we want with 8+ hour outages
tmp <- outage90_rounded_8 %>% 
  select(tot_po8, decile_tot_po8) %>% 
  mutate(rounded_po = round(tot_po8)) %>% 
  group_by(rounded_po) %>% 
  mutate(freq = n()) %>% 
  filter(row_number() == 1)
# filter(tot_po8 > 0) #remove counties with no 8+ hour instances

ggplot(tmp, aes(x = rounded_po, y = freq, fill = decile_tot_po8)) +
  geom_bar(stat = "identity") +
  ggtitle("Distribution of Total PO Events (8+ Hours)") +
  labs(x = "Number of Total PO",
       y = "Frequency",
       fill = "Breaks") +
  theme_classic() 

hist(outage90_rounded$tot_po8)
summary(outage90_rounded$tot_po8)

# COUNTY CUSTOMER TIME NON-STANDARDIZED

person_time_nonstd$dec_avg_pt <- Hmisc::cut2(person_time_nonstd$avg_person_time, g=10)

map_pt_std <- plot_usmap(regions = "counties",
                         data = person_time_nonstd,
                         values = "dec_avg_pt",
                         color = "grey",
                         size = 0.2
) + 
  labs(title = "County Yearly Averaged Totals of Customer Hours",
       # subtitle = "90th Percentile Threshold (0.5%)",
       fill = "Decile Categories") +
  # scale_fill_manual(values = mycolors,
  #                   name="Decile \nCategories",
  #                   na.value = "grey"
  # ) +
  scale_fill_viridis(discrete = TRUE,
                     option = "inferno",
                     direction = -1) +
  # scale_fill_brewer(palette="RdYlBu", direction = -1, na.value = "grey70") +
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size = 20))

map_pt_std
ggsave(path = here::here("Visuals"),
       filename = "nat_map_customer_hours.jpg", 
       dpi=300,
       height=4, width=7, units="in")

# COUNTY SVI DESIGNATION
map_svi_desig <- plot_usmap(regions = "counties",
                            data = svi_counties_8hr,
                            values = "svi_rank_level",
                            color = "black") +
  labs(title = "SVI Designation for Counties with 8+ Hour Outages",
       fill = "SVI Designation") +
  theme(legend.text = element_text(size=12),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = c("Low SVI" = "green",
                               "Medium SVI" = "blue",
                               "High SVI" = "red"))

print(map_svi_desig)

# COUNTY DME DESIGNATION (GET THE DATA FROM POUS_EXTRA_FIGURES)
df_joined <- df_joined %>% 
  arrange(dme_rate_level)
dme_cat <- unique(df_joined$dme_rate_level)
dme_cat[4]

map_dme_desig <- plot_usmap(regions = "counties",
                            data = dme_counties_8hr,
                            values = "dme_rate_level",
                            color = "black") +
  labs(title = "DME Designation for Counties with 8+ Hour Outages",
       fill = "DME Designation") +
  theme(legend.text = element_text(size=12),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = c("1st Quartile (0% - 46.01%]" = "green",
                               "2nd Quartile (46.01% - 59.39%]" = "blue",
                               "3rd Quartile (59.39% - 76.37%]" = "orange",
                               "4th Quartile (76.37% - 100%]" = "red"))
# scale_fill_manual(values = c(paste0(dme_cat[1]) = "green",
#                              paste0(dme_cat[2]) = "blue",
#                              paste0(dme_cat[3]) = "orange",
#                              oaste0(dme_cat[4]) = "red"))

print(map_dme_desig)


names(df_joined)











map_8hr_dme <- plot_usmap(regions = "counties",
                          data = df_8hr_dme_map,
                          values = "dme_rate_level",
                          color = "black") +
  labs(title = "DME Designation for Counties with 8+ Hour Outages",
       fill = "DME Designation") +
  theme(legend.text = element_text(size=12),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 20)) #+
# scale_fill_brewer(palette="YlOrRd", na.value = "grey70")

print(map_8hr_dme)






##
# 
# svi_counties_alloutages <- left_join(outage90, po_w_social_vars, by = "fips") %>% 
#   # select(all_of(joined_keep_vars)) %>% 
#   group_by(fips) %>% 
#   filter(row_number() == 1) %>% 
#   dplyr::select(state_name, county_name, svi_rank_level) 
# 
# prop.table(table(svi_counties_alloutages$svi_rank_level))

svi_counties_8hr <- df_joined %>% 
  group_by(fips) %>% 
  filter(row_number() == 1) %>% 
  dplyr::select(state_name, county_name, svi_rank_level)
prop.table(table(svi_counties_8hr$svi_rank_level)) 
# check to make sure that those with 8 hour outages have same distribution across SVI levels 
# high SVI increased by 1% and low SVI decreased by 1%; not too drastic though

df_outage_time_svi <- df_joined %>% 
  group_by(month, time_of_outage, svi_rank_level) %>% 
  filter(row_number() == 1) %>% 
  select(-contains("dme"))

df_outage_time_dme <- df_joined %>% 
  group_by(month, time_of_outage, dme_rate_level) %>% 
  filter(row_number() == 1) %>% 
  select(-contains("svi"))


ggplot(df_outage_time_svi, aes(month, time_of_outage, z = sum_outage_svi)) +
  stat_contour_filled() +
  scale_fill_distiller(super = metR::ScaleDiscretised, palette = "RdYlBu") +
  facet_wrap(~svi_rank_level) +
  labs(title = "Start Time of 8+ Hour Outage by County SVI Tertiles",
       fill = "Counts of 8+ Hour Outages") +
  ylab("Time of Outage Military Time") +
  scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", 
                              "July", "August", "September", "October", "November", "December"), 
                   expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 23, by = 4), limits = c(0, 23), 
                     expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90))
#interestingly, the medium SVI rank has the highest number of outages (occurring at night) --> could be that they are citi3s
#check what types of places high vs medium svi is


ggplot(df_outage_time_dme, aes(month, time_of_outage, z = sum_outage_dme)) +
  stat_contour_filled() +
  scale_fill_distiller(super = metR::ScaleDiscretised, palette = "RdYlBu") +
  facet_wrap(~dme_rate_level) +
  labs(title = "Start Time of 8+ Hour Outage by Rate of DME per Medicare (per 1000)",
       fill = "Counts of 8+ Hour Outages") +
  ylab("Time of Outage Military Time") +
  scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", 
                              "July", "August", "September", "October", "November", "December"), 
                   expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 23, by = 4), limits = c(0, 23), 
                     expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90))

#####CHI SQUARED TEST
library("MASS")
tmp_svi <- df_outage_time_svi %>% 
  group_by(svi_rank_level) %>% 
  mutate(sum_outage_rank = sum(sum_outage_svi)) %>% 
  filter(row_number() == 1) %>% 
  select(svi_rank_level, sum_outage_rank)

chisq.test(df_outage_time_svi$svi_rank_level, df_outage_time_svi$sum_outage_svi)
names(df_outage_time_svi)

chisq.test(df_outage_time_dme$dme_rate_level, df_outage_time_dme$sum_outage_dme)
names(df_outage_time_dme)


data(ChickWeight)
chisq.test(ChickWeight$Diet, ChickWeight$weight)
chisq.test(ChickWeight$Time, ChickWeight$weight)












Hmisc::cut2(outage90$tot_po8, g=10)

names(outage_event_90pctle)









# Gen data for public -----------------------------------------------------

for_public_outages <- outage90_rounded %>% 
  select(-state_name, -county_name, -tot_po4, -tot_po24)

for_public_custout <- full_join(customer_time_nonstd %>% select(fips, avg_customers_out),
                                customer_time_std %>% select(fips, avg_customer_time_std), 
                                by = "fips") %>% 
  mutate(avg_customers_out = round(avg_customers_out, 0),
         avg_customer_time_std = round(avg_customer_time_std, 0))

for_public_outages_custout <- full_join(for_public_outages, for_public_custout)

write_csv(for_public_outages_custout, here("Data", "for_public", "yrly_avg_po_custout.csv"))










