####***********************
#### Code Description ####
# Author: Vivian
# Date: 10/17/2021
# Goal: Create tables for manuscript
# contour plot link reference: https://plotly.com/ggplot2/contour-plots/
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

####***********************
#*****TABLES***** ####
####***********************

####***********************
# Prep outage data ####
# set the regions and round outage data because what does it mean to have 0.33 of an outage per year --> nothing
####***********************
mountain_west <- c("Arizona", "Colorado", "Idaho", "New Mexico", "Montana", "Utah", "Nevada", "Wyoming")
pacific_west <- c("Alaska", "Hawaii", "California", "Oregon", "Washington")
south_atlantic <- c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia")
east_south_central <- c("Alabama", "Kentucky", "Mississippi", "Tennessee")
west_south_central <- c("Arkansas", "Louisiana", "Oklahoma", "Texas")
east_north_central <- c("Indiana", "Illinois", "Michigan", "Ohio", "Wisconsin")
west_north_central <- c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota")
new_england <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont")
middle_atlantic <- c("New Jersey", "New York", "Pennsylvania")

#* outage_event_90pctle should provide the start time visuals
outage_event_90pctle <- read_csv(here::here("Data", "Outputs", "outage_event_90pctle.csv")) %>% 
  mutate(ind_day_outage = ifelse(hour(hour_collapser) >= 7 & hour(hour_collapser) <= 19, 1, 0),
         ind_night_outage = ifelse(ind_day_outage != 1, 1, 0),
         ind_weekend = ifelse(weekdays(hour_collapser) == "Saturday" | weekdays(hour_collapser) == "Sunday", 1, 0),
         ind_weekday = ifelse(ind_weekend != 1, 1, 0),
         time_of_outage = hour(hour_collapser_final),
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
outage90 <- read_csv(file = here::here("Data", "Outputs", "final_outage90.csv")) %>% 
  group_by(fips) %>% 
  mutate_at(vars(starts_with("tot")), mean) %>% 
  filter(row_number() == 1) 

#* outage90_rounded is used to obtain information about yearly averages of PO frequency among states and counties 
#* this differs from outage90 because we want the ROUNDED reports since 5.3 1+ hour outages/year doesn't mean anything. 
#* What is .3 of an outage? It makes more sense to round for interpretation
outage90_rounded <- outage90 %>% 
  mutate_at(vars(starts_with("tot")), round, 0)

####***********************
# Basic descriptive statistics about our data ####
# how many counties in our dataset, how many counties have 1+ hour and 8+ hour outages etc
####***********************

#the total number of counties in our study
all_counties_in_study <- read_csv((here::here("Data", "Outputs", "county_cust_hr_all.csv"))) %>% 
  dplyr::select(state_name, fips) %>% 
  group_by(fips) %>% 
  filter(row_number() == 1) %>% 
  drop_na(fips) #remember to drop the na fips because those include county = Unknown or nonsensical entries like "zip"
dim(all_counties_in_study) #2975 fips in our study

#number of counties with AT LEAST 1+ hour outage
nrow(outage90 %>% filter(tot_po_yr > 0)) # even if there are 8.3 annual average outages, it still counts as > 1 
nrow(outage_event_90pctle %>% filter(po_duration_hr > 0) %>% group_by(fips) %>% filter(row_number() == 1)) #2951

#number of counties with AT LEAST 8+ hour outage
nrow(outage90 %>% filter(tot_po8 > 0)) #2737
nrow(outage_event_90pctle %>% filter(po_duration_hr >= 8) %>% group_by(fips) %>% filter(row_number() == 1))


#total 1+ hour outages and total 8+ hour outages
outage_counts <- read_csv(file = here::here("Data", "Outputs", "final_outage90.csv")) 
sum(outage_counts$tot_po_yr)
nrow(outage_event_90pctle %>% filter(po_duration_hr > 0)) #448147 <- total of all 1+ hour outages
sum(outage_counts$tot_po8) #24697 <- the total for all average yearly 8+ hours for fips
nrow(outage_event_90pctle %>% filter(po_duration_hr >= 8)) #24441 <- the total of all 8+ hour outages

#top 10 states with highest number of outages for 1+ hour outages
top10_states_num_any_outage <- outage90_rounded %>% 
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
top_counties_any_outage <- outage_event_90pctle %>% 
  group_by(fips) %>% 
  mutate(sum_any_outage = n()) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(sum_any_outage)) %>% 
  dplyr::select(state_name, county_name, fips, sum_any_outage)

write_xlsx(top_counties_any_outage, here::here("Data", "Manuscript Results", "table_top_counties_any_outage.xlsx"))

####***********************
# County customer-time non-standardized ####
####***********************
customer_time_nonstd <- read_csv(here::here("Data", "Outputs", "customer_time_nonstd.csv"))

top_states_customer_time <- customer_time_nonstd %>% 
  group_by(state_name) %>% 
  mutate(sum_avg_customer_time = sum(avg_customer_time)) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(sum_avg_customer_time))

sum(top_states_customer_time$sum_avg_customer_time) #sum of the annual customer time per state; 673038943

write_xlsx(top_states_customer_time, here::here("Data", "Manuscript Results", "table_top_states_customer_time.xlsx"))
# write_xlsx(top_states_person_time, here::here("Data", "Manuscript Results",  "test.xlsx"))


top_counties_person_time <- customer_time_nonstd %>% 
  arrange(desc(avg_customer_time))

write_xlsx(top_counties_person_time, here::here("Data", "Manuscript Results", "table_top_counties_person_time.xlsx"))

####***********************
# 8+ hour outage ####
# data creation, limit to 8+ hour duration, create those deciles, and round
####***********************
#below is an important cutoff for counties with at least one 8+ hour outages
outage90_rounded_8 <- outage90_rounded %>% 
  filter(tot_po8 > 0)

outage90_rounded_8$decile_tot_po8 <- Hmisc::cut2(outage90_rounded_8$tot_po8, g=10) #gte 8 is the cutoff
top_decile_threshold <- 8

#summary and distribution of 8+ hour outages
summary(outage90_rounded$tot_po8)
hist(outage90_rounded$tot_po8)
sum(outage90_rounded$tot_po8) #8197

#top 10 states with highest number of outages
top10_states_num_8hr_outage <- outage90_rounded %>% 
  group_by(state_name) %>% 
  mutate(sum_8hr_outage = sum(tot_po8)) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(sum_8hr_outage)) %>% 
  dplyr::select(state_name, sum_8hr_outage)

write_xlsx(top10_states_num_8hr_outage, here::here("Data", "Manuscript Results", "table_top10_states_num_8hr_outage.xlsx"))

# #top 10 states with highest proportion of top category outages
top_decile_threshold <- unname(quantile(outage90_rounded$tot_po8, probs = seq(.1, .9, by = .1), na.rm = TRUE))[9]

top10_states_prop_8hr_outage <- outage90_rounded %>% 
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

#top counties with highest number of any duration outages
top_counties_8hr_outage <- outage_event_90pctle %>% 
  group_by(fips) %>% 
  mutate(sum_8hr_outage = n()) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(sum_8hr_outage)) %>% 
  dplyr::select(state_name, county_name, fips, sum_8hr_outage)

write_xlsx(top_counties_8hr_outage, here::here("Data", "Manuscript Results", "table_top_counties_8hr_outage.xlsx"))

####***********************
# 8+ hour outages & SVI analysis ####
####***********************
outage90$decile_tot_po8 <- Hmisc::cut2(outage90$tot_po8, g=10)

#create svi and dme use quartiles
po_w_social_vars <- read_csv(here::here("Data", "Outputs", "po_w_social_vars.csv")) %>% 
  mutate(dme_bene_per_1000 = (mdcr_dme/mdcr_bene)*1000)

q1 <- unname(quantile(po_w_social_vars$dme_bene_per_1000, prob=c(0.25, 0.5, 0.75), na.rm = TRUE)[1])
q2 <- unname(quantile(po_w_social_vars$dme_bene_per_1000, prob=c(0.25, 0.5, 0.75), na.rm = TRUE)[2])
q3 <- unname(quantile(po_w_social_vars$dme_bene_per_1000, prob=c(0.25, 0.5, 0.75), na.rm = TRUE)[3])
q4 <- unname(quantile(po_w_social_vars$dme_bene_per_1000, prob=c(0.25, 0.5, 0.75, 1), na.rm = TRUE)[4])


quantile(po_w_social_vars$dme_bene_per_1000, na.rm = TRUE)
po_w_social_vars <- po_w_social_vars %>% 
  mutate(svi_rank_level = case_when(svi_rank <= 0.25 ~ "Quartile 1 [0 - 0.25]",
                                    0.25 < svi_rank & svi_rank <= 0.5 ~ "Quartile 2 (0.25 - 0.5]",
                                    0.5 < svi_rank & svi_rank <= 0.75 ~ "Quartile 3 (0.5 - 0.75]",
                                    svi_rank > 0.75 ~ "Quartile 4 (0.75 - 1]"),
         dme_rate_level = case_when(dme_bene_per_1000 <= q1 ~ paste0("1st Quartile", " [0 - ", round(q1, digits = 0), "]"),
                                    q1 < dme_bene_per_1000 & dme_bene_per_1000 <= q2 ~ paste0("2nd Quartile", " (", round(q1, digits = 0), " - ", round(q2, digits = 0), "]"),
                                    q2 < dme_bene_per_1000 & dme_bene_per_1000 <= q3 ~ paste0("3rd Quartile", " (", round(q2, digits = 0), " - ", round(q3, digits = 0), "]"),
                                    q3 < dme_bene_per_1000 ~ paste0("4th Quartile", " (", round(q3, digits = 0), " - ", round(q4, digits = 0), "]")),
         svi_rank_level = factor(svi_rank_level, levels = c("Quartile 1 [0 - 0.25]", "Quartile 2 (0.25 - 0.5]", "Quartile 3 (0.5 - 0.75]", "Quartile 4 (0.75 - 1]"))) %>% #MUST RELEVEL THE SVI TO BE LOW MEDIUM HIGH FOR FACET
  dplyr::select(fips, svi_rank_level, dme_rate_level)

joined_keep_vars <- c("state_name", "county_name", "fips", "tot_po_yr", "tot_po8", "svi_rank_level", "dme_rate_level")
df_joined <- left_join(outage90_rounded, po_w_social_vars, by = "fips") %>% 
  dplyr::select(all_of(joined_keep_vars)) 

####***********************
#### 8+ hour outages & SVI/DME nonparametric tests ####
####***********************
#make binary for highest quartile vs all other quartiles
df_nonparam_test <- df_joined %>% 
  mutate(svi_rank_level_cat2 = ifelse(svi_rank_level == "Quartile 4 (0.75 - 1]", "High SVI", "Lower SVI"),
         dme_rate_level_cat2 = ifelse(dme_rate_level == "4th Quartile (76 - 478]", "High DME", "Lower DME"))

#https://data.library.virginia.edu/the-wilcoxon-rank-sum-test/
wilcox.test(tot_po_yr ~ svi_rank_level_cat2, data = df_nonparam_test, exact = FALSE)
wilcox.test(tot_po_yr ~ dme_rate_level_cat2, data = df_nonparam_test, exact = FALSE)

wilcox.test(tot_po8 ~ svi_rank_level_cat2, data = df_nonparam_test, exact = FALSE)
wilcox.test(tot_po8 ~ dme_rate_level_cat2, data = df_nonparam_test, exact = FALSE)


####***********************
# High SVI information ####
####***********************
keep_svi_names <- c("state_name", "county_name", "fips", "tot_po_yr", "tot_po8", "svi_rank_level")
df_svi <- left_join(outage90_rounded, po_w_social_vars, by = "fips") %>% 
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
df_dme <- left_join(outage90_rounded, po_w_social_vars, by = "fips") %>% 
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
#*****FIGURES***** ####
####***********************

####***********************
# Contour plots of start time GENERAL ####
####***********************
contour_keep_vars <- c("state_name", "county_name", "fips", "year", "census_division", "hour_collapser_final", "po_duration_hr", "time_of_outage")

df_contour <- outage_event_90pctle %>% 
  dplyr::select(all_of(contour_keep_vars)) %>% 
  filter(po_duration_hr >= 8) %>% 
  group_by(time_of_outage, month(hour_collapser_final)) %>% 
  mutate(sum_outages_time_month = n(),
         month = as.factor(month(hour_collapser_final))) %>% 
  filter(row_number() == 1) 

ggplot(df_contour, aes(month(hour_collapser_final), time_of_outage)) +
  # geom_contour_filled(aes(z = sum_outages_time_month)) +
  stat_contour_filled(aes(z = sum_outages_time_month)) +
  scale_fill_distiller(super = metR::ScaleDiscretised, palette = "RdYlBu") +
  labs(title = "Start Time of 8+ Hour Outages by Month",
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
       height=4, width=7, units="in")

region_keep_vars <- c("state_name", "county_name", "fips", "year", "census_division", "census_region", "hour_collapser", "po_duration_hr", "time_of_outage")
df_contour_region <- outage_event_90pctle %>% 
  dplyr::select(all_of(region_keep_vars)) %>% 
  filter(po_duration_hr >= 8) %>% 
  group_by(census_region, time_of_outage, month(hour_collapser)) %>% 
  mutate(sum_outages_time_month = n()) %>% 
  filter(row_number() == 1) 

ggplot(df_contour_region, aes(month(hour_collapser), time_of_outage, z = sum_outages_time_month)) +
  stat_contour_filled() +
  scale_fill_distiller(super = metR::ScaleDiscretised, palette = "RdYlBu") +
  facet_wrap(~census_region) +
  labs(title = "Census Region Start Time of 8+ Hour Outages by Month",
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
        legend.key.height= unit(1, 'cm'),
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
summary(outage90_rounded$tot_po_yr)
sd(outage90_rounded$tot_po_yr)
sum(outage90_rounded$tot_po_yr) 
sum(outage90$tot_po_yr) 
IQR(outage90$tot_po_yr)
max(outage90$tot_po_yr)

#8+ hour
summary(outage90_rounded$tot_po8)
sd(outage90_rounded$tot_po8)
sum(outage90_rounded$tot_po8) 
sum(outage90$tot_po8) 
IQR(outage90$tot_po8)
max(outage90$tot_po8)

#number of counties with 1+ hour outage ROUNDED
nrow(outage90_rounded %>% filter(tot_po_yr > 0)) #2951

#number of counties with 8+ hour outage ROUNDED
nrow(outage90_rounded %>% filter(tot_po8 > 0)) #2480


df_gt_0_po <- subset(outage90_rounded, tot_po_yr > 0)
df_gt_0_po$decile_tot_po_yr <- Hmisc::cut2(df_gt_0_po$tot_po_yr, g=10)
table(df_gt_0_po$decile_tot_po_yr)
df_antijoin_po <- anti_join(outage90_rounded, df_gt_0_po, by = "fips")

outage90_rounded_map <- rbind(df_gt_0_po, df_antijoin_po) %>% 
  mutate(decile_tot_po_yr_v2 = ifelse(tot_po_yr == 0, "0", as.character(decile_tot_po_yr)))
unique(outage90_rounded_map$decile_tot_po_yr)
unique(outage90_rounded_map$decile_tot_po_yr_v2)

#version 2 contains counties with 0 outages
outage90_rounded_map$decile_tot_po_yr_v2 <- factor(outage90_rounded_map$decile_tot_po_yr_v2, levels = c("0",
                                                                                                        "[ 1, 15)",
                                                                                                        "[15, 23)",
                                                                                                        "[23, 31)",
                                                                                                        "[31, 39)",
                                                                                                        "[39, 46)",
                                                                                                        "[46, 54)",
                                                                                                        "[54, 63)",
                                                                                                        "[63, 75)",
                                                                                                        "[75, 96)",
                                                                                                        "[96,396]"))



unique(outage90_rounded_map$decile_tot_po_yr_v2)

# AVERAGE YEARLY COUNTS OF ANY-HOUR POWER OUTAGES 
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

####***********************
#### Customer hours national map ####
####***********************
customer_time_nonstd$dec_avg_pt <- Hmisc::cut2(customer_time_nonstd$avg_customer_time, g=10)

map_pt_std <- plot_usmap(regions = "counties",
                         data = customer_time_nonstd,
                         values = "dec_avg_pt",
                         color = "grey",
                         size = 0.2
) + 
  labs(title = "County Yearly Averaged Totals of Customer Hours",
       # subtitle = "90th Percentile Threshold (0.5%)",
       fill = "Decile Categories") +
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
#### SVI and DME distribution (all) counties by quartile ####
####***********************

#svi map (all)
map_svi_invest <- plot_usmap(regions = "counties",
                             data = po_w_social_vars,
                             values = "svi_rank_level",
                             color = "black") +
  labs(title = "SVI Designation for All Counties in Study",
       fill = "SVI Quartile") +
  scale_fill_manual(values=c("#0072B2", "#56B4E9", "#F0E442", "red"),
                    labels = c("1st Quartile [0-0.25]", "2nd Quartile (0.25-0.5]", "3rd Quartile (0.5-0.75]", "4th Quartile (0.75-1]")) +
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
  labs(title = "DME Quartile Designation for All Counties in Study",
       fill = "DME Quartile") +
  scale_fill_manual(values=c("#0072B2", "#56B4E9", "#F0E442", "red"),
                    labels = c("1st Quartile [0-46]", "2nd Quartile (46-59]", "3rd Quartile (59-76]", "4th Quartile (76-478]")) +
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
#### colored distribution of 8+ hour outage ####
####***********************
tmp <- outage90_rounded %>% 
  dplyr::select(state_name, county_name, tot_po8, decile_tot_po8) %>% 
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
traj_keep_vars <- c("state_name", "county_name", "fips", "year", "census_division", "hour_collapser_final", "po_duration_hr")
df_traj_8hr <- outage_event_90pctle %>% 
  dplyr::select(traj_keep_vars) %>% 
  filter(po_duration_hr >= 8) %>% 
  group_by(state_name, month(hour_collapser_final)) %>% 
  mutate(sum_outage_state_month = n()) %>% 
  filter(row_number() == 1) %>% 
  rename(month = "month(hour_collapser_final)") %>% 
  dplyr::select(state_name, month, sum_outage_state_month, census_division)

tmp_8hr <- df_traj_8hr %>% 
  group_by(state_name) %>% 
  mutate(sum_state = sum(sum_outage_state_month)) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(sum_state)) 

table_top10_state_8hr_outage <- tmp_8hr %>% 
  dplyr::select(state_name, sum_state)

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

####***********************
####CONTOUR PLOTS SVI AND DME ####
####CREATE CONTOUR PLOT OF HOUR AND STUFF BUT NEED FROM ANOTHER FILE 
####***********************
joined_keep_vars <- c("state_name", "county_name", "fips", "po_duration_hr", "hour_collapser", "time_of_outage", "svi_rank_level", "dme_rate_level", "census_region")
df_contour_svi_dme <- left_join(outage_event_90pctle, po_w_social_vars, by = "fips") %>% 
  dplyr::select(all_of(joined_keep_vars)) %>% 
  filter(po_duration_hr >= 8) %>% 
  mutate(month = month(hour_collapser)) %>% 
  group_by(month, time_of_outage, svi_rank_level) %>% 
  mutate(sum_outage_svi = n()) %>% 
  group_by(month, time_of_outage, dme_rate_level) %>%
  mutate(sum_outage_dme = n())

svi_counties_alloutages <- left_join(outage_event_90pctle, po_w_social_vars, by = "fips") %>% 
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


ggplot(df_outage_time_svi, aes(month, time_of_outage, z = sum_outage_svi)) +
  stat_contour_filled() +
  scale_fill_distiller(super = metR::ScaleDiscretised, palette = "RdYlBu") +
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
        legend.key.height= unit(1, 'cm'),
        plot.title = element_text(size = 15, hjust = 0.5)) +
  theme(panel.spacing.x = unit(1, "lines")) # Change horizontal spacing between facets

#interestingly, the medium SVI rank has the highest number of outages (occurring at night) --> could be that they are citi3s
ggsave(path = here::here("Visuals"),
       filename = "contour_svi.jpg", 
       dpi=300,
       height=4, width=7, units="in")


#KEEP FINAL (NEW DOUBLE FACET)
ggplot(df_contour_svi_dme, aes(month, time_of_outage, z = sum_outage_svi)) +
  stat_contour_filled() +
  scale_fill_distiller(super = metR::ScaleDiscretised, palette = "RdYlBu") +
  facet_wrap(~svi_rank_level + census_region) +
  labs(title = "Start Time of 8+ Hour Outage by County SVI Quartiles and Census Region",
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
        legend.key.height= unit(1, 'cm'),
        plot.title = element_text(size = 12, hjust = 0.5),
        strip.text.x = element_text(size = 5),
        strip.text.y = element_text(size = 5)) +
  theme(panel.spacing.x = unit(1, "lines")) # Change horizontal spacing between facets

#interestingly, the medium SVI rank has the highest number of outages (occurring at night) --> could be that they are citi3s
ggsave(path = here::here("Visuals"),
       filename = "contour_svi_region.jpg", 
       dpi=300,
       height=4, width=7, units="in")

ggplot(df_outage_time_dme, aes(month, time_of_outage, z = sum_outage_dme)) +
  stat_contour_filled() +
  scale_fill_distiller(super = metR::ScaleDiscretised, palette = "RdYlBu") +
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
        legend.key.height= unit(1, 'cm'),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(panel.spacing.x = unit(1, "lines")) # Change horizontal spacing between facets

ggsave(path = here::here("Visuals"),
       filename = "contour_dme.jpg", 
       dpi=300,
       height=4, width=7, units="in")


