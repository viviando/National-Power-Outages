####***********************
#### Code Description ####
# Author: Vivian
# Date: 8/28/2022
# Goal: Evaluate processed data
####***********************

####***********************
#PART 0: set up environment ####
####***********************

here()


####***********************
#PART 1: summary statistics ####
####***********************

processed_dta <- readRDS(here("Data", "Updated_PO", "data_with_coverage_exclusions_sample.RDS")) 
names(processed_dta)

#remove those with poor spatial coverage
processed_dta_pc50 <- processed_dta %>% 
  filter(pc > 0.5)

# Is the distribution of proportion of customers out close to 0.005 like before
# filter to only instances where customers out > 0
# if proportions are over 100%, then count as a full outage
eval_processed_dta_pc50 <- processed_dta_pc50 %>% 
  filter(customers_out_total > 0) %>% 
  filter(!is.na(downscaled_county_estimate)) %>% 
  mutate(prop_cust_out = customers_out_total/downscaled_county_estimate,
         prop_cust_out = ifelse(prop_cust_out > 1, 1, prop_cust_out)) 

summary(eval_processed_dta_pc50$prop_cust_out)
hist(eval_processed_dta_pc50$prop_cust_out)
quantile(eval_processed_dta_pc50$prop_cust_out, probs =  c(0.5, 0.75, 0.9, 0.95)) #now 90th percentile is 0.0017290982% after limiting to those greater than 50% out
q_90th <- unname(quantile(eval_processed_dta_pc50$prop_cust_out, probs =  c(0.5, 0.75, 0.9, 0.95))[3])

# Identify when greater than 90th percentile
processed_dta_pc50 <- processed_dta_pc50 %>% 
  mutate(prop_cust_out = customers_out_total/downscaled_county_estimate,
         prop_cust_out = ifelse(prop_cust_out > 1, 1, prop_cust_out),
         i_gt_90pctle = ifelse(prop_cust_out > q_90th, 1, 0))

county_cust_hr_all_new_denominator <- processed_dta_pc50


# Identify PO using function ----------------------------------------------
f_outage_tracker <- function(outage_threshold){
  
  county <- eval(substitute(fips), county_cust_hr_all_new_denominator) #changed county to fips
  # hr_diff_uncollapsed <- eval(substitute(hr_diff_uncollapsed), county_cust_hr_all_new_denominator)
  ind_outage <- eval(substitute(outage_threshold), county_cust_hr_all_new_denominator)
  
  j = 1
  out_track <- rep(0, nrow(county_cust_hr_all_new_denominator))
  for (i in 1:nrow(county_cust_hr_all_new_denominator)){
    
    #deal with the first row
    print(i)
    
    if (i == 1){
      if (ind_outage[[i]] == 0){
        out_track[[i]] = 0
      }
      else{
        out_track[[i]] = 1
      }
    }
    #deal with non-first row
    else{
      #if the consecutive row is 1-0 then set to 0 and increment the counter
      if (ind_outage[[i]] == 0 &
          ind_outage[[i - 1]] == 1){
        out_track[[i]] = 0
        j = j+1
      }
      #if the consecutive row is 0-1 then increment counter and set to new counter value
      else if (ind_outage[[i]] == 1 &
               ind_outage[[i - 1]] == 0){
        out_track[[i]] = j
      }
      #if the current row is 1-1 and the county previously are the same then set to counter
      else if (ind_outage[[i]] == 1 &
               ind_outage[[i - 1]] == 1 &
               county[[i]] == county[[i - 1]] #&
               # hr_diff_uncollapsed[[i]] <= 48 &
               # hr_diff_uncollapsed[[i]] > 0
               )
        {
        out_track[[i]] = j
      }
      #if the consecutive row is 1-1 but county is different or there is a long duration between outages, then increase counter
      else if (ind_outage[[i]] == 1 &
               ind_outage[[i - 1]] == 1 &
               (county[[i]] != county[[i - 1]] #|
                # hr_diff_uncollapsed[[i]] > 48 |
                # hr_diff_uncollapsed[[i]] < 0
                )){
        j = j+1
        out_track[[i]] = j
      }
      #all other situations should be 0
      else{
        out_track[[i]] = 0
      }
    }
  }
  
  out_track
}

outage_tracker_all <- as_tibble(f_outage_tracker(i_gt_90pctle))

outage_tracker_all <- outage_tracker_all %>% 
  rename(outage_90pctle = "value")

outage_event_expanded_new_dta <- cbind(county_cust_hr_all_new_denominator, outage_tracker_all) 
write_csv(outage_event_expanded_new_dta, here::here("Data", "Outputs", "outage_event_expanded_new_dta.csv")) 

####***********************
#### Part 6: Condense expanded PO data to obtain hourly counts of 1+ hour and 8+ hour PO ####
# We want to collapse expanded PO data to be shorter so we have information at a county-year-hour level of outage #s
####***********************
outage_event_expanded_new_dta <- read_csv(here::here("Data", "Outputs", "outage_event_expanded_new_dta.csv"))

fips_yr_in_dta <- outage_event_expanded_new_dta %>% 
  select(fips, year) %>% 
  distinct()

write_csv(fips_yr_in_dta, here::here("Data", "Outputs", "fips_yr_in_dta.csv")) 

outage_event_90pctle <- outage_event_expanded_new_dta %>% 
  filter(outage_90pctle != 0) %>%
  group_by(outage_90pctle) %>%
  slice(c(1, n())) %>%
  mutate(po_duration_hr = as.numeric(difftime(lead(hour), hour, units = "hours")),
         po_duration_hr = ifelse(po_duration_hr == 0, 0.5, po_duration_hr)) %>%
  filter(row_number() == 1)

write_csv(outage_event_90pctle, here::here("Data", "Outputs", "outage_event_90pctle_w_new_dta.csv")) 

outage_event_90pctle <- outage_event_expanded_new_dta %>% 
  group_by(fips, year) %>% 
  filter(outage_90pctle != 0) %>%
  group_by(outage_90pctle) %>%
  slice(c(1, n())) %>%
  mutate(po_duration_hr = as.numeric(difftime(lead(hour), hour, units = "hours")),
         po_duration_hr = ifelse(po_duration_hr == 0, 0.5, po_duration_hr)) %>%
  filter(row_number() == 1)

# try to consider those with 0 outages too

fips_w_po <- outage_event_90pctle %>% 
  group_by(fips, year) %>% 
  mutate(ind_po1 = ifelse(po_duration_hr >= 1, 1, 0),
         ind_po8 = ifelse(po_duration_hr >= 8, 1, 0)) %>% 
  filter(row_number() == 1) %>% 
  select(fips, year, starts_with("ind"))


fips_w_0_outages <- anti_join(fips_yr_in_dta, fips_w_po, by = c("fips", "year"))

outage90_w_obs <- outage_event_90pctle %>% 
  group_by(year, fips) %>% 
  mutate(tot_po_yr = n(),
         tmp_ind_po4 = ifelse(po_duration_hr >= 4, 1, 0),
         tmp_ind_po8 = ifelse(po_duration_hr >= 8, 1, 0),
         tmp_ind_po24 = ifelse(po_duration_hr >= 24, 1, 0),
         tot_po4 = sum(tmp_ind_po4),
         tot_po8 = sum(tmp_ind_po8),
         tot_po24 = sum(tmp_ind_po24)) %>% 
  # mutate_at(vars(starts_with("tot")), sum) %>% 
  filter(row_number() == 1)


fips_w_po_and_0_po <- rbind(outage90_w_obs, fips_w_0_outages)

replace_0 <- function(x)(ifelse(is.na(x), 0, x))
fips_w_po_and_0_po <- fips_w_po_and_0_po %>% 
  mutate_at(vars(starts_with(("tot"))), replace_0) %>% 
  select(clean_state_name, clean_county_name, fips, year, starts_with("tot")) %>% 
  rename(state_name = clean_state_name,
         county_name = clean_county_name)

write_csv(fips_w_po_and_0_po, here::here("Data", "Outputs", "fips_w_po_and_0_po.csv")) 

# end

#ADD BACK THE COUNTIES THAT ARE STILL IN DATASET BUT WITH 0 PO EVENTS

#get a list of all counties in our dataset
# all_fips <- read_csv(here::here("Data", "Outputs", "county_cust_hr_all_new_denominator.csv")) %>% 
#   group_by(fips) %>% 
#   filter(row_number() == 1) %>% 
#   select(state_name, county_name, fips)
# 
# write_csv(all_fips, here::here("Data", "Outputs", "all_fips.csv"))
fips_in_dta <- read_csv(here::here("Data", "Outputs", "fips_in_dta.csv"))


#only with observations that exceed 90th percentile
outage_event_90pctle <- read_csv(here::here("Data", "Outputs", "outage_event_90pctle_w_new_dta.csv"))

outage90_w_obs <- outage_event_90pctle %>% 
  group_by(year, fips) %>% 
  mutate(tot_po_yr = n(),
         tmp_ind_po4 = ifelse(po_duration_hr >= 4, 1, 0),
         tmp_ind_po8 = ifelse(po_duration_hr >= 8, 1, 0),
         tmp_ind_po24 = ifelse(po_duration_hr >= 24, 1, 0),
         tot_po4 = sum(tmp_ind_po4),
         tot_po8 = sum(tmp_ind_po8),
         tot_po24 = sum(tmp_ind_po24)) %>% 
  # mutate_at(vars(starts_with("tot")), sum) %>% 
  filter(row_number() == 1) %>% 
  group_by(fips) %>% 
  mutate(n_yr = n()) %>% 
  select(clean_state_name, clean_county_name, fips, downscaled_county_estimate, n_yr, starts_with("tot_po"))

outage90_w_obs

replace_0 <- function(x)(ifelse(is.na(x), 0, x))
final_outage90 <- rbind(tmp_outage90, tmp_outage90_2yrs, tmp_outage90_1yr, tmp_outage90_1yr, tmp_outage90_none_fips) %>% 
  mutate_at(vars(starts_with(("tot"))), replace_0)



rm(list=ls(pattern="tmp_"))


write_csv(final_outage90, here::here("Data", "Outputs", "final_outage90_w_new_dta.csv")) 


final_outage90_3yrs_only <- outage_event_90pctle %>% 
  group_by(year, fips) %>% 
  mutate(tot_po_yr = n(),
         tmp_ind_po4 = ifelse(po_duration_hr >= 4, 1, 0),
         tmp_ind_po8 = ifelse(po_duration_hr >= 8, 1, 0),
         tmp_ind_po24 = ifelse(po_duration_hr >= 24, 1, 0),
         tot_po4 = sum(tmp_ind_po4),
         tot_po8 = sum(tmp_ind_po8),
         tot_po24 = sum(tmp_ind_po24)) %>% 
  # mutate_at(vars(starts_with("tot")), sum) %>% 
  filter(row_number() == 1) %>% 
  group_by(fips) %>% 
  mutate(n_yr = n()) %>% 
  select(clean_state_name, clean_county_name, fips, downscaled_county_estimate, n_yr, starts_with("tot_po")) %>% 
  filter(n_yr == 3)
write_csv(final_outage90_3yrs_only, here::here("Data", "Outputs", "final_outage90_w_new_dta_3yrs_only.csv")) 

length(unique(final_outage90_3yrs_only$fips))

final_outage90_23yrs_only <- outage_event_90pctle %>% 
  group_by(year, fips) %>% 
  mutate(tot_po_yr = n(),
         tmp_ind_po4 = ifelse(po_duration_hr >= 4, 1, 0),
         tmp_ind_po8 = ifelse(po_duration_hr >= 8, 1, 0),
         tmp_ind_po24 = ifelse(po_duration_hr >= 24, 1, 0),
         tot_po4 = sum(tmp_ind_po4),
         tot_po8 = sum(tmp_ind_po8),
         tot_po24 = sum(tmp_ind_po24)) %>% 
  # mutate_at(vars(starts_with("tot")), sum) %>% 
  filter(row_number() == 1) %>% 
  group_by(fips) %>% 
  mutate(n_yr = n()) %>% 
  select(clean_state_name, clean_county_name, fips, downscaled_county_estimate, n_yr, starts_with("tot_po")) %>% 
  filter(n_yr == 3 | n_yr == 2)
write_csv(final_outage90_23yrs_only, here::here("Data", "Outputs", "final_outage90_w_new_dta_23yrs_only.csv")) 

length(unique(final_outage90_23yrs_only$fips))






