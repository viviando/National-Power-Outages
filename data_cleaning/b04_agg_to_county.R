# Aggregate to the county-hour level


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)


# Do ----------------------------------------------------------------------

counties <-
  list.files(here("data_proc", "data", "rand_sample_working"),
             full.names = TRUE)

county_cust_estimates <-
  list.files(here("data_proc", "data", "county_cust_estimates"),
             full.names = TRUE)
county_cust_estimates <- lapply(county_cust_estimates, read_csv)
county_cust_estimates <-
  lapply(county_cust_estimates, mutate, city_name = as.character(city_name))
county_cust_estimates <- bind_rows(county_cust_estimates) %>% 
  mutate(city_name = iconv(city_name, sub = ""))



# Helper to identify quiet APIs -------------------------------------------
f_id_quiet_api <- function(county_df){
  
  quiet_api_track <- county_df$customers_out
  
  for (i in 1:nrow(county_df)){
    
    #deal with first row
    if (i == 1){
      quiet_api_track[i] = 0
    }
    
    #deal with remaining rows
    else{
      if(county_df$utility_name[i - 1] != county_df$utility_name[i] |
         county_df$city_name[i - 1] != county_df$city_name[i] |
         (quiet_api_track[i - 1] == 0 & is.na(county_df$customers_out[i]))){
        quiet_api_track[i] = 0
      }
      
      else if(!is.na(county_df$customers_out[i]) &
              county_df$utility_name[i - 1] == county_df$utility_name[i] &
              county_df$city_name[i - 1] == county_df$city_name[i]){
        quiet_api_track[i] = 1
        j = 1
      }
      
      else if(is.na(county_df$customers_out[i]) &
              county_df$utility_name[i - 1] == county_df$utility_name[i] &
              county_df$city_name[i - 1] == county_df$city_name[i]){
        quiet_api_track[i] = j + 1
        j = j + 1
      }
      
      else{
        quiet_api_track[i] = 0 #whenever we get to a new subcounty-utility combo, restart
      }  
    }  
  }
  return(quiet_api_track)
}


f_outage_tracker <- function(outage_threshold, data){
  
  city_name <- eval(substitute(city_name), data) #changed city_name to fips
  # hour_collapser_final <- eval(substitute(hour_collapser_final), data)
  ind_outage <- eval(substitute(outage_threshold), data)
  
  j = 1
  out_track <- rep(0, nrow(data))
  for (i in 1:nrow(data)){
    
    #deal with the first row
    # print(i)
    
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
      #if the current row is 1-1 and the city_name previously are the same then set to counter
      else if (ind_outage[[i]] == 1 &
               ind_outage[[i - 1]] == 1 &
               city_name[[i]] == city_name[[i - 1]] #&
               # hour_collapser_final[[i]] <= 48 &
               # hour_collapser_final[[i]] > 0
      ){
        out_track[[i]] = j
      }
      #if the consecutive row is 1-1 but city_name is different or there is a long duration between outages, then increase counter
      else if (ind_outage[[i]] == 1 &
               ind_outage[[i - 1]] == 1 &
               (city_name[[i]] != city_name[[i - 1]] #|
                # hour_collapser_final[[i]] > 48 |
                # hour_collapser_final[[i]] < 0
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

# Run ---------------------------------------------------------------------

# i <- 912
for (i in 1:length(counties)) {
  county <- read_rds(counties[[i]])
  county <- county %>%
    # select(-city_year) %>%
    ungroup() %>%
    as_tibble() %>%
    mutate(year = year(date),
           city_name = as.character(city_name)) %>%
    filter(year > 2017) %>% 
    mutate(city_name = iconv(city_name, sub = ""))
  
  if (dim(county)[1] > 1 & 
      c("new_locf_rep") %in% names(county) == TRUE) {
    county <- county %>%
      select(-city_year) %>% 
      left_join(county_cust_estimates,
                by = c(
                  "clean_state_name",
                  "clean_county_name",
                  "city_name",
                  "year"
                ))
    
    county <- cbind(county, as.data.frame(f_id_quiet_api(county))) %>% 
      rename(quiet_api = `f_id_quiet_api(county)`)
    
    county <- county %>%
      mutate(new_cust_served = case_when(is.na(new_locf_rep) ~ NA_real_,
                                         TRUE ~ customers_served))
    
    # identify baseline customers at a subcounty utility level
    baseline_customers <- county %>% 
      group_by(utility_name, clean_state_name, clean_county_name, city_name) %>% 
      mutate(customers_out = ifelse(customers_out == -99, NA, customers_out)) %>% 
      # filter(customers_out < quantile(customers_out, prob = c(0.9), na.rm = TRUE)) %>% 
      summarise(base_cust = round(median(customers_out, na.rm = TRUE)))
    
    # identify suspiciously long periods with high customers out (suspiciously long is a quiet but not non-responsive api)
    # identify median of the distribution of minutes for outages > 1+ hours (median because data skewed)
    tmp <- county %>% 
      select(utility_name, clean_state_name, clean_county_name, city_name, date, customers_out, new_cust_served) %>% 
      mutate(i_outage = ifelse((customers_out > new_cust_served*0.005), 1, 0),
           i_outage = ifelse(is.na(i_outage), 0, i_outage)) 
    

    tmp2 <- cbind(tmp, (f_outage_tracker(i_outage, tmp))) %>% 
      rename(po_tracker = `(f_outage_tracker(i_outage, tmp))`) %>% 
      select(utility_name, clean_state_name, clean_county_name, city_name, date, customers_out, new_cust_served, po_tracker) %>% 
      filter(po_tracker != 0) %>% 
      group_by(po_tracker) %>% 
      filter(n() > 6) %>% #outages that are greater than 60 minutes (1)
      mutate(po_duration = n()) %>% 
      filter(row_number() == 1)
      
    rm(tmp)

    threshold_sus = round(median(tmp2$po_duration), 0)
      
    county <- county %>% 
      mutate(sus_reporting = ifelse(quiet_api > threshold_sus + 1, 1, 0)) %>% 
      left_join(., baseline_customers, by = c("utility_name", "clean_state_name", "clean_county_name","city_name")) %>% 
      mutate(new_locf_rep = ifelse(sus_reporting == 1 & new_locf_rep > base_cust, base_cust, new_locf_rep)) %>% 
      select(-quiet_api, -sus_reporting, -base_cust)
    
    if (length(unique(county$city_year)) > 100){
      
      utility_city_list1 <- county %>% 
        distinct(utility_name, city_name) %>% 
        mutate(utility_city = paste0(utility_name, city_name)) %>% 
        select(utility_city)
      
      utility_city_list <- utility_city_list1 %>% 
        slice(1:round(nrow(utility_city_list1)/2))
    

      county1 <- county %>% 
        mutate(utility_city = paste0(utility_name, city_name)) %>% 
        filter(utility_city %in% utility_city_list$utility_city)
      
      county2 <- county %>% 
        mutate(utility_city = paste0(utility_name, city_name)) %>% 
        filter(!utility_city %in% utility_city_list$utility_city)
      
      print(unique(county1$clean_county_name))
      rm(county, utility_city_list1, utility_city_list)
      
      county1 <- county1 %>%
        select(-customers_out,-customers_out_NA,-customers_out_api_on,-customers_served, -utility_city) %>%
        distinct() %>%
        pivot_wider(
          names_from = c(utility_name, city_name),
          values_from = c(new_locf_rep, new_cust_served)
        )
      
      county2 <- county2 %>%
        select(-customers_out,-customers_out_NA,-customers_out_api_on,-customers_served, -utility_city) %>%
        distinct() %>%
        pivot_wider(
          names_from = c(utility_name, city_name),
          values_from = c(new_locf_rep, new_cust_served)
        )
      
      col_unknown <- county1 %>% 
        select(contains("Unknown"))
      
      col_reliable1 <- county1 %>%
        select(-c(1:5), -contains("Unknown")) %>% 
        purrr::discard( ~ sum(!is.na(.x)) / 157822 <= .50) %>% #the 157822 refers to total 10-min increments in our 3 year study period
        # select(-c(1:5), -contains("Unknown")) %>% 
        cbind(col_unknown) %>% 
        select(order(desc(colnames(.))))
      
      county1 <- cbind(county1 %>% select(1:5), col_reliable1)
      
      col_unknown <- county2 %>% 
        select(contains("Unknown"))
      
      col_reliable2 <- county2 %>%
        select(-c(1:5), -contains("Unknown")) %>% 
        purrr::discard( ~ sum(!is.na(.x)) / 157822 <= .50) %>% #the 157822 refers to total 10-min increments in our 3 year study period
        # select(-c(1:5), -contains("Unknown")) %>% 
        cbind(col_unknown) %>% 
        select(order(desc(colnames(.))))
      
      county2 <- cbind(county2 %>% select(1:5), col_reliable2)
      
      rm(col_reliable1, col_reliable2, col_unknown)
      
      sum_to_customers_out <-
        (dim(county1)[2] - 5) / 2 + 5 # figuring out where the columns end
      sum_to_customers_served <- dim(county1)[2]
      start_customers_served <- sum_to_customers_out + 1
      
      county1 <-
        county1 %>%
        mutate(
          customers_out_total = rowSums(.[6:sum_to_customers_out], na.rm = TRUE),
          customers_served_total = rowSums(.[start_customers_served:sum_to_customers_served], na.rm = TRUE)
        ) %>%
        select(
          clean_state_name,
          clean_county_name,
          date,
          fips,
          customers_out_total,
          customers_served_total
        )

      sum_to_customers_out <-
        (dim(county2)[2] - 5) / 2 + 5 # figuring out where the columns end
      sum_to_customers_served <- dim(county2)[2]
      start_customers_served <- sum_to_customers_out + 1
      
      county2 <-
        county2 %>%
        mutate(
          customers_out_total = rowSums(.[6:sum_to_customers_out], na.rm = TRUE),
          customers_served_total = rowSums(.[start_customers_served:sum_to_customers_served], na.rm = TRUE)
        ) %>%
        select(
          clean_state_name,
          clean_county_name,
          date,
          fips,
          customers_out_total,
          customers_served_total
        ) %>% 
        select(date, customers_out_total, customers_served_total)
      
      
      county <- full_join(county1, county2, by = "date") %>% 
        mutate(customers_out_total = rowSums(select(., "customers_out_total.x", "customers_out_total.y"), na.rm = TRUE),
               customers_served_total = rowSums(select(., "customers_served_total.x", "customers_served_total.y"), na.rm = TRUE)) %>% 
        select(-ends_with(c(".x", ".y")))
      
      # sum to hourly level
      county <-
        county %>%
        mutate(hour = floor_date(date, unit = 'hour')) %>%
        group_by(clean_state_name, clean_county_name, hour, fips) %>%
        summarise(
          customers_out_total = mean(customers_out_total, na.rm = TRUE),
          customers_served_total = max(customers_served_total, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        mutate(customers_out_total = round(customers_out_total, digits = 0))
      
      state_county <-
        paste0(unique(county$clean_state_name)[1],
               unique(county$clean_county_name)[1],
               '.RDS')
      
      
      write_rds(
        x = county,
        file = here("data_proc", "data", "hourly_county", state_county)
      )
      
    }
    
    else{
      county <- county %>%
        select(-customers_out,-customers_out_NA,-customers_out_api_on,-customers_served) %>%
        distinct() %>%
        pivot_wider(
          names_from = c(utility_name, city_name),
          values_from = c(new_locf_rep, new_cust_served)
        )
      
      
      print(unique(county$clean_county_name))
      
      # remove columns with more than 50% NA - want to exclude timeseries that
      # are not reliable; should make this a constant which is changeable for
      # the final form of the pipeline
      
      col_char <- county %>% 
        select(1:5)
      
      col_unknown <- county %>% 
        select(contains("Unknown"))
      
      col_reliable <- county %>%
        select(-c(1:5), -contains("Unknown")) %>% 
        purrr::discard( ~ sum(!is.na(.x)) / 157822 <= .50) %>% #the 157822 refers to total 10-min increments in our 3 year study period
        # select(-c(1:5), -contains("Unknown")) %>% 
        cbind(col_unknown) %>% 
        select(order(desc(colnames(.))))
      
      county <- cbind(col_char, col_reliable)
      
      sum_to_customers_out <-
        (dim(county)[2] - 5) / 2 + 5 # figuring out where the columns end
      sum_to_customers_served <- dim(county)[2]
      start_customers_served <- sum_to_customers_out + 1
      
      if (sum_to_customers_out >= 6) {
        county <-
          county %>%
          mutate(
            customers_out_total = rowSums(.[6:sum_to_customers_out], na.rm = TRUE),
            customers_served_total = rowSums(.[start_customers_served:sum_to_customers_served], na.rm = TRUE)
          ) %>%
          select(
            clean_state_name,
            clean_county_name,
            date,
            fips,
            customers_out_total,
            customers_served_total
          )
         
    
      # sum to hourly level
      county <-
        county %>%
        mutate(hour = floor_date(date, unit = 'hour')) %>%
        group_by(clean_state_name, clean_county_name, hour, fips) %>%
        summarise(
          customers_out_total = mean(customers_out_total, na.rm = TRUE),
          customers_served_total = max(customers_served_total, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        mutate(customers_out_total = round(customers_out_total, digits = 0))
      
      state_county <-
        paste0(unique(county$clean_state_name)[1],
               unique(county$clean_county_name)[1],
               '.RDS')
      
      
      write_rds(
        x = county,
        file = here("data_proc", "data", "hourly_county", state_county)
      )
    }
    }
  }
}


