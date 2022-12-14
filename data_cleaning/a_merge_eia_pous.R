####***********************
#### Code Description #### 
# Author: Vivian
# Date: 3/10/2021
# Goal: Ultimate goal is to understand how many utilities the POUS dataset covers across the US across years. 
# We use the data from EIA and merge with POUS. There is a lot a lot *a lot* of string manipulation because
# inconsistencies between and within datasets is painfully common. There are general rules that I have implemented
# below so that no one else should have to do that. I also have a separate key that links observations when
# no amount of rule bending could identify the match (ie - subsidiaries that operate under parent companies with
# completely different names). More details throughout the code.
####***********************

####***********************
#### Part 0: Calling in libraries #### 
####***********************

#* read in libraries, set directories, etc
#* data from eia seems promising - https://www.eia.gov/electricity/data/eia861/
#* download yearly data - start off with 2017 and merge with other years to form one dataset
#* EIA data about 2020 recently came out in October 2021, so should adjust code to include that info

###EIA and POUS data by year
library(raster)
library(openintro)
library(forcats)
library(stringi)
library(dplyr)
library(stringr) 
library(lubridate)
library(readxl)
library(data.table)
library(tidyverse)
library(here)

# set filepaths
here("")
path_eia <- "~/Desktop/0_PhD/Spring 2021/Rotations - Joan/Power Outages/poweroutage/EIA/"
path_pous <- "~/Desktop/0_PhD/Spring 2021/Rotations - Joan/Power Outages/poweroutage/"
out_path <- "~/Desktop/0_PhD/Spring 2021/Rotations - Joan/Power Outages/poweroutage/output/"

####***********************
#### Part 1: Download full POUS dataset #### 
####* RUN ONLY ONCE bc this can take a long time
####* this is to cut a slice of it by year for the merge (easier to manage)
####* full_df = the full pous data
####***********************
# here()
# full_df <- read_csv(here::here("Data", "POUS", "POUS_Export_Raw_CityByUtility_20170101_20201231.csv"))
# df_pous_cut <- full_df %>%
#   janitor::clean_names() %>%
#   select(utility_name, state_name, county_name, recorded_date_time) %>%
#   filter(state_name != "Quebec" & state_name != "Nova Scotia" & state_name != "New Brunswick" & state_name != "Ontario" &
#          state_name != "British Columbia" & state_name != "Virgin Islands" & state_name != "Cayman Islands" & state_name != "Alberta"&
#          state_name != "Bermuda") %>%
#   mutate(year = year(recorded_date_time),
#          state_name = ifelse(state_name == "District Of Columbia", "DC", state_name),
#          county_name = ifelse(county_name == "District Of Columbia", "District of Columbia", county_name)) 
# write_csv(df_pous_cut, here::here("Data", "Outputs", "df_pous_cut.csv"))
# df_pous_cut <- read_csv(here::here("Data", "Outputs", "df_pous_cut.csv"))
# length(unique(df_pous_cut$utility_name)) #695 utilities
# length(unique(full_df$UtilityName)) #707 total utilities in all US (+ territories)
# names(full_df)

####***********************
#### Part 2: Data cleaning for EIA data for 1 year ####
####* IMPORTANT NOTE: I initially created this code piece by piece because I worked with 2017 only data to 
####* make sure that everything worked according to plan/to catch any data errors. Because we are working with 
####* data over MANY years, it was better to put the process into a function so we could
####* easily run it for each year (fun_utilities_merge)
####***********************

fun_utilities_merge <- function(eia_year, pous_year){
  
  
  ####***********************
  #### Part 2a: Loading in and cleaning EIA/POUS data ####
  ####***********************
  
  # read in eia datasets (note that you should remove the first row of excel before reading into R for eia_utility)
  eia_service <- read_excel(here::here("Data", "EIA", eia_year, paste0("Service_Territory_", eia_year, ".xlsx"))) %>% janitor::clean_names()
  eia_utility <- read_excel(here::here("Data", "EIA", eia_year, paste0("Utility_Data_", eia_year, ".xlsx"))) %>% janitor::clean_names()
  
  #JUNE 30 2021 ADDITION - must remove any EIA retail power marketer bc they should be serviced by another REP that is also listed in EIA
  #november 9 addition --> did we ever do anything here?
  # eia_service <- eia_service %>% janitor::clean_names()
  # eia_utility <- eia_utility %>% janitor::clean_names() 
  
  # join service and utility data to have one file (both have useful information)
  # duke energy is strange in that it could be "duke energy" or "duke power", which causes probs later on
  df_eia_org <- full_join(eia_service, eia_utility, by = c("data_year", "utility_name", "state")) %>%
    select("data_year", "utility_name", "state", "county", "ownership_type") %>%
    mutate(utility_name = ifelse(str_detect(utility_name, "Duke Energy"), "Duke Energy", as.character(utility_name)),
           utility_name = str_replace(utility_name, "Potomac Electric Power Co", "Pepco")) #for DC utility)
  df_eia_org <<- df_eia_org
  
  #create unique key to merge eia datasets because we want to fill in missing ownership type if we already have that info
  key_util_owner <- df_eia_org %>% distinct(utility_name, ownership_type) %>%
    arrange(utility_name, ownership_type) %>%
    mutate(ownership_type = ifelse(is.na(ownership_type) & lag(utility_name) == utility_name,
                                   lag(ownership_type),
                                   ownership_type))
  key_util_owner <- key_util_owner %>% distinct(utility_name, ownership_type)
  
  # load in a key that links utilities between EIA and POUS (usually subsidiaries and parent but there are some edge cases included too)
  # ask vivian if you need it!
  key_parent_co <- read_excel(here::here("Data", "Keys", "key_parent_co.xlsx"))

  # join with (1) utility_owner to fill out missing ownership_type and (2) excel file with the power companies
  df_eia <- full_join(df_eia_org, key_util_owner, by = c("utility_name")) %>% 
    rename(ownership_type = ownership_type.y) %>%
    full_join(., key_parent_co, by = c(utility_name = "EIA")) %>%
    mutate(utility_name = ifelse(!is.na(Parent), Parent, utility_name)) %>%
    select(-c(Parent, ownership_type.x, Notes1, Notes2, Notes3))
  
  # manipulate strings to standardize 
  # many rules have been implemented and be careful of the order (some of it may seem unorganized, but there is a method to this madness!)
  # removed wolverine because it is suspected to be a duplicate (there are subsidiaries under Wolverine but both appear, suggesting duplication that we don't 
  # want when counting covered vs not covered utilities)
  rm_eia <- c()
  # rm_eia <- c("Wolverine Power Marketing Coop", "Wolverine Power Supply Coop", "Wolverine Alternative Investments, LLC", "Wolverine Pwr Supply Coop, Inc")
  df_eia <- df_eia %>% filter(! utility_name %in% rm_eia) %>%
    mutate(utility_name = forcats::fct_relabel(utility_name, trimws),
           utility_name = ifelse(state == "VT" & str_detect(utility_name, "Village of |Town of |City of"),
                                 str_replace(utility_name, "Village of |Town of |City of", ""),
                                 as.character(utility_name)),
           utility_name = ifelse(state == "TN" & str_detect(utility_name, "City of"),
                                 str_replace(utility_name, "City of", ""),
                                 as.character(utility_name)),
           utility_name = ifelse(state == "NC" & str_detect(utility_name, "Electric Cooperative"),
                                 str_replace(utility_name, "Electric Cooperative", "EMC"),
                                 as.character(utility_name)),
           utility_name = str_replace(utility_name, "City of", ""), #JUST REMOVE ALL OF THEM?? IF SO, WE CAN REMOVE ABOVE
           utility_name = str_replace(utility_name, "-County ", " "),
           utility_name = str_replace(utility_name, "Co Inc", "Company"),
           utility_name = str_replace(utility_name, "Rural Elec Assn", "REA"),
           utility_name = str_replace(utility_name, "Rrl Elec Assn", "REA"),
           utility_name = str_replace(utility_name, "Rural  Electric Coop Corp", "RECC"),
           utility_name = str_replace(utility_name, "Rural Electric Coop Corp", "RECC"),
           utility_name = str_replace(utility_name, "Rural Elec Coop Corp", "RECC"),
           # utility_name = str_replace(utility_name, "Electric Coop Inc", "RECC"), #THIS POSES A PROBLEM IN SC BUT FORGOT WHERE ELSE THIS WAS AN ISSUE??
           utility_name = str_replace(utility_name, "Rural E C C", "RECC"),
           utility_name = str_replace(utility_name, "Rural Electric Inc", "REC"),
           utility_name = str_replace(utility_name, "Rural Electric Cooperative", "REC"),
           utility_name = str_replace(utility_name, "Rural Electric Coop", "REC"),
           utility_name = str_replace(utility_name, "Rural Elec Coop", "REC"),
           utility_name = str_replace(utility_name, "R E C", "REC"),
           utility_name = str_replace(utility_name, "Rural E C A", "RECA"),
           utility_name = str_replace(utility_name, "Consolidated", "Con"),
           utility_name = str_replace(utility_name, "El Cons Coop", "Electric Cooperative"),
           utility_name = str_replace(utility_name, "Elec Coop, Inc", "Electric Cooperative"),
           utility_name = str_replace(utility_name, "Elec Coop Inc", "Electric Cooperative"),
           utility_name = str_replace(utility_name, "Elec Coop", "Electric Cooperative"),
           utility_name = str_replace(utility_name, "El Coop", "Electric Cooperative"),
           utility_name = str_replace(utility_name, "Coop Elec Assn", "Cooperative Electric"),
           utility_name = str_replace(utility_name, "Cooperative Elec Assn", "Cooperative Electric"),
           utility_name = str_replace(utility_name, "Coop Electric Assn", "Cooperative Electric"),
           utility_name = str_replace(utility_name, "Coop Assn", "Cooperative Association"),
           utility_name = str_replace(utility_name, "Co-op", "Cooperative"),
           utility_name = str_replace(utility_name, "Co-Op", "Cooperative"),
           utility_name = str_replace(utility_name, "Coop, Inc", "Cooperative"),
           utility_name = str_replace(utility_name, "Coop Inc", "Cooperative"),
           utility_name = str_replace(utility_name, "Coop Services", "Cooperative Services"),
           utility_name = str_replace(utility_name, "Coop Corp", "Cooperative"),
           utility_name = str_replace(utility_name, "AEP", "American Electric Power"),
           utility_name = str_replace(utility_name, "Electric Power Assn", "EPA"),
           utility_name = str_replace(utility_name, "Elec Power Assn", "EPA"),
           utility_name = str_replace(utility_name, "Elec Pwr Assn", "EPA"),
           utility_name = str_replace(utility_name, "El Pwr Assn", "EPA"),
           utility_name = str_replace(utility_name, "E P A", "EPA"),
           utility_name = str_replace(utility_name, "Utils", "Utilities"),
           utility_name = str_replace(utility_name, "Pub Serv", "Public Service"),
           utility_name = str_replace(utility_name, "Peoples Utility Dist", "Pud"),
           utility_name = str_replace(utility_name, "People's Ut Dt", "Pud"),
           utility_name = str_replace(utility_name, "Peoples Util Dist", "Pud"),
           # utility_name = str_replace(utility_name, "Rural Electric Cooperative, Inc", "Electric Cooperative"), #only made for OK, don't need anymore because changed excel key
           utility_name = str_replace(utility_name, "E C C", "Electric Cooperative"),
           utility_name = str_replace(utility_name, "E C", "Electric Cooperative"),
           utility_name = str_replace(utility_name, "Electric Assn Inc", "Electric Association"),
           utility_name = str_replace(utility_name, "Elec Assn Inc", "Electric Association"),
           utility_name = str_replace(utility_name, "Elec Assn, Inc", "Electric Association"),
           utility_name = str_replace(utility_name, "Rural E M C", "Remc"),
           utility_name = str_replace(utility_name, "R E M C", "Remc"),
           utility_name = str_replace(utility_name, "Rural Emc", "Remc"), 
           utility_name = str_replace(utility_name, "Rural EMC", "Remc"),
           utility_name = str_replace(utility_name, "Rural Elec Member Corporation", "Remc"), #some instances in GA
           utility_name = str_replace(utility_name, "Rural Elec Member Corp", "Remc"),
           utility_name = str_replace(utility_name, "Rural El Member Corporation", "Remc"),
           utility_name = str_replace(utility_name, "Rural El Member Corp", "Remc"),
           utility_name = str_replace(utility_name, "Electric Membership Corp", "EMC"),
           utility_name = str_replace(utility_name, "Electric Members Corp", "EMC"),
           utility_name = str_replace(utility_name, "Electric Member Corp", "EMC"),
           utility_name = str_replace(utility_name, "Elec Member Corp", "EMC"),
           utility_name = str_replace(utility_name, "El Member Corp", "EMC"),
           utility_name = str_replace(utility_name, "E M C", "EMC"),
           utility_name = str_replace(utility_name, "Assn", "Association"),
           # utility_name = str_replace(utility_name, "A & N", "A&N"),
           utility_name = str_replace(utility_name, "Gas and Electric", "Gas & Electric"),
           utility_name = str_replace(utility_name, "Gas & Elec ", "Gas & Electric "),
           utility_name = str_replace(utility_name, "Elec & Gas", "Electric & Gas"),
           utility_name = str_replace(utility_name, "Conven ", "Convenience "),
           utility_name = str_replace(utility_name, " & ", "&"),
           utility_name = ifelse(stri_sub(utility_name, -2, -1) == "Co",
                                 stri_replace_last_fixed(utility_name, 'Co', ''),
                                 as.character(utility_name)),
           utility_name = ifelse(stri_sub(utility_name, -4, -1) == "Coop",
                                 stri_replace_last_fixed(utility_name, 'Coop', 'Cooperative'),
                                 as.character(utility_name)),
           utility_name = ifelse(stri_sub(utility_name, -5, -1) == ", Inc",
                                 stri_replace_last_fixed(utility_name, ', Inc', ''),
                                 as.character(utility_name)),
           utility_name = gsub("[(].*", "", utility_name),
           utility_name = gsub(" - ", " ", utility_name), #remove hyphens
           utility_name = gsub("-", " ", utility_name),
           utility_name = gsub("'", "", utility_name), #remove apostrophies
           utility_name = gsub(",", "", utility_name),
           utility_name = ifelse(str_detect(utility_name, "PUD No 1 of|PUD 1 of|PUD No 2 of"),
                                 paste0(gsub("^.*?of","", utility_name), " Pud"), 
                                 as.character(utility_name)),
           utility_name = str_replace(utility_name, " County ", " "),
           utility_name = str_replace(utility_name, "  ", " "),
           utility_name = forcats::fct_relabel(utility_name, trimws)
    ) 
  
  
  ####***********************
  # Part 2b: Data cleaning for pous data (one year) ####
  ####***********************
  #keep only year of interest
  #manipulate strings as needed
  # rm_pous <- c("Association of Missouri Electric Cooperatives", "South Dakota REA")
  rm_pous <- c()
  df_pous <- df_pous_cut %>% filter(year == pous_year) %>%
    filter(! utility_name %in% rm_pous) %>%
    mutate(state_name = ifelse(state_name != "DC", state2abbr(state_name), state_name),
           utility_name = forcats::fct_relabel(utility_name, trimws),
           utility_name = ifelse(state_name == "NC" & stri_sub(utility_name, -8, -1) == "Electric",
                                 stri_replace_last_fixed(utility_name, 'Electric', 'EMC'),
                                 as.character(utility_name)), #NC is weird about this naming, so should change the pous data
           utility_name = ifelse(state_name == "NC" & stri_sub(utility_name, -20, -1) == "Electric Cooperative",
                                 stri_replace_last_fixed(utility_name, 'Electric Cooperative', 'EMC'),
                                 as.character(utility_name)),
           utility_name = ifelse(state_name == "NC" & stri_sub(utility_name, -13, -1) == "Electric Coop",
                                 stri_replace_last_fixed(utility_name, 'Electric Coop', 'EMC'),
                                 as.character(utility_name)),
           utility_name = ifelse(state_name == "MS" & str_detect(utility_name, "Electric|Electric Power"),
                                 str_replace(utility_name, "Electric|Electric Power", "EPA"),
                                 as.character(utility_name)), 
           utility_name = ifelse(state_name == "KS" & year == "2019", 
                                 str_replace(utility_name, "Westar Energy", "Evergy"),
                                 as.character(utility_name)), #KS
           utility_name = str_replace(utility_name, "Electrification", "Electric"),
           utility_name = str_replace(utility_name, "Consolidated", "Con"),
           utility_name = str_replace(utility_name, "Electic", "Electric"),
           utility_name = str_replace(utility_name, "Rural Electric Cooperative Corporation", "RECC"), #TX
           utility_name = str_replace(utility_name, "Rural Electric Cooperative", "REC"), #IA, OH, PA
           utility_name = str_replace(utility_name, "Rural Electric Coop", "REC"), #NY, PA (EDIT? MAKE THIS INTO THE END OF A CHARACTER)
           utility_name = str_replace(utility_name, "Rural Electric Association", "REA"),
           utility_name = str_replace(utility_name, "Rural Electric", "REC"), #OH
           utility_name = str_replace(utility_name, "Electric REMC", "EMC"), #instances in TN and KY
           utility_name = str_replace(utility_name, "Electric and Gas", "Electric&Gas"), #SC
           utility_name = str_replace(utility_name, "Electric Membership Corporation", "EMC"),
           utility_name = str_replace(utility_name, "Electric Membership Corp", "EMC"),
           utility_name = str_replace(utility_name, "Electric Corporation", "EMC"), #NC
           utility_name = str_replace(utility_name, "Appalachian Power", "American Electric Power"),
           utility_name = str_replace(utility_name, "Pacific Power", "PacifiCorp"),
           utility_name = str_replace(utility_name, "Rocky Mountain Power", "PacifiCorp"),
           utility_name = str_replace(utility_name, "OG&E Energy Corp.", "OG&E"),
           utility_name = str_replace(utility_name, "South Carolina Electric and Gas", "Dominion"),
           utility_name = str_replace(utility_name, "Minnesota Power", "Allete"),
           utility_name = str_replace(utility_name, "PPD", "Public Power District"), #NE issue
           utility_name = str_replace(utility_name, "Sam Houston Electric Cooperative", "Sam Houston"),
           utility_name = str_replace(utility_name, "Southern Iowa Electric Cooperative", "Iowa Association of Electric Cooperatives"),
           utility_name = str_replace(utility_name, "Mason County PUD3", "PUD No 3 of Mason County"),
           utility_name = str_replace(utility_name, " ECA", " Electric Cooperative Association"), #occurs in TX, MO
           utility_name = str_replace(utility_name, "Gas and Electric", "Gas & Electric"),
           utility_name = str_replace(utility_name, "Power and Light", "Power & Light"),
           utility_name = str_replace(utility_name, "CEA", "Cooperative Electric Association"),
           utility_name = str_replace(utility_name, "Co-Op", "Cooperative"),
           utility_name = str_replace(utility_name, "Co-op", "Cooperative"),
           utility_name = str_replace(utility_name, "Assn", "Association"),
           utility_name = str_replace(utility_name, "HomeWorks Tri-County Electric", "Tri-County Electric"), #remove from MI tri works
           # utility_name = str_replace(utility_name, "St.", "St"),
           utility_name = str_replace(utility_name, " & ", "&"),
           utility_name = gsub("'", "", utility_name), #remove apostrophies
           utility_name = gsub(" - ", " ", utility_name), #remove hyphens
           utility_name = gsub("-", " ", utility_name),
           utility_name = str_replace(utility_name, " County ", " "),
           utility_name = gsub(",", "", utility_name),
           utility_name = sub("\\.", "", utility_name)
    )
  
  ####***********************
  # Part 2c: Merge EIA and POUS Datasets ####
  # generally a bit complicated, but the idea is to have a dataframe that shows the following only once:
  # (1) matched utilities
  # (2) unmatched utilities in EIA data
  # (3) unmatched utilities in POUS data
  ####***********************
  df_merged <- full_join(df_eia, df_pous, by = c("state" = "state_name", "county" = "county_name")) %>%
    rename(utility_pous = utility_name.y,
           utility_eia = utility_name.x,
           ownership_type_eia = ownership_type) %>%
    mutate(i_match = ifelse(str_detect(utility_eia, fixed(utility_pous, ignore_case=TRUE)) | str_detect(utility_pous, fixed(utility_eia, ignore_case=TRUE)), 1, 0)) %>%
    group_by(state, county) %>%
    mutate(nrow_county = sum(i_match == 1)) %>%
    group_by(state, county, utility_eia) %>%
    mutate(nrow_countyutility = sum(i_match == 1))
  
  a_df_merged <<- df_merged
  
  df_unique_pous <- df_merged %>% filter(i_match == 0) %>% 
    ungroup() %>%
    distinct(state, county, utility_pous, .keep_all = TRUE) %>%  
    select(utility_eia, state, county, utility_pous) %>%
    mutate(i_unique_pous = 1)
  
  df_merged <- full_join(df_merged, df_unique_pous, by = c("state", "county", "utility_eia", "utility_pous"))
  
  df_pous_add <- df_merged %>% mutate(i_unique_pous = ifelse(i_unique_pous != 1, 0, 1)) %>% 
    filter(i_unique_pous == 1) %>%
    mutate(utility_eia = "")
  
  df_merged <- rbind(df_merged, df_pous_add) 
  
  df_merged <- df_merged %>% arrange(state, county, utility_eia, desc(i_match)) %>%
    mutate(i_keep_unique_eia = ifelse((i_match) == 1, 1, 0),
           i_keep_nonunique = ifelse(nrow_countyutility == 0 & row_number()==1, 1, 0)
    ) %>%
    ungroup()
  
  df_merged <- df_merged %>% filter(i_keep_unique_eia == 1 | i_unique_pous == 1 | i_keep_nonunique == 1) %>%
    group_by(state, county, utility_eia) %>%
    arrange(desc(i_match)) %>%
    mutate(utility_eia = ifelse(row_number() == 1, utility_eia, "")) %>%
    group_by(state, county, utility_pous) %>%
    arrange(desc(i_match)) %>%
    mutate(utility_pous = ifelse(row_number() == 1, utility_pous, "")) %>%
    filter(utility_eia != "" | utility_pous != "") %>%                 
    group_by(state, county) %>%
    mutate(n_matched_county = sum(i_match),
           n_util_county = n(),
           ownership_type_eia = ifelse(utility_eia == "", "", ownership_type_eia)) %>%
    select(-c(nrow_county, nrow_countyutility, i_keep_unique_eia, i_unique_pous, i_keep_nonunique)) %>%
    arrange(state, county) %>%
    rename(eia_year = data_year,
           pous_year = year)
  
  df_anti_join <- anti_join(df_eia, df_merged, by = c("utility_name" = "utility_eia", "state" = "state", "county" = "county")) %>%
    rename(utility_eia = utility_name) 
  
  # df_eia <<- df_eia
  # df_merged <<- df_merged
  # df_anti_join <<- df_anti_join
  
  # This is the final dataset with (1) ALL eia utilities and (2) ALL pous utilities merged with matching indicators at state-county-year level
  df_merged <- bind_rows(df_merged, df_anti_join) %>%
    arrange(state, county) %>%
    mutate(i_match = ifelse(is.na(i_match), 0, i_match),
           pous_year = ifelse(pous_year == "", {{pous_year}}, pous_year))
  
  #output merged data for specified pous year
  df_merged <<- df_merged
  
}
# names(df_merged)



####***********************
# Part 4: Run the fun_utilities_merge function for below years ####
# RUN THE ABOVE FOR YEARS 2017, 2018, 2019, 2020 (FOR 2020, USE EIA == 2019, POUS == 2020)
# currently, I just manually changed the datasets, but I should be able to put them in a lapply and make it into a function
# function inputs could be eia_year = 2017, 2018, 2019, 2019 and pous_year = 2017, 2018, 2019, 2020
####***********************
fun_utilities_merge(2017, 2017)
df_2017 <- df_merged
fun_utilities_merge(2018, 2018)
df_2018 <- df_merged
fun_utilities_merge(2019, 2019)
df_2019 <- df_merged
fun_utilities_merge(2019, 2020)
df_2020 <- df_merged
rm(df_merged)


####***********************
# Part 5: Stack datasets for individual years together/export ####
# the goal of this is not to analyze the data but it is to understand how many utilities EIA and POUS cover
# the follow up code is eia_pous_utilities_covered_calc.R
####***********************
# df_final_merged <- bind_rows(df_merged, df_18_merged, df_19_merged, df_20_merged)
df_final_merged_2 <- bind_rows(df_2017, df_2018, df_2019, df_2020) %>% 
  write_csv(here::here("Data", "Outputs", "df_covered_utilities_final_updated_629.csv"))


