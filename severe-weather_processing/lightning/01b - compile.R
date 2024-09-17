library(tidyverse)
library(data.table)
require(fst)

################################################################################
### Read in all the datafiles
################################################################################

setwd("~/Desktop/lightning-data-lis/lightning-rds/2017")
df_2017 <- list.files(pattern = ".rds") %>%
  map(readRDS) %>% 
  data.table::rbindlist()


setwd("~/Desktop/lightning-data-lis/lightning-rds/2018")
df_2018 <- list.files(pattern = ".rds") %>%
  map(readRDS) %>% 
  data.table::rbindlist()


setwd("~/Desktop/lightning-data-lis/lightning-rds/2019")
df_2019 <- list.files(pattern = ".rds") %>%
  map(readRDS) %>% 
  data.table::rbindlist()


setwd("~/Desktop/lightning-data-lis/lightning-rds/2020")
df_2020 <- list.files(pattern = ".rds") %>%
  map(readRDS) %>% 
  data.table::rbindlist()

################################################################################
### Bind
################################################################################

df <- rbind(df_2017, df_2018, df_2019, df_2020)


################################################################################
### convert time to UTC
################################################################################

df <- df %>% mutate(time_utc =  as.POSIXct(flash_time, origin = "1993-01-01", tz = "UTC"))


################################################################################
### write out
################################################################################
setwd("~/Desktop/lightning-data-lis/03 final-data")
write.fst(df, "lighting-flashes-2017-2020.fst")



