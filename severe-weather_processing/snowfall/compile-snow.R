library(tidyverse)
library(data.table)
require(fst)
require(dplyr)

################################################################################
### Read in all the datafiles
################################################################################

setwd("~/Desktop/snow-tif/processed")
df <- list.files(pattern = ".fst") %>%
  map(read_fst) %>% 
  data.table::rbindlist()



setwd("~/Desktop/snow-tif/processed/compiled")
write.fst(df, "snow-compiled-2018-2020.fst")