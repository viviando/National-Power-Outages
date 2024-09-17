require(dplyr)
require(stringr)
require(tidyverse)
require(weathermetrics)
require(lubridate)
require(fst)

setwd("~/Desktop/gridmet")

gm <- read_fst("gridmet-windspeed-precip-2018-2020.fst")



### splitting the system index variable:


gm_sep <-gm %>% 
  tidyr::separate("system:index", into = c("date",  NA), sep = "_")

gm_un <- gm_sep%>%
  mutate(date = ymd(date)) %>%
  mutate(fips = str_pad(GEOID, width=5, side="left", pad="0"))



write_fst(gm_un, "gridmet-windspeed-precip-2018-2020-final.fst")