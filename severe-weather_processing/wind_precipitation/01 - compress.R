require(fst)
require(data.table)
setwd("~/Desktop/gridmet")
dta <- fread("gridmet-windspeed-2018-2020.csv")
write_fst(dta, "gridmet-windspeed-precip-2018-2020.fst")

