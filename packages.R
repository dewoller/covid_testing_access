## library() calls go here
library(conflicted)
library(dotenv)
library(targets)
library(tidyverse)
library(readxl)
library(geosphere)
library(lubridate)
library(rmarkdown)
library(janitor)
library(sf)
library(osrm)         # routing package
library(sp)
library(sparkline)
library(gt)

# library(rvest)
conflict_prefer("pluck", "purrr")

conflicted::conflict_prefer('filter','dplyr')
conflicted::conflict_prefer('summarise','dplyr')


GLOBAL_BASE_DATE_TIME = lubridate::ymd_hms('2020-07-06 10:00:00', tz='Australia/Melbourne')
GLOBAL_BASE_DATE = floor_date(GLOBAL_BASE_DATE_TIME, unit='day' )




