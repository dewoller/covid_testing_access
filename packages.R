## library() calls go here
library(conflicted)
library(dotenv)
library(drake)
library(tidyverse)
#library(revgeo)
#library(ggmap)

library(geosphere)
#library(spatstat)
#library("R.cache")
#library("googleway")
#library("purrrlyr")
#library(tidyjson)
library(lubridate)
#library(foreign )
#library(wrapr )   # for the qc function
library(rmarkdown)
library(janitor)
#library(caret)
#library(parsnip)
#library(gt)
#library(testthat)

library(sf)
#library(readxl)

#library(stplanr)      # geographic transport data package
#library(tmap)         # visualization package (see Chapter 8)
library(osrm)         # routing package
#library(dodgr)         # routing package
#library(multidplyr)         # routing package

#library(graphhopper)         # routing package

#install.packages('stplanr')
#install.packages('dodgr')
#install.packages('GGally')


#library(corrplot) # for correlation plot
#library(GGally) # for parallel coordinate plot
#library(e1071)


library(geohash)
library(cleangeo)
library(sp)
library(rgeos)
library(workflowr)
library(leaflet)

library(rvest)
conflict_prefer("pluck", "purrr")

conflicted::conflict_prefer('filter','dplyr')
conflicted::conflict_prefer('summarise','dplyr')


GLOBAL_BASE_DATE_TIME = lubridate::ymd_hms('2020-07-06 10:00:00', tz='Australia/Melbourne')
GLOBAL_BASE_DATE = floor_date(GLOBAL_BASE_DATE_TIME, unit='day' )




