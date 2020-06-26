#### Downloads latest data
library(RCurl)
library(RSocrata)


#### Download trips
download.file("https://data.bts.gov/api/views/w96p-f2qv/rows.csv?accessType=DOWNLOAD", "./data/trips.csv")

#### Download covid cases
download.file("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", "./data/covid.csv")

#### Download election results
download.file("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-16/master/2016_US_County_Level_Presidential_Results.csv", "./data/votes.csv")





