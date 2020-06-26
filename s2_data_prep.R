
#### Prepares the data for analysis
library(data.table)
        
##### Load data ####
covid = fread("./data/covid.csv")
trips = fread("./data/trips.csv")
pop = fread("./data/countypop.csv")
votes = fread("./data/votes.csv")


##### Formatting ####

#### Format population data
#Get FIPS code for pop
colnames(pop) <- tolower(colnames(pop))
pop[ , county_fips := paste0(state, sprintf("%03d", county))]
pop <- pop[sumlev == 50, !c("sumlev","state","county","stname","ctyname")]
pop[ , county_fips := as.numeric(county_fips)]
setnames(pop, "popestimate2019","pop")
pop[ , pop := as.numeric(pop)]


#### Format trip data
#Format trip colnames
colnames(trips) <- gsub(" ", "_", tolower(colnames(trips)))
colnames(trips) <- gsub("number_of_|population_", "", tolower(colnames(trips)))
trips[ , date := gsub("/","-",date)]
setnames(trips, "state_postal_code","state")

#Keep only total trips
trips <- trips[, colnames(trips)[!grepl("trips_",colnames(trips))], with = F]
trips <- trips[ , !c("staying_at_home","not_staying_at_home")]

#Integer to Numeric, for analysis
trips[ , trips := as.numeric(trips)]

#Keep just county level
trips <- trips[ level == "County", !"level", with = F]
trips <- trips[ , !"state"]

#### Format covid data
#Rename to merge
setnames(covid, "fips", c("county_fips"))
covid <- covid[ , .(date, county_fips, cases, deaths)]


#### Format vote data
setnames(votes, c("combined_fips","state_abbr"), c("county_fips","state"))


#### Merging data ####
#Merge trips and cases
covidtrips.county <- merge(covid, trips, by = c("date","county_fips"), all = T)
#Merge population
covidtrips.county <- merge(pop, covidtrips.county, by = "county_fips")
#Merge votes
covidtrips.county <- merge(covidtrips.county, votes[ , .(county_fips,state, votes_gop, total_votes)], by = "county_fips")

#Sort for clarity
cols = c("region", "division", "state", "state_fips", "county_fips", "county_name", 
         "date", "pop", "votes_gop","total_votes", "cases", "deaths", "trips")
covidtrips.county <- covidtrips.county[ , cols, with = F]
rm(covid, pop, trips)

#### Calculations ####

#Convert to date value
covidtrips.county[ , date_val := as.Date(date, format = "%Y-%m-%d")]

#Format NA
#covidtrips.county[, (c("cases","deaths","trips")) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = c("cases","deaths","trips")]

#New cases
covidtrips.county[order(date_val), new_cases := (cases - shift(cases, fill = 0)), by = county_fips]
covidtrips.county[order(date_val), new_deaths := (deaths - shift(deaths, fill = 0)), by = county_fips]

# #Per capita
# cols = c("cases","deaths","trips", "new_cases","new_deaths")
# covidtrips.county[ , (paste0(cols,"_pc")) := lapply(.SD, function(x) 100000*x/pop), .SDcols = cols]

#Percent GOP
covidtrips.county[ , per_gop := votes_gop/total_votes]

#Division labels
divlabel <- data.table("division" = unique(covidtrips.county$division), 
                       "divlabel" = sapply(unique(covidtrips.county$division), function(x) {
                         paste0("Division ", x, " (", paste(unique(covidtrips.county[ division == x, state]), collapse = ", "),")")
                       })
)
#Aggregate
casecols = c("cases","deaths","trips", "new_cases","new_deaths")
votecols = c("pop","votes_gop","total_votes")

#By state
covidtrips.state <- covidtrips.county[ , lapply(.SD, function(x) sum(x,na.rm = T)), .SDcols = c(votecols,casecols), by = .(date_val,state)]
covidtrips.state[ , (votecols) := lapply(.SD, function(x) sum(x,na.rm = T)), .SDcols = votecols, by = .(state)]


#By Division
covidtrips.division <- covidtrips.county[ , lapply(.SD, function(x) sum(x,na.rm = T)), .SDcols = c(votecols,casecols), by = .(date_val,division)]
covidtrips.division[ , (votecols) := lapply(.SD, function(x) sum(x,na.rm = T)), .SDcols = votecols, by = .(division)]
covidtrips.division <- merge(covidtrips.division, divlabel, by = "division")

#By nationally
covidtrips.nation <- covidtrips.county[ , lapply(.SD, function(x) sum(x,na.rm = T)), .SDcols = c(votecols,casecols), by = .(date_val)]
covidtrips.nation[ , (votecols) := lapply(.SD, function(x) sum(x,na.rm = T)), .SDcols = votecols]

#Percent GOP
covidtrips.state[ , per_gop := votes_gop/total_votes]
covidtrips.division[ , per_gop := votes_gop/total_votes]
covidtrips.nation[ , per_gop := votes_gop/total_votes]


#Save
fwrite(covidtrips.county, file = "./data/merged_covidtrips.csv")
save(covidtrips.county, covidtrips.state, covidtrips.division, covidtrips.nation, file = "./data/merged_covidtrips.RData")
