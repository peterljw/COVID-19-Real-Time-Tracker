library(tidyverse)
library(covid19.analytics)
library(maps)
library(cronR)
library(aws.s3)

######## Helper Functions ########
options(scipen=10000)

Caps = function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

normalize <- function(x){
  x = x^0.6
  return((x- min(x)) /(max(x)-min(x)))
}

setwd("~/Desktop/Data Science/Projects/COVID-19-Analysis/covid_dashboard")
######## 1.Data Preprocessing ########

country_population <- readRDS("country_population.rds")
state_population = readRDS("state_population.rds")

##### 1.1 Gloabl Time-Series by Country #####

#### normal confirmed time-series: ts.country.confirmed ####
ts.country.confirmed = covid19.data("ts-confirmed")
ts.country.confirmed$Country.Region = as.character(ts.country.confirmed$Country.Region)
ts.country.confirmed$Country.Region[ts.country.confirmed$Country.Region == "US"] = "USA"
ts.country.confirmed$Country.Region[ts.country.confirmed$Country.Region == "United Kingdom"] = "UK"
ts.country.confirmed$Country.Region[ts.country.confirmed$Country.Region == "Korea, South"] = "South Korea"
ts.country.confirmed$Country.Region[ts.country.confirmed$Country.Region == "Taiwan*"] = "Taiwan"
ts.country.confirmed =  ts.country.confirmed %>%
  group_by(Country.Region) %>%
  select(-c(Province.State, Lat, Long)) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(.[[ncol(ts.country.confirmed)-3]]))

#### normal standardized confirmed time-series: df.global.ts ####
top10_countries = ts.country.confirmed$Country.Region[1:10]
ts.country.confirmed.top10 = ts.country.confirmed %>% filter(Country.Region %in% top10_countries)
df.global.ts = tibble()
n = 5000
for(i in c(1:10)) {
  # get the variables
  Count = unlist(ts.country.confirmed.top10[i,-1])[unlist(ts.country.confirmed.top10[i,-1]) > n]
  Day = c(1:length(Count))
  Country = rep(top10_countries[i], length(Count))
  # append to the pre-specified table
  df.new = tibble(Country, Day, Count)
  df.global.ts = rbind(df.global.ts, df.new)
}
df.global.ts$Country = factor(df.global.ts$Country, levels = top10_countries)

#### confirmed per million confirmed time-series: df.global.million.ts ####
ts.country.confirmed.million = inner_join(ts.country.confirmed, country_population,
                                          by = c("Country.Region" = "Country"))
ts.country.confirmed.million = ts.country.confirmed.million %>% filter(Population > 1000000)
for(i in 2:ncol(ts.country.confirmed.million)) {
  ts.country.confirmed.million[,i] = ts.country.confirmed.million[,i]/ts.country.confirmed.million$Population*1000000
}
ts.country.confirmed.million$Population = NULL
ts.country.confirmed.million = ts.country.confirmed.million %>% arrange(desc(.[[ncol(ts.country.confirmed.million)]]))

top10_countries = ts.country.confirmed.million$Country.Region[1:10]
ts.country.confirmed.million.top10 = ts.country.confirmed.million %>% filter(Country.Region %in% top10_countries)
df.global.million.ts = tibble()
n = 250
for(i in c(1:10)) {
  # get the variables
  Count = unlist(ts.country.confirmed.million.top10[i,-1])[unlist(ts.country.confirmed.million.top10[i,-1]) > n]
  Day = c(1:length(Count))
  Country = rep(top10_countries[i], length(Count))
  # append to the pre-specified table
  df.new = tibble(Country, Day, Count)
  df.global.million.ts = rbind(df.global.million.ts, df.new)
}
df.global.million.ts$Country = factor(df.global.million.ts$Country, levels = top10_countries)

#### normal deaths time-series: ts.country.deaths ####
ts.country.deaths = covid19.data("ts-deaths", local.data = FALSE, debrief = FALSE)
ts.country.deaths$Country.Region = as.character(ts.country.deaths$Country.Region)
ts.country.deaths$Country.Region[ts.country.deaths$Country.Region == "US"] = "USA"
ts.country.deaths$Country.Region[ts.country.deaths$Country.Region == "United Kingdom"] = "UK"
ts.country.deaths$Country.Region[ts.country.deaths$Country.Region == "Korea, South"] = "South Korea"
ts.country.deaths$Country.Region[ts.country.deaths$Country.Region == "Taiwan*"] = "Taiwan"
ts.country.deaths =  ts.country.deaths %>%
  group_by(Country.Region) %>%
  select(-c(Province.State, Lat, Long)) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(.[[ncol(ts.country.deaths)-3]]))

#### normal standardized deaths time-series: df.global.ts.country.deaths ####
top10_countries = ts.country.deaths$Country.Region[1:10]
ts.country.deaths.top10 = ts.country.deaths %>% filter(Country.Region %in% top10_countries)
df.global.ts.country.deaths = tibble()
n = 1000
for(i in c(1:10)) {
  # get the variables
  Count = unlist(ts.country.deaths.top10[i,-1])[unlist(ts.country.deaths.top10[i,-1]) > n]
  Day = c(1:length(Count))
  Country = rep(top10_countries[i], length(Count))
  # append to the pre-specified table
  df.new = tibble(Country, Day, Count)
  df.global.ts.country.deaths = rbind(df.global.ts.country.deaths, df.new)
}
df.global.ts.country.deaths$Country = factor(df.global.ts.country.deaths$Country, top10_countries)


#### deaths per million time-series: df.global.ts.country.deaths.million ####
ts.country.deaths.million = inner_join(ts.country.deaths, country_population,
                                       by = c("Country.Region" = "Country"))
ts.country.deaths.million = ts.country.deaths.million %>% filter(Population > 1000000)
for(i in 2:ncol(ts.country.deaths.million)) {
  ts.country.deaths.million[,i] = ts.country.deaths.million[,i]/ts.country.deaths.million$Population*1000000
}
ts.country.deaths.million$Population = NULL
ts.country.deaths.million = ts.country.deaths.million %>% arrange(desc(.[[ncol(ts.country.deaths.million)]]))

top10_countries = ts.country.deaths.million$Country.Region[1:10]
ts.country.deaths.million.top10 = ts.country.deaths.million %>% filter(Country.Region %in% top10_countries)
df.global.ts.country.deaths.million = tibble()
n = 50
for(i in c(1:10)) {
  # get the variables
  Count = unlist(ts.country.confirmed.million.top10[i,-1])[unlist(ts.country.confirmed.million.top10[i,-1]) > n]
  Day = c(1:length(Count))
  Country = rep(top10_countries[i], length(Count))
  # append to the pre-specified table
  df.new = tibble(Country, Day, Count)
  df.global.ts.country.deaths.million = rbind(df.global.ts.country.deaths.million, df.new)
}
df.global.ts.country.deaths.million$Country = factor(df.global.ts.country.deaths.million$Country, levels = top10_countries)

#### normal recovered time-series: ts.country.recovered ####
ts.country.recovered = covid19.data("ts-recovered")
ts.country.recovered$Country.Region = as.character(ts.country.recovered$Country.Region)
ts.country.recovered$Country.Region[ts.country.recovered$Country.Region == "US"] = "USA"
ts.country.recovered$Country.Region[ts.country.recovered$Country.Region == "United Kingdom"] = "UK"
ts.country.recovered$Country.Region[ts.country.recovered$Country.Region == "Korea, South"] = "South Korea"
ts.country.recovered$Country.Region[ts.country.recovered$Country.Region == "Taiwan*"] = "Taiwan"
ts.country.recovered =  ts.country.recovered %>%
  group_by(Country.Region) %>%
  select(-c(Province.State, Lat, Long)) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(.[[ncol(ts.country.recovered)-3]]))


##### 1.2 U.S. Time-Series by State #####

#### normal confirmed time-series: ts.us.confirmed ####
ts.us.confirmed = covid19.data(case = "ts-confirmed-US")
ts.us.confirmed = ts.us.confirmed %>%
  group_by(Province_State) %>%
  select(-c(Country_Region, Lat, Long_)) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(.[[ncol(ts.us.confirmed)-3]]))

#### normal standardized confirmed time-series: df.us.ts ####
top10_states = ts.us.confirmed$Province_State[1:10]
ts.us.confirmed.top10 = ts.us.confirmed %>% filter(Province_State %in% top10_states)
df.us.ts = tibble()
n = 1000
for(i in c(1:10)) {
  # get the variables
  Count = unlist(ts.us.confirmed.top10[i,-1])[unlist(ts.us.confirmed.top10[i,-1]) > n]
  Day = c(1:length(Count))
  State = rep(top10_states[i], length(Count))
  # append to the pre-specified table
  df.new = tibble(State, Day, Count)
  df.us.ts = rbind(df.us.ts, df.new)
}
df.us.ts$State = factor(df.us.ts$State, levels = top10_states)


#### confirmed per million confirmed time-series: df.us.million.ts ####
ts.us.confirmed.million = inner_join(ts.us.confirmed, state_population,
                                     by = c("Province_State" = "State"))
ts.us.confirmed.million = ts.us.confirmed.million %>% filter(Population > 1000000)
for(i in 2:ncol(ts.us.confirmed.million)) {
  ts.us.confirmed.million[,i] = ts.us.confirmed.million[,i]/ts.us.confirmed.million$Population*1000000
}
ts.us.confirmed.million$Population = NULL
ts.us.confirmed.million = ts.us.confirmed.million %>% arrange(desc(.[[ncol(ts.us.confirmed.million)]]))

top10_states = ts.us.confirmed.million$Province_State[1:10]
ts.us.confirmed.million.top10 = ts.us.confirmed.million %>% filter(Province_State %in% top10_states)
df.us.million.ts = tibble()
n = 50
for(i in c(1:10)) {
  # get the variables
  Count = unlist(ts.us.confirmed.million.top10[i,-1])[unlist(ts.us.confirmed.million.top10[i,-1]) > n]
  Day = c(1:length(Count))
  State = rep(top10_states[i], length(Count))
  # append to the pre-specified table
  df.new = tibble(State, Day, Count)
  df.us.million.ts = rbind(df.us.million.ts, df.new)
}
df.us.million.ts$State = factor(df.us.million.ts$State, levels = top10_states)

#### normal deaths time-series: ts.us.deaths ####
ts.us.deaths= covid19.data(case = "ts-deaths-US")
ts.us.deaths = ts.us.deaths %>%
  group_by(Province_State) %>%
  select(-c(Country_Region, Lat, Long_)) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(.[[ncol(ts.us.deaths)-3]]))

#### normal standardized deaths time-series: df.us.ts.deaths ####
top10_states = ts.us.deaths$Province_State[1:10]
ts.us.deaths.top10 = ts.us.deaths %>% filter(Province_State %in% top10_states)
df.us.ts.deaths = tibble()
n = 200
for(i in c(1:10)) {
  # get the variables
  Count = unlist(ts.us.deaths.top10[i,-1])[unlist(ts.us.deaths.top10[i,-1]) > n]
  Day = c(1:length(Count))
  State = rep(top10_states[i], length(Count))
  # append to the pre-specified table
  df.new = tibble(State, Day, Count)
  df.us.ts.deaths = rbind(df.us.ts.deaths, df.new)
}
df.us.ts.deaths$State = factor(df.us.ts.deaths$State, levels = top10_states)

#### deaths per million time-series: df.us.million.ts.deaths ####
ts.us.deaths.million = inner_join(ts.us.deaths, state_population,
                                  by = c("Province_State" = "State"))
ts.us.deaths.million = ts.us.deaths.million %>% filter(Population > 1000000)
for(i in 2:ncol(ts.us.deaths.million)) {
  ts.us.deaths.million[,i] = ts.us.deaths.million[,i]/ts.us.deaths.million$Population*1000000
}
ts.us.deaths.million$Population = NULL
ts.us.deaths.million = ts.us.deaths.million %>% arrange(desc(.[[ncol(ts.us.deaths.million)]]))

top10_states = ts.us.deaths.million$Province_State[1:10]
ts.us.deaths.million.top10 = ts.us.deaths.million %>% filter(Province_State %in% top10_states)
df.us.million.ts.deaths = tibble()
n = 10
for(i in c(1:10)) {
  # get the variables
  Count = unlist(ts.us.deaths.million.top10[i,-1])[unlist(ts.us.deaths.million.top10[i,-1]) > n]
  Day = c(1:length(Count))
  State = rep(top10_states[i], length(Count))
  # append to the pre-specified table
  df.new = tibble(State, Day, Count)
  df.us.million.ts.deaths = rbind(df.us.million.ts.deaths, df.new)
}
df.us.million.ts.deaths$State = factor(df.us.million.ts.deaths$State, levels = top10_states)

##### 1.3 Global Aggregated Cases and Deaths by Country #####
agg.country = covid19.data("aggregated", local.data = FALSE, debrief = FALSE)
agg.country = agg.country %>%
  select(Country_Region, Confirmed, Deaths, Recovered) %>%
  group_by(Country_Region) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(Confirmed))

##### aggregated metrics by country w/ geographical columns: agg.country.map ####
world_map <- map_data("world")
agg.country.map = agg.country
agg.country.map$Country_Region = as.character(agg.country$Country_Region)
agg.country.map$Country_Region[agg.country.map$Country_Region == "US"] = "USA"
agg.country.map$Country_Region[agg.country.map$Country_Region == "United Kingdom"] = "UK"
agg.country.map$Country_Region[agg.country.map$Country_Region == "Korea, South"] = "South Korea"
agg.country.map$Country_Region[agg.country.map$Country_Region == "Taiwan*"] = "Taiwan"
agg.country.map <- inner_join(agg.country.map, world_map, by = c("Country_Region" = "region"))
country_population <- readRDS("country_population.rds")
agg.country.map = left_join(agg.country.map, country_population, by = c("Country_Region" = "Country"))
agg.country.map = agg.country.map %>%
  mutate(ConfirmedPerMillion = round(Confirmed/Population*1000000,2),
         DeathsPerMillion = round(Deaths/Population*1000000,2))

##### aggregated metrics by country w/ population: agg.country.table #### 
agg.country.table = agg.country
agg.country.table$Country_Region = as.character(agg.country.table$Country_Region)
agg.country.table$Country_Region[agg.country.table$Country_Region == "US"] = "USA"
agg.country.table$Country_Region[agg.country.table$Country_Region == "United Kingdom"] = "UK"
agg.country.table$Country_Region[agg.country.table$Country_Region == "Korea, South"] = "South Korea"
agg.country.table$Country_Region[agg.country.table$Country_Region == "Taiwan*"] = "Taiwan"
agg.country.table = agg.country.table %>% inner_join(country_population, by = c("Country_Region" = "Country")) %>%
  mutate("Confirmed/M" = round(Confirmed/Population*1000000,2),
         "Deaths/M" = round(Deaths/Population*1000000,2),
         "Recovered/M" = round(Deaths/Population*1000000,2))
colnames(agg.country.table) = c("Country", colnames(agg.country.table)[-1])

##### 1.4 U.S. Aggregated Cases and Deaths By State #####
agg.us = covid19.data("aggregated", local.data = FALSE, debrief = FALSE)
agg.us = agg.us %>%
  filter(Country_Region == "US" & Province_State != "Recovered") %>%
  select(Province_State, Confirmed, Deaths) %>%
  group_by(Province_State) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(Confirmed))

##### aggregated metrics by US states w/ geographical columns: agg.us.map ####
states_map = map_data("state")
agg.us.map = agg.us %>% mutate(Province_State = tolower(Province_State))
agg.us.map = inner_join(agg.us.map, states_map, by = c("Province_State" = "region"))
agg.us.map$Province_State = sapply(agg.us.map$Province_State, Caps)

##### aggregated metrics by US states w/ geographical columns: agg.us.table ####
state_population_copy = state_population
state_population_copy$State[state_population_copy$State == "New York State"] = "New York"
state_population_copy$State[state_population_copy$State == "Washington, D.C"] = "District Of Columbia"
state_population_copy$State[state_population_copy$State == "The State Of Wisconsin"] = "Wisconsin"
state_population_copy$State[state_population_copy$State == "Washington State"] = "Washington"
agg.us.map = agg.us.map %>% left_join(state_population_copy, by = c("Province_State" = "State"))
agg.us.map = agg.us.map %>%
  mutate(ConfirmedPerMillion = round(Confirmed/Population*1000000,2),
         DeathsPerMillion = round(Deaths/Population*1000000,2))
agg.us.table = agg.us %>% left_join(state_population_copy,by = c("Province_State" = "State")) %>%
  mutate("Confirmed/M" = round(Confirmed/Population*1000000,2),
         "Deaths/M" = round(Deaths/Population*1000000,2))
colnames(agg.us.table) = c("State", colnames(agg.us.table)[-1])

##### 1.5 Global Active Cases, Deaths and Recovered Time-Series #####
#### Global time-series: ts.country.all ####
ts.country.all = covid19.data("ts-ALL") %>%
  select(-c(Province.State, Country.Region, Long, Lat)) %>%
  group_by(status) %>%
  summarise_all(funs(sum))
ts.country.all = as_tibble(t(as.matrix(ts.country.all)))
colnames(ts.country.all) = unlist(ts.country.all[1,])
ts.country.all = ts.country.all[-1,]
ts.country.all$confirmed = as.integer(ts.country.all$confirmed)
ts.country.all$death = as.integer(ts.country.all$death)
ts.country.all$recovered = as.integer(ts.country.all$recovered)
ts.country.all = ts.country.all %>% mutate(active = confirmed - death - recovered)

##### 1.6 U.S. Active Cases, Deaths and Recovered Time-Series #####
#### US time-series: ts.us.all ####
ts.us.all = covid19.data("ts-ALL") %>%
  filter(Country.Region == "US") %>%
  select(-c(Province.State, Country.Region, Long, Lat)) %>%
  group_by(status) %>%
  summarise_all(funs(sum))
ts.us.all = as_tibble(t(as.matrix(ts.us.all)))
colnames(ts.us.all) = unlist(ts.us.all[1,])
ts.us.all = ts.us.all[-1,]
ts.us.all$confirmed = as.integer(ts.us.all$confirmed)
ts.us.all$death = as.integer(ts.us.all$death)
ts.us.all$recovered = as.integer(ts.us.all$recovered)
ts.us.all = ts.us.all %>% mutate(active = confirmed - death - recovered)

# ##### 1.7 Symptoms #####
# df.symptoms = readRDS("df_symptoms.rds")
# 
# ##### 1.8 patient data #####
# df.patients = readRDS("df_patients.rds")
# 
# ##### 1.9* Colors #####
# newColor = "black"
# countColor = "darkorange"

###### Upload to S3 ######

s3BucketName <- "peter-covid-dashboard-data"
Sys.setenv("AWS_ACCESS_KEY_ID" = "",
           "AWS_SECRET_ACCESS_KEY" = "",
           "AWS_DEFAULT_REGION" = "us-east-2")

s3saveRDS(x = ts.country.confirmed, bucket = s3BucketName, object = "ts.country.confirmed.rds")
s3saveRDS(x = df.global.ts, bucket = s3BucketName, object ="df.global.ts.rds")
s3saveRDS(df.global.million.ts, bucket = s3BucketName, object ="df.global.million.ts.rds")

s3saveRDS(ts.country.deaths, bucket = s3BucketName, object ="ts.country.deaths.rds")
s3saveRDS(df.global.ts.country.deaths, bucket = s3BucketName, object ="df.global.ts.country.deaths.rds")
s3saveRDS(df.global.ts.country.deaths.million, bucket = s3BucketName, object ="df.global.ts.country.deaths.million.rds")

s3saveRDS(ts.country.recovered, bucket = s3BucketName, object ="ts.country.recovered.rds")

s3saveRDS(ts.us.confirmed, bucket = s3BucketName, object ="ts.us.confirmed.rds")
s3saveRDS(df.us.ts, bucket = s3BucketName, object ="df.us.ts.rds")
s3saveRDS(df.us.million.ts, bucket = s3BucketName, object ="df.us.million.ts.rds")

s3saveRDS(ts.us.deaths, bucket = s3BucketName, object ="ts.us.deaths.rds")
s3saveRDS(df.us.ts.deaths, bucket = s3BucketName, object ="df.us.ts.deaths.rds")
s3saveRDS(df.us.million.ts.deaths, bucket = s3BucketName, object ="df.us.million.ts.deaths.rds")

s3saveRDS(agg.country.map, bucket = s3BucketName, object ="agg.country.map.rds")
s3saveRDS(agg.country.table, bucket = s3BucketName, object ="agg.country.table.rds")
s3saveRDS(agg.us.map, bucket = s3BucketName, object ="agg.us.map.rds")
s3saveRDS(agg.us.table, bucket = s3BucketName, object ="agg.us.table.rds")

s3saveRDS(ts.country.all, bucket = s3BucketName, object ="ts.country.all.rds")
s3saveRDS(ts.us.all, bucket = s3BucketName, object ="ts.us.all.rds")