library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(tseries)
library(forecast)
library(tidyquant)
library(tidyverse)
library(ggiraph)
library(covid19.analytics)
library(plotly)
library(patchwork)
library(hrbrthemes)
library(maps)
library(formattable)
# remotes::install_github("GuangchuangYu/nCov2019")
library(nCov2019)
Sys.setenv(TZ = "America/Los_Angeles")

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

plot_multi_histogram <- function(df, feature, label_column) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.7, position="identity", aes(y = ..density.., text = ..density..), color = "gray30", bins=12) +
    geom_density(alpha=0.7) +
    labs(x=feature, y = "Density")
  plt + 
    guides(fill=guide_legend(title=label_column)) +
    theme(legend.position = "none")
}

######## Data Preprocessing ########

# us aggregated
agg.us = covid19.data("aggregated", local.data = FALSE, debrief = FALSE)
agg.us = agg.us %>%
  filter(Country_Region == "US" & Province_State != "Recovered") %>%
  select(Province_State, Confirmed, Deaths) %>%
  group_by(Province_State) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(Confirmed))
states_map = map_data("state")
agg.us.map = agg.us %>% mutate(Province_State = tolower(Province_State))
agg.us.map = inner_join(agg.us.map, states_map, by = c("Province_State" = "region"))
agg.us.map$Province_State = sapply(agg.us.map$Province_State, Caps)
state_population = readRDS("state_population.rds")
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

# world aggregated
agg.world = covid19.data("aggregated", local.data = FALSE, debrief = FALSE)
agg.world = agg.world %>%
  select(Country_Region, Confirmed, Deaths, Recovered) %>%
  group_by(Country_Region) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(Confirmed))
world_map <- map_data("world")
agg.world.map = agg.world
agg.world.map$Country_Region = as.character(agg.world$Country_Region)
agg.world.map$Country_Region[agg.world.map$Country_Region == "US"] = "USA"
agg.world.map$Country_Region[agg.world.map$Country_Region == "United Kingdom"] = "UK"
agg.world.map$Country_Region[agg.world.map$Country_Region == "Korea, South"] = "South Korea"
agg.world.map$Country_Region[agg.world.map$Country_Region == "Taiwan*"] = "Taiwan"
agg.world.map <- inner_join(agg.world.map, world_map, by = c("Country_Region" = "region"))
country_population <- readRDS("country_population.rds")
agg.world.map = left_join(agg.world.map, country_population, by = c("Country_Region" = "Country"))
agg.world.map = agg.world.map %>%
  mutate(ConfirmedPerMillion = round(Confirmed/Population*1000000,2),
         DeathsPerMillion = round(Deaths/Population*1000000,2))
agg.world.table = agg.world
agg.world.table$Country_Region = as.character(agg.world.table$Country_Region)
agg.world.table$Country_Region[agg.world.table$Country_Region == "US"] = "USA"
agg.world.table$Country_Region[agg.world.table$Country_Region == "United Kingdom"] = "UK"
agg.world.table$Country_Region[agg.world.table$Country_Region == "Korea, South"] = "South Korea"
agg.world.table$Country_Region[agg.world.table$Country_Region == "Taiwan*"] = "Taiwan"
agg.world.table = agg.world.table %>% inner_join(country_population, by = c("Country_Region" = "Country")) %>%
  mutate("Confirmed/M" = round(Confirmed/Population*1000000,2),
         "Deaths/M" = round(Deaths/Population*1000000,2),
         "Recovered/M" = round(Deaths/Population*1000000,2))
colnames(agg.world.table) = c("Country", colnames(agg.world.table)[-1])


# confirmed time-series
ts.confirmed = covid19.data("ts-confirmed", local.data = FALSE, debrief = FALSE)
ts.confirmed$Country.Region = as.character(ts.confirmed$Country.Region)
ts.confirmed$Country.Region[ts.confirmed$Country.Region == "US"] = "USA"
ts.confirmed$Country.Region[ts.confirmed$Country.Region == "United Kingdom"] = "UK"
ts.confirmed$Country.Region[ts.confirmed$Country.Region == "Korea, South"] = "South Korea"
ts.confirmed$Country.Region[ts.confirmed$Country.Region == "Taiwan*"] = "Taiwan"
ts.confirmed =  ts.confirmed %>%
  group_by(Country.Region) %>%
  select(-c(Province.State, Lat, Long)) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(.[[ncol(ts.confirmed)-3]]))

top10_countries = ts.confirmed$Country.Region[1:10]
ts.confirmed.top10 = ts.confirmed %>% filter(Country.Region %in% top10_countries)
df.global.ts = tibble()
n = 1000
for(i in c(1:10)) {
  # get the variables
  Count = unlist(ts.confirmed.top10[i,-1])[unlist(ts.confirmed.top10[i,-1]) > n]
  Day = c(1:length(Count))
  Country = rep(top10_countries[i], length(Count))
  # append to the pre-specified table
  df.new = tibble(Country, Day, Count)
  df.global.ts = rbind(df.global.ts, df.new)
}
df.global.ts$Country = factor(df.global.ts$Country, levels = top10_countries)

# confirmed per million time-series
ts.confirmed.million = inner_join(ts.confirmed, country_population,
                                 by = c("Country.Region" = "Country"))
ts.confirmed.million = ts.confirmed.million %>% filter(Population > 1000000)
for(i in 2:ncol(ts.confirmed.million)) {
  ts.confirmed.million[,i] = ts.confirmed.million[,i]/ts.confirmed.million$Population*1000000
}
ts.confirmed.million$Population = NULL
ts.confirmed.million = ts.confirmed.million %>% arrange(desc(.[[ncol(ts.confirmed.million)]]))

top10_countries = ts.confirmed.million$Country.Region[1:10]
ts.confirmed.million.top10 = ts.confirmed.million %>% filter(Country.Region %in% top10_countries)
df.global.million.ts = tibble()
n = 50
for(i in c(1:10)) {
  # get the variables
  Count = unlist(ts.confirmed.million.top10[i,-1])[unlist(ts.confirmed.million.top10[i,-1]) > n]
  Day = c(1:length(Count))
  Country = rep(top10_countries[i], length(Count))
  # append to the pre-specified table
  df.new = tibble(Country, Day, Count)
  df.global.million.ts = rbind(df.global.million.ts, df.new)
}
df.global.million.ts$Country = factor(df.global.million.ts$Country, levels = top10_countries)

# deaths time-series
ts.deaths = covid19.data("ts-deaths", local.data = FALSE, debrief = FALSE)
ts.deaths$Country.Region = as.character(ts.deaths$Country.Region)
ts.deaths$Country.Region[ts.deaths$Country.Region == "US"] = "USA"
ts.deaths$Country.Region[ts.deaths$Country.Region == "United Kingdom"] = "UK"
ts.deaths$Country.Region[ts.deaths$Country.Region == "Korea, South"] = "South Korea"
ts.deaths$Country.Region[ts.deaths$Country.Region == "Taiwan*"] = "Taiwan"
ts.deaths =  ts.deaths %>%
  group_by(Country.Region) %>%
  select(-c(Province.State, Lat, Long)) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(.[[ncol(ts.deaths)-3]]))

top10_countries = ts.deaths$Country.Region[1:10]
ts.deaths.top10 = ts.deaths %>% filter(Country.Region %in% top10_countries)
df.global.ts.deaths = tibble()
n = 1000
for(i in c(1:10)) {
  # get the variables
  Count = unlist(ts.deaths.top10[i,-1])[unlist(ts.deaths.top10[i,-1]) > n]
  Day = c(1:length(Count))
  Country = rep(top10_countries[i], length(Count))
  # append to the pre-specified table
  df.new = tibble(Country, Day, Count)
  df.global.ts.deaths = rbind(df.global.ts.deaths, df.new)
}
df.global.ts.deaths$Country = factor(df.global.ts.deaths$Country, top10_countries)

# deaths per million time-series
ts.deaths.million = inner_join(ts.deaths, country_population,
                                  by = c("Country.Region" = "Country"))
ts.deaths.million = ts.deaths.million %>% filter(Population > 1000000)
for(i in 2:ncol(ts.deaths.million)) {
  ts.deaths.million[,i] = ts.deaths.million[,i]/ts.deaths.million$Population*1000000
}
ts.deaths.million$Population = NULL
ts.deaths.million = ts.deaths.million %>% arrange(desc(.[[ncol(ts.deaths.million)]]))

top10_countries = ts.deaths.million$Country.Region[1:10]
ts.deaths.million.top10 = ts.deaths.million %>% filter(Country.Region %in% top10_countries)
df.global.ts.deaths.million = tibble()
n = 50
for(i in c(1:10)) {
  # get the variables
  Count = unlist(ts.confirmed.million.top10[i,-1])[unlist(ts.confirmed.million.top10[i,-1]) > n]
  Day = c(1:length(Count))
  Country = rep(top10_countries[i], length(Count))
  # append to the pre-specified table
  df.new = tibble(Country, Day, Count)
  df.global.ts.deaths.million = rbind(df.global.ts.deaths.million, df.new)
}
df.global.ts.deaths.million$Country = factor(df.global.ts.deaths.million$Country, levels = top10_countries)

# recovered time-series
ts.recovered = covid19.data("ts-recovered")
ts.recovered$Country.Region = as.character(ts.recovered$Country.Region)
ts.recovered$Country.Region[ts.recovered$Country.Region == "US"] = "USA"
ts.recovered$Country.Region[ts.recovered$Country.Region == "United Kingdom"] = "UK"
ts.recovered$Country.Region[ts.recovered$Country.Region == "Korea, South"] = "South Korea"
ts.recovered$Country.Region[ts.recovered$Country.Region == "Taiwan*"] = "Taiwan"
ts.recovered =  ts.recovered %>%
  group_by(Country.Region) %>%
  select(-c(Province.State, Lat, Long)) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(.[[ncol(ts.recovered)-3]]))

# u.s. time-series
ts.us.confirmed = unlist(ts.confirmed[1, -1])
ts.us.confirmed = tibble("Date" = names(ts.us.confirmed), "Count" = ts.us.confirmed)
ts.us.confirmed$Date = as.Date(ts.us.confirmed$Date)
ts.us.confirmed$New = c(NA, diff(ts.us.confirmed$Count))

ts.us.deaths = unlist(ts.deaths[1, -1])
ts.us.deaths = tibble("Date" = names(ts.us.deaths), "Count" = ts.us.deaths)
ts.us.deaths$Date = as.Date(ts.us.deaths$Date)
ts.us.deaths$New = c(NA, diff(ts.us.deaths$Count))

ts.us.recovered = unlist(ts.recovered[1, -1])
ts.us.recovered = tibble("Date" = names(ts.us.recovered), "Count" = ts.us.recovered)
ts.us.recovered$Date = as.Date(ts.us.recovered$Date)
ts.us.recovered$New = c(NA, diff(ts.us.recovered$Count))

# us state time-series
nCov = load_nCov2019(lang = "en")
df.nCov = nCov$province
ts.us = df.nCov %>% filter(country == "United States") %>% 
  select("Date" = time, "State" = province, "Confirmed" = cum_confirm, "Deaths" = cum_dead)
ts.us$State = sapply(ts.us$State, Caps)
ts.us = ts.us %>% inner_join(state_population) %>%
  mutate(ConfirmedPerMillion = round(Confirmed/Population*1000000,2),
         DeathsPerMillion = round(Deaths/Population*1000000),2)

# symptoms
df.symptoms = readRDS("df_symptoms.rds")

# patient data
df.patients = readRDS("df_patients.rds")

newColor = "black"
countColor = "darkorange"

######## Shiny Server ########

server <- function(input, output, session) { 
  
  updateSelectizeInput(session, 'trend_country',
                       choices = ts.confirmed$Country.Region,
                       selected = "USA",
                       server = TRUE
  )
  
  updateSelectizeInput(session, 'forecast_country',
                       choices = ts.confirmed$Country.Region,
                       selected = "USA",
                       server = TRUE
  )
  
  withProgress(message = 'Status:', value = 0, {
    n <- 4
    incProgress (1/n, detail = paste("Retrieving live data (20%)"))
    Sys.sleep(1)
    incProgress (1/n, detail = paste("Retrieving live data (40%)"))
    Sys.sleep(1)
    incProgress (1/n, detail = paste("Retrieving live data (80%)"))
    Sys.sleep(1)
    incProgress (1/n, detail = paste("Finalizing (90%)"))
    Sys.sleep(1)
  })
  
  ####### overview-us #######
  ##### 1. value boxes #####
  output$us_confirmed <- renderValueBox({
    valueBox(
      comma(unlist(agg.world[1,2]), format = "d"), 
      paste0("Confirmed Cases ", "(+", comma(tail(ts.us.confirmed$New, 1), format = "d"),")"), 
      icon = icon("notes-medical"),
      color = "yellow"
    )
  })
  output$us_death <- renderValueBox({
    valueBox(
      comma(unlist(agg.world[1,3]), format = "d"), 
      paste0("Deaths ", "(+", comma(tail(ts.us.deaths$New, 1), format = "d"),")"), 
      icon = icon("user-minus"),
      color = "red"
    )
  })
  output$us_recovered <- renderValueBox({
    valueBox(
      comma(unlist(agg.world[1,4]), format = "d"), 
      paste0("Recovered ", "(+", comma(tail(ts.us.recovered$New, 1), format = "d"),")"),
      icon = icon("user-check"),
      color = "green"
    )
  })
  
  
  ##### 2. us_map #####
  us_map_plot = reactive({
    if(input$us_metric_selection == "Confirmed"){
      if(input$us_log & (!input$us_pop_log)) {
        ggplotly(
          ggplot(agg.us.map, aes(long, lat, group = group, fill = log(Confirmed), text = paste0(sapply(Province_State, Caps), "\n", "Confirmed Cases:", Confirmed))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else if(input$us_log & input$us_pop_log) {
        ggplotly(
          ggplot(agg.us.map, aes(long, lat, group = group, fill = log(ConfirmedPerMillion), text = paste0(sapply(Province_State, Caps), "\n", "Confirmed Cases/Million:", ConfirmedPerMillion))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else if((!input$us_log) & input$us_pop_log) {
        ggplotly(
          ggplot(agg.us.map, aes(long, lat, group = group, fill = ConfirmedPerMillion, text = paste0(sapply(Province_State, Caps), "\n", "Confirmed Cases/Million:", ConfirmedPerMillion))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else{
        ggplotly(
          ggplot(agg.us.map, aes(long, lat, group = group, fill = Confirmed, text = paste0(sapply(Province_State, Caps), "\n", "Confirmed Cases:", Confirmed))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
    }
    else {
      if(input$us_log & (!input$us_pop_log)) {
        ggplotly(
          ggplot(agg.us.map, aes(long, lat, group = group, fill = log(Deaths), text = paste0(sapply(Province_State, Caps), "\n", "Deaths:", Deaths))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else if(input$us_log & input$us_pop_log) {
        ggplotly(
          ggplot(agg.us.map, aes(long, lat, group = group, fill = log(DeathsPerMillion), text = paste0(sapply(Province_State, Caps), "\n", "Deaths/Million:", DeathsPerMillion))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else if((!input$us_log) & input$us_pop_log) {
        ggplotly(
          ggplot(agg.us.map, aes(long, lat, group = group, fill = DeathsPerMillion, text = paste0(sapply(Province_State, Caps), "\n", "Deaths/Million:", DeathsPerMillion))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else{
        ggplotly(
          ggplot(agg.us.map, aes(long, lat, group = group, fill = Deaths, text = paste0(sapply(Province_State, Caps), "\n", "Deaths:", Deaths))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
    }
  })
  
  output$us_map <- renderPlotly({
    us_map_plot()
  })
  
  ##### 3. us_line_plot ##### 
  us_line_plot = reactive({
    if(input$us_metric_selection == "Confirmed"){
      if(input$us_log & input$us_pop_log) {
        top10_states = ts.us %>% group_by(State) %>% summarise(max = max(ConfirmedPerMillion)) %>% 
          arrange(desc(max)) %>% select(State)
        top10_states = top10_states$State[1:10]
        ts.us$State = factor(ts.us$State, top10_states)
        
        ggplotly(
          ggplot(ts.us %>% filter(State %in% top10_states), aes(Date, log(ConfirmedPerMillion), group = State, color = State,
                                                                text = paste0(State, "\n", "Total Cases/Million:", ConfirmedPerMillion))) +
            geom_line(alpha = 0.85) +
            ggtitle("U.S. Total Confirmed Cases/Million (Top 10)") +
            xlab("Date") +
            ylab("Total Confirmed Cases/Million (Log Scale)"), tooltip = c("text")
        )
      }
      else if(input$us_log & (!input$us_pop_log)) {
        top10_states = ts.us %>% group_by(State) %>% summarise(max = max(Confirmed)) %>% 
          arrange(desc(max)) %>% select(State)
        top10_states = top10_states$State[1:10]
        ts.us$State = factor(ts.us$State, top10_states)
        
        ggplotly(
          ggplot(ts.us %>% filter(State %in% top10_states), aes(Date, log(Confirmed), group = State, color = State,
                                                                text = paste0(State, "\n", "Total Cases:", Confirmed))) +
            geom_line(alpha = 0.85) +
            ggtitle("U.S. Total Confirmed Cases (Top 10)") +
            xlab("Date") +
            ylab("Total Confirmed Cases (Log Scale)"), tooltip = c("text")
        )
      }
      else if((!input$us_log) & input$us_pop_log) {
        top10_states = ts.us %>% group_by(State) %>% summarise(max = max(ConfirmedPerMillion)) %>% 
          arrange(desc(max)) %>% select(State)
        top10_states = top10_states$State[1:10]
        ts.us$State = factor(ts.us$State, top10_states)
        
        ggplotly(
          ggplot(ts.us %>% filter(State %in% top10_states), aes(Date, ConfirmedPerMillion, group = State, color = State,
                                                                text = paste0(State, "\n", "Total Cases/Million:", ConfirmedPerMillion))) +
            geom_line(alpha = 0.85) +
            ggtitle("U.S. Total Confirmed Cases/Million (Top 10)") +
            xlab("Date") +
            ylab("Total Confirmed Cases/Million"), tooltip = c("text")
        )
      }
      else{
        top10_states = ts.us %>% group_by(State) %>% summarise(max = max(Confirmed)) %>% 
          arrange(desc(max)) %>% select(State)
        top10_states = top10_states$State[1:10]
        ts.us$State = factor(ts.us$State, top10_states)
        
        ggplotly(
          ggplot(ts.us %>% filter(State %in% top10_states), aes(Date, Confirmed, group = State, color = State,
                                                                text = paste0(State, "\n", "Total Cases:", Confirmed))) +
            geom_line(alpha = 0.85) +
            ggtitle("U.S. Total Confirmed Cases (Top 10)") +
            xlab("Date") +
            ylab("Total Confirmed Cases"), tooltip = c("text")
        )
      }
    }
    else {
      if(input$us_log & input$us_pop_log) {
        top10_states = ts.us %>% group_by(State) %>% summarise(max = max(DeathsPerMillion)) %>% 
          arrange(desc(max)) %>% select(State)
        top10_states = top10_states$State[1:10]
        ts.us$State = factor(ts.us$State, top10_states)
        
        ggplotly(
          ggplot(ts.us %>% filter(State %in% top10_states), aes(Date, log(DeathsPerMillion), group = State, color = State,
                                                                text = paste0(State, "\n", "Total Deaths/Million:", DeathsPerMillion))) +
            geom_line(alpha = 0.85) +
            ggtitle("U.S. Total Deaths/Million (Top 10)") +
            xlab("Date") +
            ylab("Total Deaths/Million (Log Scale)"), tooltip = c("text")
        )
      }
      else if(input$us_log & (!input$us_pop_log)) {
        top10_states = ts.us %>% group_by(State) %>% summarise(max = max(Deaths)) %>% 
          arrange(desc(max)) %>% select(State)
        top10_states = top10_states$State[1:10]
        ts.us$State = factor(ts.us$State, top10_states)
        
        ggplotly(
          ggplot(ts.us %>% filter(State %in% top10_states), aes(Date, log(Deaths), group = State, color = State,
                                                                text = paste0(State, "\n", "Total Deaths:", Deaths))) +
            geom_line(alpha = 0.85) +
            ggtitle("U.S. Total Deaths (Top 10)") +
            xlab("Date") +
            ylab("Total Deaths (Log Scale)"), tooltip = c("text")
        )
      }
      else if((!input$us_log) & input$us_pop_log) {
        top10_states = ts.us %>% group_by(State) %>% summarise(max = max(DeathsPerMillion)) %>% 
          arrange(desc(max)) %>% select(State)
        top10_states = top10_states$State[1:10]
        ts.us$State = factor(ts.us$State, top10_states)
        
        ggplotly(
          ggplot(ts.us %>% filter(State %in% top10_states), aes(Date, DeathsPerMillion, group = State, color = State,
                                                                text = paste0(State, "\n", "Total Deaths/Million:", DeathsPerMillion))) +
            geom_line(alpha = 0.85) +
            ggtitle("U.S. Total Deaths/Million (Top 10)") +
            xlab("Date") +
            ylab("Total Deaths/Million"), tooltip = c("text")
        )
      }
      else{
        top10_states = ts.us %>% group_by(State) %>% summarise(max = max(Deaths)) %>% 
          arrange(desc(max)) %>% select(State)
        top10_states = top10_states$State[1:10]
        ts.us$State = factor(ts.us$State, top10_states)
        
        ggplotly(
          ggplot(ts.us %>% filter(State %in% top10_states), aes(Date, Deaths, group = State, color = State,
                                                                text = paste0(State, "\n", "Deaths:", Deaths))) +
            geom_line(alpha = 0.85) +
            ggtitle("U.S. Total Deaths (Top 10)") +
            xlab("Date") +
            ylab("Total Deaths"), tooltip = c("text")
        )
      }
    }
  })
  
  output$us_line_plot <- renderPlotly({
    us_line_plot()
  })
  ##### 4. us_table #####
  output$us_table <- ({
    DT::renderDataTable(agg.us.table, options = list(autoWidth = FALSE))
  })
  
  
  ####### overview-world #######
  ##### 1. value boxes #####
  output$world_confirmed <- renderValueBox({
    valueBox(
      comma(sum(agg.world$Confirmed), format = "d"), 
      paste0("Confirmed Cases ", "(+", 
             comma(sum(ts.confirmed[,ncol(ts.confirmed)]) - sum(ts.confirmed[,ncol(ts.confirmed)-1]), format = "d"),")"),
      icon = icon("notes-medical"),
      color = "yellow"
    )
  })
  output$world_death <- renderValueBox({
    valueBox(
      comma(sum(agg.world$Deaths), format = "d"), 
      paste0("Deaths ", "(+", 
             comma(sum(ts.deaths[,ncol(ts.deaths)]) - sum(ts.deaths[,ncol(ts.deaths)-1]), format = "d"),")"),
      icon = icon("user-minus"),
      color = "red"
    )
  })
  output$world_recovered <- renderValueBox({
    valueBox(
      comma(sum(agg.world$Recovered), format = "d"), 
      paste0("Recovered ", "(+", 
             comma(sum(ts.recovered[,ncol(ts.recovered)]) - sum(ts.recovered[,ncol(ts.recovered)-1]), format = "d"),")"),
      icon = icon("user-check"),
      color = "green"
    )
  })
  
  ##### 2. world_map #####
  world_map_plot = reactive({
    if(input$world_metric_selection == "Confirmed"){
      if(input$world_log & input$world_pop_log) {
        ggplotly(
          ggplot(agg.world.map, aes(long, lat, group = group, fill = log(ConfirmedPerMillion), text = paste0(sapply(Country_Region, Caps), "\n", "Confirmed Cases/Million:", ConfirmedPerMillion))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else if(input$world_log & (!input$world_pop_log)){
        ggplotly(
          ggplot(agg.world.map, aes(long, lat, group = group, fill = log(Confirmed), text = paste0(sapply(Country_Region, Caps), "\n", "Confirmed Cases:", Confirmed))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else if((!input$world_log) & input$world_pop_log){
        ggplotly(
          ggplot(agg.world.map, aes(long, lat, group = group, fill = ConfirmedPerMillion, text = paste0(sapply(Country_Region, Caps), "\n", "Confirmed Cases/Million:", ConfirmedPerMillion))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else{
        ggplotly(
          ggplot(agg.world.map, aes(long, lat, group = group, fill = Confirmed, text = paste0(sapply(Country_Region, Caps), "\n", "Confirmed Cases:", Confirmed))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
    }
    else {
      if(input$world_log & input$world_pop_log) {
        ggplotly(
          ggplot(agg.world.map, aes(long, lat, group = group, fill = log(DeathsPerMillion), text = paste0(sapply(Country_Region, Caps), "\n", "Deaths/Million:", DeathsPerMillion))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else if(input$world_log & (!input$world_pop_log)) {
        ggplotly(
          ggplot(agg.world.map, aes(long, lat, group = group, fill = log(Deaths), text = paste0(sapply(Country_Region, Caps), "\n", "Deaths:", Deaths))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else if((!input$world_log) & input$world_pop_log) {
        ggplotly(
          ggplot(agg.world.map, aes(long, lat, group = group, fill = DeathsPerMillion, text = paste0(sapply(Country_Region, Caps), "\n", "Deaths/Million:", DeathsPerMillion))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else{
        ggplotly(
          ggplot(agg.world.map, aes(long, lat, group = group, fill = Deaths, text = paste0(sapply(Country_Region, Caps), "\n", "Deaths:", Deaths))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
    }
  })
  
  output$world_map <- renderPlotly({
    world_map_plot()
  })
  
  ##### 3. world_line_plot ##### 
  world_line_plot = reactive({
    if(input$world_metric_selection == "Confirmed"){
      if(input$world_log & input$world_pop_log) {
        ggplotly(
          ggplot(df.global.million.ts, aes(Day, log(Count), group = Country, color = Country,
                                           text = paste0(Country, "\n", "Total Cases/Million:", Count))) +
            geom_line() +
            ggtitle(label = "Global Total Confirmed Cases Per Million (Top 10)") +
            xlab("Days After 50 Cases/Million") +
            ylab("Total Confirmed Cases/Million (Log Scale)"), tooltip = c("text")
        )
      }
      else if(input$world_log & (!input$world_pop_log)) {
        ggplotly(
          ggplot(df.global.ts, aes(Day, log(Count), group = Country, color = Country,
                                   text = paste0(Country, "\n", "Total Cases:", Count))) +
            geom_line() +
            ggtitle(label = "Global Total Confirmed Cases (Top 10)") +
            xlab("Days After 1000 Cases") +
            ylab("Total Confirmed Cases (Log Scale)"), tooltip = c("text")
        )
      }
      else if((!input$world_log) & input$world_pop_log) {
        ggplotly(
          ggplot(df.global.million.ts, aes(Day, Count, group = Country, color = Country,
                                   text = paste0(Country, "\n", "Total Cases/Million:", Count))) +
            geom_line() +
            ggtitle("Global Total Confirmed Cases (Top 10)") +
            xlab("Days After 50 Cases/Million") +
            ylab("Total Confirmed Cases/Million"), tooltip = c("text")
        )
      }
      else{
        ggplotly(
          ggplot(df.global.ts, aes(Day, Count, group = Country, color = Country,
                                  text = paste0(Country, "\n", "Total Cases:", Count))) +
            geom_line() +
            ggtitle("Global Total Confirmed Cases (Top 10)") +
            xlab("Days After 1000 Cases") +
            ylab("Total Confirmed Cases"), tooltip = c("text")
        )
      }
    }
    else {
      if(input$world_log & input$world_pop_log) {
        ggplotly(
          ggplot(df.global.ts.deaths.million, aes(Day, log(Count), group = Country, color = Country,
                                          text = paste0(Country, "\n", "Total Deaths/Million:", Count))) +
            geom_line() +
            ggtitle("Global Total  Deaths/Million (Top 10)") +
            xlab("Days After 50 Deaths/Million") +
            ylab("Total  Deaths/Million (Log Scale)"), tooltip = c("text")
        )
      }
      else if(input$world_log & (!input$world_pop_log)) {
        ggplotly(
          ggplot(df.global.ts.deaths, aes(Day, log(Count), group = Country, color = Country,
                                          text = paste0(Country, "\n", "Total Deaths:", Count))) +
            geom_line() +
            ggtitle("Global Total  Deaths (Top 10)") +
            xlab("Days After 1000 Deaths") +
            ylab("Total  Deaths (Log Scale)"), tooltip = c("text")
        )
      }
      else if((!input$world_log) & input$world_pop_log) {
        ggplotly(
          ggplot(df.global.ts.deaths.million, aes(Day, Count, group = Country, color = Country,
                                          text = paste0(Country, "\n", "Total Deaths/Million:", Count))) +
            geom_line() +
            ggtitle("Global Total  Deaths/Million (Top 10)") +
            xlab("Days After 50 Deaths/Million") +
            ylab("Total  Deaths/Million"), tooltip = c("text")
        )
      }
      else{
        ggplotly(
          ggplot(df.global.ts.deaths, aes(Day, Count, group = Country, color = Country,
                                          text = paste0(Country, "\n", "Total Deaths:", Count))) +
            geom_line() +
            ggtitle("Global Total  Deaths (Top 10)") +
            xlab("Days After 1000 Deaths") +
            ylab("Total  Deaths"), tooltip = c("text")
        )
      }
    }
  })
  
  output$world_line_plot <- renderPlotly({
    world_line_plot()
  })
  
  
  ##### 4. world_table #####
  output$world_table <- ({
    DT::renderDataTable(agg.world.table, options = list(autoWidth = FALSE))
  })
  
  
  
  ####### trend_plot #######
  trend_data = reactive({
    if(input$trend_metric_selection == "confirmed"){
      df = ts.confirmed %>% filter(Country.Region == input$trend_country)
      ts.country = unlist(df[, -1])
      ts.country = tibble("Date" = names(ts.country), "Count" = ts.country)
      ts.country$Date = as.Date(ts.country$Date)
      ts.country$New = c(NA, diff(ts.country$Count))
      ts.country$New = abs(ts.country$New)
      
      auto.arima.model = auto.arima(tail(ts.country$New, 20), seasonal = TRUE)
      pred = forecast(auto.arima.model, h = 5)

      Date = seq((tail(ts.country$Date, 1)), (tail(ts.country$Date, 1) + 5), by = 1)
      Prediction = c(tail(ts.country$New, 1), pred$mean)
      Upperbound = c(tail(ts.country$New, 1), pred$lower[,1])
      Lowerbound = c(tail(ts.country$New, 1), pred$upper[,1])
      df.pred = tibble(Date, Prediction, Upperbound, Lowerbound)
      
      list(ts.country, df.pred)
    }
    else if(input$trend_metric_selection == "deaths"){
      df = ts.deaths %>% filter(Country.Region == input$trend_country)
      ts.country = unlist(df[, -1])
      ts.country = tibble("Date" = names(ts.country), "Count" = ts.country)
      ts.country$Date = as.Date(ts.country$Date)
      ts.country$New = c(NA, diff(ts.country$Count))
      ts.country$New = abs(ts.country$New)
      
      auto.arima.model = auto.arima(tail(ts.country$New, 20), seasonal = TRUE)
      pred = forecast(auto.arima.model, h = 5)
      
      Date = seq((tail(ts.country$Date, 1)), (tail(ts.country$Date, 1) + 5), by = 1)
      Prediction = c(tail(ts.country$New, 1), pred$mean)
      Upperbound = c(tail(ts.country$New, 1), pred$lower[,1])
      Lowerbound = c(tail(ts.country$New, 1), pred$upper[,1])
      df.pred = tibble(Date, Prediction, Upperbound, Lowerbound)
      
      list(ts.country, df.pred)
    }
    else{
      df = ts.recovered %>% filter(Country.Region == input$trend_country)
      ts.country = unlist(df[, -1])
      ts.country = tibble("Date" = names(ts.country), "Count" = ts.country)
      ts.country$Date = as.Date(ts.country$Date)
      ts.country$New = c(NA, diff(ts.country$Count))
      ts.country$New = abs(ts.country$New)
      
      auto.arima.model = auto.arima(tail(ts.country$New, 15), seasonal = TRUE)
      pred = forecast(auto.arima.model, h = 5)
      
      Date = seq((tail(ts.country$Date, 1)), (tail(ts.country$Date, 1) + 5), by = 1)
      Prediction = c(tail(ts.country$New, 1), pred$mean)
      Upperbound = c(tail(ts.country$New, 1), pred$lower[,1])
      Lowerbound = c(tail(ts.country$New, 1), pred$upper[,1])
      df.pred = tibble(Date, Prediction, Upperbound, Lowerbound)
      
      list(ts.country, df.pred)
    }
  })
  
  output$trend_plot <- renderPlot({
    ts.country = trend_data()[[1]]
    df.pred = trend_data()[[2]]
    coeff = tail(ts.country$Count, 1)/tail(ts.country$New, 1)
    if(coeff == Inf){
      coeff = 100
    }
    ggplot(ts.country, aes(x=Date)) +
      geom_line(aes(y=New), color=newColor, alpha = 0.3) +
      geom_ma(aes(y=New), ma_fun = SMA, n = input$moving_n, color=newColor, linetype = "longdash") +
      geom_bar(aes(y=Count/coeff), stat="identity", fill=countColor, color=countColor, alpha=.2) + 
      geom_line(data = df.pred, aes(Date, Prediction), alpha = 0.3, linetype = "dashed") +
      geom_ribbon(data = df.pred, aes(x = Date, ymin = Lowerbound, ymax = Upperbound), alpha = 0.2,  fill = "darkorange") +
      scale_y_continuous(
        # Features of the first axis
        name = "Number of New Cases",
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="Total Cases")
      ) + 
      theme(
        axis.title.y = element_text(color = newColor, size=11),
        axis.title.y.right = element_text(color = countColor, size=11)
      )
  })

  
  ####### symptoms_plot #######
  output$symptoms_plot <- renderPlotly({
    ggplotly(ggplot(df.symptoms, aes(symptom, normalize(value)*10, fill = normalize(value)*10, text = paste0(symptom,"\n","Prevalence Level:", round(normalize(value)*10, 2)))) +
               geom_bar(stat = "identity", alpha = 0.85) +
               ylab(paste("Relative Prevalence Level", "(Note: * symptoms are risk factors that could potentially increase one's likelihood of dying from COVID-19)", sep = "\n")) +
               xlab("Symptom") +
               ylim(0, 10) +
               theme(legend.position = "none") +
               scale_fill_gradient(low="antiquewhite", high="darkorange") + 
               coord_flip(), tooltip = c("text"), height = 650)
      
  })
    
    ####### demographics #######
    
    age_boxplot = reactive({
      if(input$outcome_switch) {
        ggplotly(ggplot(data = df.patients %>% filter(!is.na(outcome)), aes(y = age, x = outcome, fill = outcome)) +
                   geom_boxplot(alpha = 0.85) + xlab("Outcome") + ylab("Age") + theme(legend.position = "none"))
      }else {
        ggplotly(ggplot(data = df.patients[!is.na(df.patients$age),], aes(x = "age", y = age)) +
                   geom_boxplot(fill = "darkorange", alpha = 0.85) +
                   xlab(NULL) +
                   ylab("Age") +
                   coord_flip())
      }
    })
  
  density_boxplot = reactive({
    if(input$outcome_switch) {
      ggplotly(plot_multi_histogram(df.patients %>% filter(!is.na(outcome)), feature = "age", label_column = "outcome"), tooltip = c("text"))
    }else {
      ggplotly(ggplot(data = df.patients %>% filter(!is.na(age)), aes(age)) + 
                 geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), bins = 18, fill = "darkorange", color = "gray30") +
                 geom_density(alpha=0.7, color = "gray30") +
                 xlab("Age") +
                 ylab("Proportion")
      )
    }
  })
  
  gender_plot = reactive({
    if(input$outcome_switch) {
      ggplotly(ggplot(data = df.patients %>% filter(!is.na(outcome) & !is.na(sex)), aes(x = outcome)) +
                 geom_bar(aes(fill=sex), position="fill", alpha = 0.85) +
                 xlab("Gender") +
                 ylab("Proportion") +
                 theme(legend.position = "none"))
    }else {
      ggplotly(ggplot(data = df.patients %>% filter(!is.na(sex)), aes(x = sex, fill = sex, text = round(..count../sum(..count..), 2))) +
                 geom_bar(aes(y = ..count../sum(..count..)), alpha = 0.85) +
                 xlab("Gender") +
                 ylab("Proportion") +
                 theme(legend.position = "none") +
                 coord_flip()
               , tooltip = c("text"))
    }
  })
    
  output$age_boxplot <- renderPlotly({
    age_boxplot()
    })
  
  output$age_densityplot <- renderPlotly({
    density_boxplot()
  })
  
  output$gender_plot <- renderPlotly({
    gender_plot()
  })
  
}