library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
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

######## Helper Functions ########
options(scipen=10000)

Caps = function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

######## Data Preprocessing ########

agg.us = covid19.data("aggregated")
agg.us = agg.us %>%
  filter(Country_Region == "US" & Province_State != "Recovered") %>%
  select(Province_State, Confirmed, Deaths) %>%
  group_by(Province_State) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(Confirmed))
states_map = map_data("state")
agg.us.map = agg.us %>% mutate(Province_State = tolower(Province_State))
agg.us.map = inner_join(agg.us.map, states_map, by = c("Province_State" = "region"))


agg.world = covid19.data("aggregated")
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


ts.confirmed = covid19.data("ts-confirmed")
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


ts.deaths = covid19.data("ts-deaths")
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
  df.global.ts.deaths = rbind(df.global.ts, df.new)
}

nCov = load_nCov2019()
df.nCov = nCov$province
ts.us = df.nCov %>% filter(country == "United States") %>% 
  select("Date" = time, "State" = province, "Confirmed" = cum_confirm, "Deaths" = cum_dead)
ts.us$State = sapply(ts.us$State, Caps)
top10_states = ts.us %>% group_by(State) %>% summarise(max = max(Confirmed)) %>% 
  arrange(desc(max)) %>% select(State)
top10_states = top10_states$State[1:10]

coeff = NA
newColor = "black"
countColor = "darkorange"

######## Shiny Server ########

server <- function(input, output, session) { 
  
  updateSelectizeInput(session, 'trend_country',
                       choices = ts.confirmed$Country.Region,
                       selected="US",
                       server = TRUE
  )
  
  
  ####### overview-us #######
  ##### 1. value boxes #####
  output$us_confirmed <- renderValueBox({
    valueBox(
      comma(unlist(agg.world[1,2]), format = "d"), "Confirmed Cases", icon = icon("notes-medical"),
      color = "yellow"
    )
  })
  output$us_death <- renderValueBox({
    valueBox(
      comma(unlist(agg.world[1,3]), format = "d"), "Deaths", icon = icon("user-minus"),
      color = "red"
    )
  })
  output$us_recovered <- renderValueBox({
    valueBox(
      comma(unlist(agg.world[1,4]), format = "d"), "Recovered", icon = icon("user-check"),
      color = "green"
    )
  })
  
  ##### 2. us_map #####
  us_map_plot = reactive({
    if(input$us_metric_selection == "Confirmed"){
      if(input$us_log) {
        ggplotly(
          ggplot(agg.us.map, aes(long, lat, group = group, fill = log(Confirmed), text = paste0(sapply(Province_State, Caps), "\n", "Confirmed Cases:", Confirmed))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }else{
        ggplotly(
          ggplot(agg.us.map, aes(long, lat, group = group, fill = Confirmed, text = paste0(sapply(Province_State, Caps), "\n", "Confirmed Cases:", Confirmed))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
    }
    else {
      if(input$us_log) {
        ggplotly(
          ggplot(agg.us.map, aes(long, lat, group = group, fill = log(Deaths), text = paste0(sapply(Province_State, Caps), "\n", "Deaths:", Deaths))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }else{
        ggplotly(
          ggplot(agg.us.map, aes(long, lat, group = group, fill = Deaths, text = paste0(sapply(Province_State, Caps), "\n", "Deaths:", Deaths))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
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
      if(input$us_log) {
        ggplotly(
          ggplot(ts.us %>% filter(State %in% top10_states), aes(Date, log(Confirmed), group = State, color = State,
                                                                text = paste0(State, "\n", "Total Cases:", Confirmed))) +
            geom_line() +
            ggtitle("U.S. Total Confirmed Cases (Top 10 States)") +
            xlab("Date") +
            ylab("Total Confirmed Cases (Log Scale)"), tooltip = c("text")
        )
      }else{
        ggplotly(
          ggplot(ts.us %>% filter(State %in% top10_states), aes(Date, Confirmed, group = State, color = State,
                                                                text = paste0(State, "\n", "Total Cases:", Confirmed))) +
            geom_line() +
            ggtitle("U.S. Total Confirmed Cases (Top 10 States)") +
            xlab("Date") +
            ylab("Total Confirmed Cases"), tooltip = c("text")
        )
      }
    }
    else {
      if(input$us_log) {
        ggplotly(
          ggplot(ts.us %>% filter(State %in% top10_states), aes(Date, log(Deaths), group = State, color = State,
                                                                text = paste0(State, "\n", "Deaths:", Deaths))) +
            geom_line() +
            ggtitle("U.S. Total Deaths (Top 10 States)") +
            xlab("Date") +
            ylab("Total Deaths (Log Scale)"), tooltip = c("text")
        )
      }else{
        ggplotly(
          ggplot(ts.us %>% filter(State %in% top10_states), aes(Date, Deaths, group = State, color = State,
                                                                text = paste0(State, "\n", "Deaths:", Deaths))) +
            geom_line() +
            ggtitle("U.S. Total Deaths (Top 10 States)") +
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
    DT::renderDataTable(agg.us, options = list(autoWidth = FALSE))
  })
  
  
  ####### overview-world #######
  ##### 1. value boxes #####
  output$world_confirmed <- renderValueBox({
    valueBox(
      comma(sum(agg.world$Confirmed), format = "d"), "Confirmed Cases", icon = icon("notes-medical"),
      color = "yellow"
    )
  })
  output$world_death <- renderValueBox({
    valueBox(
      comma(sum(agg.world$Deaths), format = "d"), "Deaths", icon = icon("user-minus"),
      color = "red"
    )
  })
  output$world_recovered <- renderValueBox({
    valueBox(
      comma(sum(agg.world$Recovered), format = "d"), "Recovered", icon = icon("user-check"),
      color = "green"
    )
  })
  
  ##### 2. world_map #####
  world_map_plot = reactive({
    if(input$world_metric_selection == "Confirmed"){
      if(input$world_log) {
        ggplotly(
          ggplot(agg.world.map, aes(long, lat, group = group, fill = log(Confirmed), text = paste0(sapply(Country_Region, Caps), "\n", "Confirmed Cases:", Confirmed))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }else{
        ggplotly(
          ggplot(agg.world.map, aes(long, lat, group = group, fill = Confirmed, text = paste0(sapply(Country_Region, Caps), "\n", "Confirmed Cases:", Confirmed))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
    }
    else {
      if(input$world_log) {
        ggplotly(
          ggplot(agg.world.map, aes(long, lat, group = group, fill = log(Deaths), text = paste0(sapply(Country_Region, Caps), "\n", "Deaths:", Deaths))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }else{
        ggplotly(
          ggplot(agg.world.map, aes(long, lat, group = group, fill = Deaths, text = paste0(sapply(Country_Region, Caps), "\n", "Deaths:", Deaths))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
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
      if(input$world_log) {
        ggplotly(
          ggplot(df.global.ts, aes(Day, log(Count), group = Country, color = Country,
                                   text = paste0(Country, "\n", "Total Cases:", Count))) +
            geom_line() +
            ggtitle("Global Total Confirmed Cases (Top 10 Countries)") +
            xlab("Days After 1000 Cases") +
            ylab("Total Confirmed Cases (Log Scale)"), tooltip = c("text")
        )
      }else{
        ggplotly(
          ggplot(df.global.ts, aes(Day, Count, group = Country, color = Country,
                                  text = paste0(Country, "\n", "Total Cases:", Count))) +
            geom_line() +
            ggtitle("Global Total Confirmed Cases (Top 10 Countries)") +
            xlab("Days After 1000 Cases") +
            ylab("Total Confirmed Cases"), tooltip = c("text")
        )
      }
    }
    else {
      if(input$world_log) {
        ggplotly(
          ggplot(df.global.ts.deaths, aes(Day, log(Count), group = Country, color = Country,
                                          text = paste0(Country, "\n", "Total Deaths:", Count))) +
            geom_line() +
            ggtitle("Global Total  Deaths (Top 10 Countries)") +
            xlab("Days After 1000 Deaths") +
            ylab("Total  Deaths (Log Scale)"), tooltip = c("text")
        )
      }else{
        ggplotly(
          ggplot(df.global.ts.deaths, aes(Day, Count, group = Country, color = Country,
                                          text = paste0(Country, "\n", "Total Deaths:", Count))) +
            geom_line() +
            ggtitle("Global Total  Deaths (Top 10 Countries)") +
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
    DT::renderDataTable(agg.world, options = list(autoWidth = FALSE))
  })
  ####### trend_plot #######
  trend_data = reactive({
    df = ts.confirmed %>% filter(Country.Region == input$trend_country)
    ts.country.confirmed = unlist(df[, -1])
    ts.country.confirmed = tibble("Date" = names(ts.country.confirmed), "Count" = ts.country.confirmed)
    ts.country.confirmed$Date = as.Date(ts.country.confirmed$Date)
    ts.country.confirmed$New = c(NA, diff(ts.country.confirmed$Count))
    ts.country.confirmed
  })
  
  
  output$trend_plot <- renderPlot({
    ts.country.confirmed = trend_data()
    coeff = tail(ts.country.confirmed$Count, 1)/tail(ts.country.confirmed$New, 1)
    ggplot(ts.country.confirmed, aes(x=Date)) +
      geom_line(aes(y=New), color=newColor, alpha = 0.3) +
      geom_ma(aes(y=New), ma_fun = SMA, n = input$moving_n, color=newColor) +
      geom_bar(aes(y=Count/coeff), stat="identity", fill=countColor, color=countColor, alpha=.2) + 
      
      scale_y_continuous(
        
        # Features of the first axis
        name = "Number of New Cases",
        
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="Total Confirmed Cases")
      ) + 
      
      
      theme(
        axis.title.y = element_text(color = newColor, size=11),
        axis.title.y.right = element_text(color = countColor, size=11)
      )
  })
  
  ####### demographics #######
  
}