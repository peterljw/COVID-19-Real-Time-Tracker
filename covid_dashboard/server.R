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
library(aws.s3)
Sys.setenv(TZ = "America/Los_Angeles")

# source("../data_ingestion/data_ingest.R")

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


######## 1.Data Preprocessing ########

country_population <- readRDS("country_population.rds")
state_population = readRDS("state_population.rds")
df.symptoms = readRDS("df_symptoms.rds")
df.patients = readRDS("df_patients.rds")
newColor = "black"
countColor = "darkorange"

s3BucketName <- "peter-covid-dashboard-data"
Sys.setenv("AWS_ACCESS_KEY_ID" = "",
           "AWS_SECRET_ACCESS_KEY" = "",
           "AWS_DEFAULT_REGION" = "us-east-2")

ts.country.confirmed = s3readRDS(object = "ts.country.confirmed.rds", bucket = s3BucketName)
df.global.ts = s3readRDS(object = "df.global.ts.rds", bucket = s3BucketName)
df.global.million.ts = s3readRDS(object = "df.global.million.ts.rds", bucket = s3BucketName)

ts.country.deaths = s3readRDS(object = "ts.country.deaths.rds", bucket = s3BucketName)
df.global.ts.country.deaths = s3readRDS(object = "df.global.ts.country.deaths.rds", bucket = s3BucketName)
df.global.ts.country.deaths.million = s3readRDS(object = "df.global.ts.country.deaths.million.rds", bucket = s3BucketName)

ts.country.recovered = s3readRDS(object = "ts.country.recovered.rds", bucket = s3BucketName)

ts.us.confirmed = s3readRDS(object = "ts.us.confirmed.rds", bucket = s3BucketName)
df.us.ts = s3readRDS(object = "df.us.ts.rds", bucket = s3BucketName)
df.us.million.ts = s3readRDS(object = "df.us.million.ts.rds", bucket = s3BucketName)

ts.us.deaths = s3readRDS(object = "ts.us.deaths.rds", bucket = s3BucketName)
df.us.ts.deaths = s3readRDS(object = "df.us.ts.deaths.rds", bucket = s3BucketName)
df.us.million.ts.deaths = s3readRDS(object = "df.us.million.ts.deaths.rds", bucket = s3BucketName)

agg.country.map = s3readRDS(object = "agg.country.map.rds", bucket = s3BucketName)
agg.country.table = s3readRDS(object = "agg.country.table.rds", bucket = s3BucketName)
agg.us.map = s3readRDS(object = "agg.us.map.rds", bucket = s3BucketName)
agg.us.table = s3readRDS(object = "agg.us.table.rds", bucket = s3BucketName)

ts.country.all = s3readRDS(object = "ts.country.all.rds", bucket = s3BucketName)
ts.us.all = s3readRDS(object = "ts.us.all.rds", bucket = s3BucketName)


######## Shiny Server ########

server <- function(input, output, session) { 
  
  updateSelectizeInput(session, 'trend_state',
                       choices = ts.us.confirmed$Province_State,
                       selected = "California",
                       server = TRUE
  )
  
  updateSelectizeInput(session, 'trend_country',
                       choices = ts.country.confirmed$Country.Region,
                       selected = "USA",
                       server = TRUE
  )
  
  updateSelectizeInput(session, 'forecast_country',
                       choices = ts.country.confirmed$Country.Region,
                       selected = "USA",
                       server = TRUE
  )
  
  withProgress(message = 'Status:', value = 0, {
    n <- 2
    incProgress (1/n, detail = paste("Retrieving live data (50%)"))
    Sys.sleep(1.2)
    incProgress (1/n, detail = paste("Retrieving live data (100%)"))
    Sys.sleep(1.2)
  })

  ####### overview-us #######
  ##### 1. value boxes #####
  output$us_confirmed <- renderValueBox({
    valueBox(
      comma(unlist(tail(ts.us.all,1)[1]), format = "d"), 
      paste0("Confirmed Cases ", "(+", comma(unlist((tail(ts.us.all,1) - tail(ts.us.all,2)[1,])[1]), format = "d"),")"),
      icon = icon("notes-medical"),
      color = "yellow"
    )
  })
  output$us_death <- renderValueBox({
    valueBox(
      comma(unlist(tail(ts.us.all,1)[2]), format = "d"), 
      paste0("Deaths ", "(+", comma(unlist((tail(ts.us.all,1) - tail(ts.us.all,2)[1,])[2]), format = "d"),")"), 
      icon = icon("user-slash"),
      color = "red"
    )
  })
  output$us_recovered <- renderValueBox({
    valueBox(
      "N/A",
      paste0("Recovered ", "(+", comma(unlist((tail(ts.us.all,1) - tail(ts.us.all,2)[1,])[3]), format = "d"),")"),
      icon = icon("user-check"),
      color = "green"
    )
  })
  output$us_active <- renderValueBox({
    valueBox(
      "N/A",
      paste0("Active ", "(+", comma(unlist((tail(ts.us.all,1) - tail(ts.us.all,2)[1,])[4]), format = "d"),")"),
      icon = icon("user-minus"),
      color = "orange"
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
        ggplotly(
          ggplot(df.us.million.ts, aes(Day, log(Count), group = State, color = State,
                                           text = paste0(State, "\n", "Total Cases/Million:", Count))) +
            geom_line() +
            ggtitle(label = "U.S. States Total Confirmed Cases Per Million (Top 10)") +
            xlab("Days After 10 Cases/Million") +
            ylab("Total Confirmed Cases/Million (Log Scale)"), tooltip = c("text")
        )
      }
      else if(input$us_log & (!input$us_pop_log)) {
        ggplotly(
          ggplot(df.us.ts, aes(Day, log(Count), group = State, color = State,
                                   text = paste0(State, "\n", "Total Cases:", Count))) +
            geom_line() +
            ggtitle(label = "U.S. States Total Confirmed Cases (Top 10)") +
            xlab("Days After 1000 Cases") +
            ylab("Total Confirmed Cases (Log Scale)"), tooltip = c("text")
        )
      }
      else if((!input$us_log) & input$us_pop_log) {
        ggplotly(
          ggplot(df.us.million.ts, aes(Day, Count, group = State, color = State,
                                           text = paste0(State, "\n", "Total Cases/Million:", Count))) +
            geom_line() +
            ggtitle("U.S. States Total Confirmed Cases (Top 10)") +
            xlab("Days After 50 Cases/Million") +
            ylab("Total Confirmed Cases/Million"), tooltip = c("text")
        )
      }
      else{
        ggplotly(
          ggplot(df.us.ts, aes(Day, Count, group = State, color = State,
                                   text = paste0(State, "\n", "Total Cases:", Count))) +
            geom_line() +
            ggtitle("U.S. States Total Confirmed Cases (Top 10)") +
            xlab("Days After 1000 Cases") +
            ylab("Total Confirmed Cases"), tooltip = c("text")
        )
      }
    }
    else {
      if(input$us_log & input$us_pop_log) {
        ggplotly(
          ggplot(df.us.million.ts.deaths, aes(Day, log(Count), group = State, color = State,
                                                          text = paste0(State, "\n", "Total Deaths/Million:", Count))) +
            geom_line() +
            ggtitle("U.S. States Total  Deaths/Million (Top 10)") +
            xlab("Days After 10 Deaths/Million") +
            ylab("Total Deaths/Million (Log Scale)"), tooltip = c("text")
        )
      }
      else if(input$us_log & (!input$us_pop_log)) {
        ggplotly(
          ggplot(df.us.ts.deaths, aes(Day, log(Count), group = State, color = State,
                                                  text = paste0(State, "\n", "Total Deaths:", Count))) +
            geom_line() +
            ggtitle("U.S. States Total  Deaths (Top 10)") +
            xlab("Days After 200 Deaths") +
            ylab("Total Deaths (Log Scale)"), tooltip = c("text")
        )
      }
      else if((!input$us_log) & input$us_pop_log) {
        ggplotly(
          ggplot(df.us.million.ts.deaths, aes(Day, Count, group = State, color = State,
                                                          text = paste0(State, "\n", "Total Deaths/Million:", Count))) +
            geom_line() +
            ggtitle("U.S. States Total Deaths/Million (Top 10)") +
            xlab("Days After 10 Deaths/Million") +
            ylab("Total Deaths/Million"), tooltip = c("text")
        )
      }
      else{
        ggplotly(
          ggplot(df.us.ts.deaths, aes(Day, Count, group = State, color = State,
                                                  text = paste0(State, "\n", "Total Deaths:", Count))) +
            geom_line() +
            ggtitle("U.S. States Total  Deaths (Top 10)") +
            xlab("Days After 200 Deaths") +
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
      comma(unlist(tail(ts.country.all,1)[1]), format = "d"), 
      paste0("Confirmed Cases ", "(+", 
             comma(unlist((tail(ts.country.all,1) - tail(ts.country.all,2)[1,])[1]), format = "d"),")"),
      icon = icon("notes-medical"),
      color = "yellow"
    )
  })
  output$world_death <- renderValueBox({
    valueBox(
      comma(unlist(tail(ts.country.all,1)[2]), format = "d"), 
      paste0("Deaths ", "(+", 
             comma(unlist((tail(ts.country.all,1) - tail(ts.country.all,2)[1,])[2]), format = "d"),")"),
      icon = icon("user-slash"),
      color = "red"
    )
  })
  output$world_recovered <- renderValueBox({
    valueBox(
      "N/A", 
      "Recovered",
      icon = icon("user-check"),
      color = "green"
    )
  })
  output$world_active <- renderValueBox({
    valueBox(
      "N/A", 
      "Active ",
      icon = icon("user-minus"),
      color = "orange"
    )
  })
  
  ##### 2. world_map #####
  world_map_plot = reactive({
    if(input$world_metric_selection == "Confirmed"){
      if(input$world_log & input$world_pop_log) {
        ggplotly(
          ggplot(agg.country.map, aes(long, lat, group = group, fill = log(ConfirmedPerMillion), text = paste0(sapply(Country_Region, Caps), "\n", "Confirmed Cases/Million:", ConfirmedPerMillion))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else if(input$world_log & (!input$world_pop_log)){
        ggplotly(
          ggplot(agg.country.map, aes(long, lat, group = group, fill = log(Confirmed), text = paste0(sapply(Country_Region, Caps), "\n", "Confirmed Cases:", Confirmed))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else if((!input$world_log) & input$world_pop_log){
        ggplotly(
          ggplot(agg.country.map, aes(long, lat, group = group, fill = ConfirmedPerMillion, text = paste0(sapply(Country_Region, Caps), "\n", "Confirmed Cases/Million:", ConfirmedPerMillion))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else{
        ggplotly(
          ggplot(agg.country.map, aes(long, lat, group = group, fill = Confirmed, text = paste0(sapply(Country_Region, Caps), "\n", "Confirmed Cases:", Confirmed))) +
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
          ggplot(agg.country.map, aes(long, lat, group = group, fill = log(DeathsPerMillion), text = paste0(sapply(Country_Region, Caps), "\n", "Deaths/Million:", DeathsPerMillion))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else if(input$world_log & (!input$world_pop_log)) {
        ggplotly(
          ggplot(agg.country.map, aes(long, lat, group = group, fill = log(Deaths), text = paste0(sapply(Country_Region, Caps), "\n", "Deaths:", Deaths))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else if((!input$world_log) & input$world_pop_log) {
        ggplotly(
          ggplot(agg.country.map, aes(long, lat, group = group, fill = DeathsPerMillion, text = paste0(sapply(Country_Region, Caps), "\n", "Deaths/Million:", DeathsPerMillion))) +
            scale_fill_gradient(low = "gray99", high = "coral3") +
            geom_polygon() +
            theme(legend.position = "none") +
            xlab("Longtitude") +
            ylab("Latitude"), tooltip = c("text")
        )
      }
      else{
        ggplotly(
          ggplot(agg.country.map, aes(long, lat, group = group, fill = Deaths, text = paste0(sapply(Country_Region, Caps), "\n", "Deaths:", Deaths))) +
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
            xlab("Days After 250 Cases/Million") +
            ylab("Total Confirmed Cases/Million (Log Scale)"), tooltip = c("text")
        )
      }
      else if(input$world_log & (!input$world_pop_log)) {
        ggplotly(
          ggplot(df.global.ts, aes(Day, log(Count), group = Country, color = Country,
                                   text = paste0(Country, "\n", "Total Cases:", Count))) +
            geom_line() +
            ggtitle(label = "Global Total Confirmed Cases (Top 10)") +
            xlab("Days After 5000 Cases") +
            ylab("Total Confirmed Cases (Log Scale)"), tooltip = c("text")
        )
      }
      else if((!input$world_log) & input$world_pop_log) {
        ggplotly(
          ggplot(df.global.million.ts, aes(Day, Count, group = Country, color = Country,
                                           text = paste0(Country, "\n", "Total Cases/Million:", Count))) +
            geom_line() +
            ggtitle("Global Total Confirmed Cases (Top 10)") +
            xlab("Days After 250 Cases/Million") +
            ylab("Total Confirmed Cases/Million"), tooltip = c("text")
        )
      }
      else{
        ggplotly(
          ggplot(df.global.ts, aes(Day, Count, group = Country, color = Country,
                                   text = paste0(Country, "\n", "Total Cases:", Count))) +
            geom_line() +
            ggtitle("Global Total Confirmed Cases (Top 10)") +
            xlab("Days After 5000 Cases") +
            ylab("Total Confirmed Cases"), tooltip = c("text")
        )
      }
    }
    else {
      if(input$world_log & input$world_pop_log) {
        ggplotly(
          ggplot(df.global.ts.country.deaths.million, aes(Day, log(Count), group = Country, color = Country,
                                                  text = paste0(Country, "\n", "Total Deaths/Million:", Count))) +
            geom_line() +
            ggtitle("Global Total  Deaths/Million (Top 10)") +
            xlab("Days After 50 Deaths/Million") +
            ylab("Total Deaths/Million (Log Scale)"), tooltip = c("text")
        )
      }
      else if(input$world_log & (!input$world_pop_log)) {
        ggplotly(
          ggplot(df.global.ts.country.deaths, aes(Day, log(Count), group = Country, color = Country,
                                          text = paste0(Country, "\n", "Total Deaths:", Count))) +
            geom_line() +
            ggtitle("Global Total  Deaths (Top 10)") +
            xlab("Days After 1000 Deaths") +
            ylab("Total Deaths (Log Scale)"), tooltip = c("text")
        )
      }
      else if((!input$world_log) & input$world_pop_log) {
        ggplotly(
          ggplot(df.global.ts.country.deaths.million, aes(Day, Count, group = Country, color = Country,
                                                  text = paste0(Country, "\n", "Total Deaths/Million:", Count))) +
            geom_line() +
            ggtitle("Global Total  Deaths/Million (Top 10)") +
            xlab("Days After 50 Deaths/Million") +
            ylab("Total Deaths/Million"), tooltip = c("text")
        )
      }
      else{
        ggplotly(
          ggplot(df.global.ts.country.deaths, aes(Day, Count, group = Country, color = Country,
                                          text = paste0(Country, "\n", "Total Deaths:", Count))) +
            geom_line() +
            ggtitle("Global Total  Deaths (Top 10)") +
            xlab("Days After 1000 Deaths") +
            ylab("Total Deaths"), tooltip = c("text")
        )
      }
    }
  })
  
  output$world_line_plot <- renderPlotly({
    world_line_plot()
  })
  
  
  ##### 4. world_table #####
  output$world_table <- ({
    DT::renderDataTable(agg.country.table, options = list(autoWidth = FALSE))
  })
  
  
  
  ####### trend_plot #######
  trend_data_us = reactive({
    if(input$trend_metric_selection_us == "confirmed"){
      df =  ts.us.confirmed %>% filter(Province_State == input$trend_state)
      ts.state = unlist(df[, -1])
      ts.state = tibble("Date" = names(ts.state), "Count" = ts.state)
      ts.state$Date = as.Date(ts.state$Date)
      ts.state$New = c(NA, diff(ts.state$Count))
      ts.state$New = abs(ts.state$New)
      ts.state$MA = rollmeanr(ts.state$New , k = 14, fill = NA)
      
      auto.arima.model = auto.arima(tail(ts.state$MA, 60), seasonal = TRUE)
      pred = forecast(auto.arima.model, h = 14)
      
      Date = seq((tail(ts.state$Date, 1)), (tail(ts.state$Date, 1) + 14), by = 1)
      Prediction = c(tail(ts.state$MA, 1), pred$mean)
      Upperbound = c(tail(ts.state$MA, 1), pred$lower[,1])
      Lowerbound = c(tail(ts.state$MA, 1), pred$upper[,1])
      df.pred = tibble(Date, Prediction, Upperbound, Lowerbound)
      
      list(ts.state, df.pred)
    }
    else{
      df =  ts.us.deaths %>% filter(Province_State == input$trend_state)
      ts.state = unlist(df[, -1])
      ts.state = tibble("Date" = names(ts.state), "Count" = ts.state)
      ts.state$Date = as.Date(ts.state$Date)
      ts.state$New = c(NA, diff(ts.state$Count))
      ts.state$New = abs(ts.state$New)
      ts.state$MA = rollmeanr(ts.state$New , k = 14, fill = NA)
      
      auto.arima.model = auto.arima(tail(ts.state$MA, 60), seasonal = TRUE)
      pred = forecast(auto.arima.model, h = 14)
      
      Date = seq((tail(ts.state$Date, 1)), (tail(ts.state$Date, 1) + 14), by = 1)
      Prediction = c(tail(ts.state$MA, 1), pred$mean)
      Upperbound = c(tail(ts.state$MA, 1), pred$lower[,1])
      Lowerbound = c(tail(ts.state$MA, 1), pred$upper[,1])
      df.pred = tibble(Date, Prediction, Upperbound, Lowerbound)
      
      list(ts.state, df.pred)
    }
  })
  
  trend_data_world = reactive({
    if(input$trend_metric_selection_world == "confirmed"){
      df =  ts.country.confirmed %>% filter(Country.Region == input$trend_country)
      ts.country = unlist(df[, -1])
      ts.country = tibble("Date" = names(ts.country), "Count" = ts.country)
      ts.country$Date = as.Date(ts.country$Date)
      ts.country$New = c(NA, diff(ts.country$Count))
      ts.country$New = abs(ts.country$New)
      ts.country$MA = rollmeanr(ts.country$New , k = 14, fill = NA)
      
      auto.arima.model = auto.arima(tail(ts.country$MA, 60))
      pred = forecast(auto.arima.model, h = 14)
      
      Date = seq((tail(ts.country$Date, 1)), (tail(ts.country$Date, 1) + 14), by = 1)
      Prediction = c(tail(ts.country$MA, 1), pred$mean)
      Upperbound = c(tail(ts.country$MA, 1), pred$lower[,1])
      Lowerbound = c(tail(ts.country$MA, 1), pred$upper[,1])
      df.pred = tibble(Date, Prediction, Upperbound, Lowerbound)
      
      list(ts.country, df.pred)
    }
    else if(input$trend_metric_selection_world == "deaths"){
      df =  ts.country.deaths %>% filter(Country.Region == input$trend_country)
      ts.country = unlist(df[, -1])
      ts.country = tibble("Date" = names(ts.country), "Count" = ts.country)
      ts.country$Date = as.Date(ts.country$Date)
      ts.country$New = c(NA, diff(ts.country$Count))
      ts.country$New = abs(ts.country$New)
      ts.country$MA = rollmeanr(ts.country$New , k = 14, fill = NA)
      
      auto.arima.model = auto.arima(tail(ts.country$MA, 60))
      pred = forecast(auto.arima.model, h = 14)
      
      Date = seq((tail(ts.country$Date, 1)), (tail(ts.country$Date, 1) + 14), by = 1)
      Prediction = c(tail(ts.country$MA, 1), pred$mean)
      Upperbound = c(tail(ts.country$MA, 1), pred$lower[,1])
      Lowerbound = c(tail(ts.country$MA, 1), pred$upper[,1])
      df.pred = tibble(Date, Prediction, Upperbound, Lowerbound)
      
      list(ts.country, df.pred)
    }
    else{
      df =  ts.country.recovered %>% filter(Country.Region == input$trend_country)
      ts.country = unlist(df[, -1])
      ts.country = tibble("Date" = names(ts.country), "Count" = ts.country)
      ts.country$Date = as.Date(ts.country$Date)
      ts.country$New = c(NA, diff(ts.country$Count))
      ts.country$New = abs(ts.country$New)
      ts.country$MA = rollmeanr(ts.country$New , k = 14, fill = NA)
      
      auto.arima.model = auto.arima(tail(ts.country$MA, 60))
      pred = forecast(auto.arima.model, h = 14)
      
      Date = seq((tail(ts.country$Date, 1)), (tail(ts.country$Date, 1) + 14), by = 1)
      Prediction = c(tail(ts.country$MA, 1), pred$mean)
      Upperbound = c(tail(ts.country$MA, 1), pred$lower[,1])
      Lowerbound = c(tail(ts.country$MA, 1), pred$upper[,1])
      df.pred = tibble(Date, Prediction, Upperbound, Lowerbound)
      
      list(ts.country, df.pred)
    }
  })
  
  state_trend_plot = reactive({
    ts.state = trend_data_us()[[1]]
    df.pred = trend_data_us()[[2]]
    
    if(input$trend_us == "Short-term Trend") {
      ggplotly(ggplot(tail(ts.state,90), aes(x=Date)) +
                 geom_bar(aes(y=New), stat="identity", fill=countColor, color=countColor, alpha=0.3) + 
                 geom_line(aes(y=MA),alpha=0.6) +
                 geom_line(data = df.pred, aes(Date, Prediction), alpha = 0.5, linetype = "dashed") +
                 geom_ribbon(data = df.pred, aes(x = Date, ymin = Lowerbound, ymax = Upperbound), alpha = 0.2,  fill = "darkorange") +
                 ylab("New Cases"))
    }
    else{
      ggplotly(ggplot(ts.state, aes(x=Date)) +
                 geom_bar(aes(y=New), stat="identity", fill=countColor, color=countColor, alpha=0.3) + 
                 geom_line(aes(y=MA),alpha=0.6) +
                 geom_line(data = df.pred, aes(Date, Prediction), alpha = 0.5, linetype = "dashed") +
                 geom_ribbon(data = df.pred, aes(x = Date, ymin = Lowerbound, ymax = Upperbound), alpha = 0.2,  fill = "darkorange") +
                 ylab("New Cases"))
    }
  })
  
  country_trend_plot = reactive({
    ts.country = trend_data_world()[[1]]
    df.pred = trend_data_world()[[2]]
    
    if(input$trend_world == "Short-term Trend") {
      ggplotly(ggplot(tail(ts.country,90), aes(x=Date)) +
                 geom_bar(aes(y=New), stat="identity", fill=countColor, color=countColor, alpha=0.3) + 
                 geom_line(aes(y=MA),alpha=0.6) +
                 geom_line(data = df.pred, aes(Date, Prediction), alpha = 0.5, linetype = "dashed") +
                 geom_ribbon(data = df.pred, aes(x = Date, ymin = Lowerbound, ymax = Upperbound), alpha = 0.2,  fill = "darkorange") +
                 ylab("New Cases"))
    }
    else{
      ggplotly(ggplot(ts.country, aes(x=Date)) +
                 geom_bar(aes(y=New), stat="identity", fill=countColor, color=countColor, alpha=0.3) + 
                 geom_line(aes(y=MA),alpha=0.6) +
                 geom_line(data = df.pred, aes(Date, Prediction), alpha = 0.5, linetype = "dashed") +
                 geom_ribbon(data = df.pred, aes(x = Date, ymin = Lowerbound, ymax = Upperbound), alpha = 0.2,  fill = "darkorange") +
                 ylab("New Cases"))
    }
  })
  
  output$trend_plot_us <- renderPlotly({
    state_trend_plot()
  })
  
  output$trend_plot_world <- renderPlotly({
    country_trend_plot()
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