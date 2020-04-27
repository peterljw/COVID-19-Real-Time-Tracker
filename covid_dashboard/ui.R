library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(DT)
library(shinyWidgets)
library(tidyverse)


ui <- dashboardPage(
  
  # dsahboard color theme
  skin = "black",
  
  ####### Dashboard Title #######
  dashboardHeader(title = "COVID-19 Report"),
  
  ####### Dashboard Sidebar #######
  dashboardSidebar(    
    sidebarMenu(
      # overview
      menuItem("Overview", tabName = "overview", icon = icon("search"),
               menuSubItem("U.S.", tabName = "overview-us"),
               menuSubItem("World", tabName = "overview-world")
               ),
      # trend
      menuItem("Trend Analysis", tabName = "trend", icon = icon("chart-line")),
      # simulation
      menuItem("Epidemic Simulation", tabName = "simulation", icon = icon("stopwatch"),
               menuSubItem("U.S. (in progress)", tabName = "simulation-us")),
      # demographics
      menuItem("Patient Demographics", tabName = "demographics", icon = icon("user"),
               menuSubItem("(in progress)", tabName = "simulation-us"))
      
      )
  ),
  ####### Dashboard Body #######
  dashboardBody(
    ####### overview-us #######
    tabItems(
      tabItem(tabName = "overview-us",
              fluidRow(
                valueBoxOutput("us_confirmed"),
                valueBoxOutput("us_death"),
                valueBoxOutput("us_recovered")
              ),
              fluidRow(
                column(3,
                       box(width = NULL,
                           selectInput("us_metric_selection", h4("Metric Selection"), 
                                       choices = c("Confirmed" = "Confirmed",
                                                   "Deaths" = "Deaths"),
                                       selected = "Confirmed"),
                           materialSwitch(inputId = "us_log", label = "Log Scale", value = TRUE, status = "warning"),
                           materialSwitch(inputId = "pop_log", label = "Cases/Million (in progress)", value = FALSE, status = "warning")
                           )
                ),
                column(9,
                       box(width = NULL, solidHeader = TRUE,
                           tabsetPanel(
                             tabPanel("Map",
                                      plotlyOutput("us_map")),
                             tabPanel("Line Plot",
                                      plotlyOutput("us_line_plot")),
                             tabPanel("Table",
                                      DT::dataTableOutput("us_table"))
                           )
                       )
                )
              )
      ),
      
      ####### overview-world #######
      tabItem(tabName = "overview-world",
              fluidRow(
                valueBoxOutput("world_confirmed"),
                valueBoxOutput("world_death"),
                valueBoxOutput("world_recovered")
                ),
              fluidRow(
                column(3,
                       box(width = NULL,
                           selectInput("world_metric_selection", h4("Metric Selection"), 
                                       choices = c("Confirmed" = "Confirmed",
                                                   "Deaths" = "Deaths"),
                                       selected = "Confirmed"),
                           materialSwitch(inputId = "world_log", label = "Log Scale", value = TRUE, status = "warning"),
                           materialSwitch(inputId = "pop_log", label = "Cases/Million (in progress)", value = FALSE, status = "warning")
                       )
                ),
                column(9,
                       box(width = NULL, solidHeader = TRUE,
                           tabsetPanel(
                             tabPanel("Map",
                                      plotlyOutput("world_map")),
                             tabPanel("Line Plot",
                                      plotlyOutput("world_line_plot")),
                             tabPanel("Table",
                                      DT::dataTableOutput("world_table"))
                             )
                           )
                       )
                )
              ),
      ####### trend #######
      tabItem(tabName = "trend",
              fluidRow(
                column(2,
                       box(width = NULL,
                           selectizeInput("trend_country","Country Selection", choices=NULL, selected="US"),
                           numericInput("moving_n", "Enter Moving Average Days", 10, min = 1, max = 50)
                       )
                ),
                column(10,
                       box(width = NULL, solidHeader = TRUE,
                           tabsetPanel(
                             tabPanel("Current Trend",
                                      plotOutput("trend_plot")),
                             tabPanel("10-Days Forecast (in progress)",
                                      plotOutput("prediction_plot"))
                             )
                           )
                       )
                )
              ),

      ####### demographics #######
      tabItem(tabName = "demographics-us")
      )
    )
    
  )