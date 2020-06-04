library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(DT)
library(shinyWidgets)
library(tidyverse)


ui <- dashboardPage(
  
  # dsahboard color theme
  skin = "black",
  
  ####### Dashboard Title #######
  dashboardHeader(title = "COVID-19 Tracker"),
  
  ####### Dashboard Sidebar #######
  dashboardSidebar(    
    sidebarMenu(
      # overview
      menuItem("Overview", tabName = "overview", icon = icon("search"),
               menuSubItem("U.S.", tabName = "overview-us"),
               menuSubItem("World", tabName = "overview-world")
               ),
      # trend
      menuItem("Trend By Country", tabName = "trend", icon = icon("chart-line")),
      # symptoms
      menuItem("Common Symptoms", tabName = "symptoms", icon = icon("notes-medical")),
      # demographics
      menuItem("Patient Demographics", tabName = "demographics", icon = icon("user")),
      # about
      menuItem("About", tabName = "about", icon = icon("info"))
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
                           materialSwitch(inputId = "us_pop_log", label = "Cases/Million", value = FALSE, status = "warning")
                           )
                ),
                column(9,
                       box(width = NULL, solidHeader = TRUE,
                           tabsetPanel(
                             tabPanel("Map",
                                      plotlyOutput("us_map") %>% withSpinner(type = 1, color = "darkorange")),
                             tabPanel("Line Plot",
                                      plotlyOutput("us_line_plot") %>% withSpinner(type = 1, color = "darkorange")),
                             tabPanel("Table",
                                      DT::dataTableOutput("us_table") %>% withSpinner(type = 1, color = "darkorange"))
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
                           materialSwitch(inputId = "world_pop_log", label = "Cases/Million", value = FALSE, status = "warning")
                       )
                ),
                column(9,
                       box(width = NULL, solidHeader = TRUE,
                           tabsetPanel(
                             tabPanel("Map",
                                      plotlyOutput("world_map") %>% withSpinner(type = 1, color = "darkorange")),
                             tabPanel("Line Plot",
                                      plotlyOutput("world_line_plot") %>% withSpinner(type = 1, color = "darkorange")),
                             tabPanel("Table",
                                      DT::dataTableOutput("world_table") %>% withSpinner(type = 1, color = "darkorange"))
                             )
                           )
                       )
                )
              ),
      ####### trend #######
      # current trend
      tabItem(tabName = "trend",
              fluidRow(
                column(3,
                       box(width = NULL,
                           selectInput("trend_metric_selection", "Metric Selection", 
                                       choices = c("Confirmed" = "confirmed",
                                                   "Deaths" = "deaths",
                                                   "Recovered" = "recovered"),
                                       selected="Confirmed"),
                           selectizeInput("trend_country","Country Selection", choices=NULL),
                           numericInput("moving_n", "Enter Moving Average Days", 14, min = 1, max = 50)
                       )
                ),
                column(9,
                       box(width = NULL, solidHeader = TRUE,
                           plotOutput("trend_plot") %>% withSpinner(type = 1, color = "darkorange")
                       )
                )
              )
              ),
    
      ####### symptoms #######
      tabItem(tabName = "symptoms",
              plotlyOutput("symptoms_plot") %>% withSpinner(type = 1, color = "darkorange")
              ),
      ####### demographics #######
      tabItem(tabName = "demographics",
              fluidRow(
                column(3,
                       box(width = NULL,
                           materialSwitch(inputId = "outcome_switch", label = "Group By Outcome", value = TRUE, status = "warning")
                       )
                ),
                column(9,
                       box(width = NULL, solidHeader = TRUE,
                           tabsetPanel(
                             tabPanel("Age",
                                      plotlyOutput("age_boxplot") %>% withSpinner(type = 1, color = "darkorange"),
                                      plotlyOutput("age_densityplot") %>% withSpinner(type = 1, color = "darkorange")
                                      ),
                             tabPanel("Gender",
                                      plotlyOutput("gender_plot") %>% withSpinner(type = 1, color = "darkorange")
                                      )
                           )
                       )
                  )
                )
              ),
      
      ####### about #######
      tabItem(tabName = "about",
              box(width = NULL, solidHeader = TRUE, includeMarkdown("about.Rmd"))
              )
      )
    )
    
  )