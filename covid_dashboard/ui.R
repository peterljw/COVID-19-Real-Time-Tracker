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
               menuSubItem("World", tabName = "overview-world"),
               menuSubItem("U.S.", tabName = "overview-us")
               ),
      # trend
      menuItem("Trend", tabName = "trend", icon = icon("chart-line"),
               menuSubItem("World", tabName = "trend-world"),
               menuSubItem("U.S.", tabName = "trend-us")
               ),
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
                valueBoxOutput("us_confirmed", width = 3),
                valueBoxOutput("us_active", width = 3),
                valueBoxOutput("us_recovered", width = 3),
                valueBoxOutput("us_death", width = 3)
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
                valueBoxOutput("world_confirmed", width = 3),
                valueBoxOutput("world_active", width = 3),
                valueBoxOutput("world_recovered", width = 3),
                valueBoxOutput("world_death", width = 3)
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
      tabItem(tabName = "trend-us",
              fluidRow(
                column(3,
                       box(width = NULL,
                           selectInput("trend_metric_selection_us", "Metric Selection", 
                                       choices = c("Confirmed" = "confirmed",
                                                   "Deaths" = "deaths"),
                                       selected="Confirmed"),
                           selectizeInput("trend_state","State Selection", choices=NULL),
                           radioButtons("trend_us", label = "Scope",
                                        choices = list("Short-term Trend" = "Short-term Trend", "Long-term Trend" = "Long-term Trend"), 
                                        selected = "Short-term Trend")
                       )
                ),
                column(9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("trend_plot_us") %>% withSpinner(type = 1, color = "darkorange")
                       )
                )
              )
      ),
      
      tabItem(tabName = "trend-world",
              fluidRow(
                column(3,
                       box(width = NULL,
                           selectInput("trend_metric_selection_world", "Metric Selection", 
                                       choices = c("Confirmed" = "confirmed",
                                                   "Deaths" = "deaths",
                                                   "Recovered" = "recovered"),
                                       selected="Confirmed"),
                           selectizeInput("trend_country","Country Selection", choices=NULL),
                           radioButtons("trend_world", label = "Scope",
                                        choices = list("Short-term Trend" = "Short-term Trend", "Long-term Trend" = "Long-term Trend"), 
                                        selected = "Short-term Trend")
                       )
                ),
                column(9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("trend_plot_world") %>% withSpinner(type = 1, color = "darkorange")
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