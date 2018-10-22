#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(lubridate)
library(plotly)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    skin = "black",
    dashboardHeader(
      title="Gun Violence in US"
    ),
    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon=icon("home")),
        menuItem("Explore by States", tabName = "state", icon=icon("location-arrow")),
        menuItem("Explore by time", tabName = "time", icon=icon("time", class = NULL, lib = "glyphicon")),
        menuItem("Github Links", icon=icon("github"), href="https://github.com/danangan/gun-violence-us-dv")
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
      tabItems(
        tabItem("overview",
          fluidRow(
            infoBoxOutput("totalIncident"),
            infoBoxOutput("stateWithMostIncident"),
            infoBoxOutput("numberKilled")
          ),
          fluidRow(
            box(
              solidHeader=T,
              width=9,
              background = NULL,
              h2('Gun Violence Incidents in US'),
              plotlyOutput('trendOverviewPlot')
            ),
            box(
              solidHeader=T,
              width=3,
              title='Year Range',
              selectInput('startYear', 'From', choices = c(2013, 2014, 2015, 2016, 2017), selected = 2014),
              selectInput('endYear', 'To', choices = c(2013, 2014, 2015, 2016, 2017), selected = 2017)
            ),
            box(
              title='About the data',
              solidHeader = T,
              width=3,
              p('The data is record of a gun violence incident occured in United States from 2013-2018.'),
              p('Data source: kaggle')
            ),
            box(
              solidHeader=T,
              width=9,
              background = NULL,
              h2('Gun Violence Incidents in US by State'),
              plotlyOutput('overviewPlot')
            )
          )
        ),
        tabItem("state",
          fluidRow(
            column(width=9,
              tabsetPanel(
                tabPanel('Scatter Map', 
                  fluidRow(
                    box(
                      solidHeader=T,
                      width=12,
                      plotlyOutput("scatterMap", height = 425)
                    )
                  )
                ),
                tabPanel('Scatter Chart',
                  fluidRow(
                    box(
                      solidHeader=T,
                      width=12,
                      plotlyOutput("scatterPlotState", height = 425)
                    )
                  )
                ),
                tabPanel('Boxplot',
                  fluidRow(
                     box(
                       solidHeader=T,
                       width=12,
                       fluidRow(
                          column(width=6,
                            plotlyOutput("boxPlotInjuriedState", height = 425)
                          ),
                          column(width=6,
                            plotlyOutput("boxPlotKilledState", height = 425)
                          )
                       )
                     )
                   )
                ),
                tabPanel('Trend Chart',
                  fluidRow(
                     box(
                       solidHeader=T,
                       width=12,
                       plotlyOutput("trendState", height = 425)
                     )
                   )
                )
              )                   
            ),
            column(width=3,
              box(
                solidHeader=T,
                width=12,
                title='Display',
                selectInput('selectState', 'State', choices = state.name, selected = 'California'),
                selectInput('startYearState', 'From', choices = c(2013, 2014, 2015, 2016, 2017), selected = 2014),
                selectInput('endYearState', 'To', choices = c(2013, 2014, 2015, 2016, 2017), selected = 2017)
              )
            )
          ),
          fluidRow(
            infoBoxOutput("totalIncidentState"),
            infoBoxOutput("totalDeathState"),
            infoBoxOutput("totalInjuredState")
          )
        ),
        tabItem("time",
          fluidRow(
            box(
              solidHeader=T,
              width=9,
              h2('Detailed Trend by Month'),
              plotlyOutput("timePlot", height = 500) 
            ),
            box(
              solidHeader=T,
              width=3,
              title='Display',
              selectInput('selectYear', 'Year', choices = c(2013, 2014, 2015, 2016, 2017), selected = 2017),
              selectInput('variationTime', 'Variation', choices = c('Bar chart', 'Line Chart'), selected = 2014)
            )
          )
        )
      )
    )
  )
)
