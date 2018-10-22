#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

options(width=50)
knitr::opts_chunk$set(cache=TRUE,tidy=TRUE)
options(scipen = 9999)
rm(list=ls())

library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(usmap)
library(maps)
library(mapdata)
library(reshape2)

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiZGFuYW5nYW4iLCJhIjoiY2puanQxMDd2MGJoNTNqbm9wd2duYzRqaSJ9.L1_K0YXQO27s5JpgDk26Eg')

gun <- read.csv('gun-violence-data.csv')

states <- map_data("state")

gun$date <- ymd(gun$date)

gun$year <- year(gun$date)

gun$month <- month(gun$date)

gun <- gun %>%
  mutate(n_victim = n_killed + n_injured)

grouped_by_state_year <- gun %>%
  group_by(state, year) %>%
  summarise(n_incident=n(), n_killed=sum(n_killed), n_injured=sum(n_injured)) %>%
  arrange(-n_incident)

numberFormatter <- function (number) {
  formatC(number, format="f", big.mark=",", digits=0)
}

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa')
)

shinyServer(function(input, output) {
  output$trendOverviewPlot <- renderPlotly({
    gun_by_state <- gun %>%
      filter(year >= input$startYear, year <= input$endYear) %>%
      group_by(year) %>%
      summarise(n_incident = n(), n_killed = sum(n_killed), n_injured = sum (n_injured)) %>%
      melt(id.vars = 'year')   
    
    plot <- ggplot(data = gun_by_state, aes(x=year, y=value, fill=variable)) + geom_col(position = "dodge2")
    
    ggplotly(plot)    
  })  
  
  
  output$overviewPlot <- renderPlotly({
    display <- grouped_by_state_year %>%
      filter(year >= input$startYear, year <= input$endYear) %>%
      group_by(state) %>%
      summarise(n_incident=sum(n_incident), n_killed=sum(n_killed), n_injured=sum(n_injured))
    
    display$hover <- with(display, paste(state, '<br>',"Death", numberFormatter(n_killed), "<br>","Injured", numberFormatter(n_injured)))
    
    display$code <- state.abb[match(display$state, state.name)]
    plot_mapbox()
    plot_geo(display, locationmode = 'USA-states', color = I("black")) %>%
      add_trace(
        z = ~n_incident, 
        locations = ~code,
        text = ~ hover,
        color = ~n_incident,
        colors = 'Reds'
      ) %>%
      colorbar(title = "Number of incidents") %>%
      layout(
        title = paste0('Year ', input$startYear, '-', input$endYear),
        geo = g
      )
  })
  
  output$totalIncident <- renderInfoBox({
    display <- grouped_by_state_year %>%
      filter(year >= input$startYear, year <= input$endYear) %>%
      group_by(state) %>%
      summarise(n_incident=sum(n_incident))
    
    average <- sum(display$n_incident)/((as.numeric(input$endYear) - as.numeric(input$startYear) + 1)*12*30)
    
    infoBox(
      title = "Total incidents",
      value = numberFormatter(sum(display$n_incident)),
      color = 'red',
      subtitle = paste0(numberFormatter(average), ' average incident/day')
    )
  })

  output$stateWithMostIncident <- renderInfoBox({
    display <- grouped_by_state_year %>%
      filter(year >= input$startYear, year <= input$endYear) %>%
      group_by(state) %>%
      summarise(n_incident=sum(n_incident)) %>%
      arrange(-n_incident)
    
    infoBox(
      title = "State with most incidents",
      value = display[1,1],
      icon = icon('map-pin'),
      color = 'red',
      subtitle = paste0(numberFormatter(as.integer(display[1,'n_incident'])), ' incidents')
    )
  })

  output$numberKilled <- renderInfoBox({
    display <- grouped_by_state_year %>%
      filter(year >= input$startYear, year <= input$endYear) %>%
      group_by(state) %>%
      summarise(n_killed=sum(n_killed), n_injured=sum(n_injured))    

    infoBox(
      title = "Victims",
      value = paste0(numberFormatter(display$n_killed), ' killed'),
      icon = icon('remove'),
      color = 'red',
      subtitle = paste0(numberFormatter(display$n_injured), ' injured')
    )
  })
  
  output$scatterMap <- renderPlotly({

    gun_by_state <- gun %>%
      filter(year >= input$startYearState, year <= input$endYearState) %>%
      filter(state == input$selectState)
    
    plot_mapbox(
      gun_by_state,
      x = ~longitude, 
      y = ~latitude, 
      color = ~n_victim, 
      colors = c('#ffeda0','#f03b20'),
      text = ~n_victim, 
      hoverinfo = 'text', 
      showlegend = FALSE) %>%
      layout(
        xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
        yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
        mapbox = list(
          style = 'light',
          zoom = 4,
          center = list(lat = ~median(latitude, na.rm = T), lon = ~median(longitude, na.rm = T))),
        margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)
      )
  })
  
  output$totalIncidentState <- renderInfoBox({
    gun_by_state <- gun %>%
      filter(year >= input$startYearState, year <= input$endYearState) %>%
      filter(state == input$selectState)
    
    average <- nrow(gun_by_state)/((as.numeric(input$endYearState) - as.numeric(input$startYearState) + 1)*12*30)
    
    infoBox(
      title = "Total incidents",
      value = numberFormatter(nrow(gun_by_state)),
      color = 'red',
      subtitle = paste0(numberFormatter(average), ' average incident/day')
    )    
  })

  output$totalDeathState <- renderInfoBox({
    gun_by_state <- gun %>%
      filter(year >= input$startYearState, year <= input$endYearState) %>%
      filter(state == input$selectState)
    
    average <- sum(gun_by_state$n_killed)/((as.numeric(input$endYearState) - as.numeric(input$startYearState) + 1)*12*30)
    
    infoBox(
      title = "Total death",
      value = numberFormatter(sum(gun_by_state$n_killed)),
      color = 'red',
      icon = icon('remove'),
      subtitle = paste0(numberFormatter(average), ' average death/day')
    )    
  })
  
  output$totalInjuredState <- renderInfoBox({
    gun_by_state <- gun %>%
      filter(year >= input$startYearState, year <= input$endYearState, state == input$selectState)
    
    average <- sum(gun_by_state$n_injured)/((as.numeric(input$endYearState) - as.numeric(input$startYearState) + 1)*12*30)
    
    infoBox(
      title = "Total injured",
      value = numberFormatter(sum(gun_by_state$n_injured)),
      color = 'red',
      icon = icon('remove'),
      subtitle = paste0(numberFormatter(average), ' average injured/day')
    )    
  })  
  
  output$scatterPlotState <- renderPlotly({
    gun_by_state <- gun %>%
      filter(year >= input$startYearState, year <= input$endYearState, state == input$selectState)
    
    plot <- ggplot(gun_by_state, aes(y=n_killed, x=n_injured)) + geom_point(alpha=0.3, color='#f03b20', size=3)
    
    ggplotly(plot)
  })
  
  output$boxPlotKilledState <- renderPlotly({
    gun_by_state <- gun %>%
      filter(year >= input$startYearState, year <= input$endYearState, state == input$selectState)
    
    plot <- ggplot(gun_by_state, aes(y=n_killed)) + geom_boxplot(color='#f03b20')
    
    ggplotly(plot)
  })
  
  output$boxPlotInjuriedState <- renderPlotly({
    gun_by_state <- gun %>%
      filter(year >= input$startYearState, year <= input$endYearState, state == input$selectState)
    
    plot <- ggplot(gun_by_state, aes(y=n_injured)) + geom_boxplot(color='#f03b20')
    
    ggplotly(plot)
  })
  
  output$trendState <- renderPlotly({
    gun_by_state <- gun %>%
      filter(year >= input$startYearState, year <= input$endYearState, state == input$selectState) %>%
      group_by(year) %>%
      summarise(n_incident = n(), n_killed = sum(n_killed), n_injured = sum (n_injured)) %>%
      melt(id.vars = 'year')   
    
    plot <- ggplot(data = gun_by_state, aes(x=year, y=value, fill=variable)) + geom_col(position = "dodge2")
    
    ggplotly(plot)    
  })
  
  output$timePlot <- renderPlotly({
    gun_by_state <- gun %>%
      filter(year == input$selectYear) %>%
      group_by(month) %>%
      summarise(n_incident = n(), n_killed = sum(n_killed), n_injured = sum (n_injured)) %>%
      melt(id.vars = 'month')   
    
    if(input$variationTime == 'Bar chart') {
      plot <- ggplot(data = gun_by_state, aes(x=month, y=value, fill=variable)) + geom_col(position = "dodge2")      
    } else {
      plot <- ggplot(data = gun_by_state, aes(x=month, y=value, color=variable)) + geom_line() 
    }
    
    ggplotly(plot)    
  })
})
