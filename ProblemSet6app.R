library(shiny)
library(tidyverse)     # for data cleaning and plotting
library(gardenR)       # for Lisa's garden data
library(lubridate)     # for date manipulation
library(openintro)     # for the abbr2state() function
library(palmerpenguins)# for Palmer penguin data
library(maps)          # for map data
library(ggmap)         # for mapping points on maps
library(gplots)        # for col2hex() function
library(RColorBrewer)  # for color palettes
library(sf)            # for working with spatial data
library(leaflet)       # for highly customizable mapping
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(gganimate)     # for adding animation layers to ggplots
library(gifski)        # for creating the gif (don't need to load this library every time,but need it installed)
library(transformr)    # for "tweening" (gganimate)
library(shiny)         # for creating interactive apps
library(patchwork)     # for nicely combining ggplot2 graphs  
library(gt)            # for creating nice tables
library(rvest)         # for scraping data
library(robotstxt) 
library(rsconnect)
data("garden_harvest")

#COVID-19 data from the New York Times
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

ui <- fluidPage(
  submitButton(text = "Go!"),
  selectInput("state", 
              "State", 
              choices = list(Alabama = "Alabama", Alaska = "Alaska",
                             Arizona = "Arizona", Arkansas = "Arkansas",
                             California = "California", Colorado = "Colorado",
                             Connecticut = "Connecticut", 
                             Delaware = "Delaware",
                             Florida = "Florida", Georgia = "Georgia",
                             Hawaii = "Hawaii", Idaho = "Idaho",
                             Illinois = "Illinois", Indiana = "Indiana",
                             Iowa = "Iowa", Kansas = "Kansas",
                             Kentucky = "Kentucky", Louisiana = "Louisiana",
                             Maine = "Maine", Maryland = "Maryland",
                             Massachusetts = "Massachusetts", 
                             Michigan = "Michigan", Minnesota = "Minnesota",
                             Mississippi = "Mississippi", 
                             Missouri = "Missouri", Montana = "Montana",
                             Nebraska = "Nebraska", Nevada = "Nevada",
                              'New Hampshire' = "New Hampshire",
                             'New Jersey' = "New Jersey",
                             'New Mexico' = "New Mexico",
                             'New York' = "New York",
                             'North Carolina' = "North Carolina",
                             'North Dakota' = "North Dakota",
                             Ohio = "Ohio",
                             Oklahoma = "Oklahoma",
                             Oregon = "Oregon",
                             Pennsylvania = 'Pennsylvania',
                             'Rhode Island' = "Rhode Island",
                             'South Carolina' = "South Carolina",
                             'South Dakota' = "South Dakota",
                             Tennessee = "Tennessee",
                             Texas = "Texas",
                             Utah = "Utah",
                             Vermont = "Vermont",
                             Virginia = "Virginia",
                             Washington = "Washington",
                             'West Virginia' = "West Virginia",
                             Wisconsin = "Wisconsin",
                             Wyoming = "Wyoming"),
              multiple = 1),
  plotOutput(outputId = "covidplot")
)
server <- function(input, output) {
  output$covidplot <- renderPlot(
    covid19 %>% 
      filter(state %in% c(input$state)) %>% 
      filter(cases >= 20) %>%
      group_by(state) %>% 
      mutate(newday = n()/n(),
             days_since = cumsum(newday)-1) %>%
      ggplot(aes(x = days_since, y = cases, color = state)) +
      geom_line() +
      scale_y_log10() +
      labs(title = "Cumulative Covid Cases by State",
           y = "Cases",
           x = "Days since the 20th Case") +
      theme_minimal()
  )
}
shinyApp(ui = ui, server = server)
