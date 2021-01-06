# New York City Flight 2013 with R Shiny
# author: "Irfani Firdausy (irfani@gmail.com)"
# date: "12/23/2020"


library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(tidyverse)
library(nycflights13)
library(plotly)
library(ggplot2)
library(maps)
library(hexbin)

# Define UI for application
shinyUI(fluidPage(theme = shinytheme("united"),

                  #s: navbar
                  navbarPage("NYC Flights",
                             # s : menu 1 --
                             tabPanel(icon("home"),
                                      fluidRow(column(12,tags$img(src="home.jpg",width="960px",height="540px",class="img-responsive"),align="center")
                                      ),
                                      fluidRow(column(12,
                                                      uiOutput("homefile"),
                                                     align="left")
                                      )
                             ),
                             # s : menu 2 --
                             tabPanel("Data Info",
                                      h3("Data Info"),
                                      hr(),
                                      column(12, tabsetPanel(id = 'tabs1',
                                                             tabPanel("Airlines",
                                                                      br(),
                                                                      DT::dataTableOutput("AirlinesData")
                                                             ),
                                                             tabPanel("US Airports",
                                                                      br(),
                                                                      plotlyOutput("us_airportmap"),
                                                                      br(),
                                                                      DT::dataTableOutput("AirportsData")
                                                             ),
                                                             tabPanel("Planes",
                                                                      br(),
                                                                      DT::dataTableOutput("PlanesData")
                                                             ),
                                                             tabPanel("Weather",
                                                                      br(),
                                                                      DT::dataTableOutput("WeathersData")
                                                             )
                                                  )
                                      )

                             ),
                             # e : menu 2 --
                             # s : menu 3 --
                             tabPanel("Flights",
                                      h3("Flights Info"),
                                      hr(),
                                      fluidRow(column(3,class="bg-primary",br(),
                                                      selectInput(inputId = "airport",
                                                                  label = "select NYC airport",
                                                                  choices = c("EWR", "LGA", "JFK"),
                                                                  selected = "JFK"),
                                                      selectInput(inputId = "month",
                                                                  label = "select month",
                                                                  choices = c(1:12),
                                                                  selected = "1"),
                                                      selectInput(inputId = "day",
                                                                  label = "select day",
                                                                  choices = c(1:31),
                                                                  selected = "1"),
                                                      br()
                                      ),
                                      column(9, tabsetPanel(id = 'tabs2',
                                                            tabPanel("Map",
                                                                     br(), uiOutput("info_2"),
                                                                     plotlyOutput("flightmap")
                                                            ),
                                                             tabPanel("Data",
                                                                      br(),
                                                                      DT::dataTableOutput("FlightsData")
                                                             ),

                                                             tabPanel("Graph",
                                                                      br(),
                                                                      h4('How many flights a day ?'),
                                                                      plotlyOutput("graph_flight_count")

                                                             )
                                              )
                                      )
                                      )

                             ),
                             # e : menu 3 --
                             # s : menu 4 --
                             tabPanel("Delay",
                                      h3("Delay Info"),
                                      hr(),
                                      fluidRow(column(3,class="bg-primary",br(),
                                                      selectInput(inputId = "airport1",
                                                                  label = "select NYC airport",
                                                                  choices = c("EWR", "LGA", "JFK"),
                                                                  selected = "JFK"),
                                                      selectInput(inputId = "month1",
                                                                  label = "select month",
                                                                  choices = c(1:12),
                                                                  selected = "1"),
                                                      br()
                                      ),
                                      column(9, tabsetPanel(id = 'tabs2',
                                                            tabPanel("Dep. Delay Graph",
                                                                     br(),
                                                                     h4("Departure Delay by Carriers"),
                                                                     DT::dataTableOutput("Delay_Dep_Data_Sumarry"),
                                                                     br(),
                                                                     plotlyOutput("dep_graph_count"),
                                                                     br(),
                                                                     fluidRow(column(6,
                                                                     plotlyOutput("dep_graph_time")),
                                                                     column(6,
                                                                     plotlyOutput("dep_graph_avgtime"))
                                                                     )

                                                            ),
                                                            tabPanel("Dep. Delay Data",
                                                                     br(),
                                                                     h4("Departure Delay Data"),
                                                                     DT::dataTableOutput("Delay_Dep_Data")

                                                            ),
                                                            tabPanel("Arr. Delay Graph",
                                                                     br(),
                                                                     h4("Arrival Delay by Carriers"),
                                                                     DT::dataTableOutput("Delay_Arr_Data_Sumarry"),
                                                                     br(),
                                                                     plotlyOutput("arr_graph_count"),
                                                                     br(),
                                                                     fluidRow(column(6,
                                                                                     plotlyOutput("arr_graph_time")),
                                                                              column(6,
                                                                                     plotlyOutput("arr_graph_avgtime"))
                                                                     )

                                                            ),
                                                            tabPanel("Arrival Delay Data",
                                                                     br(),
                                                                     h4("Arrival Delay Data"),
                                                                     DT::dataTableOutput("Delay_Arr_Data")
                                                            )
                                      )
                                      )
                                      )

                             ),
                             # e : menu 4 --
                             # s : menu 5 --
                             tabPanel("About",
                                      h3("About Us"),
                                      hr(),
                                      uiOutput("aboutfile")
                             ),
                             # e : menu 5 --
                             tags$script(HTML("var header = $('.navbar > .container-fluid');
                       header.append('<div style=\"float:right;margin-top:5px\"><h4>Explore NYC Fligths 2013 Data</h4></div>');
                       console.log(header)"))
                  )
                  # e: navbar
))
