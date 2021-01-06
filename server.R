# New York City Flight 2013 with R Shiny
# author: "Irfani Firdausy (irfani@gmail.com)"
# date: "12/23/2020"

library(shiny)
library(DT)
library(dplyr)
library(tidyverse)
library(nycflights13)
library(plotly)
library(ggplot2)
library(maps)
library(hexbin)

Sys.setlocale("LC_ALL", "en_US.UTF-8")

#read data from txt
qairports = read.csv("www/airports.csv", header=F, stringsAsFactor=F, encoding="UTF-8", sep=",")
colnames(qairports) = c("no", "name", "city_name","country","faa","icao","lat","lon","elevation","utc","u","tzone","type","our")

shinyServer(function(input, output,session) {

    #s: menu home
    output$homefile <- renderUI({
      includeHTML("www/home.html")
    })
    # e: menu home

    #s: menu info data
    output$AirlinesData <- DT::renderDataTable(
      DT::datatable({
        airlines
      },
      options = list(lengthMenu=list(c(10,20,50),c('10','20','50')),pageLength=10,scrollY = "100%",searchHighlight = TRUE,order = list(1, 'asc'),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                       "}"),
                     autowidth = TRUE,
                     #columnDefs=list(list(className='dt-center',targets="_all")),
                     columnDefs = list(list(targets=c(0), visible=TRUE, width='20%'),
                                       list(targets=c(1), visible=TRUE, width='80%'),
                                       list(className='dt-center',targets="_all")
                     )
      ),
      filter = "top",selection = 'multiple',style = 'bootstrap',class = 'cell-border stripe',
      rownames = FALSE,
      colnames = c("Carrier","Name")
      ))

    output$us_airportmap <-renderPlotly({
      geo <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showland = TRUE,
        landcolor = toRGB("white"),
        countrycolor = toRGB("gray80")
      )

      fig <- plot_geo(locationmode = 'USA-states', color = I("blue"))
      fig <- fig %>% add_markers(
        data =  airports, x = ~lon, y = ~lat, text = ~name,
        hoverinfo = "text", alpha = 0.5
      )

      fig <- fig %>% layout(
        title = 'US Airports',
        geo = geo, showlegend = FALSE
      )

      fig
    })

    output$AirportsData <- DT::renderDataTable(
      DT::datatable({
        airports
      },
      options = list(lengthMenu=list(c(10,20,50),c('10','20','50')),pageLength=10,scrollY = "100%",searchHighlight = TRUE,order = list(1, 'asc'),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                       "}"),
                     autowidth = TRUE,
                     #columnDefs=list(list(className='dt-center',targets="_all")),
                     columnDefs = list(list(targets=c(0), visible=TRUE, width='10%'),
                                       list(targets=c(1), visible=TRUE, width='30%'),
                                       list(targets=c(2), visible=TRUE, width='15%'),
                                       list(targets=c(3), visible=TRUE, width='15%'),
                                       list(targets=c(4), visible=TRUE, width='5%'),
                                       list(targets=c(5), visible=TRUE, width='5%'),
                                       list(targets=c(6), visible=TRUE, width='5%'),
                                       list(targets=c(7), visible=TRUE, width='15%'),
                                       list(className='dt-center',targets="_all")
                     )
      ),
      filter = "top",selection = 'multiple',style = 'bootstrap',class = 'cell-border stripe',
      rownames = FALSE,
      colnames = c("Faa","Name","Lat","Lon","Alt","Tz","Dst","Tzone")
      ))

    output$PlanesData <- DT::renderDataTable(
      DT::datatable({
        planes
      },
      options = list(lengthMenu=list(c(10,20,50),c('10','20','50')),pageLength=10,scrollY = "100%",searchHighlight = TRUE,order = list(1, 'asc'),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                       "}"),
                     autowidth = TRUE,
                     #columnDefs=list(list(className='dt-center',targets="_all")),
                     columnDefs = list(list(targets=c(0), visible=TRUE, width='20%'),
                                       list(targets=c(1), visible=TRUE, width='30%'),
                                       list(targets=c(2), visible=TRUE, width='15%'),
                                       list(targets=c(3), visible=TRUE, width='15%'),
                                       list(targets=c(4), visible=TRUE, width='5%'),
                                       list(targets=c(5), visible=TRUE, width='5%'),
                                       list(targets=c(6), visible=TRUE, width='5%'),
                                       list(targets=c(7), visible=TRUE, width='5%'),
                                       list(targets=c(8), visible=TRUE, width='10%'),
                                       list(className='dt-center',targets="_all")
                     )
      ),
      filter = "top",selection = 'multiple',style = 'bootstrap',class = 'cell-border stripe',
      rownames = TRUE,
      colnames = c("Tailnum","Year","Type","Manufacturer","Model","Engines","Seats","Speed","Engine")
      ))

    output$WeathersData <- DT::renderDataTable(
      DT::datatable({
        weather %>%
          select(time_hour,origin,temp,dewp,humid,wind_dir,wind_speed,pressure,visib)%>%
        mutate(time_hour = format(time_hour, "%Y-%m-%d %H:%M:%S"))
     },
      options = list(lengthMenu=list(c(10,20,50),c('10','20','50')),pageLength=10,scrollY = "100%",searchHighlight = TRUE,order = list(1, 'asc'),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                       "}"),
                     autowidth = TRUE,
                     #columnDefs=list(list(className='dt-center',targets="_all")),
                     columnDefs = list(list(targets=c(0), visible=TRUE, width='20%'),
                                       list(targets=c(1), visible=TRUE, width='30%'),
                                       list(targets=c(2), visible=TRUE, width='15%'),
                                       list(targets=c(3), visible=TRUE, width='15%'),
                                       list(targets=c(4), visible=TRUE, width='5%'),
                                       list(targets=c(5), visible=TRUE, width='5%'),
                                       list(targets=c(6), visible=TRUE, width='5%'),
                                       list(targets=c(7), visible=TRUE, width='5%'),
                                       list(targets=c(8), visible=TRUE, width='10%'),
                                       list(className='dt-center',targets="_all")
                     )
      ),
      filter = "top",selection = 'multiple',style = 'bootstrap',class = 'cell-border stripe',
      rownames = FALSE,
      colnames = c("Date Time","Origin","Temp","Dewp","Humid","Wind Dir","Wind Speed","Pressure","Visib")
      ))
  #e: menu info data

    #s : flights
    flight_data <- reactive({
      qfl <- flights %>%
        filter(origin==input$airport & month==input$month & day==input$day) %>%
        left_join(qairports,by=c("origin"="faa"))  %>%
        rename(c('origin_name'='name','origin_lat'='lat','origin_lon'='lon')) %>%
        left_join(qairports,by=c("dest"="faa"))  %>%
        rename(c('dest_name'='name','dest_lat'='lat','dest_lon'='lon')) %>%
        select(time_hour,flight,carrier,tailnum,origin,dest,dep_time,sched_dep_time,dep_delay,arr_time,sched_arr_time,arr_delay,origin_name,origin_lat,origin_lon,flight,tailnum,dest_name,dest_lat,dest_lon)
      qfl
    })

    flight_airports <- reactive({
      flight_data <- flight_data()
      qfl_airports <- flight_data %>% distinct(dest,dest_name,origin) %>% select(dest,dest_name,origin)
      airport1 <- qfl_airports %>%
        left_join(qairports,by=c('dest'='faa'))%>%
        select(dest,dest_name,lat,lon,origin)
      airport1
    })

    output$info_2 <- renderUI({
        bln <- as.numeric(input$month)
        dsp_info = sprintf("<h4>Flight from %s on %s, %s </h4>",input$airport,month.name[bln],input$day)
        HTML(dsp_info)
    })

    output$flightmap <-renderPlotly({
      flight_airports<-flight_airports()
      flight_data <- flight_data()

      geo <- list(
        scope = 'north america',
        projection = list(type = 'azimuthal equal area'),
        showland = TRUE,
        landcolor = toRGB("white"),
        countrycolor = toRGB("gray80")
      )

      fig <- plot_geo(locationmode = 'USA-states', color = I("blue"))
      fig <- fig %>% add_markers(
        data =  flight_airports, x = ~lon, y = ~lat, text = ~dest_name,
        hoverinfo = "text", alpha = 0.5
      )
      fig <- fig %>% add_segments(
        data =  flight_data,
        x = ~origin_lon, xend = ~dest_lon,
        y = ~origin_lat, yend = ~dest_lat,
        alpha = 0.3, size = I(1), hoverinfo = "none"
      )
      fig <- fig %>% layout(
        title = 'New York City Flights paths',
        geo = geo, showlegend = FALSE
      )
      fig
    })

    output$FlightsData <- DT::renderDataTable(
      DT::datatable({
        flight_data() %>%
          select(time_hour,dep_time,sched_dep_time,dep_delay,arr_time,sched_arr_time,arr_delay,origin,dest,flight,carrier,tailnum) %>% mutate(time_hour = format(time_hour, "%Y-%m-%d %H:%M:%S"))
      },
      fillContainer = T,
      options = list(lengthMenu=list(c(10,20,50),c('10','20','50')),pageLength=10,scrollY = "100%",searchHighlight = TRUE,order = list(1, 'asc'),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                       "}"),
                     autowidth = TRUE,
                     #columnDefs=list(list(className='dt-center',targets="_all")),
                     columnDefs = list(list(targets=c(0), visible=TRUE, width='10%'),
                                       list(targets=c(1), visible=TRUE, width='10%'),
                                       list(targets=c(2), visible=TRUE, width='15%'),
                                       list(targets=c(3), visible=TRUE, width='15%'),
                                       list(targets=c(4), visible=TRUE, width='5%'),
                                       list(targets=c(5), visible=TRUE, width='5%'),
                                       list(targets=c(6), visible=TRUE, width='5%'),
                                       list(targets=c(7), visible=TRUE, width='5%'),
                                       list(targets=c(8), visible=TRUE, width='5%'),
                                       list(targets=c(9), visible=TRUE, width='5%'),
                                       list(targets=c(10), visible=TRUE, width='5%'),
                                       list(targets=c(11), visible=TRUE, width='5%'),
                                       list(className='dt-center',targets="_all")
                     )
      ),
      filter = "top",selection = 'multiple',style = 'bootstrap',class = 'cell-border stripe',
      rownames = FALSE,
      colnames = c("time_hour","dep_time","sched_dep_time","dep_delay","arr_time","sched_arr_time","arr_delay","origin","dest","flight","carrier","tailnum")
      ))

    output$graph_flight_count <- renderPlotly({
      q1 <- flights %>%
        filter(origin==input$airport & month==input$month & day==input$day) %>%
        group_by(origin,dest) %>%
        summarise(total.count=n())

      fig <- plot_ly()
      fig <- fig %>%
        add_trace(
          type = 'scatter',
          mode = 'lines+markers',
          x = q1$dest,
          y = q1$total.count,
          text = paste(q1$origin, q1$dest, sep=" - "),
          hovertemplate = paste('<b>%{text}</b> : %{y} flights'),
          showlegend = TRUE
        )
      fig
    })
    #e: flights

    #s : delay
    #s : departure
    qdelay_dep_data <- reactive({
      dep_carrier <- flights %>%
        filter(origin==input$airport1 & month==input$month1 & dep_delay>0 ) %>%
        left_join(weather,by=c("origin"="origin","time_hour"="time_hour"))  %>%
        left_join(airlines,by=c('carrier'='carrier')) %>%
        select(time_hour,dep_time,sched_dep_time,dep_delay,origin,dest,flight,carrier,name,tailnum,temp,dewp,humid,wind_dir,wind_speed,pressure,visib)
      dep_carrier
    })

    qdelay_dep_data_1 <- reactive({
      q <- qdelay_dep_data() %>%
        group_by(origin,carrier,name) %>%
        summarise(total_count=n(),
                  sum_time=sum(as.numeric(dep_delay), na.rm = TRUE),
                  avg_time=mean(as.numeric(dep_delay), na.rm = TRUE),
                  .groups = 'drop'

        ) %>%
        select(origin,carrier,name,total_count,sum_time,avg_time)
      q
    })

    output$Delay_Dep_Data_Sumarry <- DT::renderDataTable(
      DT::datatable({
        qdelay_dep_data_1()
      },
      fillContainer = T,
      options = list(lengthMenu=list(c(10,20,50),c('10','20','50')),pageLength=10,scrollY = "100%",searchHighlight = TRUE,order = list(1, 'asc'),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                       "}"),
                     autowidth = TRUE,
                     #columnDefs=list(list(className='dt-center',targets="_all")),
                     columnDefs = list(list(targets=c(0), visible=TRUE, width='10%'),
                                       list(targets=c(1), visible=TRUE, width='10%'),
                                       list(targets=c(2), visible=TRUE, width='20%'),
                                       list(targets=c(3), visible=TRUE, width='20%'),
                                       list(targets=c(4), visible=TRUE, width='20%'),
                                       list(targets=c(4), visible=TRUE, width='20%'),

                                       list(className='dt-center',targets="_all")
                     )
      ),
      filter = "top",selection = 'multiple',style = 'bootstrap',class = 'cell-border stripe',
      rownames = FALSE,
      colnames = c("airport","carrier","name","total_count","sum_time","avg_time")
      ))

    output$dep_graph_count <-  renderPlotly({
      fig <- plot_ly(qdelay_dep_data_1(), x = ~carrier, y = ~total_count, type = 'bar', name = 'Total Count Departure Delay by Carriers',marker = list(color = 'rgb(128,0,0)',
                                                                                                                                                       line = list(color = 'rgb(8,48,107)', width = 1)))
      fig <- fig %>% layout(title ='Total Count Departure Delay By Carriers',xaxis = list(title = 'Carrier'),yaxis = list(title = 'Count Dep Delay'))
      fig
    })

    output$dep_graph_time <-  renderPlotly({
      fig <- plot_ly(qdelay_dep_data_1(), x = ~carrier, y = ~sum_time, type = 'bar', name = 'Total Sum Time ( in minute) Departure Delay by Carrier',marker = list(color = 'rgb(255,0,0)',
                               line = list(color = 'rgb(8,48,107)', width = 1)))
      fig <- fig %>% layout(title ='Sum Departure Delay By Carriers',xaxis = list(title = 'Carrier'),yaxis = list(title = 'Sum Dep Delay (in minute)'))
    })

    output$dep_graph_avgtime <-  renderPlotly({
      fig <- plot_ly(qdelay_dep_data_1(), x = ~carrier, y = ~avg_time, type = 'bar', name = 'Avg Time ( in minute) Departure Delay by Carrier',marker = list(color = 'rgb(250,128,114)',
                                              line = list(color = 'rgb(8,48,107)', width = 1)))
      fig <- fig %>% layout(title ='Average Departure Delay By Carriers',xaxis = list(title = 'Carrier'),yaxis = list(title = 'Avg Delay (in minute)'))
    })


    output$Delay_Dep_Data <- DT::renderDataTable(
      DT::datatable({
        qdelay_dep_data() %>%  mutate(time_hour = format(time_hour, "%Y-%m-%d %H:%M:%S"))
      },
      fillContainer = T,
      options = list(lengthMenu=list(c(10,20,50),c('10','20','50')),pageLength=10,scrollY = "100%",searchHighlight = TRUE,order = list(0, 'asc'),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                       "}"),
                     autowidth = TRUE,
                     #columnDefs=list(list(className='dt-center',targets="_all")),
                     columnDefs = list(list(targets=c(0), visible=TRUE, width='10%'),
                                       list(targets=c(1), visible=TRUE, width='10%'),
                                       list(targets=c(2), visible=TRUE, width='15%'),
                                       list(targets=c(3), visible=TRUE, width='15%'),
                                       list(targets=c(4), visible=TRUE, width='5%'),
                                       list(targets=c(5), visible=TRUE, width='5%'),
                                       list(targets=c(6), visible=TRUE, width='5%'),
                                       list(targets=c(7), visible=TRUE, width='5%'),
                                       list(targets=c(8), visible=TRUE, width='5%'),
                                       list(targets=c(9), visible=TRUE, width='5%'),
                                       list(targets=c(10), visible=TRUE, width='5%'),
                                       list(targets=c(11), visible=TRUE, width='5%'),
                                       list(targets=c(12), visible=TRUE, width='5%'),
                                       list(targets=c(13), visible=TRUE, width='5%'),
                                       list(targets=c(14), visible=TRUE, width='5%'),
                                       list(targets=c(15), visible=TRUE, width='5%'),

                                       list(className='dt-center',targets="_all")
                     )
      ),
      filter = "top",selection = 'multiple',style = 'bootstrap',class = 'cell-border stripe',
      rownames = FALSE,
      colnames = c("time_hour","dep_time","sched_dep_time","dep_delay","origin","dest","flight","carrier","name","tailnum","temp","dewp","humid","wind_dir","wind_speed","pressure","visib")
      ))
    #e: departure delay


    # s:arrival delay
    qdelay_arr_data <- reactive({
      arr_carrier <- flights %>%
        filter(arr_delay>0 & origin==input$airport1 & month==input$month1) %>%
        left_join(weather,by=c("origin"="origin","time_hour"="time_hour"))  %>%
        left_join(airlines,by=c('carrier'='carrier')) %>%
        select(time_hour,arr_time,sched_arr_time,arr_delay,origin,dest,flight,carrier,name,tailnum,temp,dewp,humid,wind_dir,wind_speed,pressure,visib)
      arr_carrier
    })

    qdelay_arr_data_1 <- reactive({
      q <- qdelay_arr_data() %>%
        group_by(origin,carrier,name) %>%
        summarise(total_count=n(),
                  sum_time=sum(as.numeric(arr_delay), na.rm = TRUE),
                  avg_time=mean(as.numeric(arr_delay), na.rm = TRUE),
                  .groups = 'drop'

        ) %>%
        select(origin,carrier,name,total_count,sum_time,avg_time)
      q
    })

    output$Delay_Arr_Data_Sumarry <- DT::renderDataTable(
      DT::datatable({
        qdelay_arr_data_1()
      },
      fillContainer = T,
      options = list(lengthMenu=list(c(10,20,50),c('10','20','50')),pageLength=10,scrollY = "100%",searchHighlight = TRUE,order = list(1, 'asc'),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                       "}"),
                     autowidth = TRUE,
                     #columnDefs=list(list(className='dt-center',targets="_all")),
                     columnDefs = list(list(targets=c(0), visible=TRUE, width='10%'),
                                       list(targets=c(1), visible=TRUE, width='10%'),
                                       list(targets=c(2), visible=TRUE, width='20%'),
                                       list(targets=c(3), visible=TRUE, width='20%'),
                                       list(targets=c(4), visible=TRUE, width='20%'),
                                       list(targets=c(4), visible=TRUE, width='20%'),

                                       list(className='dt-center',targets="_all")
                     )
      ),
      filter = "top",selection = 'multiple',style = 'bootstrap',class = 'cell-border stripe',
      rownames = FALSE,
      colnames = c("airport","carrier","name","total_count","sum_time","avg_time")
      ))

    output$arr_graph_count <-  renderPlotly({
      fig <- plot_ly(qdelay_arr_data_1(), x = ~carrier, y = ~total_count, type = 'bar', name = 'Total Count Arrival Delay by Carriers',marker = list(color = 'rgb(70,130,180)',                                                                         line = list(color = 'rgb(8,48,107)', width = 1)))
      fig <- fig %>% layout(title ='Total Count Arrival Delay By Carriers',xaxis = list(title = 'Carrier'),yaxis = list(title = 'Count Arr Delay'))
      fig
    })

    output$arr_graph_time <-  renderPlotly({
      fig <- plot_ly(qdelay_dep_data_1(), x = ~carrier, y = ~sum_time, type = 'bar', name = 'Total Sum Time ( in minute) Arrival Delay by Carrier',marker = list(color = 'rgb(0,191,255)',
                                                 line = list(color = 'rgb(8,48,107)', width = 1)))
      fig <- fig %>% layout(title ='Sum Arrival Delay By Carriers',xaxis = list(title = 'Carrier'),yaxis = list(title = 'Sum Arr Delay (in minute)'))
    })

    output$arr_graph_avgtime <-  renderPlotly({
      fig <- plot_ly(qdelay_dep_data_1(), x = ~carrier, y = ~avg_time, type = 'bar', name = 'Avg Time ( in minute) Arrival Delay by Carrier',marker = list(color = 'rgb(158,202,225)',                                                                                       line = list(color = 'rgb(8,48,107)', width = 1)))
      fig <- fig %>% layout(title ='Average Arival Delay By Carriers',xaxis = list(title = 'Carrier'),yaxis = list(title = 'Avg Delay (in minute)'))
    })

    output$Delay_Arr_Data <- DT::renderDataTable(
      DT::datatable({
        qdelay_arr_data() %>%  mutate(time_hour = format(time_hour, "%Y-%m-%d %H:%M:%S"))
      },
      fillContainer = T,
      options = list(lengthMenu=list(c(10,20,50),c('10','20','50')),pageLength=10,scrollY = "100%",searchHighlight = TRUE,order = list(0, 'asc'),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                       "}"),
                     autowidth = TRUE,
                     #columnDefs=list(list(className='dt-center',targets="_all")),
                     columnDefs = list(list(targets=c(0), visible=TRUE, width='10%'),
                                       list(targets=c(1), visible=TRUE, width='10%'),
                                       list(targets=c(2), visible=TRUE, width='15%'),
                                       list(targets=c(3), visible=TRUE, width='15%'),
                                       list(targets=c(4), visible=TRUE, width='5%'),
                                       list(targets=c(5), visible=TRUE, width='5%'),
                                       list(targets=c(6), visible=TRUE, width='5%'),
                                       list(targets=c(7), visible=TRUE, width='5%'),
                                       list(targets=c(8), visible=TRUE, width='5%'),
                                       list(targets=c(9), visible=TRUE, width='5%'),
                                       list(targets=c(10), visible=TRUE, width='5%'),
                                       list(targets=c(11), visible=TRUE, width='5%'),
                                       list(targets=c(12), visible=TRUE, width='5%'),
                                       list(targets=c(13), visible=TRUE, width='5%'),
                                       list(targets=c(14), visible=TRUE, width='5%'),
                                       list(targets=c(15), visible=TRUE, width='5%'),

                                       list(className='dt-center',targets="_all")
                     )
      ),
      filter = "top",selection = 'multiple',style = 'bootstrap',class = 'cell-border stripe',
      rownames = FALSE,
      colnames = c("time_hour","arr_time","sched_arr_time","arr_delay","origin","dest","flight","carrier","name","tailnum","temp","dewp","humid","wind_dir","wind_speed","pressure","visib")
      ))

    #e : delay

    #s: menu about
    output$aboutfile <- renderUI({
      includeHTML("www/about.html")
    })
    # e: menu about

})
