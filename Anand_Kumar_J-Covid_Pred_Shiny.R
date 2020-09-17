#loading packages
library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(covid19.analytics)
library(dplyr)
library(rstan)
library(prophet)
library(lubridate)
library(ggplot2)
library(dygraphs)
library(plotly)
options(scipen=999)

#dataset for TAB 1
TS.data = covid19.data("ts-ALL")
TS.data$Country.Region = paste(TS.data$Country.Region,TS.data$Province.State)
TS.data = TS.data%>%mutate(popup_info = paste("Country : ", Country.Region, "<br/>",
                                              "Total No. of cases: ",TS.data[,ncol(TS.data)-1]))
#dataset for TAB 2
#data
tsc = covid19.data("ts-confirmed")
tsc$Country.Region = paste(tsc$Country.Region,tsc$Province.State)
tsc = tsc[-1]

#data1
tsc1 = tsc%>%mutate(popup_info = paste("Country : ", Country.Region, "<br/>",
                                      "Total No. of cases: ",tsc[,ncol(tsc)]))


#ui
ui = navbarPage("TABS →",
                tabPanel("Descriptive",
                         fluidPage(theme = shinytheme("cerulean"),
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("countryInput1", "Select Country",
                                                   choice = c("Global",sort(unique(TS.data$Country.Region)))
                                                   ),
                                       width = 2),
                                     mainPanel(
                                       fluidRow(
                                         plotlyOutput(outputId = "lineplot")
                                       ),
                                       fluidRow(
                                         splitLayout(cellWidths = c("50%", "50%"),fluidRow(plotOutput(outputId = "piechart")),
                                                     fluidRow(plotlyOutput(outputId = "barplot")))
                                         )
                                       )
                                     )
                                   )
                         ),
                tabPanel("Predictive",
                         fluidPage(theme = shinytheme("cerulean"),
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("countryInput", "Select Country",
                                                   choice = c("Global",sort(unique(tsc$Country.Region)))
                                       ),
                                       fluidRow(
                                         textOutput("textInput")
                                       ),
                                       width = 2),
                                     mainPanel(
                                       fluidRow(
                                         leafletOutput(outputId = "mymap")
                                         ),
                                       fluidRow(
                                         dygraphOutput(outputId = "predplot")
                                         ),
                                       fluidRow(
                                         splitLayout(cellWidths = c("50%", "50%"),fluidRow(plotOutput(outputId = "forecastplot")),
                                                     fluidRow(plotOutput(outputId = "accuracyplot")))
                                         )
                                       )
                                     )
                                   )
                         )
                )

# server
server <- shinyServer(function(input, output){
  #TAB 1
  
  observe({
    if(input$countryInput1 == "Global")
      TS.data = TS.data
    else
      filtered  = req({
        TS.data = TS.data %>% filter(Country.Region == input$countryInput1)
        })
    
    #data cleaning
    confirmed = TS.data %>% filter(status == "confirmed")
    confirmed = confirmed[-c(1:4,ncol(confirmed)-1,ncol(confirmed))]
    freq = as.integer(colSums(confirmed))
    dates = colnames(confirmed)
    confirmed = data.frame(dates, freq)
    confirmed$status = rep("confirmed", nrow(confirmed))
    
    death = TS.data %>% filter(status == "death")
    death = death[-c(1:4,ncol(death)-1,ncol(death))]
    freq = as.integer(colSums(death))
    dates = colnames(death)
    death = data.frame(dates, freq)
    death$status = rep("death", nrow(death))
    
    recovered = TS.data %>% filter(status == "recovered")
    recovered = recovered[-c(1:4,ncol(recovered)-1,ncol(recovered))]
    freq = as.integer(colSums(recovered))
    dates = colnames(recovered)
    recovered = data.frame(dates, freq)
    recovered$status = rep("recovered", nrow(recovered))
    
    TS.data1 = rbind(confirmed, death, recovered)
    
    #TAB 1
    #line plot
    output$lineplot <- renderPlotly({
      a = ggplot(data = TS.data1, aes(x = dates, y = freq, group = status, colour = status)) + 
        geom_line() +
        geom_point() +
        labs(x = "Date", y = "No. of cases",title = "COVID19 Cases")+
        theme_minimal()+
        theme(axis.text.x=element_text(angle=45,vjust = 0.5))
      
      ggplotly(a, dynamicTicks = T, tooltip = c("x", "y", "group")) %>%
        rangeslider() %>%
        layout(hovermode = "days")
      })
    
    #piechart
    output$piechart <- renderPlot({
      bp = ggplot(TS.data1, aes(x="", y=freq, fill=status))+
        geom_bar(width = 1, stat = "identity")
      bp + coord_polar("y", start =0) 
      })
    
    #barplot
    output$barplot <- renderPlotly({
      ggplotly(ggplot(TS.data1, aes(x = status, y = freq, colour = status))+
                geom_bar(width = 1, stat = "identity"))
      })
  #TAB 2
  output$mymap <- renderLeaflet({
    leaflet()%>%addProviderTiles(providers$Esri.NatGeoWorldMap)%>%
      addCircleMarkers(lat = tsc1$Lat, lng = tsc1$Long,popup = tsc1$popup_info,
                       label = tsc1$Country.Region, radius = sqrt(sqrt(sqrt(tsc1[,ncol(tsc1)-3]))))
    })
  })
  #filtering
  observe({
    filtered  = req({
      tsc %>% filter(Country.Region == input$countryInput)
    })
    if(input$countryInput == "Global"){
      tsc = tsc[-c(1:3)]
      ds = ymd(colnames(tsc)) #saving date
      tsc = lapply(tsc, as.numeric)
      tsc = data.frame(tsc)
      y = as.integer(colSums(tsc)) #saving counts
      df = data.frame(ds, y) #merging
    } else{
      filtered1 = data.frame(t(filtered))
      filtered1 = cbind(rownames(filtered1), data.frame(filtered1, row.names = NULL))
      colnames(filtered1) = c("Date", "Confirmed")
      filtered1 = filtered1[-c(1:3),]
      filtered1$Date = ymd(filtered1$Date)
      filtered1$Confirmed = as.numeric(filtered1$Confirmed)
      #saving in a df
      ds = filtered1$Date
      y = filtered1$Confirmed
      df = data.frame(ds, y)
      }
      #mymap proxy
      leafletProxy('mymap') %>%
        setView(lng = filtered$Long, lat = filtered$Lat, zoom = 5)
    #forecasting
    m = prophet(df)
    
    #prediction
    future = make_future_dataframe(m, periods = 100)
    forecast = predict(m, future)
    
    #accuracy
    pred = forecast$yhat[1:nrow(df)]
    actual = m$history$y
    
    #predplot
    output$predplot <- renderDygraph({
      dyplot.prophet(m, forecast, main = paste("Prediction for the spread of COVID-19 in ", input$countryInput)) %>%
        dygraphs::dyOptions(maxNumberWidth = 20,  pointSize = 3,
                            pointShape = "circle", digitsAfterDecimal = 0)
      })
    
    #forecastplot
    output$forecastplot <- renderPlot({
      prophet_plot_components(m, forecast)
    })
    
    #accuracyplot
    output$accuracyplot <- renderPlot({
      plot(actual, pred, main = "Actual vs Pred")
      abline(lm(pred~actual), col = "red")
      })
    
    #textInput
    output$textInput <- renderText({
      smry = summary(lm(pred~actual))
      paste("R Squared : ", format(smry[["r.squared"]], digits = 6))
    })
  })
})
# Run the application 
shinyApp(ui = ui, server = server)
