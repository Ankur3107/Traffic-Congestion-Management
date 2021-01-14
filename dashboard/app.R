library(shiny)
library(shinydashboard)
library(shinyTime)
library(forecast)
body <- dashboardBody(
  tabItems(
    
    
    tabItem(tabName = "home",
            
            
            box(h1("Traffic Congestion Management System"),width = "100%",align="centre"),
            box(plotOutput("map_plot"),width = "100%"),
            
            fluidRow(
              
              box(
                dateInput("date_input", "Date Input:",format = "yyyy-mm-dd",value = "2015-01-01"),
                timeInput("time_input", "Enter time:", value = strptime("10:00:00", "%T")),
                actionButton("button", "Submit")
              ),
              box(
                h3("Route A: ", em("via road A40")),
                textOutput("time_output"),
                h3("Route B: ", em("via road A49")),
                textOutput("time_output_2"),
                h3("Final Observation : "),
                textOutput("time_output_3")
              )
            )
            
    ),
    
    tabItem(tabName = "dashboard",
            
            fluidRow(
              tabBox(
                title = "Forecasting Route A",
                id = "tabset1",
                
                tabPanel("TimeSeries",
                         plotOutput("plot1")),
                
                tabPanel("Forecast",
                         plotOutput("plot2")),
                
                tabPanel("Decompse",
                         plotOutput("plot3"))
                
                
              ),
              
              box(title = "Inputs",solidHeader = TRUE,collapsible = TRUE,
                  
                  sliderInput("tsslider", "TimeSeries input:", 1, 100, 5),
                  sliderInput("fslider","Forecast input:",1 ,10 , 2)
              )
              
              
            ),
            
            fluidRow(
              tabBox(
                title = "Forecasting Route B",
                id = "tabset1",
                
                tabPanel("TimeSeries",
                         plotOutput("plot1_2")),
                
                tabPanel("Forecast",
                         plotOutput("plot2_2")),
                
                tabPanel("Decompse",
                         plotOutput("plot3_2"))
                
                
              ),
              
              box(title = "Inputs",solidHeader = TRUE,collapsible = TRUE,
                  
                  sliderInput("tsslider_2", "TimeSeries input:", 1, 100, 5),
                  sliderInput("fslider_2","Forecast input:",1 ,10 , 2)
              )
              
              
            )
            
            
            
    ),
    tabItem( tabName = "report",
             fluidRow(
               box(
                 
                 title = "AR model",
                 id = "arModel",
                 plotOutput("plot_arModel"),
                 width = "100%"
                 
               )
             ),
             fluidRow(
               box(
                 
                 title = "MA model",
                 id = "maModel",
                 plotOutput("plot_maModel"),
                 width = "100%"
                 
               )
             ),
             fluidRow(
               box(
                 
                 title = "ARMA model",
                 id = "armaModel",
                 plotOutput("plot_armaModel"),
                 width = "100%"
                 
               )
             ),
             fluidRow(
               box(
                 
                 title = "ARIMA model",
                 id = "arimaModel",
                 plotOutput("plot_arimaModel"),
                 width = "100%"
                 
               )
             )
    ),
    
    tabItem(tabName = "key_desc",
            
            
            box(h1("Keyword Description"),width = "100%"),
            
            box(
              h4(strong("1. Time Series")),
              p("It is the plot of all the data points from the given CSV file of the dataset."),
              h4(strong("2. Forecast")),
              p("First we set an Time Series Input Window, i.e. the values of data set which are to be taken for analysis. Then we set the Forecast Window, i.e. the output values that are to be forecasted from the input window. A forecast is the prediction of data by using given dataset and applying the ARIMA model."),
              h4(strong("3. Decompose")),
              p("The decomposition of time series is a statistical method that deconstructs a time series into several components, each representing one of the underlying categories of patterns. Trend, Seasonal and Random are the decompositions used in TCM."),
              h4(strong("4. Trend")),
              p("A trend exists when there is a long-term increase or decrease in the data. It does not have to be linear. Sometimes we will refer to a trend changing direction when it might go from an increasing trend to a decreasing trend."),
              h4(strong("5. Seasonal")),
              p("A seasonal pattern exists when a series is influenced by seasonal factors (e.g., the quarter of the year, the month, or day of the week). Seasonality is always of a fixed and known period. Hence, seasonal time series are sometimes called periodic time series."),
              h4(strong("6. Random")),
              p("Irregular variations or random variations constitute one of four components of a time series. They correspond to the movements that appear irregularly and generally during short periods. Irregular variations do not follow a particular model and are not predictable. In practice, all the components of time series that cannot be attributed to the influence of cyclic fluctuations or seasonal variations or those of the secular tendency are classed as irregular."),
              width = "100%"
            )
            
            
            
    )
    
  )
  
)

shinyApp(
  ui = dashboardPage(skin = "gray",
                     dashboardHeader(title = "TCM"),
                     dashboardSidebar(
                       sidebarMenu(
                         menuItem("Home", tabName = "home", icon = icon("home")),
                         menuItem("Report", tabName = "dashboard", icon = icon("dashboard")),
                         menuItem("Model Comparison", tabName = "report", icon=icon("bar-chart")),
                         menuItem("Keyword Description", tabName = "key_desc", icon=icon("question-circle"))
                       )
                     ),
                     body
  ),
  
  server = function(input, output) {
    # The currently selected tab from the first box
    output$tabset1Selected <- renderText({
      input$tabset1
    })
    
    set.seed(122)
    histdata <- ts_a40
    histdata_2 <- ts_a49
    
    
    
    
    
    
    
    output$map_plot <- renderPlot({
      map_plot
    })
    
    observeEvent(input$button,{
      
      result1 <- 0
      output$time_output <- renderText({
        loc_time<- strftime(input$time_input, "%T")
        loc_time <- strsplit(as.character(loc_time),split = ':')
        
        hour <- as.integer(loc_time[[1]][1])
        mini <- as.integer(loc_time[[1]][2])
        
        count <- hour*4 + round(mini/15)
        result1 <- round(f$`Point Forecast`[count+1])
        result1
        #count
        
      })
      
      result2 <- 0
      
      output$time_output_2 <- renderText({
        loc_time<- strftime(input$time_input, "%T")
        loc_time <- strsplit(as.character(loc_time),split = ':')
        
        hour <- as.integer(loc_time[[1]][1])
        mini <- as.integer(loc_time[[1]][2])
        
        count <- hour*4 + round(mini/15)
        result2 <- round(f_2$`Point Forecast`[count+1])
        result2
        #count
        
      })
      
      output$time_output_3 <- renderText({
        
        if(result1>result2)
          "Go via a40"
        else
          "Go via a49"
        
      })
      
    })
    
    
    
    
    
    
    
    
    
    
    output$plot1 <- renderPlot({
      data <- histdata_2[seq_len(input$tsslider*96)]
      plot(data,xlab="Index",ylab="No of Vehicles",main="TimeSeries Data Representation")
    })
    
    output$plot2 <- renderPlot({
      
      train_length <- input$tsslider*96
      test_length <- length(ts_a40) - train_length
      
      train_end <- time(ts_a40)[train_length]
      test_start <- time(ts_a40)[train_length+1]
      
      training <- window(ts_a40, end = train_end)
      testing <- window(ts_a40, start = test_start)
      
      fcast <- forecast(training, h=input$fslider*96)
      plot(fcast,xlab="Index",ylab="No of Vehicles",main="TimeSeries Forecast Data Representation")
      
      
    })
    output$plot3 <- renderPlot({
      plot(decompose(ts_a40))
      
    })
    
    output$plot1_2 <- renderPlot({
      data <- histdata[seq_len(input$tsslider_2*96)]
      plot(data,xlab="Index",ylab="No of Vehicles",main="TimeSeries Data Representation")
    })
    
    output$plot2_2 <- renderPlot({
      
      train_length <- input$tsslider_2*96
      test_length <- length(ts_a49) - train_length
      
      train_end <- time(ts_a49)[train_length]
      test_start <- time(ts_a49)[train_length+1]
      
      training <- window(ts_a49, end = train_end)
      testing <- window(ts_a49, start = test_start)
      
      fcast_2 <- forecast(training, h=input$fslider_2*96)
      plot(fcast_2,xlab="Index",ylab="No of Vehicles",main="TimeSeries Forecast Data Representation")
      
      
    })
    output$plot3_2 <- renderPlot({
      plot(decompose(ts_a49))
      
    })
    
    output$plot_arModel <- renderPlot({
      plot(forecast(test_AR, h=5*96),main="AR model Forecast")
      
    })
    
    output$plot_maModel <- renderPlot({
      plot(forecast(test_MA, h=5*96),main="MA model Forecast")
      
    })
    output$plot_armaModel <- renderPlot({
      plot(forecast(test_ARMA, h=5*96),main="ARMA model Forecast")
      
    })
    output$plot_arimaModel <- renderPlot({
      plot(forecast(ts_a40, h=5*96),main="ARIMA model Forecast")
      
    })
    
    
  }
)
