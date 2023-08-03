#application for anomaly detection system for early warning of malaria in Thailand
source("packages.R")
source("ano_algo_fxns.R")
source("load_data.R") 
library(mapproj)
library(sf)

#remotes::install_github("r-spatial/sf")
#library(sf)

ui <- bs4Dash::dashboardPage(
  title = "Shiny Application",
  bs4Dash::dashboardHeader(
    title = "",
    align = "Anomaly Detection of Malaria in Thailand"
  ),
  #creates three main tabs
  bs4Dash::dashboardSidebar(
    bs4Dash::sidebarMenu(
      bs4Dash::menuItem(
        text = "About",
        tabName = "about",
        icon = icon("circle-info")
      ),
      bs4Dash::menuItem(
        text = "Summary",
        tabName = "summary",
        icon = icon("sort")
      ),
      bs4Dash::menuItem(
        text = "Analytics",
        tabName = "Analytics",
        icon = icon("map")
      )
      
    )
    
  )
  , bs4Dash::dashboardBody(
    bs4Dash::tabItems(
      bs4Dash::tabItem(
        tabName = "about", 
        #uncomment to include translation 
        # selectInput(inputId = "lang", label = "Select Language", choices = c("en", 
        #                                                                                         "th"
        # ) 
        # ),
        #uncomment to include translation
        # bs4Dash::bs4Card(
        #   title = "General",
        #   status = "white",
        #   background = "white",
        #   width = 12, solidHeader = TRUE,
        #   collapsible = TRUE,usei18n(i18n),i18n$at(
        #   "The purpose of this project is to develop a user 
        #   interface that supports anomaly detection of malaria 
        #   outbreaks in Thailand. The overarching goal is to support malaria surveillance efforts in Thailand. Using malaria case data from 2012, 
        #   anomaly detection algorithms were initially created using statistical 
        #   and machine learning approaches. This is further described below. 
        #   An anomaly is defined as strange or unusual behaviour. In terms of this project, 
        #  an anomaly is higher than usual malaria cases at a provincial level and at a specific 
        #  time of year. Each algorithm has different definitions of anomalies and 
        #  is better defined in the methods section. The user interface was created to allow clear 
        #   visualisation of anomalous activity while allowing the user to select their preferred method and time frame for the analysis.")
        # )
        
        #adds project description 
        bs4Dash::bs4Card(
          title = "General",
          status = "white",
          background = "white",
          width = 12, solidHeader = TRUE,
          collapsible = TRUE,
          "The purpose of this project is to develop a user 
          interface that supports anomaly detection for early warning of malaria 
          outbreaks in Thailand. The overarching goal is to support malaria surveillance efforts in Thailand. Using malaria case data from 2012 to 2022, 
          anomaly detection algorithms were initially created using statistical 
          and machine learning approaches. This is further described below. 
          An anomaly is defined as strange or unusual behaviour. In terms of this project, 
         an anomaly is higher than usual malaria cases at a provincial level and at a specific 
         time of year. Each algorithm has different definitions of anomalies and 
         is better defined in the methods section. The user interface was created to allow clear 
          visualisation of anomalous activity while allowing the user to select their preferred method and time frame for the analysis.")
        , fluidRow(
          bs4Dash::bs4Card(
            title = "Methods",
            status = "white",
            background = "white",
            width = 6, solidHeader = TRUE,
            collapsible = TRUE,
            #describes the methods used in this application
            strong("Statistical Profiling:"),"Calculates the moving average at weekly intervals and uses standard
          deviation bands to define the thresholds. Cases observations above 3 SD above the mean are defined as anomalous (definition can be set by user)",br(),br(),
            strong("Predictive Confidence Interval:"),"Uses historical data to create a predictive model using ARIMA. The model is 
                 then used to forecast the user-defined time period. The mean absolute percentage error is 
                 calculated and used to calculate the confidence interval bands. Case observations 3 SD  above the mean error are defined as anomalous", br(), br(),
            strong("Unsupervised Clustering:"), "Time series unsupervised clustering is applied to case data. More specifically, a hierarchical type is used. Cases in the smallest 
                 cluster with a minimum 5 observations is classified as anomalous", br(), br(),
            strong("Density-Based Profiling:"),"DBSCAN is used for density-based clustering of malaria cases. Cases in the smallest cluster with minimum 5 observations is classified as anomalous ",br(),br(),
            strong("Density-Based Profiling with Temp and Precip:"),"DBSCAN is used for density-based clustering of malaria cases combined with
          precipitation and temperature data. Cases in the smallest cluster with minimum 5 observations is classified as anomalous",br(), br(),
            strong("Historical Average:"),"Uses the historical average to calculate the rolling mean using the previous 14 days and comparing current 
          observations to the average number of cases for the previous 3 years. Current cases greater than the prior 3 year average value is 
          classified as an anomaly",br(), br(),
            strong("Weekly Case Previous Year:"),"Historical data is used to calculate the weekly sum of cases. Current weekly observations are compared to the weekly sum of cases from the 
          previous year. Weekly sum of cases that is greater than the weekly sum from the previous year is classified as an anomaly",br(), br(),
            strong("Monthly Case Four Years:"), "Historical data is used to calculate the monthly total of cases. The current monthly observation is compared to mean monthly total from prior 4 years. Current
          observations with case counts higher than 2 SD from the mean monthly value is classified as an anomaly",br(), br(),
            strong("General:"),"Methods highlighted blue in the dropdown menu under the 'Analysis' tab are time-series and statistical based while methods highlighted in orange are machine-learning based."
            ),
          
          bs4Dash::bs4Card(
            title = "Tabs",
            status = "white",
            background = "white",
            width = 6, solidHeader = TRUE,
            collapsible = TRUE,
            strong("Summary"), "Contains information on the current's week anomaly status across all of Thailand. This page 
         lists the provinces where anomalies have been identified this week. A trend plot is also provided to show the total malaria
         case counts during this week and broken down by species",br(),br(),
            strong("Analytics"), "This page provides more details on how the analysis is implemented and maps the 
         provinces with anomalies for improved visualisation. The user selects the method and the range in weeks used for the analysis. The results are shown on a map and in a list"
          ))
        
      ),
      bs4Dash::tabItem(
        tabName = "summary", fluidRow(
          bs4Dash::bs4TabCard(
           
            collapsible = FALSE,
            width = 8,
            tabPanel(title = "WEEKLY SUMMARY", value = "ano_sum",
                     plotOutput("summary_plot", width = "100%", height = "80vh"),align = "right"), 
            tabPanel(title = "TREND PLOT", value = "trend_week",
                     plotOutput("summary_trend_plot", width = "100%", height = "80vh"),align = "center")
            #align = "center"
          ),bs4Dash::box(
            title = h6("ANOMALIES IDENTIFIED", style ='color:gray'),
            tableOutput("summary_table"),
            collapsible = FALSE,
            width = 4,
            align = "center"
          ))
        
      
      ),
      bs4Dash::tabItem(
        tabName = "Analytics",
        
        fluidRow(
          
          bs4Dash::bs4Card(
            title = "INSTRUCTIONS",
            status = "white",
            background = "white",
            width = 4,solidHeader = TRUE, collapsible = TRUE, strong("1:"), "Select Species",br(),
            strong("2:"), "Select 'Weeks Prior' for analysis", br(), strong("3:"),"Select Method in the drop down box",
            br(), strong("4:"), "Press 'Compute'", height = "120px", algin = "center",style = 'margin-top: 0px'
            
            
          ),
          bs4Dash::bs4Card( title = "SPECIES", status = "white", width = 4, background = "white", 
                            solidHeader = TRUE,
                            selectInput(
                              inputId = "species",
                              label = "",
                              choices = c("Vivax", 
                                          "Falciparum", 
                                          "All") 
                            ),height = "135px"), align = "center"
          ,
          bs4Dash::bs4Card(
            title = "% PROV W/ ANOMALIES",
            status = "white",
            background = "white",
            width = 4,solidHeader = TRUE, height = "135px",h4(textOutput("percent_prov"), style ='color:#1e90ff ; text-align: bottom ;margin-top: 25px')
          )
        )
        ,
        
        fluidRow(
          column(
            width = 4,
            sliderInput(
              inputId = "Weeks_Prior",
              label = "Weeks Prior",
              min = 1,
              max = 520,
              value = 4,
              step = 1
            )
          ),
          #colour codes the methods. The time series-based method is blue and the machine learning-based ones are orange
          column(
            width = 4,tags$style("#method .option[data-value='Historical Average'],.option[data-value='Statistical Profiling'],.option[data-value='Historical Average'],.option[data-value='Predictive Confidence Interval'] ,.option[data-value='Weekly Case Previous Year'],.option[data-value='Monthly Case Four Years'],.option[data-value='Baseline: Weekly Three Year Median'] {
          background: #99CCFF !important;
          color: black !important;
        
        }
          .option[data-value='Unsupervised Clustering'],.option[data-value='Unsupervised Clustering'],.option[data-value='Density-Based Profiling'],.option[data-value='Density-Based Profiling with Temp and Precip']  {
          background: #FFCC99 !important;
          color: black !important;
        }"),
            #define the methods used for anomaly detection
            selectInput(
              inputId = "method",
              label = "Method",
              choices = c("Baseline: Weekly Three Year Median",
                          "Statistical Profiling", 
                          "Predictive Confidence Interval",
                          "Historical Average",
                          "Weekly Case Previous Year",
                          "Monthly Case Four Years",
                          "Unsupervised Clustering", 
                          "Density-Based Profiling",
                          "Density-Based Profiling with Temp and Precip"
              ), selectize = TRUE
            )
            , align = "center"),
          column(
            width = 4, align = "center",
            HTML("<br>"),
            #creates the action button
            actionButton(
              inputId = "go",
              class = "btn-primary",
              width = "300px", style = "color: white; background-color: #0275d8",
              "Compute"
            )
          )
        )
        
        ,fluidRow(
          #outputs the data table and the trend line using sparkline
          bs4Dash::box(
            title = h6("ANOMALIES IDENTIFIED", style ='color:gray'),
            sparklineOutput("test_app"),
            DT::dataTableOutput("first"),
            collapsible = FALSE,
            width = 4,
            align = "center"
          )
          , 
          
          bs4Dash::bs4TabCard(
            #outputs the anomaly detection map and the standardised incidence ratio map 
            width = 8,
            collapsible = FALSE,
            status = "white",
            tabPanel(title = "ANOMALY MAP", value = "test3",
                     addSpinner(plotOutput("modelPlot", width = "100%", height = "80vh"), spin = "cube", color = "royalblue"),align = "center"), 
            tabPanel(title = "STANDARDISED INCIDENCE RATIO MAP", value = "test2",
                     addSpinner(plotOutput("modelPlot2", width = "100%", height = "80vh"),spin = "cube", color = "royalblue"),align = "center"),
            # tabPanel(title = "METHOD COMPARISON", value = "test4",
            #          addSpinner(DT::dataTableOutput("comparison"),spin = "cube", color = "royalblue"),align = "center"),
            tabPanel(title = "METHOD COMPARISON", value = "test4",
                    DT::dataTableOutput("comparison"),align = "center"),
            tabPanel(title = "MAP DESCRIPTIONS",align = "left", strong("Anomaly Map:"),"This map highlights provinces with unsual or anomalous activity
                   activity as a result of the time frame and method selected.", br(),br(),
                     strong("Standardised Incidence Ratio Map:"),"This map shows the ratio of the observed number of malaria cases compared to the expected
                   value for each province standardised by the total and provincial population of Thailand. The SIR of each province is calculated and compared to each other. The higher
                   the SIR, the higher the incidence cases compared to the expected value.")
            
            
          )
        )
      )
    )
  ) 
)

# Define the server
server <- function(input, output, session) {
  
  #uncomment to enable translation feature (not working for now)
  # eventReactive(input$lang, {
  #   default_lang <- input$lang
  #   print(default_lang)
  #   i18n$set_translation_language(input$lang)
  # 
  #   update_lang( session, input$lang)
  #   
  #   target_lang <- input$lang
  #   renderUI(
  #     #i18n$set_translation_language(input$lang)
  #     update_lang(input$lang, session)
  #     #target_lang <- input$lang
  #   )
  #   
  # })
  
  modelOut <- eventReactive(input$go, {
    #define the nujmber of weeks wanted to analysis
    week_num <- input$Weeks_Prior
    
    #convert the number of previous weeks into days 
    prev_time <- weeks(week_num)
    latest_date <- as.POSIXct('2022-05-25')
    
    #calculates the province for standardised incidence calculations
    prov <- SIR_EV(total_data,prev_time,latest_date)
    #filters the data based on species selected
    if(input$species == "All"){
      ano_act <- find_anomaly(prev_time,latest_date,input$method, prov,temp_data_long, precip_data_long)
    }else if(input$species == "Vivax"){
      species_v = "V"
      ano_act <- find_anomaly(prev_time,latest_date,input$method, prov,temp_data_long, precip_data_long, species_v )
    }else {
      species_f = "F"
      ano_act <- find_anomaly(prev_time,latest_date,input$method, prov,temp_data_long, precip_data_long, species_f)}
    
    return(ano_act)
  })
  
    #Plot Thai Map 
    output$modelPlot <- renderPlot({
    
    #joins the data to the map from rworld map
    thaiLevel1 <- rworldmap::joinData2Map(modelOut(),nameMap="thaimap",nameJoinIDMap="NAME_1",nameJoinColumnData="province")
    thaiLevel1@data$id <- rownames(thaiLevel1@data)
    thaiLevel1@data$status = as.factor(thaiLevel1@data$status)
    
    map <- fortify(thaiLevel1)
    
    map$id <- as.integer(map$id)
    
    #extracts the data associated with each province and connects that to each province
    dat <- data.frame(id = thaiLevel1@data$id, state = thaiLevel1@data$NAME_1, status =thaiLevel1@data$status)
    dat$id <- as.numeric(dat$id)
    
    map_df <- inner_join(map, dat, by = "id")
    
    #plots the data for each province
    ggplot() +
      geom_map(data = map_df, map = map_df,
               aes(map_id = id, x = long, y = lat, group = group, fill = status),
               size = 0.25) +
      ggplot2::coord_map() + scale_fill_manual(labels=c( "No Anomaly Detected","Anomaly Detected"), values = c("dodgerblue","brown1"), name = "Status")+
      labs(x = "longitude", y = "latitude", title = "")  + ggplot2::coord_sf(
        crs = 4326, default_crs = 4326,
        xlim = c(96, 106), ylim = c(6, 20.5)
      ) +
      theme_minimal()
    
  }, height = 550, width = 550)
  
  
  #adding sparklines
  line_string <- "type: 'line', lineColor: 'blue', fillColor: '#A7DBD8', highlightLineColor: 'orange', highlightSpotColor: 'orange'"
  
  cd <- list(list(targets = 1, render = JS("function(data, type, full){ return '<span class=sparkSamples>' + data + '</span>' }")))
  
  cb = JS(paste0("function (oSettings, json) {\n  $('.sparkSamples:not(:has(canvas))').sparkline('html', { ", 
                 line_string, " });\n}"), collapse = "")
  
  
  #render the table that shows cases averted and costs
  output$first <- DT::renderDataTable({
    validate(
      need(input$go, ""),
    )
    dt <- modelOut()[modelOut()$status > 0,]
    dt <- dt[c("province","cases")]
    prov_ano <- DT::datatable(dt,rownames = FALSE, options = list(columnDefs = cd,fnDrawCallback = cb))
    return(prov_ano)
    
  })
  
  #render the table that shows accuracy when comparing methods (static output comparing methods using
  #all historical data)
  output$comparison <- DT::renderDataTable({
    Method <- c("Statistical Profiling","Predictive Confidence Interval",
                "Unsupervised Clustering","Density-Based Profiling",
                "Density-Based Profiling with Temp and Precip",
                "Historical Average"," Weekly Case Previous Year",
                "Monthly Case Four Years","Baseline: Weekly Three Year Median")
    NumberReported <- c(882 ,6493,75, 5, 452, 10875, 35602, 5577, 32630 )
    NumberActual <- rep(7,9)
    NumberActualDetected <- c(1,2,0,0,0,6,6,0,6)
    AccuracyPercentage <- (NumberActualDetected/NumberActual)*100
    dt <- data.table(Method,NumberReported,NumberActual,NumberActualDetected,AccuracyPercentage)
    
    return(dt)

  })
  
  #Analysis for the weekly summary page
  #filters the weekly summary data
  method = "Historical Average"
  prev_time <- weeks(1)
  latest_date <- as.POSIXct('2022-05-25')
  
  data_week_summary <- find_anomaly(prev_time,latest_date,method)
  
  #case counts for trend plot
  prev_date_week_sum <- latest_date - weeks(1)
  #filters the data for just the previous week
  data_week <- filter_data(total_data,prev_date_week_sum, latest_date)
  
  #creates the proportion map for the summary page 
  output$summary_plot <- renderPlot({
    
    data <- week_summary(data_week_summary)
    
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = c("brown3","cornflowerblue"))) +
      geom_rect() +
      geom_text(x=0.5, aes(y=labelPosition, label=label,family = "AvantGarde"), size=8, colour = "gray56") +
      coord_polar(theta="y") +
      xlim(c(-1, 4)) +
      theme_void() +
      theme(legend.position = "none")
    
  })
  
  #calculates the percentage of provinces with anomalies detected
  output$percent_prov <- renderText({
    validate(
      need(input$go, ""),
    )
    percent <- nrow(modelOut()[modelOut()$status == 1,])/ nrow(modelOut()[modelOut()$status == 0,])*100
  })
  
  #outputs the trend plot for the weekly summary page. The plot is separated by border type and and contains 
  #information about the species makeup of these cases
  output$summary_trend_plot <- renderPlot({
    
    ggplot(data_week) +
      aes(x = blood_draw_date, fill = result_code_detail_1) +
      geom_histogram(bins = 30L) +
      scale_fill_hue(direction = 1) +
      theme_minimal()+
      facet_wrap(vars(type_border)) + labs(y = "Malaria Case Counts", x = "Date") + scale_fill_discrete(name = "Species")
    
  })
  
  #render the summary table containing provinces with anomalies detected
  output$summary_table <- renderTable({
    
    prov_ano <- data.frame(data_week_summary$province[data_week_summary$status > 0])
    colnames(prov_ano) <- "Provinces with Anomalies"
    return(prov_ano)
  })
  
  #Plots the standardised incidence ratio map 
  output$modelPlot2 <- renderPlot({
    
    thai_SIR <- rworldmap::joinData2Map(modelOut(),nameMap="thaimap",nameJoinIDMap="NAME_1",nameJoinColumnData="province")
    thai_SIR@data$id <- rownames(thai_SIR@data) 
    map_SIR <- fortify(thai_SIR)
    
    map_SIR$id <- as.integer(map_SIR$id)
    dat_SIR <- data.frame(id = thai_SIR@data$id, state = thai_SIR@data$NAME_1, SIR =thai_SIR@data$SIR )
    dat_SIR$id <- as.numeric(dat_SIR$id) 
    
    map_df_SIR <- inner_join(map_SIR, dat_SIR, by = "id")
    
    ggplot() +
      geom_map(data = map_df_SIR, map = map_df_SIR,
               aes(map_id = id, x = long, y = lat, group = group, fill = SIR),
               size = 0.25) +
      coord_map() + 
      labs(x = "longitude", y = "latitude", title = "") + coord_sf(
       crs = 4326, default_crs = 4326,
       xlim = c(96, 106), ylim = c(6, 20.5)
      ) + theme_minimal()
      
    
  }, height = 550, width = 500)
  
  
}
#rsconnect::configureApp(appName = "Malaria_Anomaly_Detection_App", account = "moru",size="xlarge")
#rsconnect::showLogs()
# Run the application 
shinyApp(ui = ui, server = server) 