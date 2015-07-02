require(shinydashboard)
require(leaflet)
require(rCharts)

states <- read.csv("states.csv", stringsAsFactors = F)[,1]
states <- c("All", states)

header <- dashboardHeader(title = "Tornadoes")

sidebar <- dashboardSidebar(
  h6("This report is based on a NOAA severe weather Dataset containing over 900K meteorological observations that took place in the US between the years 1950 and 2011.", style  = "margin: 20px 10px 0px 10px; color:#cccccc;line-height: 140%;"),
  h6("Data cleaning and ShinyApp by Ariel Lev. https://rpubs.com/ariellev/storm", style  = "margin: 20px 10px 0px 10px; color:#cccccc;line-height: 140%;"),
  div(style = "position: fixed; margin-top: 20px; margin-bottom: 20px;",
      radioButtons("provider", "Provider", c("MapBox" = "mapBox", "OpenStreetMap" = "openStreetMap"), selected = "mapBox"),      
      radioButtons("type", "Type", c("Markers" = "markers", "Choropleth" = "choropleth"), selected = "markers"),
      conditionalPanel(condition = "input.type == 'markers'",
                       checkboxGroupInput("layer", label="Layer", c("Fatalities" = "Fatalities", "Injuries" = "Injuries", "Crops" = "Crops", "Properties" = "Properties"), selected=c("Fatalities", "Injuries"))),   
      conditionalPanel(condition = "input.type == 'choropleth'",
                       radioButtons("layer_choropleth", label="Layer", c("Fatalities" = "Fatalities", "Injuries" = "Injuries", "Crops" = "Crops", "Properties" = "Properties"), selected=c("Fatalities"))),   
      div(selectInput("state", "State",  states), style = "width: 75%"),                             
      div(sliderInput("period", "Years", min = 1950, max = 2011, value = c(1950,2011)), style = "width: 75%")
  )
)

body <- dashboardBody(
  includeCSS("styles.css"),
  fluidRow(               
    column(width = 12,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("mymap", height = 600),
               htmlOutput("legend")
           ),
           box(width = NULL, solidHeader = TRUE,
               h1(textOutput("state"), style = "margin-top: 0px; margin-bottom: 10px"),
               textOutput("stats_total"),
               p(textOutput("stats_per_year"),style = "margin-top: 10px;") 
           )                       
    )
  ),
  fluidRow(
    conditionalPanel(condition = "input.state != 'All'",
                     
                     column(width = 12,
                            box(width = NULL, solidHeader = TRUE,
                                #h1("Trend", style = "margin-top: 0px; margin-bottom: 10px"),
                                div(style = "width: inherit;", showOutput("barPlot", "polycharts")),               
                                selectInput("bar_layer", "",  c("Fatalities" = "Fatalities", "Injuries" = "Injuries", "Crops" = "Crops", "Properties" = "Properties"), selected = "input.layer[1]")                                            
                            )         
                     ))
  ),
  fluidRow(               
    column(width = 12,
           box(width = NULL, solidHeader = TRUE,
               h1("Trend", style = "margin-top: 0px; margin-bottom: 10px"),
               div(style = "width: inherit;", showOutput("trendPlot", "polycharts")),           
               selectInput("trend_layer", "",  c("Casualties" = "casualties", "Assets" = "assets"), selected = "casualties")                                            
           )         
    )
  ),
  fluidRow(               
    column(width = 12,
           box(width = NULL, solidHeader = TRUE,
               h1("Month wise", style = "margin-top: 0px; margin-bottom: 10px"),
               div(style = "width: inherit;", showOutput("histogramPlot", "polycharts"))
           )         
    )
  ),
  fluidRow(               
    column(width = 12,
           box(width = NULL, solidHeader = TRUE,
               DT::dataTableOutput("myTable"),
               selectInput("myTable_mode", "",  c("Complete" = "complete", "Summarized" = "summarized"), selected = "summarized")               
           )         
    )
  )  
)

dashboardPage(
  header,
  sidebar,
  body
)