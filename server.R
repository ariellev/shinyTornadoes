require(shiny)
require(leaflet)
require(htmltools)
require(jsonlite)
require(dplyr)
require(DT)
require(rCharts)
require(reshape2)
require(lubridate)

##
# pop up content of a tornado marker used in fatailities and injuries
##
content <- function(type, total, date) {
  paste(sep = "<br/>",
        paste("<b>",total, type ,"</b>"),
        date              
  )
}

##
# pop up content of a tornado marker used in crops and properties
##
contentUSD <- function(type, total, date) {
  paste(sep = "<br/>",
        paste("<b>", type, format(as.numeric(total), big.mark=",", scientific = F), "USD","</b>"),
        date              
  )
}

##
# returns true if vector value are between givens years
##
between <- function(vec, from, to) {
  start <- as.Date(paste0("01/01/", from), format = "%m/%d/%Y")
  end <- as.Date(paste0("12/31/", to), format = "%m/%d/%Y")
  (vec >= start) & (vec <= end)
}

##
# renders an overall statistics for a given year span
##
renderStatsTotal <- function(input, stats) {
  renderText({
    stats <- sapply(stats, function(x) format( x, big.mark = ","))
    if (stats[1] == stats[2]) {
      sprintf("%s|  %s tornadoes • %s Fatalities • %s Injuries • %s USD", stats[1],stats[3],stats[4],stats[5],stats[6])    
    } else {
      sprintf("%s - %s| %s tornadoes • %s Fatalities • %s Injuries • %s USD", stats[1],stats[2],stats[3],stats[4],stats[5],stats[6])    
    }
  })}

##
# Renders average statistics for a single year based on total-stats
##
renderStatsPerYear <- function(input, stats) {
  renderText({
    if (stats[1] != stats[2]) {
      years <- stats[2] - stats[1]
      stats <- sapply(stats, function(x) format( round(x/years,1), big.mark = ","))    
      sprintf("Annual average| %s tornadoes • %s Fatalities • %s Injuries • %s USD", stats[3],stats[4],stats[5],stats[6])    
    }
  })}

##
# renders data table
##
renderMyTables <- function(dt, input) {
  DT::renderDataTable({
    if (input$myTable_mode == "summarized") {
      dt %>% group_by(state) %>% summarize(fatalities = sum(fatalities), injuries = sum(injuries), properties = sum(properties), crops = sum(crops))    
    } else { 
      dt
    }},selection = "none")
}

##
# renders trend plot
##
renderTrendPlot <- function(dt, input) {
  renderChart({
    dt$year <- year(dt$date)
    dt$month <- month(dt$date)
    
    # distinction between single and multiple year mode
    if (length(unique(dt$year)) > 1) {
      trend <- dt %>% group_by(year)
      x_axis = "year"    
    } else {
      trend <- dt %>% group_by(month)    
      x_axis = unique(dt$year)
    }
    
    # summarizing data
    trend <- trend %>% summarize(fatalities = sum(fatalities), injuries = sum(injuries), properties = sum(properties), crops = sum(crops))  
    
    # making data juicy and ready to be plotted, setting axis etc..    
    if (input$trend_layer == "casualties") {
      trend <- melt(trend[,1:3], 1)
      names(trend) <- c("year", "type", "value")
      y_axis <- "persons"
    } else {
      trend <- melt(trend[,c(1,4:5)], 1)
      names(trend) <- c("year", "type", "value") 
      trend$value <- trend$value / 10^6
      y_axis <- "1,000,000 USD" 
    }
    
    trend$type <- as.factor(trend$type)
    
    # plotting    
    p2 <- rPlot(value ~ year, type = "line", data = trend, size = list(const = 3), color = "type")
    p2$addParams(width = 1350, dom = 'trendPlot', class="plot")
    p2$guides(y = list(min = 0, title = y_axis), x = list(title = x_axis),color = list(scale = "#! function(value){
                                                                                       color_mapping = {fatalities: '#990000', injuries: 'navy', crops: '#556b2f', properties : '#b8860b' }
                                                                                       return color_mapping[value];                  
  } !#"))
    
    return(p2)
})
}

##
# renders bar plot
##
renderBarPlot <- function(dt, input) {  
  renderChart({
    
    # preparing data, setting colors and axes
    bars <- group_by(dt, postal, state)
    
    if (input$bar_layer == "Fatalities") {
      bars <- bars %>% summarize(fatalities = sum(fatalities))
      y_axis <- "persons"    
      bar_col <- "#990000"
    }
    
    if (input$bar_layer == "Injuries") {
      bars <- bars %>% summarize(injuries = sum(injuries))
      y_axis <- "persons"  
      bar_col <- "navy"      
    }
    
    if (input$bar_layer == "Crops") {
      bars <- bars %>% summarize(crops = sum(crops))
      bars$crops <- bars$crops / 10^6      
      y_axis <- "1,000,000 USD"
      bar_col <- "#556b2f"      
    }
    
    if (input$bar_layer == "Properties") {
      bars <- bars %>% summarize(properties = sum(properties))
      bars$properties <- bars$properties / 10^6
      y_axis <- "1,000,000 USD" 
      bar_col <- "#b8860b"      
    }
    
    # nasty workaround to deatch the grouping attribute so the 
    # data can be sorted by values rather than postal code.
    names(bars) <- c("postal", "state", "value")   
    bars <- as.data.frame(cbind(bars$postal, bars$state, bars$value), stringsAsFactors = F)
    names(bars) <- c("postal", "state", "value")           
    bars$value <- as.numeric(bars$value)
    
    # sorting
    bars <- arrange(bars, desc(value))
    bars$color <- "#2A3135" # grey
    bars$color[which(bars$state == input$state)] <- bar_col;
    
    p2 <- rPlot(x = list(var = "postal", sort = "value"), y = "value" , type = "bar", data = bars, size = list(const = 3), color = "color")
    p2$addParams(width = 1350, dom = 'barPlot', class="plot")
    p2$guides(y = list(min = 0, title = y_axis), x = list(title = ""), color = list(scale = "#! function(color){
                                                                                    return color;                  
  } !#"))
    p2$set(legendPosition = "none")
    return(p2)
})
}

##
# renders "Month wise" histogram
##
renderHistogramPlot <- function(dt, input) {
  renderChart({
    dt$year <- year(dt$date)
    dt$month <- month(dt$date)
    
    scatter <- dt %>% group_by(month) %>% summarize(count = n())        
    p2 <- rPlot(count ~ month, type = "line", data = scatter, size = list(const = 3))
    p2$addParams(width = 1350, dom = 'histogramPlot')
    p2$guides(y = list(min = 0, title = "tornadoes"), x = list(title = "month"))
    
    return(p2)
  })
}

# reading data
states <- read.csv("states.csv", stringsAsFactors = F)
tornadoes <- read.csv("tornadoes.csv", stringsAsFactors = F)

casualties <- tornadoes[tornadoes$fatalities > 0 | tornadoes$injuries > 0, -(8:9)]
damage <- tornadoes[tornadoes$properties > 0 | tornadoes$crops > 0, -(6:7)]

# proxy objects. So we can use original data as an anchor to subset from,
# whenever the input fields direct to.
h <- casualties
d <- damage
a <- tornadoes

# total is needed for the overall stats.
total <- nrow(tornadoes)

# choropleth coloring and features
states_json <- readLines("us-states.json", warn = FALSE) %>% paste(collapse = "\n") %>% fromJSON(simplifyVector = FALSE)
s_json <- states_json

fatal_subtotal <- group_by(casualties, state) %>% summarize(subtotal = sum(fatalities))
injury_subtotal <- group_by(casualties, state) %>% summarize(subtotal = sum(injuries))
prop_subtotal <- group_by(damage, state) %>% summarize(subtotal = sum(properties))
crop_subtotal <- group_by(damage, state) %>% summarize(subtotal = sum(crops))
palette <- list("Fatalities" = c("#fb0000", "#990000"),"Injuries" = c("#0000cd", "#00004d"), "Crops" = c("#7fa046", "#39481f"), "Properties" = c("#f2b41e", "#705107"))

##
# returns a density of a property relatively to the max values of a given type.
##
density <- function(feature_prop, type) {
  name <- feature_prop$name
  dens <- 0
  if (type == "Fatalities") {
    index <- which(fatal_subtotal$state == name)
    dens <- fatal_subtotal[index,]$subtotal
  }  
  if (type == "Injuries") {
    index <- which(injury_subtotal$state == name)
    dens <- injury_subtotal[index,]$subtotal
  } 
  if (type == "Crops") {
    index <- which(crop_subtotal$state == name)
    dens <- crop_subtotal[index,]$subtotal
  } 
  if (type == "Properties") {
    index <- which(prop_subtotal$state == name)
    dens <- prop_subtotal[index,]$subtotal
  }
  if (length(dens) == 0) {
    dens <- 0
  }
  dens
}

maximum <- 0

##
# returns a palette based on given type
##
qpal <- function(type) {
  if (type == "Fatalities") {
    maximum <- max(fatal_subtotal$subtotal)
  }  
  if (type == "Injuries") {
    maximum <- max(injury_subtotal$subtotal)
  } 
  if (type == "Crops") {
    maximum <- max(crop_subtotal$subtotal)
  } 
  if (type == "Properties") {
    maximum <- max(prop_subtotal$subtotal)
  }           
  colorBin(c("#ffffff", palette[[type]][2]), c(0,maximum), n = 10, pretty = T)
}

# init map
map <- leaflet(data = casualties) %>% setView(lat = 37.6, lng = -95.665, zoom = 4, options = list(animate = T))

shinyServer(
  function(input,output, session) {  
    # resolving provider
    output$mymap <- renderLeaflet({  
      if (input$provider == "openStreetMap") {
        map <- map %>% addTiles()
      } else {
        map <- map %>% addTiles("//{s}.tiles.mapbox.com/v4/ariellev02.md09o8kh/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoiYXJpZWxsZXYwMiIsImEiOiIwZGVmOWI1NmYzMzE3ZmI3MWUwNmQzNDk1NDMyYzE5OSJ9.BTrDAcc81lL5FReG_ljgag",attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>')
      }
      
      # resolving years
      d <- damage[between(damage$date, input$period[1], input$period[2]),]
      h <- casualties[between(casualties$date, input$period[1], input$period[2]),]
      t <- tornadoes[between(tornadoes$date, input$period[1], input$period[2]),]
      
      # resolving single/multiple states      
      if (input$state != "All") { 
        # rendering bar plot
        bars <- t        
        output$barPlot <- renderBarPlot(bars, input)
        
        # subsetting
        d <- d[d$state == input$state,]
        h <- h[h$state == input$state,] 
        t <- t[t$state == input$state,] 
        
        # zooming
        coordinates <- states[states$state == input$state, 5:6]
        map <- map %>% setView(lat = coordinates[1], lng = coordinates[2], zoom = 5)                
      }
      
      if (input$state == "All") {
        total <- nrow(subset(tornadoes, between(date, input$period[1], input$period[2])))
      } else {
        total <- nrow(subset(tornadoes, between(date, input$period[1], input$period[2]) & state == input$state))
      }
      
      # rendering stats
      output$stats_total <- renderStatsTotal(input, c(input$period[1], input$period[2], total, sum(h$fatalities), sum(h$injuries), sum(d$properties + d$crops)))
      output$stats_per_year <- renderStatsPerYear(input, c(input$period[1], input$period[2], total, sum(h$fatalities), sum(h$injuries), sum(d$properties + d$crops)))
      
      # rendering table, trend and month-wise histogram
      output$myTable <- renderMyTables(t, input)
      output$trendPlot <- renderTrendPlot(t, input)
      output$histogramPlot <- renderHistogramPlot(t, input)
      
      if (input$type == 'choropleth') {                
        if (input$state == "All") {
          s_json <- states_json
          s_json$features <- lapply(s_json$features, function(feat) {
            feat$properties$style <- list(fillColor = qpal(input$layer_choropleth)(density(feat$properties, input$layer_choropleth)),
                                          weight = 1,
                                          color = 'black',
                                          opacity = 1,
                                          dashArray = '3',
                                          fillOpacity = 0.6
            )
            feat
          })          
        } else {
          # subsetting features to display a single state
          s_json <- states_json$features[which(states$state == input$state)]
          s_json[[1]]$properties$style <- list(fillColor = qpal(input$layer_choropleth)(density(s_json[[1]]$properties, input$layer_choropleth)),
                                               weight = 1,
                                               color = 'black',
                                               opacity = 1,
                                               dashArray = '3',
                                               fillOpacity = 0.6) 
        }
        
        # adding features
        map <- map %>% addGeoJSON(s_json)
        map        
      } else {         
        # Displaying Markers according to input layer    
        if ("Properties" %in% input$layer) {
          map <- map %>% addCircleMarkers(data = d, radius = ~log(d$properties), popup = contentUSD("Properties.", as.character(d$properties), d$date), stroke = F, fillOpacity = 0.5, color = "darkgoldenrod") 
        }
        
        if ("Crops" %in% input$layer) {
          map <- map %>% addCircleMarkers(data = d, radius = ~log(d$crops), popup = contentUSD("Corps.", as.character(d$crops), d$date), stroke = F, fillOpacity = 0.5, color = "darkolivegreen") 
        }
        
        if ("Injuries" %in% input$layer) {
          map <- map %>% addCircleMarkers(data = h, radius = ~log(h$injuries), popup = content("Injuries", as.character(h$injuries), h$date), stroke = F, fillOpacity = 0.5, color = "navy")         
        } 
        
        if ("Fatalities" %in% input$layer) {
          map <- map %>% addCircleMarkers(data = h, radius = ~log(h$fatalities), popup = content("Fatalities", as.character(h$fatalities), h$date), stroke = F, fillOpacity = 0.5, color = "crimson") 
        }
      }       
      map
    }
    )
    
    # renders the map legend
    output$legend <- renderUI({
      if (input$type == "choropleth") {
        div(span(style = sprintf("width:0.7em; height:0.7em; background-color:%s; display:inline-block;", palette[[input$layer_choropleth]][1])), span(input$layer_choropleth, style = "font-size: 0.7em; color: gray;"))        
      } else {
        div(
          span(style = "width:0.7em; height:0.7em; background-color:crimson; display:inline-block;"), span("Fatalities", style = "font-size: 0.7em; color: gray;"), 
          span(style = "width:0.7em; height:0.7em; background-color:navy; display:inline-block; margin-left: 5px"), span("Injuries", style = "font-size: 0.7em; color: gray;"),
          span(style = "width:0.7em; height:0.7em; background-color:darkolivegreen; display:inline-block;margin-left: 5px"), span("Crops", style = "font-size: 0.7em; color: gray;"),
          span(style = "width:0.7em; height:0.7em; background-color:darkgoldenrod; display:inline-block;margin-left: 5px"), span("Properties", style = "font-size: 0.7em; color: gray;")            
        )}
    }
    )
    
    # renders plot when box widgets changes. 
    output$myTable <- renderMyTables(t, input)
    output$trendPlot <- renderTrendPlot(t, input)
    output$histogramPlot <- renderHistogramPlot(t, input)
    output$state <- renderText({input$state})
    output$stats_total <- renderStatsTotal(input, c(input$period[1], input$period[2], total, sum(h$fatalities), sum(h$injuries), sum(d$properties + d$crops))) 
  }
)