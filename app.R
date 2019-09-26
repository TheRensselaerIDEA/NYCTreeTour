# This is /data/CampfireShiny/TreeTourMwshiny/app.R

if (!require("rlang")) {
  install.packages("rlang",  dependencies = TRUE)
  library(rlang)
}

if (!require("Rcpp")) {
  install.packages("Rcpp", dependencies = TRUE)
  library(Rcpp)
}
if (!require("utils")) {
  install.packages("utils", dependencies = TRUE)
  library(utils)
}
if (!require("R6")) {
  install.packages("R6", dependencies = TRUE)
  library(R6)
}

if (!require("later")) {
  install.packages("later",  dependencies = TRUE)
  library(later)
}

if (!require("promises")) {
  install.packages("promises",  dependencies = TRUE)
  library(promises)
}
if (!require("BH")) {
  install.packages("BH", dependencies = TRUE)
  library(BH)
}

if (!require("plyr")) {
  install.packages("plyr", dependencies = TRUE)
  library(plyr)
}

if (!require("httpuv")) {
  install.packages("httpuv",  dependencies = TRUE)
  library(httpuv)
}
if (!require("htmltools")) {
  install.packages("htmltools", dependencies = TRUE)
  library(htmltools)
}

if (!require("xtable")) {
  install.packages("xtable",  dependencies = TRUE)
  library(xtable)
}
if (!require("sourcetools")) {
  install.packages("sourcetools", dependencies = TRUE)
  library(sourcetools)
}

if (!require("readr")) {
  install.packages("readr", dependencies = TRUE)
  library(readr)
}

if (!require("tibble")) {
  install.packages("tibble",  dependencies = TRUE)
  library(tibble)
}

if (!require("tidyverse")) {
  install.packages("tidyverse", dependencies = TRUE)
  library(tidyverse)
}


if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}

if (!require("htmlwidgets")) {
  install.packages("htmlwidgets",  dependencies = TRUE)
  library(htmlwidgets)
}

if (!require("leaflet")) {
  install.packages("leaflet",  dependencies = TRUE)
  library(leaflet)
}

if (!require("proj4")) {
  install.packages("proj4",  dependencies = TRUE)
  library(proj4)
}

if (!require("shiny")) {
  install.packages("shiny",  dependencies = TRUE)
  library(shiny)
}

if (!require("devtools")) {
  install.packages("devtools", dependencies = TRUE)
  library(devtools)
}

if (!require("mwshiny")) {
  devtools::install_github("delosh653/mwshiny")
  library(mwshiny)
}

# Hannah's test fix
source("~/data/CampfireShiny/TreeTourMwshiny/mwshiny_fix.R")

# First-things first: load the data!
#tour_locations <<- as_tibble(readRDS("~/data/CampfireShiny/CampusTour/fixed_Queens.rds"))[1:1000,]
tour_locations <<- as_tibble(readRDS("~/data/CampfireShiny/CampusTour/fixed_Queens.rds"))[1:10000,]

# vector of strings that are the names of my windows
win_titles <- c("Controller","Floor", "Wall", "Monitor")

ui_list <- list()

# First we add what we want to see in the Controller to the list
ui_list[["Controller"]] <- fluidPage(
  tags$p(),
  tags$p(),
  fluidRow(column(10, align ="center",
                  h1("New York City Tree Explorer")
  )
  ),
  tags$p(),
  fluidRow(column(10, align ="center",
  selectInput("borough", "Borough:",
              c("Manhattan" = "Manhattan",
                "Queens"="Queens",
                "Staten Island"="Staten Island",
                "Brooklyn"="Brooklyn",
                "Bronx"="Bronx"), selected = "Queens") , 
  actionButton("loaddata", "Load borough data and refresh map") 
  )
  ), 
  tags$p(),
  fluidRow(column(10, align ="center",
                  checkboxGroupInput("checkGroup1", label = "Health of trees:", 
                                     choices = list("Good" = "Good", "Fair" = "Fair", "Poor" = "Poor"),
                                     selected = c("Good", "Fair","Poor")),
                  # selectInput('selectInput2', 'Zipcode of trees:', 
                  #             selected=unique(tour_locations$postcode)[1:10], 
                  #             unique(tour_locations$postcode), multiple=TRUE, selectize=TRUE),
                  selectInput('selectInput3', 'Species of trees:', 
                              selected=c("Norway maple","London planetree","pin oak","honeylocust",
                                         "Sophora","Callery pear","littleleaf linden","silver maple",
                                         "Japanese zelkova","cherry","ginkgo","red maple","American linden",
                                         "eastern redcedar","green ash","sweetgum","Japanese tree lilac",
                                         "silver linden","golden raintree","Amur maple","northern red oak",
                                         "swamp white oak","American hornbeam","crimson king maple","purple-leaf plum",
                                         "sugar maple","'Schubert' chokecherry","Siberian elm","black locust",
                                         "black oak"), 
                              c("red maple","pin oak","London planetree","honeylocust","Amur maple" , 
                                "ginkgo","American linden","American elm","silver maple","black cherry" , 
                                "eastern redcedar","tulip-poplar","Norway maple","Chinese fringetree",
                                "sweetgum" , "Sophora","Callery pear","swamp white oak","willow oak",
                                "Japanese zelkova" , "crab apple","silver linden","sassafras","black oak",
                                "littleleaf linden" , "horse chestnut","cherry","white pine",
                                "Atlantic white cedar","blackgum" , "sycamore maple","northern red oak",
                                "hardy rubber tree","Japanese tree lilac","green ash" , "golden raintree",
                                "Schumard's oak","weeping willow","black locust","Persian ironwood" , 
                                "Siberian elm","Chinese chestnut","scarlet oak","crimson king maple",
                                "white oak" , "Cornelian cherry","sugar maple","arborvitae","purple-leaf plum",
                                "Kentucky coffeetree", "Oklahoma redbud","eastern cottonwood","American hornbeam",
                                "black walnut","Atlas cedar", "silver birch","empress tree","'Schubert' chokecherry",
                                "catalpa","hawthorn" ), multiple=TRUE, selectize=TRUE),
                  tags$p(),
                  tags$p()
  )
  ),
  fluidRow(column(10, align ="center", 
                  plotOutput("healthplot")
  )
  )
)

# Then we add what we want to see in the Floor section
ui_list[["Floor"]] <- fillPage(
  leafletOutput("map", width = "100%", height = "1080px")
)

# Then we add the Wall
ui_list[["Wall"]] <-   fluidPage(
  htmlOutput("panorama")
  )    

# Finally we add the External Monitor
ui_list[["Monitor"]] <- fluidPage(
  htmlOutput("tree_info")
)

# Test Output
ui_list[["Test"]] <- fluidPage(
  textOutput("dataSelect")
)

# Calc section

serv_calc <- list()

# first we do the calculation for input switching
serv_calc[[1]] <- function(calc, sess){
    calc$points <- reactive({
      if (is.null(calc$tour_locations)) {
        # Start-up points
        tour_locations %>% 
          filter(health %in% calc$checkGroup1) %>%
          filter(labels %in% calc$selectInput3)
        # tour_locations %>% 
        #   filter(health %in% calc$checkGroup1) %>%
        #   filter(postcode %in% calc$selectInput2) %>%
        #   filter(labels %in% calc$selectInput3)
      } else {
        # Calculated points
        calc$tour_locations %>% 
          filter(health %in% calc$checkGroup1) %>%
          filter(labels %in% calc$selectInput3)
        # calc$tour_locations %>% 
        #   filter(health %in% calc$checkGroup1) %>%
        #   filter(postcode %in% calc$selectInput2) %>%
        #   filter(labels %in% calc$selectInput3)
      }

  })
}

serv_calc[[2]] <- function(calc, sess){  
  # Determine what was clicked on the map
  # FIX THIS! tour_locations is only ever the start-up borough
  calc$clicked_location <- eventReactive(calc$map_marker_click, {
#    tour_locations %>% 
      calc$tour_locations %>% 
        filter(latitude %in% calc$map_marker_click$lat) %>% 
        filter(longitude %in% calc$map_marker_click$lng) 
  }, ignoreNULL = FALSE)
}

serv_calc[[3]] <- function(calc, sess){
  observe({

  proxy <- leafletProxy("map", data = calc$points()) %>%
    clearMarkers %>%
      addCircleMarkers(data = calc$points(),
                       radius = 10, weight = 1, color = "#777777",
                       fillColor = ~pal(calc$points()$health),
                       fillOpacity = 0.7, popup = ~paste(calc$points()$labels))
    })
}

serv_calc[[4]] <- function(calc, sess){  
  # Mouseover handler (debug only)
  calc$moused_over <- eventReactive(c(calc$map_marker_mouseover, calc$map_marker_mouseout), {
    
    calc$tour_locations %>% 
      filter(latitude %in% calc$map_marker_mouseover$lat) %>% 
      filter(longitude %in% calc$map_marker_mouseover$lng) 
    
  }, ignoreNULL = FALSE)
}

serv_calc[[5]] <- function(calc, sess){  
  # Load a borough's data 
#  calc$tour_locations <- reactive({
  observeEvent(calc$loaddata, {
    # Conditional file load stuff here; load based on calc$borough
    # Set a default first 
    if (is.null(calc$borough)) {
      calc$borough <- "Queens"
#      calc$tour_locations <- readRDS("~/data/CampfireShiny/CampusTour/fixed_Queens.rds")[1:1000,]
      calc$tour_locations <- readRDS("~/data/CampfireShiny/CampusTour/fixed_Queens.rds")[1:10000,]
    } else if ( calc$borough == "Manhattan") {
#      calc$tour_locations <- readRDS("~/data/CampfireShiny/CampusTour/fixed_Manhattan.rds")[1:1000,]
      calc$tour_locations <- readRDS("~/data/CampfireShiny/CampusTour/fixed_Manhattan.rds")[1:10000,]
    } else if ( calc$borough == "Bronx") {
#      calc$tour_locations <- readRDS("~/data/CampfireShiny/CampusTour/fixed_Bronx.rds")[1:1000,]
      calc$tour_locations <- readRDS("~/data/CampfireShiny/CampusTour/fixed_Bronx.rds")[1:10000,]
    } else if ( calc$borough == "Brooklyn") {
#      calc$tour_locations <- readRDS("~/data/CampfireShiny/CampusTour/fixed_Brooklyn.rds")[1:1000,]
      calc$tour_locations <- readRDS("~/data/CampfireShiny/CampusTour/fixed_Brooklyn.rds")[1:10000,]
    } else if ( calc$borough == "Queens") {
#      calc$tour_locations <- readRDS("~/data/CampfireShiny/CampusTour/fixed_Queens.rds")[1:1000,]
      calc$tour_locations <- readRDS("~/data/CampfireShiny/CampusTour/fixed_Queens.rds")[1:10000,]
    } else {
#      calc$tour_locations <- readRDS("~/data/CampfireShiny/CampusTour/fixed_Staten_Island.rds")[1:1000,]
      calc$tour_locations <- readRDS("~/data/CampfireShiny/CampusTour/fixed_Staten_Island.rds")[1:10000,]
    }

  }, ignoreNULL = FALSE)
  
}


pal <- colorFactor(palette = c("red","orange","green"), levels = c("Poor","Fair","Good"))

# Output objects 

serv_out <- list()

serv_out[["map"]] <- function(calc, sess){
  renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Stamen.Terrain,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(mean(calc$tour_locations$longitude), mean(calc$tour_locations$latitude), zoom = 14)
  })
}

serv_out[["panorama"]] <- function(calc, sess){
  # Renders the panorama
  renderUI({
    if(length(calc$clicked_location()$latitude) != 0) {
      # display a "static" street view (ie not the interactive)
      the_url <- paste0("http://orion.tw.rpi.edu/~olyerickson/tree_pano_static.html?lat=",calc$clicked_location()$latitude,"&long=",calc$clicked_location()$longitude)
      #includeHTML(serverValues$latitude)
      redirectScript <- paste0("window = window.open('", the_url, "');")
      tags$script(HTML(redirectScript))
    } else {
      # includeHTML("http://orion.tw.rpi.edu/~olyerickson/rpi_logo_wall_2.html")
      redirectScript <- paste0("window = window.open('", "http://orion.tw.rpi.edu/~olyerickson/rpi_logo_wall.html", "');")
      tags$script(HTML(redirectScript))
    }
  })
  
}   

serv_out[["tree_info"]] <- function(calc, sess){
  # Renders the tree info 
  renderUI({
    if(length(calc$clicked_location()$tour_text) != 0) {
      redirectScript <- paste0("window = window.open('", calc$clicked_location()$tour_text, "');")
      #print(serverValues$text)
      tags$script(HTML(redirectScript))
    } else {
      redirectScript <- paste0("window = window.open('", 
                               "https://data.cityofnewyork.us/Environment/2015-Street-Tree-Census-Tree-Data/uvpi-gqnh", "');")
      tags$script(HTML(redirectScript))
    }
  })
}

serv_out[["healthplot"]] <- function(calc, sess){
  # Sonny's health plot 
  renderPlot({
    # Sonny's approach to getting the count
    new_data2_df  <- calc$points()
    colnames(new_data2_df)[colnames(new_data2_df)=="labels"] <-"Species"
    # Construct the plot
    ggplot(new_data2_df, aes(Species, stat(count))) + 
      geom_bar(aes(fill = health), position = "dodge") +
      scale_fill_manual(values=c("Fair"="orange", "Good"="green", "Poor"="red")) + 
      theme(axis.text.x=element_text(angle=45, hjust=1, vjust=0.5), text=element_text(size=25)) +
      ggtitle(paste0("Health of Trees: Borough of ", calc$borough)) +
      theme(plot.title = element_text(size=25))
  })
}

serv_out[["dataSelect"]] <- function(calc, sess){
  renderText({
    calc$tour_locations$borough[1]
    })
}


#mwsApp(win_titles, ui_list, serv_calc, serv_out, depend)
# This is the new way...
mwsApp(ui_list, serv_calc, serv_out)