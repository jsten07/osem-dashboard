#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# To deploy on an own server see: https://posit.co/download/shiny-server/ 

# library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(opensensmapr)

#' group senseboxes by grouptags and filter out small number of grouptags
#' @param boxes List of boxes, returned by opensensmapr::osem_boxes()
#' @return List of grouptags of boxes ordered by number of grouptags
filter_by_group <- function(boxes) {
  grouptags = boxes %>%
    group_by(grouptag) %>%
    filter(length(grouptag) >= 10) %>%
    filter(!is.na(grouptag)) %>%
    filter(grouptag != '') %>%
    mutate(count = row_number(locationtimestamp)) %>%
    summarise(count = max(count)) %>%
    arrange(desc(count)) %>%
    select(grouptag)
  
  return(grouptags)
}

#' group boxes by model and filter for small groups of models
#' @param boxes List of boxes, returned by opensensmapr::osem_boxes()
#' @return List of models of boxes ordered by number of models
filter_by_model <- function(boxes) {
  models = boxes %>%
    group_by(model) %>%
    filter(length(model) >= 10) %>%
    filter(!is.na(model)) %>%
    filter(model != '') %>%
    mutate(count = row_number(locationtimestamp)) %>%
    summarise(count = max(count)) %>%
    arrange(desc(count)) %>%
    select(model)
  
  return(models)
  
}

# Read boxes from rds file; much faster than requesting by osem_boxes; BUT: need to be updated manually
# TODO: When setting up dashboard on server, update boxes automatically, e.g. every night by running the following in a cron job or similar
    # saveRDS(osem_boxes(), "./data/recent_boxes")
boxes_all <- readRDS("./data/recent_boxes")

# get date of most recent update
update_date = max(boxes_all$updatedAt)

# Get grouptags and models of boxes
grouptags = filter_by_group(boxes_all)
models = filter_by_model(boxes_all)

#' update global variables by requesting most current boxes from openSenseMap API
#' Save recent boxes to rds file to be retrievable faster
#' Update grouptags, models, update date
#' @return updated boxes
update_data <- function() {
  print("updating data")
  
  boxes_all <<- osem_boxes()
  
  saveRDS(boxes_all, "./data/recent_boxes")
  
  grouptags <<- filter_by_group(boxes_all)
  
  models <<- filter_by_model(boxes_all)
  
  update_date <<- Sys.time()
  
  print(paste("data updated on ", update_date))
  
  return(boxes_all)
}


# Define user interface
ui <- dashboardPage(
  skin = 'green',
  # Header, displayed at top-left of website and as browser-tab text
  dashboardHeader(title = "oSeM-Stats"),
  # Sidebar
  dashboardSidebar(sidebarMenu(
    # Dashboard menu item; not working at shinyapp.io; also not really necessary, since there is the opensensemap
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    # Map menu item
    menuItem("Maps",
             tabName = "maps",
             icon = icon("map"))
  ),
  # disable sidebar, as map is not working at shinyapps.io
  disable = TRUE),
  # Body of website
  dashboardBody(tabItems(
    # Elements of dashboard tab
    tabItem(
      tabName = "dashboard",
      
      # First row
      fluidRow(
        # graph of registered boxes by date and exposure
        box(title = "Boxen registriert (nach Exposition)",
            width = 9,
            plotOutput("distPlot")),
        # box and button to update data
        box(
          title = "Daten-Datum",
          width = 3, 
          "Die dargestellten Daten sind zuletzt aktualisiert worden am: ",
          br(),
          strong(textOutput("dataDate")),
          # br(),
          "Wenn du die neuesten Daten haben möchtest, drücke den Knopf. Dies kann einige Momente dauern.",
          br(),
          actionButton("update",
                       "Update")
        )
      ),
      # Second row
      fluidRow(
        # boxplots of activity duration of boxes by year
        box(
          title = "Aktivität",
          solidHeader = T,
          width = 8,
          plotOutput("durationPerYear")
        ),
        # diagram controls
        box(
          title = "Filter für Diagramme",
          solidHeader = T,
          width = 4,
          # slider to control the date of data displayed in graphs
          sliderInput(
            "bins",
            "Registrierung:",
            min = as.Date(min(boxes_all$locationtimestamp)),
            max = as.Date(max(boxes_all$updatedAt)),
            value = c(as.Date(min(
              boxes_all$locationtimestamp
            )),
            as.Date(max(
              boxes_all$updatedAt
            )))
          ),
          
          box(
            # dropdown to control the grouptag of data displayed in graphs
            selectInput("grouptag",
                        "Grouptag:",
                        choices = c("all", grouptags)),
            # dropdown to control the model of data displayed in graphs
            selectInput("model",
                        "Model:",
                        choices = c("all", models)),
            # button to apply controls
            actionButton("apply",
                         "Filter anwenden")
            
          )
        )
      ),
      # Info boxes; different styles just for testing
      fluidRow(
        infoBoxOutput("newestSensebox"),
        infoBoxOutput("countBoxesTotal"),
        infoBoxOutput("countBoxesNoMeasurement")
      ),
      fluidRow(infoBoxOutput("new24h"),
               infoBoxOutput("new30d")),
      fluidRow(
        valueBoxOutput("phenomenon"),
        valueBoxOutput("measured24h"),
        valueBoxOutput("measured30d")
      )
    ),
    # map tab; does not work in shinyapps.io and should not be necessary as we have the openSenseMap (opensensemap.org)
    tabItem(tabName = "maps",
            mainPanel(
              plotOutput("mapBoxesRegistered", width = "100%")
            ))
  ))
)


# Define server logic to feed UI
server <- function(input, output, session) {
  # filter boxes depending on the diagram controls
  filter_boxes <- reactive({
    # apply when 'button' is used
    input$apply
    
    isolate({
      # only filter for date when grouptag and model are not selected
      if (input$grouptag == "all" & input$model == "all") {
        boxes_all %>%
          filter(
            as.Date(locationtimestamp) > input$bins[1] &
              as.Date(locationtimestamp) < input$bins[2]
          )
      }
      # filter for grouptag and date
      else if (input$grouptag != "all" &
               input$model == "all") {
        boxes_all %>%
          filter(grouptag == input$grouptag) %>%
          filter(
            as.Date(locationtimestamp) > input$bins[1] &
              as.Date(locationtimestamp) < input$bins[2]
          )
      }
      # filter for model and date
      else if (input$grouptag == "all" &
               input$model != "all") {
        boxes_all %>%
          filter(model == input$model) %>%
          filter(
            as.Date(locationtimestamp) > input$bins[1] &
              as.Date(locationtimestamp) < input$bins[2]
          )
      }
      # filter for grouptag, model and date
      else {
        boxes_all %>%
          filter(grouptag == input$grouptag &
                   model == input$model) %>%
          filter(
            as.Date(locationtimestamp) > input$bins[1] &
              as.Date(locationtimestamp) < input$bins[2]
          )
      }
    })
  })
  
  # graph of registered boxes by date and exposure (row one, box one)
  output$distPlot <- renderPlot({
    # change data when filter is applied
    boxes_filtered = filter_boxes()
    
    # group by exposure and count boxes per date
    exposure_counts = boxes_filtered %>%
      group_by(exposure) %>%
      mutate(count = row_number(locationtimestamp))
    
    # colors of lines
    exposure_colors = c(
      indoor = 'red',
      outdoor = 'lightgreen',
      mobile = 'blue',
      unknown = 'darkgrey'
    )
    
    # Create plot
    ggplot(exposure_counts,
           aes(x = locationtimestamp,
               y = count,
               colour = exposure)) +
      geom_line() +
      scale_colour_manual(values = exposure_colors) +
      xlab('Datum Registrierung') + ylab('Anzahl der Senseboxen') + 
      theme(text = element_text(size=16))
  })
  
  # activity boxplots (row two, box one)
  output$durationPerYear <- renderPlot({
    # change data when filter is applied
    boxes_filtered = filter_boxes()
    
    # groupt by year and get duration of activity
    duration = boxes_filtered %>%
      mutate(year = cut(as.Date(locationtimestamp),
                        breaks = 'year')) %>%
      group_by(year) %>%
      filter(!is.na(lastMeasurement)) %>%
      mutate(duration = difftime(lastMeasurement,
                                 locationtimestamp,
                                 units = 'days')) %>%
      filter(duration >= 0)
    
    # Create plot
    ggplot(duration, aes(x = substr(as.character(year), 0, 4), y = duration)) +
      geom_boxplot() +
      coord_flip() + ylab('Aktivität in Tagen') + xlab('Jahr der Registrierung') + 
      theme(text = element_text(size=16))
  })
  
  # Date of last update of data (row one, box two)
  output$dataDate <- renderText({
    as.character(update_date)
  })
  
  # infobox: name of last sensebox created
  output$newestSensebox <- renderInfoBox({
    infoBox(
      title = "Neueste Sensebox",
      subtitle = "Name",
      value = boxes_all[boxes_all$createdAt == max(boxes_all$createdAt),]$name,
      color = "green",
      icon = icon("clock")
    )
  })
  
  # infobox: number of boxes created
  output$countBoxesTotal <- renderInfoBox({
    infoBox(
      title = "Gesamt Registriert",
      subtitle = "Anzahl Senseboxen",
      value = nrow(boxes_all),
      color = "olive",
      icon = icon("handshake")
    )
  })
  
  # infobox: number of boxes that never send a measurement to osem
  output$countBoxesNoMeasurement <- renderInfoBox({
    infoBox(
      title = "Nie Gemessen",
      subtitle = "Anzahl Senseboxen",
      value = nrow(dplyr::filter(boxes_all, is.na(
        lastMeasurement
      ))),
      color = "red",
      icon = icon("times-circle")
    )
  })
  
  # infobox: number of boxes created within the last 24 hours
  output$new24h <- renderInfoBox({
    boxes24h <- boxes_all %>%
      filter(createdAt > (now() - days(1)))
    infoBox(
      title = "Registrierung Letzte 24 Stunden",
      subtitle = "Anzahl Senseboxen",
      value = nrow(boxes24h),
      color = "aqua",
      icon = icon("hourglass"),
      # fill = TRUE
    )
  })
  
  # infobox: number of boxes created within the last 30 days
  output$new30d <- renderInfoBox({
    boxes30d <- boxes_all %>%
      filter(createdAt > (now() - days(30)))
    infoBox(
      title = "Registrierung Letzte 30 Tage",
      subtitle = "Anzahl Senseboxen",
      value = nrow(boxes30d),
      color = "teal",
      icon = icon("calendar"),
      # fill = TRUE
    )
  })
  
  # TODO: Evaluating phenomena takes very long -> display other data already before finishing this
  # infobox: phenomenon with the most sensors registered
  output$phenomenon <- renderValueBox({
    phenomenon <- boxes_all %>%
      osem_phenomena()
    phen1 <- names(phenomenon)[1]
    infoBox(
      title = "Häufigstes Phänomen",
      subtitle = paste("Anzahl: ", phenomenon[1]),
      value = phen1,
      color = "blue",
      icon = icon("sun")
    )
  })
  
  # infobox: number of senseboxes that send a measurement within the last 24 hours
  output$measured24h <- renderValueBox({
    measured24h <- boxes_all %>%
      filter(lastMeasurement > (now() - days(1)))
    infoBox(
      title = "Messung Letzte 24 Stunden",
      subtitle = "Anzahl Senseboxen",
      value = nrow(measured24h),
      color = "navy",
      icon = icon("thermometer-full")
    )
  })
  
  # infobox: number of senseboxes that send a measurement within the last 30 days
  output$measured30d <- renderValueBox({
    measured30d <- boxes_all %>%
      filter(lastMeasurement > (now() - days(30)))
    infoBox(
      title = "Messung Letzte 30 Tage",
      subtitle = "Anzahl Senseboxen",
      value = nrow(measured30d),
      color = "purple",
      icon = icon("thermometer-empty")
    )
  })
  
  # render map of senseboxes, filtered by slider date (extra tab)
  output$mapBoxesRegistered <- renderPlot({
    boxes_filtered = boxes_all %>%
      filter(
        as.Date(locationtimestamp) > input$bins[1] &
          as.Date(locationtimestamp) < input$bins[2]
      )
    
    plot(boxes_filtered)
  })
  
  # update date slider (used after data is updated manually; not necessary for app startup)
  updateSliderInput(
    inputId = "bins",
    min = as.Date(min(boxes_all$locationtimestamp)),
    max = as.Date(max(boxes_all$updatedAt)),
    value = c(as.Date(min(
      boxes_all$locationtimestamp
    )),
    as.Date(max(
      boxes_all$locationtimestamp
    )))
  )
  
  # when update button is pressed, update data in background
  observeEvent(input$update, {
    not_id <- showNotification(
      "Todays' data is downloaded in the background.",
      type = "warning",
      duration = NULL,
      closeButton = FALSE
    )
    on.exit(removeNotification(not_id))
    print("update data in server")
    boxes_all <- update_data()
    filter_boxes()
    showNotification("Data refreshed.",
                     type = "message")
    session$reload() # reload session to show new data
  })
}

# Run the application
shinyApp(ui = ui, server = server)
