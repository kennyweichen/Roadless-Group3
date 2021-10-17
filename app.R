#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(leaflet)
library(mosaic)
library(RSQLite)
library(shinyWidgets)
library(shinydashboard)


# Establish database for storing data
mydb <- dbConnect(RSQLite::SQLite(), ":memory:")
init_data <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(init_data) <- c("latitude", "longitude", "us", "road")
dbWriteTable(mydb, "location", init_data, overwrite = TRUE)
dbGetQuery(mydb, "SELECT * FROM location")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
                # setBackgroundColor(color = "ghostwhite"),
                useShinydashboard(),
    # Application title
    titlePanel("Roadless Group-3: Bobby and Kenny", windowTitle = "Roadless"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton(inputId = "sample",
                         label = "Sample Data"),
            actionButton(inputId = "append",
                         label = "Add to Data"),
            checkboxInput(inputId = "USA",
                           label = "Is this in the US?",
                          value = F),
            
            #### Second Button ####
            #  to only appear if USA checkbox is checked
            conditionalPanel(condition = "input.USA == 1", 
                             checkboxInput("near_road", label = "Is it within 1 mile of a road?", value = F)
            ),
            
            
            downloadButton("downloadData", "Download"),
            tableOutput("table")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            verbatimTextOutput(outputId = "text"),
            leafletOutput("map"),
            valueBox(
                uiOutput("total_count"), "Total Sampled", icon = icon("database"), color = "red"
            ),
            valueBox(
                uiOutput("us_count"), "In the US", icon = icon("flag-usa"), color = "purple"
            ),
            valueBox(
                uiOutput("road_count"), "Near a Road", icon = icon("road")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    sample_coordinates <- eventReactive(input$sample, {
        updateCheckboxInput(session = getDefaultReactiveDomain() , "USA", value = FALSE)
        updateCheckboxInput(session = getDefaultReactiveDomain() , "near_road", value = FALSE)
        rgeo(1, latlim = c(25,50), lonlim = c(-65, -125)) 
    }, ignoreNULL = FALSE)
    
    
    observeEvent(input$USA, {
        if(!input$USA) {
            updateCheckboxInput(session = getDefaultReactiveDomain() , "near_road", value = FALSE)
        } 
        
    })
    
    
    output$total_count <- renderText({
        prettyNum(nrow(db_contents()), big.mark=",")
    })
    
    output$us_count <- renderText({
        prettyNum(nrow(filter(db_contents(), us == 1)), big.mark=",")
    })
    
    output$road_count <- renderText({
        prettyNum(nrow(filter(db_contents(), road == 1)), big.mark=",")
    })
    
    output$map <- renderLeaflet({
        leaflet_map(sample_coordinates(), radius = 1, unit = "miles")  %>%
            setView(-95, 37.5, zoom = 3.4)
            # addProviderTiles(providers$Stamen.TonerLite,
            #                  options = providerTileOptions(noWrap = TRUE)
            # ) 
            # addMarkers(data = sample_coordinates())
    })
    
    output$text <- renderText({paste0("Location Status:\nLatitude: ", sample_coordinates()$lat, "\n",
                                     "Longitude: ", sample_coordinates()$lon, "\n",
                                     "Is it in the US?: ", input$USA, "\n",
                                     "Is it within 1 mile of a road?: ", input$near_road, "\n")})
                                     # "Running Tally: There are ", nrow(db_contents()), " location(s) sampled. ",
                                     # nrow(filter(db_contents(), us == 1)), " are in the", "\n",
                                     # "United States while ",
                                     # nrow(filter(db_contents(), road == 1)), 
                                     # " of those are within 1 mile ", 
                                     # "of a road.")})

    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("download.csv")
        },
        content = function(file) {
            write.csv(db_contents(), file, row.names = FALSE)
        }
    )

    observeEvent(input$append, {
        
        data <- dbGetQuery(mydb, "SELECT * FROM location")
        
        new_data <- tibble(
            latitude = sample_coordinates()$lat,
            longitude = sample_coordinates()$lon,
            us = input$USA,
            road = input$near_road
        )
    
        if(!(sample_coordinates()$lat %in% data$lat & sample_coordinates()$lon %in% data$lon)) {
        dbWriteTable(mydb, "location", new_data, append = TRUE)
        } else {
            showModal(modalDialog(
                title = "Oh no!",
                paste0("This location has already been recorded you silly little goose!"),
                easyClose = TRUE,
                footer = NULL
            ))
        }
    }, priority = 1)


    db_contents <- eventReactive(input$append, {
        cat("button =", input$sample, "\n")
        data <- dbGetQuery(mydb, "SELECT * FROM location")
        return(data)
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
    output$table <- renderTable({
        db_contents()
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
