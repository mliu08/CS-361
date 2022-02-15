#
# OSU CS361: Intro to Software Development 1
# Individual Project
# Winter 2022
#
# Author: Maggie Liu
# Version: 1.0.0
# Description: A web app that allows a user to choose a city, and then takes 
#               EPA air quality data for that city and displays a historical
#               trend. Users can have up to 3 trends on the same graph.
#              

library(shiny)
library(shiny.router)
library(ggplot2)
library(RAQSAPI)


# Landing page
map_page <- div(
    
    headerPanel("Choose a city from the map below"),
    
    mainPanel(
        
        # instruction text
        h5("Choose a pre-selected city from the map, or enter the ZIP code of 
           your choice to see air quality trends from that area."),
        
        # map with pre-selected cities to choose from
        jumbotron("Map Goes Here!", "<< Map with visual choices>> ", 
                  button=FALSE),
        
        # zip submission form
        textInput("zip", "Or, enter a 5-digit zip code:"), 
        actionButton("submit", label = "Submit", icon = NULL, width = NULL)
        
        # TODO: data validation alert if not a zip, or not available in the db
    )
)

# Trend page
dash_page <- div(
    titlePanel("Trends"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Move sliders to change the ranges on the graph!"),
            
            #TODO: range should update based on limits in data file unless doing a standard pull
            sliderInput("time_slide", "Time",
                        min = as.Date("2015-01-01"),
                        max = as.Date("2021-01-01"),
                        value = c(as.Date("2015-01-01"), as.Date("2021-01-01")), 
                        timeFormat = "%F", dragRange = TRUE),
            sliderInput("aq_slide", "PM2.5",
                        min = 0,
                        max = 500,
                        value = c(0, 500))
        ),
        
        # Display smoothed scatter plot
        mainPanel(
            plotOutput("aqPlot")
        )
    ),
    
    fluidRow(
        actionButton("new", label = "Choose New City")
    )
    
   
)

# Router
router <- make_router(
    route("/", map_page),
    route("trends", dash_page)
)


# UI
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "flatly"),
    title = "Historical Air Quality Trends",
    
    # TODO: make this a conditional bar so pageflow is more constrained?
    tags$nav(
        tags$button(a(href = route_link("/"), "Home")),
        tags$button(a(href = route_link("trends"), "Dashboard"))
    ),
    
    router$ui
)

# Server
server <- function(input, output, session) {
    router$server(input, output, session)
    thematic::thematic_shiny()
    
    observeEvent(input$submit,{
        if (count < 2){
            
        }
    })

    output$aqPlot <- renderPlot({
        # fetch data from text file, either:
        #   AQS based on location data provided to the UI
        #   some other, faster source
        
        # TODO: replace dummy data file
        df <- read.csv("dummydata.csv")
        colnames(df) <- c('Date', 'PM2.5')
        df$Date <- as.Date(df$Date, forma = "%m/%d/%y")
        
        # TODO: do any aggregation to make it more readable?
        
        # render the plot with the data frame
        ggplot(df, aes(Date, PM2.5)) + 
            geom_point() +
            labs(x = "Date", y = "PM2.5", title = "Air quality trend for - CITY NAME") +
            scale_x_date(date_labels = "%b-%Y")
        
        #TODO: add axis labels, dynamic title, connect sliders to axes
        
    }, res = 96)
}

shinyApp(ui = ui, server = server)
