#
# OSU CS361: Intro to Software Development 1
# Individual Project
# Winter 2022
#
# Author: Maggie Liu
# Version: 1.2.0
# Description: A web app that allows a user to choose a city, and then takes 
#               air quality data for that city and displays a historical
#               trend. Users can add multiple cities to the same trend.
#              

library(shiny)
library(shiny.router)
library(shinyLP)
library(ggplot2)
library(reticulate)
library(Rcpp)
subprocess <- reticulate::import("subprocess")


# Initial page - starts a new search
new_search_page <- div(
    
    headerPanel("Choose a city from the map below"),
    
    mainPanel(
        
        # instruction text
        h5("Choose a pre-selected city from the map, or enter the name or ZIP 
           code of a city of your choice to see air quality trends from that 
           area."),
        p("TIP: not every city has data available", 
          style ="font-size:10pt;"),
        
        br(),
        
        # map with pre-selected cities to choose from
        imageOutput("map_image"),
        
        br(),
        br(),
        
        fluidRow(
            # zip submission form
            column(width = 5, textInput("zip", "Or, enter your own city / 5-digit zip code:")), 
            column(7, style = "margin-top:30px;", 
                   actionButton("new_search", label = "Submit", icon = NULL, width = NULL))
        )
    )
)

# Page to add more cities to existing trend
add_page <- div(
    
    headerPanel("Choose another city from the map below"),
    
    mainPanel(
        
        # instruction text
        h5("Choose another pre-selected city from the map, or enter the ZIP code
        of your choice to see air quality trends from that area."),
        p("TIP: Comparing more than 2 cities at once may make trends difficult
           to read.", style ="font-size:10pt;"),
        
        # map with pre-selected cities to choose from
        jumbotron("Map From Image Retrieval Service Goes Here!", 
                  "<< Map with action buttons overlaid on ~5 cities, maybe
                  with current selected location shown >>", 
                  button=FALSE),
        
        # zip submission form
        textInput("add_zip", "Or, enter a 5-digit zip code:"), 
        actionButton("added_city", label = "Submit", icon = NULL, width = NULL),
        actionButton("trend", label = "Go Back", icon = NULL, width = NULL)
    )
)

# Trend page
dash_page <- div(
    titlePanel("Trends"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Move sliders to change the ranges on the graph"),
            br(),
            
            #TODO: update range based on limits in data file unless doing a standard pull
            sliderInput("time_slide", "Time",
                        min = as.Date("2018-12-31"),
                        max = as.Date("2021-01-01"),
                        value = c(as.Date("2018-12-31"), as.Date("2021-01-01")), 
                        timeFormat = "%F", dragRange = TRUE),
            sliderInput("aq_slide", "PM2.5",
                        min = 0,
                        max = 300,
                        value = c(0, 300)),
            
            actionButton("start_over", label = "New Search", icon = NULL),
            actionButton("add_city", label = "Add City", icon = NULL)
        ),
        
        # Display plot
        mainPanel(
            plotOutput("aqPlot")
        )
    )
)

# Router
router <- make_router(
    route("/", new_search_page),
    route("trends", dash_page),
    route("add-city", add_page)
)


# UI
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "yeti"),
    title = "Historical Air Quality Trends",
    fluid = TRUE,
    router$ui
)

# Server
server <- function(input, output, session) {
    router$server(input, output, session)
    thematic::thematic_shiny()
    
    output$map_image <- renderImage({
        list(src = "stock USA map.png", contentType = 'image/png')
    })
    
    #
    # --- Data pull ---
    #
    #   1) Write desired ZIP to 'historic_aqi.txt';
    #   2) Run microservice 'get_historic_pm25.py';
    #   3) Read data from 'pm25py.csv'
    #
    #   Source: AQICN database on dbnomics
    #
    # TODO: make data pull a module
    
#
# --- EVENT HANDLERS ---
#
    # performs new search
    observeEvent(input$new_search,{
        # check if input blank
        if (input$zip == ""){
            showModal(
                modalDialog(title = "Error",
                            "Please enter a zip code or city name, or choose a city from the map.",
                            footer = tagList(
                                modalButton("Ok."))
                )
            )
        } else {
            
            # fetch data
            writeLines(as.character(input$zip), "historic_aqi.txt")
            subprocess$run('py .\\get_historic_pm25.py')
            
            # Check if the request was successful
            confirm_request <- readLines("historic_aqi.txt")
            if (confirm_request == "Location not found."){
                showModal(
                    modalDialog(title = "Sorry!",
                                "No data found for that location. Please try another.",
                                footer = tagList(
                                    modalButton("Ok."))
                    )
                )
            } else {
                df <- read.csv("pm25py.csv")
                colnames(df) <- c('Index', 'Date', as.character(confirm_request))
                df$Date <- as.Date(df$Date)
                output$aqPlot <- renderPlot({
                    
                    # render the plot with the data frame
                    ggplot(data = df, aes(Date, as.character(confirm_request))) + 
                        geom_line() +
                        labs(x = "Date", y = "PM2.5", 
                             title = paste("Air quality trend for", names(df)[3])) +
                        scale_x_date(date_labels = "%b-%Y") +
                        scale_y_continuous()
                        xlim(input$time_slide) +
                        ylim(input$aq_slide)
                    
                    #TODO: add dynamic legend & title
                    
                }, res = 96)
                
                # go to the trend page
                change_page("trends")
                
            }
        }
    })
    
    # updates df with newly-requested city
    observeEvent(input$added_city,{
        
        if (input$add_zip == ""){
            
            # check if input blank
            showModal(
                modalDialog(title = "Error",
                            "Please enter a zip code or city name, or choose a city from the map.",
                            footer = tagList(
                                modalButton("Ok."))
                )
            )
        } else if (ncol(df) == 5){
            
            # check if 3 cities already trended
            showModal(
                modalDialog(title = "Error",
                            "Only 3 cities can be trended at one time.",
                            footer = tagList(
                                modalButton("Ok."))
                )
            )
        } else {
        
            # fetch data
            writeLines(as.character(input$add_zip), "historic_aqi.txt")
            subprocess$run('py .\\get_historic_pm25.py')
            
            # Check if the request was successful
            confirm_request <- readLines("historic_aqi.txt")
            if (confirm_request == "Location not found."){
                showModal(
                    modalDialog(title = "Sorry!",
                                "No data found for that location. Please try another.",
                                footer = tagList(
                                    modalButton("Ok."))
                    )
                )
            } else{
                df_add <- read.csv("pm25py.csv")
                colnames(df_add) <- c('Index', 'Date', confirm_request)
                df <- cbind(df, df_add[3])
                output$aqPlot <- renderPlot({
                    
                    # render the plot with the data frame
                    ggplot(df, aes(x = Date, y = names(df[3:4]), color=variable)) + 
                        geom_line() +
                        labs(x = "Date", y = "PM2.5", 
                             title = "Air quality trend for - Chosen ZIP Codes") +
                        scale_x_date(date_labels = "%b-%Y") +
                        scale_y_continuous() +
                        xlim(input$time_slide) +
                        ylim(input$aq_slide)
                    
                    #TODO: add dynamic legend & title
                    
                }, res = 96)
                
                # go to the trend page
                change_page("trends")
            }
        }
    })
    
    # goes back to starting screen.
    observeEvent(input$start_over,{
        showModal(
            modalDialog(title = "Caution",
            "Starting a new search will clear all existing data.",
            footer = tagList(
                modalButton("Cancel"),
                actionButton("confirm_new", "Confirm new search")) 
            )
        )
    })
    
    # goes to the new search screen
    observeEvent(input$confirm_new,{
        change_page("/")
        # TODO: update slider min/max here
        removeModal()
    })
    
    # goes to the add-a-city screen
    observeEvent(input$add_city,{
        change_page("add-city")
    })
    
    # goes to the trend screen
    observeEvent(input$trend,{
        change_page("trends")
    })
    
    # TODO: do any aggregation to make it more readable? 
#
# --- Generate Plot ---
#
    
}

shinyApp(ui = ui, server = server)
