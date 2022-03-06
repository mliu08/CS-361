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
library(ggplot2)
library(leaflet)
library(reticulate)
library(Rcpp)
subprocess <- reticulate::import("subprocess")


# Initial page - starts a new search
new_search_page <- div(
    headerPanel("Choose a city from the map below"),
    mainPanel(
        # instruction text
        h5("Choose a pre-selected city from the map, or enter the name or ZIP 
           code of a US city of your choice to see air quality trends from that 
           area."),
        p("TIP: not every city has data available", style ="font-size:10pt;"),
        
        br(),
        
        # map with pre-selected cities to choose from
        leafletOutput('usa_map'),

        br(),
        br(),
        fluidRow(
            # zip submission form
            column(width = 7, textInput("zip", "Or, enter your own city / 5-digit zip code:")), 
            column(5, style = "margin-top:30px;", 
                   actionButton("new_search", label = "Submit", icon = NULL, width = NULL))),
    )
)

# Page to add more cities to existing trend
add_page <- div(
    headerPanel("Choose another city from the map below"),
    mainPanel(
        h5("Choose another pre-selected city from the map, or enter a US ZIP code 
           to see air quality trends from that area."),
        p("TIP: Comparing more than 2 cities at once may make trends difficult
           to read.", style ="font-size:10pt;"),
        
        br(),
        
        # map with pre-selected cities to choose from
        #leafletOutput('usa_map'),
        
        br(),
        br(),
        
        # zip submission form
        textInput("add_zip", "Or, enter a 5-digit zip code:"), 
        actionButton("added_city", label = "Submit", icon = NULL, width = NULL),
        actionButton("trend", label = "Go Back", icon = NULL, width = NULL)
    )
)

# Trend page
dash_page <- div(
    titlePanel("Trends"),
    
    # Sidebar with a slider for chart axes
    sidebarLayout(
        sidebarPanel(
            helpText("Move sliders to change the ranges on the graph"),
            br(),
            
            #TODO: update range based on limits in data file unless doing a standard pull
            sliderInput("time_slide", "Time",
                        min = as.Date("2018-12-31"),
                        max = as.Date("2022-01-01"),
                        value = c(as.Date("2018-12-31"), as.Date("2022-01-01")), 
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
            plotOutput("aqPlot"),
            br(),
            br(),
            textOutput("avg_desc")
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
    
    # Map for Input Pages - default locations = Seattle, LA, Chicago, Houston, Boston
    city_names <- c("Seattle", "Los Angeles", "Chicago", "Houston", "Boston")
    default_lat <- c(47.606209, 34.052235, 41.878113, 29.760427, 42.3601)
    default_lon <- c(-122.332069, -118.243683, -87.629799, -95.369804, -71.0589)
    output$usa_map <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>%
            setView(-94, 42, zoom = 3) %>%
            addMarkers(lng = default_lon, 
                       lat = default_lat,
                       layerId = city_names) 
    })
    

    # TODO: make data pull a module
    
#
# --- EVENT HANDLERS ---
#

    # Write map click for a city search
    observeEvent(input$usa_map_marker_click,{
        writeLines(as.character(input$usa_map_marker_click$id), "historic_aqi.txt")
        
        # go to: do a city search
        
        # go to: render df
    })
    
    # performs new search from text-input
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
            # Source: AQICN database on dbnomics
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
            } else if (confirm_request == "Entry not recognized. Please check for typos."){
                showModal(
                    modalDialog(title = "Sorry!",
                                "Entry not recognized. Please check for typos.",
                                footer = tagList(
                                    modalButton("Ok."))
                    )
                )
            } else {
                df_raw <- read.csv("pm25py.csv")
                df <- df_raw[2:3]
                colnames(df) <- c('Date', 'City1')  # as.character(confirm_request)
                df$Date <- as.Date(df$Date)
                output$aqPlot <- renderPlot({
                    
                    # render the plot with the data frame
                    ggplot(df, aes(Date, City1)) + 
                        geom_line() +
                        labs(x = "Date", y = "PM2.5", title = "Air quality trend for City 1") +
                        scale_x_date(date_labels = "%b-%Y") +
                        xlim(input$time_slide) +
                        ylim(input$aq_slide)
                    
                    #TODO: add dynamic legend & title
                    
                }, res = 96)
                
                ##
                ## microservice - Display the text equivalent for a given AQI - notionally converted from PM2.5 in this case
                ## using revised breakpoints here: https://aqicn.org/faq/2013-09-09/revised-pm25-aqi-breakpoints/
                ##
                
                pm25_avg <- mean(df$City1, na.rm = TRUE)
                
                write(pm25_avg, "pm25_avg.txt")
                subprocess$run('py .\\Weather_goodinel_mod.py')
                avg_desc <- readLines("response.txt")
                
                output$avg_desc <- renderText(
                    paste("The average air quality for the full data set was:",pm25_avg, ", which is", avg_desc))
                
                # go to the trend page
                change_page("trends")
                
            }
        }
    })
    
    # updates df with newly-requested city
    observeEvent(input$added_city,{
        
        if (input$add_zip == "") {
            # check if input blank
            showModal(
                modalDialog(title = "Error",
                            "Please enter a zip code or city name, or choose a city from the map.",
                            footer = tagList(
                                modalButton("Ok."))
                )
            )
        } else if (ncol(df) == 4) {
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
            } else if (confirm_request == "Entry not recognized. Please check for typos."){
                showModal(
                    modalDialog(title = "Sorry!",
                                "Entry not recognized. Please check for typos.",
                                footer = tagList(
                                    modalButton("Ok."))
                    )
                )
            } else{
                df_add <- read.csv("pm25py.csv")
                
                # Render the plot for 2 cities
                if (ncol(df) == 2){
                    colnames(df_add) <- c('Index', 'Date', 'City 2')
                    df <- cbind(df, df_add[3])
                    
                    # reshape dataframe to be able to plot two columns
                    df_2 <- data.frame(x = df$Date,
                                       y = c(df$City1, df$City2),
                                       group = c(rep("City1", nrow(df)),
                                                 rep("City2", nrow(df))))
                    
                    head(df_2)
                    
                    
                    output$aqPlot <- renderPlot({
                        # render the plot with the data frame
                        ggplot(df_2, aes(x, y, col = group, color=variable)) + 
                            geom_line() +
                            labs(x = "Date", y = "PM2.5", 
                                 title = "Air quality trend for - Chosen Cities") +
                            scale_x_date(date_labels = "%b-%Y") +
                            xlim(input$time_slide) +
                            ylim(input$aq_slide)
                        
                        
                    }, res = 96)

                    
                } else {
                 # Render the plot for 3 cities  
                    colnames(df_add) <- c('Index', 'Date', 'City 3')
                    df <- cbind(df, df_add[3])
                    
                    # reshape dataframe to be able to plot two columns
                    df_3 <- data.frame(x = df$Date,
                                       y = c(df$City1, df$City2, df$City3),
                                       group = c(rep("City1", nrow(df)),
                                                 rep("City2", nrow(df)),
                                                 rep("City3", nrow(df))))
                    head(df_3)
                    
                    output$aqPlot <- renderPlot({
                        # render the plot with the data frame
                        ggplot(df_3, aes(x, y, col = group, color=variable)) + 
                            geom_line() +
                            labs(x = "Date", y = "PM2.5", 
                                 title = "Air quality trend for - Chosen Cities") +
                            scale_x_date(date_labels = "%b-%Y") +
                            xlim(input$time_slide) +
                            ylim(input$aq_slide)
                        
                        
                    }, res = 96)
                }
                
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
