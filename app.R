#
# OSU CS361: Intro to Software Development 1
# Individual Project
# Winter 2022
#
# Author: Maggie Liu
# Version: 1.3.0
# Description: A web app that allows a user to choose a city, and then takes 
#               air quality data for that city and displays a historical
#               trend. Users can add up to 3 cities to the same trend.
#              

library(shiny)
library(shiny.router)
library(ggplot2)
library(reshape2)
library(leaflet)
library(reticulate)
library(Rcpp)
subprocess <- reticulate::import("subprocess")


#
# --- UI PAGES -----------------------------------------------------------------
#

# Search page 
search_page <- div(
    headerPanel("Choose a city from the map below"),                            
    mainPanel(
        
        # Instruction text
        h5("Choose a pre-selected city from the map, or enter the name or ZIP 
           code of a US city of your choice to see air quality trends from that 
           area. You can trend up to three cities."),
        br(),
        
        # Help tips depend on whether this is a new search or a city addition
        div(style ="font-size:10pt;",
          "TIP: Not every city has data available", 
          conditionalPanel(
              condition = "(input.add_city - input.back_to_trend) % 2 != 0",
              style ="font-size:10pt;",
              "TIP: Comparing more than 2 cities at once may make trends 
              difficult to read.")
          ),
        br(),
        
        # Map with pre-selected cities to choose from
        leafletOutput('usa_map', width = "100%"),    
        br(),
        br(),
        
        # Submission form for text entry
        fluidRow(
            column(width = 6, 
                   textInput("text_location", 
                             "Or, enter your own city / 5-digit zip code:")),   
            
            column(2, style = "margin-top:30px;", 
                   actionButton("search", 
                                label = "Submit",
                                icon = NULL, 
                                width = NULL)),
            
            column(2, style = "margin-top:30px;",
                   conditionalPanel(condition = "(input.add_city - 
                                    input.back_to_trend) % 2 != 0",
                                       actionButton("back_to_trend", 
                                                    label = "Go Back", 
                                                    icon = NULL, 
                                                    width = NULL)))
        )
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
            sliderInput("time_slide", "Time",
                        min = as.Date("2018-12-31"),
                        max = as.Date("2022-03-01"),
                        value = c(as.Date("2018-12-31"), as.Date("2022-03-01")), 
                        timeFormat = "%F"),
            
            sliderInput("aq_slide", "PM2.5",
                        min = 0,
                        max = 300,
                        value = c(0, 300)),
            
            actionButton("start_over", label = "New Search", icon = NULL),
            actionButton("add_city", label = "Add City", icon = NULL)
        ),
        
        # Display trend and text descriptions
        mainPanel(
            plotOutput("aq_plot"),
            br(),
            br(),
            textOutput("avg_desc_1"),                                            
            textOutput("avg_desc_2"),
            textOutput("avg_desc_3")
        )
    )
)

#
# --- ROUTER -------------------------------------------------------------------
#
router <- make_router(
    route("/", search_page),
    route("trends", dash_page)
)

#
# --- UI -----------------------------------------------------------------------
#
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "yeti"),
    title = "Historical Air Quality Trends",
    fluid = TRUE,
    router$ui
)

#
# --- SERVER MODULES -----------------------------------------------------------
#
main_server <- function(id, city, chosen, df) {
    moduleServer(id, function(input, output, session) {
        
        if (validate_input_server(id, city, chosen) == TRUE) {
            df_new <- fetch_data_server(id, city)
            
            if (!is.null(df_new)) {
                
                # Populate the reactive df or merge the new data with the old
                if(is.null(dim(df$vals))){
                    df$vals <- df_new
                } else {
                    df$vals <- merge(x = df$vals, y = df_new, all = TRUE)
                }
                
                update_city_list_server(id, chosen, city)
                
                # Prepare text output to be rendered in the main server
                desc_1 <- description_server(id, df$vals[, chosen$city_1], 
                                             chosen$city_1)
                cities <- c(chosen$city_1)
                desc_2 <- ""
                desc_3 <- ""
                
                if (chosen$city_2 != "") { 
                    cities <- c(chosen$city_1, chosen$city_2)
                    desc_2 <- description_server(id, df$vals[, chosen$city_2], 
                                                 chosen$city_2)
                }
                
                if (chosen$city_3 != "") {
                    cities <- c(chosen$city_1, chosen$city_2, chosen$city_3)
                    desc_3 <- description_server(id, df$vals[, chosen$city_3], 
                                                 chosen$city_3)
                }

                return(list(df$vals, cities, desc_1, desc_2, desc_3))
            }
        }
        return(NULL)
    })
}

# Check whether to make a request from the user input; if not, notify user
validate_input_server <- function(id, user_text, chosen) {
    moduleServer(id, function(input, output, session) {
        
        # Check if input box left blank                                                             
        if (user_text == "") {                                                  
            showModal(
                modalDialog(title = "Error",
                            "Please enter a zip code or city name, 
                            or choose a city from the map.",
                            footer = tagList(modalButton("Ok."))
                            )
                )                                                                    
        } else if (chosen$city_3 != "") {
            
            # Limit trend to 3 cities at once
            showModal(
                modalDialog(title = "Error",
                            "Only 3 cities can be trended at one time.",
                            footer = tagList(modalButton("Ok."))))
            
        } else if (chosen$city_1 == user_text || chosen$city_2 == user_text) {
            
            # Check if same city already requested
            showModal(
                modalDialog(title = "Error", "That city is already on the trend. 
                            Please choose another.",
                            footer = tagList(modalButton("Ok."))))
        } else {
            return(TRUE)
        }
        return(FALSE)
    })
}

# Perform search for air quality data. Source: AQICN database on dbnomics
fetch_data_server <- function(id, given_location) {
    moduleServer(id, function(input, output, session) {
        
        # Request data
        writeLines(as.character(given_location), "historic_aqi.txt")
        subprocess$run('py .\\get_historic_pm25.py')
        
        # Check if the request was successful
        confirm_request <- readLines("historic_aqi.txt")
        if (confirm_request == "Location not found.") {
            showModal(modalDialog(title = "Sorry!",
                                  "No data found for that location. 
                                  Please try another.",
                                  footer = tagList(modalButton("Ok."))))
        
        } else if (confirm_request == "Entry not recognized.") {
            showModal(modalDialog(title = "Sorry!",
                                  "Entry not recognized. Please check for typos.",
                                  footer = tagList(modalButton("Ok."))))
        
        } else {
            # Load data from file into data frame
            df <- read.csv("pm25py.csv")
            df <- df[,2:3]
            colnames(df) <- c('Date', as.character(confirm_request))  
            df$Date <- as.Date(df$Date)
            df <- na.omit(df)
            return(df)                                                          
        }
        return(NULL)  
    })
}

# Update the names of cities chosen in the reactive ledger
update_city_list_server <- function(id, chosen, city) {
    moduleServer(id, function(input, output, session) {

        if (chosen$city_1 == "") {
            chosen$city_1 = city
        } else if (chosen$city_2 == "") {
            chosen$city_2 = city
        } else {
            chosen$city_3 = city
        }
    })
}

# Generate a plot for the given data frame                                                   
plot_server <- function(id, df, city_names, time_slide, aq_slide) {                       
    moduleServer(id, function(input, output, session) {
        
        melty_df <- melt(df, id = "Date") 
        places <- paste(city_names, collapse = ", ")
        
        plot <- ggplot(melty_df, aes(Date, value)) + 
            geom_point(aes(color = variable, group = variable)) +
            labs(x = "Date", y = "PM2.5", 
                 title = paste("Air Quality in", places), color = "City") +                       
            scale_x_date(date_labels = "%b-%Y") +
            xlim(time_slide()) +
            ylim(aq_slide())
        
        return(plot)
    })
}

# Uses teammate's microservice to take PM2.5 value and return text description.
# Breakpoints from: https://aqicn.org/faq/2013-09-09/revised-pm25-aqi-breakpoints/ 
description_server <- function(id, city_data, city) {
    moduleServer(id, function(input, output, session) {

        pm25_avg <- round(mean(city_data, na.rm = TRUE), digits = 0)
        
        write(pm25_avg, "pm25_avg.txt")
        subprocess$run('py .\\Weather_goodinel_mod.py')
        avg_desc <- readLines("response.txt")                                   

        return(paste("The average air quality for", city,"was", 
                     pm25_avg,"μg/m^3. This is", avg_desc,".")
        )
    })
}

#
# --- SERVER -------------------------------------------------------------------
#
server <- function(input, output, session) {
    router$server(input, output, session)
    thematic::thematic_shiny()
    
    # Interactive map for input pages with markers for default locations
    city_names <- c("Seattle", "Los Angeles", "Chicago", "Houston", "Boston")
    default_lat <- c(47.606209, 34.052235, 41.878113, 29.760427, 42.3601)
    default_lon <- c(-122.332069, -118.243683, -87.629799, -95.369804, -71.0589)
    output$usa_map <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>%
            setView(-94, 42, zoom = 3) %>%
            addCircleMarkers(lng = default_lon, lat = default_lat,
                             layerId = city_names) 
    })
    
    # Keep track of chosen cities for this session
    chosen <- reactiveValues(city_1 = "", city_2 = "", city_3 = "")
    
    # Keep track of data loaded for this session
    df <- reactiveValues(vals = NULL)
    
#
# --- EVENT HANDLERS -------------------------------------
#
    # Gather and render data from map click
    observeEvent(input$usa_map_marker_click, {
        id <- "usa_map_marker_click"
        results <- main_server(id, input$usa_map_marker_click$id, chosen, df)
        
        if (!is.null(results)) {
            df$vals <- results[[1]]
            
            # Associate plot and description objects with an output to render
            output$avg_desc_1 <- renderText(results[[3]][1])
            output$avg_desc_2 <- renderText(results[[4]][1])
            output$avg_desc_3 <- renderText(results[[5]][1])
            
            output$aq_plot <- renderPlot({plot_server(id, 
                                                      df$vals, 
                                                      results[[2]], 
                                                      reactive(input$time_slide), 
                                                      reactive(input$aq_slide))}, 
                                        res = 96)
            change_page("trends")
        } 
    })                                                                          
    
    # Gather and render data from user text input
    observeEvent(input$search, {
        id <- "search"
        results <- main_server(id, input$text_location, chosen, df)
        
        if (!is.null(results)) {
            df$vals <- results[[1]]
            
            # Associate plot and description objects with an output to render
            output$avg_desc_1 <- renderText(results[[3]][1])
            output$avg_desc_2 <- renderText(results[[4]][1])
            output$avg_desc_3 <- renderText(results[[5]][1])
            
            output$aq_plot <- renderPlot({plot_server(id, 
                                                      df$vals, 
                                                      results[[2]], 
                                                      reactive(input$time_slide), 
                                                      reactive(input$aq_slide))}, 
                                         res = 96)
            change_page("trends")
        } 
    })
    
    # Start over with new search, but confirm choice first 
    observeEvent(input$start_over, {
        showModal(
            modalDialog(title = "Caution",
            "Starting a new search will clear all existing data.",
            footer = tagList(modalButton("Cancel"),
                             actionButton("confirm_new", "Confirm new search")))
        )
    })
    
    # Clear values and change page if new search confirmed
    observeEvent(input$confirm_new, {
        updateTextInput(session, "text_location", value = "")
        
        chosen$city_1 <- ""
        chosen$city_2 <- ""
        chosen$city_3 <- ""
        
        df$vals <- NULL
        
        updateSliderInput(session, "time_slide", 
                          value = c(as.Date("2018-12-31"), 
                                    as.Date("2022-03-01")))
        updateSliderInput(session, "aq_slide", value = c(0, 300))
        
        change_page("/")
        removeModal()
    })
    
    # Go to the search page
    observeEvent(input$add_city, {
        updateTextInput(session, "text_location", value = "")
        change_page("/")
    })
    
    # Return to trend page 
    observeEvent(input$back_to_trend, {
        change_page("trends")
    })
}

shinyApp(ui = ui, server = server)
