#
# OSU CS361: Intro to Software Development 1
# Individual Project
# Winter 2022
#
# Author: Maggie Liu
# Version: 1.2.0
# Description: A web app that allows a user to choose a city, and then takes 
#               air quality data for that city and displays a historical
#               trend. Users can add up to 3 cities to the same trend.
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
    headerPanel("Choose a city from the map below"),                            # TODO: camelCase everything :(
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
                                                                                # TODO: hover text / collapse definitions on pages. links to more info?
        # zip submission form
        fluidRow(
            column(width = 7, 
                   textInput("zip", 
                             "Or, enter your own city / 5-digit zip code:")),   # TODO: make submit button stick relative to input box on page resize
            column(5, style = "margin-top:30px;", 
                   actionButton("new_search", 
                                label = "Submit",
                                icon = NULL, 
                                width = NULL)))
    )
)

# Page to add more cities to existing trend
add_page <- div(                                                                # TODO: merge with initial search page? use reactive expressions / conditionalPanel()
    headerPanel("Choose another city from the map below"),
    mainPanel(
        h5("Choose another pre-selected city from the map, or enter a US ZIP code 
           to see air quality trends from that area."),
        p("TIP: Comparing more than 2 cities at once may make trends difficult
           to read.", style ="font-size:10pt;"),
        
        br(),
        
        # map with pre-selected cities to choose from
        #leafletOutput('usa_map'),  
        
        br(),                                                                   # TODO: trying to re-render the same map causes issues. 
        br(),                                                                   # TODO: also grey out city if already chosen? - conditional leaflet marker?
        
        # zip submission form
        textInput("add_zip", "Or, enter a 5-digit zip code:"), 
        actionButton("added_city", label = "Submit", icon = NULL, width = NULL),
        actionButton("back_to_trend", label = "Go Back", icon = NULL, width = NULL)
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
            
                                                                                # TODO: update range based on limits in data file unless doing a standard pull
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

#
# --- ROUTER -------------------------------------------------------------------
#
router <- make_router(
    route("/", new_search_page),
    route("trends", dash_page),
    route("add-city", add_page)
)

#
# --- SERVER MODULES -----------------------------------------------------------
#

# Check whether to make a request from the user input; if not, notify user
dataValidateServer <- function(id, user_text, city_list){
    moduleServer(id, function(input, output, session){
        
        # Check if input box left blank                                                             
        if (user_text == "") {                                                  
            showModal(
                modalDialog(title = "Error",
                            "Please enter a zip code or city name, or choose a city from the map.",
                            footer = tagList(modalButton("Ok."))
                )
            )                                                                    
        } else if (city_list$city_3 != "") {
            
            # Check if 3 cities already trended
            showModal(
                modalDialog(title = "Error",
                            "Only 3 cities can be trended at one time.",
                            footer = tagList(modalButton("Ok."))
                )
            )
        } else if (city_list$city_1 == user_text || city_list$city_2 == user_text){
            
            # Check if same city already requested
            showModal(
                modalDialog(title = "Error",
                            "That city is already on the trend. Please choose another.",
                            footer = tagList(modalButton("Ok."))
                )
            )
        } else {
            return(TRUE)
        }
        
        return(FALSE)
    })
}

# Perform search for air quality data
# Source: AQICN database on dbnomics
dataSearchServer <- function(id, given_location){
    moduleServer(id, function(input, output, session){
        
        # Request data
        writeLines(as.character(given_location), "historic_aqi.txt")
        subprocess$run('py .\\get_historic_pm25.py')
        
        # Check if the request was successful
        confirm_request <- readLines("historic_aqi.txt")
        if (confirm_request == "Location not found."){
            showModal(
                modalDialog(title = "Sorry!",
                            "No data found for that location. Please try another.",
                            footer = tagList(modalButton("Ok."))
                )
            )
        } else if (confirm_request == "Entry not recognized. Please check for typos."){
            showModal(
                modalDialog(title = "Sorry!",
                            "Entry not recognized. Please check for typos.",
                            footer = tagList(modalButton("Ok."))
                )
            )
        } else {
            # Load data from file into data frame
            df <- read.csv("pm25py.csv")
            df <- df[2:3]
            colnames(df) <- c('Date', as.character(confirm_request))  
            df$Date <- as.Date(df$Date)
            df <- na.omit(df)
            
            
            
            return(df)                                                          # TODO: df probably needs to be reactive to persist across queries
        }
    })
}

# Render the given data frame
plotServer <- function(id, df, map_aes, x_slide, y_slide){                       # TODO: add dynamic legend & title
    moduleServer(id, function(input, output, session){
        return(renderPlot({
            
            ggplot(df, map_aes) + 
                geom_line() +
                labs(x = "Date", y = "PM2.5", title = "Air quality trend for City 1") +
                scale_x_date(date_labels = "%b-%Y") +
                xlim(x_slide) +
                ylim(y_slide)                                                   # TODO: doesn't change axes when in the module vs in the server directly
            
        }, res = 96)) 
    })
}

# Implements teammate's microservice - takes PM2.5 value and returns text designation
# using revised breakpoints from here: https://aqicn.org/faq/2013-09-09/revised-pm25-aqi-breakpoints/ 
descriptionServer <- function(id, df){
    moduleServer(id, function(input, output, session){

        pm25_avg <- round(mean(df$Boston, na.rm = TRUE), digits = 0)
        
        write(pm25_avg, "pm25_avg.txt")
        subprocess$run('py .\\Weather_goodinel_mod.py')
        avg_desc <- readLines("response.txt")
        
        return( renderText(
            paste("The average air quality for the full data set was:",         # TODO: format this better
                  pm25_avg,", which is", avg_desc,".")
            )
        )
    })
}
                                                                                # TODO: fix City1/2/3. or have a reactive vector with city names and only display those
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
# --- SERVER -------------------------------------------------------------------
#
server <- function(input, output, session) {
    router$server(input, output, session)
    thematic::thematic_shiny()
    
    chosen_cities <- reactiveValues(city_1 = "", city_2 = "", city_3 = "", count = 0)
    
    # Interactive map for input pages with markers for default locations
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
    
#
# --- EVENT HANDLERS ------------------------------
#
    # Gather and render data from map click
    observeEvent(input$usa_map_marker_click,{
        if (dataValidateServer("usa_map_marker_click", input$usa_map_marker_click$id, chosen_cities) == TRUE){
            df <- dataSearchServer("usa_map_marker_click$id", 
                                 input$usa_map_marker_click$id)
            
            output$aqPlot <- plotServer("usa_map_marker_click$id", 
                                        df, aes(x = Date, y = Boston), 
                                        input$time_slide, 
                                        input$aq_slide)
            
            output$avg_desc <- descriptionServer("usa_map_marker_click$id", df)
            change_page("trends")
        }
    })
    
    # Gather and render data from user text input
    observeEvent(input$new_search,{
        if (dataValidateServer("new_search", input$zip, chosen_cities) == TRUE){
            df <- dataSearchServer("new_search", input$zip)
            output$aqPlot <- plotServer("new_search", df, 
                                        aes(Date, names(df[2])), 
                                        input$time_slide, 
                                        input$aq_slide)
            output$avg_desc <- descriptionServer("new_search", df)
            change_page("trends")   
        }
    })
    
    # updates df with newly-requested city
    observeEvent(input$added_city,{ 
        if (dataValidateServer("added_city", input$add_zip, 2)){
            df <- cbind(df, dataSearchServer("added_city", input$add_zip)[2])
            output$aqPlot <- plotServer("new_search", df, 
                                        aes(Date, names(df[2])), 
                                        input$time_slide, 
                                        input$aq_slide)
            
                                                                                # TODO: make group a parameter of plotServer? look up melt function
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
            output$avg_desc <- descriptionServer("added_city", df)
            change_page("trends") 
        }
    })
    
    # Start over but confirm choice first 
    observeEvent(input$start_over,{
        showModal(
            modalDialog(title = "Caution",
            "Starting a new search will clear all existing data.",
            footer = tagList(
                modalButton("Cancel"),
                actionButton("confirm_new", "Confirm new search")) 
            )
        )
        
        # Clear values and change page if new search confirmed
        observeEvent(input$confirm_new,{
            updateTextInput(session, "zip", value = "")
            change_page("/")
            updateSliderInput(session, "time_slide", 
                              value = c(as.Date("2018-12-31"), 
                                        as.Date("2022-01-01")))
            updateSliderInput(session, "aq_slide", value = c(0, 300))
            removeModal()
        })
    })
    
    # Go to the add-a-city screen
    observeEvent(input$add_city,{
        change_page("add-city")
    })
    
    # Return to trend screen from add-a-city screen
    observeEvent(input$back_to_trend,{
        change_page("trends")
    })
}

shinyApp(ui = ui, server = server)
