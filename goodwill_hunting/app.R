#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# options(scipen = 999) prevents numbers from appearing in scientific notation
knitr::opts_chunk$set(options(scipen = 999)) 


library(shiny)
library(shinythemes)
library(tidyverse)
library(gt)
library(janitor)
library(leaflet)
library(plotly)
library(gt)
library(sf)

# loading data (please fckn work holy shit)

good <- read.csv("goodwill_regions.csv") %>%
    drop_na() # they were in there and causin problems so...snip snip

zip_latlon <- read.csv("US Zip Codes from 2013 Government Data") %>%
    clean_names()

zillow <- read.csv("zillow_regions.csv")




# Define UI for application 
ui <- fluidPage(
    
    # set project title
    navbarPage("Goodwill Hunting",
               
               # TAB: MAP
               tabPanel(("Map - Explore"),         
                        # Show a plot of the generated distribution
                        fillPage(textInput("zip_input", "Input Zip Code"),
                                 selectInput("q_input", "Select Housing Price Quartile", choices = c("All",
                                                                                                     "q1",
                                                                                                     "q2",
                                                                                                     "q3",
                                                                                                     "q4")),
                                 leafletOutput("goodwill_map", height=500))),                                  
               
               
               # TAB: STATE BY STATE
               
               tabPanel("Search By State",
                        
                        mainPanel(textInput("state_input3", "Input State Code"),
                                  gt_output("by_state"))),
               
               
               # TAB: PLOTS
               tabPanel("Analysis",
                        tabsetPanel(
                            tabPanel("Yelp Analysis",
                                     
                                     mainPanel(textInput("state_input_yelp", "Input State Code", value = "USA"),
                                               plotlyOutput("yelp_plot"))
                                     
                                     ),
                            tabPanel("Housing Price Analysis",
                                     
                                     fillPage( 
                                         sliderInput("x_good", "Housing Price Range", min = 0, max = 3000000, step = 10000, value = 300000),
                                         selectInput("region_input", "Select Region", choices = c("United States", 
                                                                                                  "South",
                                                                                                  "Midwest",
                                                                                                  "West",
                                                                                                  "Northeast")), 
                                         fluidRow(
                                             splitLayout(cellWidths = c("50%", "50%"), 
                                                         plotlyOutput("goodwills_by_housing_price"),
                                                         plotlyOutput("overall_housing_price"))),
                                         fluidRow(
                                             gt_output("zillow_gt"),
                                             h1(""), # to insert a space between the gt tables
                                             gt_output("goodwill_gt"))
                                         
                                     )))),
               
               # TAB: ABOUT
               tabPanel(("About"),
                        h2("About This Project"),
                        tags$div("It is a well-known fact in the thrifting community that the bougier the neighborhood, the badder the donations, the better the finds. In other words, since most donation-based thrift stores like Goodwill and the Salvation Army source from the immediate surrounding areas, it logically follows that wealthier neighborhoods will tend to have nicer items. This (forthcoming) tool will allow users to optimize their thrift store selections by identifying which Goodwills near them are in neighborhoods with higher average real estate prices, which tends to correspond to household income. It will also (hopefully) do the same for store reviews, and identify any relationship between store rating and neighborhood income. My hypothesis is that stores in wealthier neighborhoods will tend to be more highly rated. "),
                        h2("How to Navigate the Project"), 
                        h2("About Me"),
                        tags$div("Introduction"),
                        h2("Data Sources"),
                        tags$div("Zillow, Goodwill (scraped locations), Region Data, Zip code coordinates,
                      Yelp (manual sample scrape, tbd)")),
               
               # TAB: MAP - SEARCH
               
               tabPanel(("Map - Search"),
                        
                        mainPanel(textInput("zip_input2", "Input Zip Code"),
                                  textInput("state_input2", "Input State Code"),
                                  leafletOutput("goodwill_map2", height = 500)),
                        sidebarPanel(dataTableOutput("goodwill_search")))
               ))






# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    # color factor
    
    q_color <- colorFactor(pal = c("#1b9e77", "#d95f02", "#7570b3", "coral1"), 
                           domain = c("q1", "q3", "q2", "q4"))
    
    
    
    # plot all goodwill locations with leaflet
    output$goodwill_map <- renderLeaflet({
        
        zlat <- zip_latlon %>%
            filter(zip == input$zip_input) %>%
            pull(lat)
        
        zlon <- zip_latlon %>%
            filter(zip == input$zip_input) %>%
            pull(lng)
        
        if(input$q_input == "All") {
            
            good <- good
            
        }
        
        else {   
            good <- good %>%
                filter(quartile == input$q_input)
        }
        
        if(input$zip_input == "") {
            (leaflet(good) %>% 
                 addCircles(lng = ~lon, lat = ~lat) %>% 
                 addTiles() %>%
                 addCircleMarkers(data = good, lat =  ~lat, lng = ~lon, 
                                  radius = 1, color = ~q_color(quartile), opacity = .9) %>%
                 addLegend(pal = q_color, values=good$quartile, opacity=1) %>%
                 
                 # set zoom to continental USA (temporary)
                 setView(lat = 39.8283
                         , lng = -98.5795 # coordinates for the middle of the country according to google
                         , zoom = 4 )
            )}
        
        else {(
            
            leaflet(good) %>% 
                addTiles() %>%
                addCircleMarkers(data = good, lat =  ~lat, lng = ~lon, 
                                 radius = 1, opacity = .9, color = ~q_color(quartile)) %>%
                addLegend(pal = q_color, values=good$quartile, opacity=1) %>%
                
                # set zoom to zip code
                setView(lat = zlat
                        , lng = zlon # coordinates for the middle of the country according to google
                        , zoom = 12 )
        )}
    })
    
    # Round 2 motherfuckers
    
    output$goodwill_map2 <- renderLeaflet({
        
        zlat2 <- zip_latlon %>%
            filter(zip == input$zip_input2) %>%
            pull(lat)
        
        zlon2 <- zip_latlon %>%
            filter(zip == input$zip_input2) %>%
            pull(lng)
        
        if(input$zip_input2 == "") {
            (
                
                leaflet(good) %>% 
                    addTiles() %>%
                    addCircleMarkers(data = good, lat =  ~lat, lng = ~lon, 
                                     radius = 1, color = ~q_color(quartile), opacity = .9) %>%
                    addLegend(pal = q_color, values=good$quartile, opacity=1) %>%
                    
                    # set zoom to continental USA (temporary)
                    setView(lat = 39.8283
                            , lng = -98.5795 # coordinates for the middle of the country according to google
                            , zoom = 4 )
            )}
        
        else {(
            
            leaflet(good) %>% 
                addCircles(lng = ~lon, lat = ~lat) %>% 
                addTiles() %>%
                addCircleMarkers(data = good, lat =  ~lat, lng = ~lon, 
                                 radius = 1, color = ~q_color(quartile)) %>%
                addLegend(pal = q_color, values=good$quartile, opacity=1) %>%
                
                # set zoom to zip code
                setView(lat = zlat2
                        , lng = zlon2 # coordinates for the middle of the country according to google
                        , zoom = 12 )
        )}
    })
    
    # this search function is gonna fckn kill me 
    
    zlat3 <- reactive(zip_latlon %>%
                          filter(zip == input$zip_input2) %>%
                          pull(lat))
    
    zlon3 <- reactive(zip_latlon %>%
                          filter(zip == input$zip_input2) %>%
                          pull(lng))
    
    point <- reactive(data_frame(mylon = zlon3, mylat = zlat3))
    
    point_sf <- reactive(st_as_sf(point, coords = c("mylon", "mylat"), crs = 4326))
    
    target <- reactive(good %>%
                           filter(state == input$state_input2) %>%
                           select(lat, lon) %>%
                           drop_na())
    
    target_sf <- reactive(st_as_sf(target, coords = c("lon", "lat"), crs = 4326))
    
    target_sf2 <- reactive(target_sf %>%
                               mutate(Dist = as.numeric(st_distance(point_sf, target_sf, by_element = TRUE))))
    
    target_sf2 <- reactive(target_sf2 %>%
                               # Filter the records with Dist <= 10 miles in meters
                               filter(Dist <= 16093.4))
    
    output$goodwill_search <- renderDataTable({
        
        expr = target_sf2
        
    })
    
    
    # State by State
    
    output$by_state <- render_gt(
        
        if(input$state_input3 == "") {
            
            good_gt <- good %>%
                arrange(desc(x2020_01)) %>%
                select(address, state, city, zip, x2020_01) %>%
                slice(1:15)
            
            good_gt <- tibble::rowid_to_column(good_gt, "Rank")

            good_gt %>%
                gt() %>%
                cols_label(address = "Address",
                           state = "State",
                           city = "City",
                           zip = "Zip Code",
                           x2020_01 = "Average House Price in Zip Code")
            
        }
        
        else {
            good_gt <- good %>%
                filter(state == input$state_input3) %>%
                arrange(desc(x2020_01)) %>%
                select(address, state, city, zip, x2020_01)
            
            good_gt <- tibble::rowid_to_column(good_gt, "Rank")
            
            good_gt %>%
                gt() %>%
                cols_label(address = "Address",
                           state = "State",
                           city = "City",
                           zip = "Zip Code",
                           x2020_01 = "Average House Price in Zip Code")
    })
    
    
    # lets try this plotting thing shall we
    
    output$goodwills_by_housing_price <- renderPlotly({
        if(input$region_input == "United States") {
            ggplotly(good %>%
                         ggplot(aes(x = x2020_01, fill = region)) +
                         geom_bar(binwidth = 20000) +
                         theme_classic() +
                         xlim(0, input$x_good) +
                         labs(x = "Housing Price in Location Zipcode",
                              y = "Number of Locations by Housing Price",
                              title = "Distribution of Goodwill Locations \nby Zip Code Housing Price",
                              fill = "Region"))
            
        }
        
        else {
            ggplotly(good %>%
                         filter(region == input$region_input) %>%
                         ggplot(aes(x = x2020_01)) +
                         geom_bar(binwidth = 20000, fill = "darkturquoise") +
                         theme_classic() +
                         xlim(0, input$x_good) +
                         labs(x = "Housing Price in Location Zipcode",
                              y = "Number of Locations by Housing Price",
                              title = "Distribution of Goodwill Locations \nby Zip Code Housing Price",
                              subtitle = paste("In the", input$region_input, sep = " ")))
        }
    })
    
    # overall housing price plot
    
    output$overall_housing_price <- renderPlotly({
        
        if(input$region_input == "United States") {
            ggplotly(zillow %>%
                         ggplot(aes(x = x2020_01, fill = region)) +
                         geom_bar(binwidth = 20000) +
                         theme_classic() +
                         xlim(0, input$x_good) +
                         labs(x = "Housing Price",
                              y = "Number of Zipcodes",
                              title = "Distribution of Zip Codes by Housing Price",
                              subtitle = paste("In the", input$region_input, sep = " ")))
        }
        
        else {
            ggplotly(zillow %>%
                         filter(region == input$region_input) %>%
                         ggplot(aes(x = x2020_01)) +
                         geom_bar(binwidth = 20000, fill = "coral1") +
                         theme_classic() +
                         xlim(0, input$x_good) +
                         labs(x = "Housing Price",
                              y = "Number of Zipcodes",
                              title = "Distribution of Zip Codes by Housing Price",
                              subtitle = paste("In the", input$region_input, sep = " ")))
            
        }
    })
    
    # Summary statistics for overall zipcode housing price average
    
    output$zillow_gt <- render_gt(
        zillow %>%
            summarize(min = min(x2020_01),
                      mean = mean(x2020_01),
                      q1 = quantile(x2020_01, c(.25)),
                      median = median(x2020_01),
                      q3 = quantile(x2020_01, c(.75)),
                      max = max(x2020_01)) %>%
            gt() %>%
            cols_label(min = "Minimum",
                       mean = "Average",
                       q1 = "First Quartile",
                       median = "Median",
                       q3 = "Third Quartile",
                       max = "Maximum") %>%
            tab_header(title = "Summary Statistics: Nationwide Average Home Prices by Zipcode")
    )
    
    # Summary statistics for goodwill zipcode housing price average
    
    output$goodwill_gt <- render_gt(
        
        good %>%
            summarize(min = min(x2020_01),
                      mean = mean(x2020_01),
                      q1 = quantile(x2020_01, c(.25)),
                      median = median(x2020_01),
                      q3 = quantile(x2020_01, c(.75)),
                      max = max(x2020_01)) %>%
            gt() %>%
            cols_label(min = "Minimum",
                       mean = "Average",
                       q1 = "First Quartile",
                       median = "Median",
                       q3 = "Third Quartile",
                       max = "Maximum") %>%
            tab_header(title = "Summary Statistics: Average Home Prices in Nationwide Goodwill Zipcodes")
    )
    
    
    # Yelp plots
    
    output$yelp_plot <- renderPlotly({
        
        if(input$state_input_yelp == "" | input$state_input_yelp == "USA") {
            
            goodwill_ratings %>%
                drop_na(x2020_01, rating) %>%
                ggplot(aes(y = rating, x = x2020_01, color = region)) +
                xlim(0,1000000) +
                geom_point() +
                geom_smooth(method = "glm", level = 0, aes(group = 1), color = "black") +
                geom_smooth(level = 0, aes(group = 1), color = "black") +
                theme_classic() +
                labs(x = "Average Housing Price in Store Zip Code",
                     y = "Yelp Star Rating",
                     title = "Relationship Between Neighborhood Housing Price and Yelp Star Rating",
                     color = "Region")
            
        }
        
        else {
            
            goodwill_ratings %>%
                drop_na(x2020_01, rating) %>%
                filter(state == input$state_input_yelp) %>%
                ggplot(aes(y = rating, x = x2020_01)) +
                xlim(0,1000000) +
                geom_point(color = "blue") +
                geom_smooth(method = "glm", level = 0, aes(group = 1), color = "black") +
                theme_classic() +
                labs(x = "Average Housing Price in Store Zip Code",
                     y = "Yelp Star Rating",
                     title = "Relationship Between Neighborhood Housing Price and Yelp Star Rating",
                     color = "Region")
        }
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
